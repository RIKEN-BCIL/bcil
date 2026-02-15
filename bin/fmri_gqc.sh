#!/usr/bin/env bash

# fMRI group QC
# Takuya Hayashi, RIKEN BDR
# Copyright (c) 2022- RIKEN


set -euo pipefail

OutputFolder="fmri_gqc_out"
BASELINE_LABEL_SUBSTR="hppd2"
BASELINE_BASE="raw"
BASELINE_METHOD="poly"
BASELINE_PARAM="2"
INCLUDE_CLEAN=0
VERBOSE=0

die(){ echo "[ERROR] $*" >&2; exit 1; }
log(){ [[ "$VERBOSE" -eq 1 ]] && echo "[INFO] $*" >&2 || true; }

usage(){
  cat >&2 <<'USAGE'
Usage:
  fmri_gqc.sh --outdir <dir> [--phantom <qc_dir> ...] [--include-clean] [--verbose] <qc_dir> ...

Inputs:
  Provide .qc output folders from fmri_qc.sh directly.
  Each .qc folder should contain fmri_filter_metrics.tsv.

  --phantom <qc_dir> ...  : Specify phantom .qc folders as reference
                            (multiple --phantom flags or space-separated before next option)

Outputs (in OutputFolder):
  group_qc.tsv                 (baseline non-clean)
  group_qc_clean.tsv           (baseline clean; only if --include-clean and data exist)
  group_qc_full.tsv            (all rows from all inputs)
  group_qc_phantom.tsv         (phantom baseline; only if --phantom)
  group_summary_*.tsv          (detailed)
  phantom_comparison_*.tsv     (phantom vs in-vivo comparison, if --phantom)
  index.html                   (entry page like fmri_qc.sh style)
  plots/*.png

Baseline (default):
  label contains "hppd2"
  base="raw", method="poly", param="2"
  non-clean for group_qc.tsv
USAGE
  exit 2
}

ARGS=()
PHANTOM_ARGS=()
while [[ $# -gt 0 ]]; do
  case "$1" in
    --outdir) OutputFolder="$2"; shift 2;;
    --phantom)
      shift
      while [[ $# -gt 0 && "$1" != --* ]]; do
        PHANTOM_ARGS+=("$1"); shift
      done
      ;;
    --include-clean) INCLUDE_CLEAN=1; shift;;
    --verbose) VERBOSE=1; shift;;
    -h|--help) usage;;
    -*) die "Unknown option: $1";;
    *) ARGS+=("$1"); shift;;
  esac
done
[[ ${#ARGS[@]} -ge 1 ]] || usage

BCILDIR=$(cd $(dirname $0); cd ..; pwd)
source $BCILDIR/bcilconf/settings.sh
command="$0 $@"

mkdir -p $OutputFolder/.files

echo "" >> $OutputFolder/command.txt
echo "--------------------" >> $OutputFolder/command.txt
echo "$(date -R)" >> $OutputFolder/command.txt
echo "$command" >> $OutputFolder/command.txt
echo "" >> $OutputFolder/command.txt

cp ${FSLDIR}/doc/fsl.css ${BCILDIR}/doc/images/BCIL_1.png ${BCILDIR}/doc/images/fsl-logo-x2.png ${BCILDIR}/doc/images/hcplogo1.jpg ${BCILDIR}/doc/images/freesurfer.png ${BCILDIR}/doc/images/bmb.png ${BCILDIR}/doc/images/favicon.ico ${BCILDIR}/doc/hcppipe_qc/magnifier.css ${BCILDIR}/doc/hcppipe_qc/magnifier.js ${BCILDIR}/doc/hcppipe_qc/jquery.min.js ${BCILDIR}/doc/hcppipe_qc/lightbox.css ${BCILDIR}/doc/hcppipe_qc/lightbox.min.js $OutputFolder/.files/

mkdir -p "$OutputFolder"/plots

# ---- Find filter_metrics.tsv inside a .qc folder ----
find_tsv_in_qcdir () {
  local qcdir="$1"
  local found=""
  for candidate in \
    "${qcdir}/fmri_filter_metrics.tsv" \
    "${qcdir}/filter_metrics.tsv" ; do
    if [[ -f "$candidate" ]]; then
      found="$candidate"
      break
    fi
  done
  # Fallback: glob for *_filter_metrics.tsv
  if [[ -z "$found" ]]; then
    for candidate in "${qcdir}"/*_filter_metrics.tsv ; do
      if [[ -f "$candidate" ]]; then
        found="$candidate"
        break
      fi
    done
  fi
  echo "$found"
}

# ---- Gather in-vivo TSVs ----
declare -a TSVS=()
for p in "${ARGS[@]}"; do
  if [[ -d "$p" ]]; then
    tsv=$(find_tsv_in_qcdir "$p")
    if [[ -n "$tsv" ]]; then
      TSVS+=("$tsv")
    else
      log "No filter_metrics.tsv found in: $p"
    fi
  elif [[ -f "$p" && "$(basename "$p")" == *filter_metrics.tsv ]]; then
    TSVS+=("$p")
  else
    log "Skipping non-existent or unrecognized path: $p"
  fi
done
[[ ${#TSVS[@]} -gt 0 ]] || die "No fmri_filter_metrics.tsv found from inputs."
log "Found ${#TSVS[@]} in-vivo tsv files."

# ---- Gather phantom TSVs ----
declare -a PHANTOM_TSVS=()
for p in "${PHANTOM_ARGS[@]}"; do
  if [[ -d "$p" ]]; then
    tsv=$(find_tsv_in_qcdir "$p")
    if [[ -n "$tsv" ]]; then
      PHANTOM_TSVS+=("$tsv")
    else
      log "No filter_metrics.tsv found in phantom dir: $p"
    fi
  elif [[ -f "$p" && "$(basename "$p")" == *filter_metrics.tsv ]]; then
    PHANTOM_TSVS+=("$p")
  else
    log "Skipping non-existent phantom path: $p"
  fi
done

HAS_PHANTOM=0
if [[ ${#PHANTOM_TSVS[@]} -gt 0 ]]; then
  HAS_PHANTOM=1
  log "Found ${#PHANTOM_TSVS[@]} phantom tsv files."
fi

# ---- Build tagged TSV list for Python ----
declare -a ALL_TSVS=()
for t in "${TSVS[@]}"; do
  ALL_TSVS+=("INVIVO:${t}")
done
for t in "${PHANTOM_TSVS[@]}"; do
  ALL_TSVS+=("PHANTOM:${t}")
done

python3 - <<'PY' "$OutputFolder" "$BASELINE_LABEL_SUBSTR" "$BASELINE_BASE" "$BASELINE_METHOD" "$BASELINE_PARAM" "$INCLUDE_CLEAN" "$HAS_PHANTOM" "${ALL_TSVS[@]}"
import sys, os, re
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

OutputFolder = sys.argv[1]
label_sub = sys.argv[2]
base_req = sys.argv[3].lower()
method_req = sys.argv[4].lower()
param_req = sys.argv[5]
include_clean = int(sys.argv[6])
has_phantom = int(sys.argv[7])
tagged_tsvs = sys.argv[8:]

plots_dir = os.path.join(OutputFolder, "plots")
os.makedirs(plots_dir, exist_ok=True)

# ---- Parse tagged TSV list ----
invivo_tsvs = []
phantom_tsvs = []
for t in tagged_tsvs:
    if t.startswith("PHANTOM:"):
        phantom_tsvs.append(t[len("PHANTOM:"):])
    elif t.startswith("INVIVO:"):
        invivo_tsvs.append(t[len("INVIVO:"):])
    else:
        invivo_tsvs.append(t)

def parse_subject_run(tsv_path: str):
    parts = tsv_path.split(os.sep)
    subj = None
    run = None
    for p in parts:
        if re.match(r"^A\d{8}$", p):
            subj = p
        if p.endswith(".qc") and p.startswith("BOLD_REST_"):
            run = p[:-3]
    year = None
    if subj and len(subj) >= 3:
        yy = int(subj[1:3])
        year = 2000 + yy
    pe = None
    if run:
        m = re.search(r"_(RL|LR|AP|PA)$", run)
        if m: pe = m.group(1)
    return subj, run, year, pe

def coerce_numeric(df):
    for c in df.columns:
        if c in ("label","base","method","param"):
            continue
        df[c] = pd.to_numeric(df[c], errors="coerce")
    return df

def read_tsvs(tsv_list, data_type="invivo"):
    dfs = []
    for tsv in tsv_list:
        try:
            df = pd.read_csv(tsv, sep="\t", dtype=str)
        except Exception as e:
            print(f"[WARN] Failed to read {tsv}: {e}", file=sys.stderr)
            continue
        if "label" not in df.columns:
            if "filter" in df.columns:
                df = df.rename(columns={"filter":"label"})
            else:
                print(f"[WARN] No label column in {tsv}", file=sys.stderr)
                continue

        df = coerce_numeric(df)
        subj, run, year, pe = parse_subject_run(tsv)
        df["tsv_path"] = tsv
        df["data_type"] = data_type
        df["subject"] = subj if subj else ""
        df["run"] = run if run else os.path.basename(os.path.dirname(tsv))
        df["year"] = float(year) if year else np.nan
        df["pe"] = pe if pe else ""
        df["is_clean"] = df["label"].astype(str).str.contains(r"_clean$", regex=True)
        dfs.append(df)
    return dfs

dfs_invivo = read_tsvs(invivo_tsvs, "invivo")
dfs_phantom = read_tsvs(phantom_tsvs, "phantom")

full = pd.concat(dfs_invivo + dfs_phantom, ignore_index=True)
full_out = os.path.join(OutputFolder, "group_qc_full.tsv")
full.to_csv(full_out, sep="\t", index=False)

invivo_full = full[full["data_type"] == "invivo"].copy()
phantom_full = full[full["data_type"] == "phantom"].copy()

# ---- baseline selector ----
def select_baseline(df, is_clean: bool):
    sel = df.copy()
    sel["base_l"] = sel["base"].astype(str).str.lower()
    sel["method_l"] = sel["method"].astype(str).str.lower()
    sel["param_s"] = sel["param"].astype(str)
    if is_clean:
        method_need = f"{method_req}-clean"
    else:
        method_need = method_req

    return sel[
        sel["label"].astype(str).str.contains(label_sub, case=False, regex=False)
        & (sel["base_l"] == base_req)
        & (sel["method_l"] == method_need)
        & (sel["param_s"] == param_req)
        & (sel["is_clean"] == bool(is_clean))
    ].copy()

def mean_cols(df, cols):
    cols = [c for c in cols if c in df.columns]
    if not cols: return np.nan
    return df[cols].mean(axis=1, skipna=True)

def robust_z(x):
    x = np.asarray(x, float)
    med = np.nanmedian(x)
    mad = np.nanmedian(np.abs(x - med))
    if not np.isfinite(mad) or mad == 0:
        return np.full_like(x, np.nan)
    return 0.6745 * (x - med) / mad

def add_derived(d):
    d["ADEV_short"] = mean_cols(d, ["adevN_tau10","adevN_tau30","adevN_tau60"])
    d["ADEV_long"]  = mean_cols(d, ["adevN_tau200","adevN_tau300","adevN_tau600","adevN_tau1200"])
    for col in ["ADEV_long","ADEV_short","tSNR","lag1"]:
        if col in d.columns:
            d[f"rz_{col}"] = robust_z(pd.to_numeric(d[col], errors="coerce").to_numpy())
    def flag_row(r):
        z = []
        for c in ("rz_ADEV_long","rz_tSNR","rz_lag1"):
            if c in r and np.isfinite(r[c]):
                z.append(abs(r[c]))
        if not z: return "NA"
        m = max(z)
        if m > 5: return "EXCLUDE"
        if m > 3: return "REVIEW"
        return "KEEP"
    d["flag"] = d.apply(flag_row, axis=1)
    return d

def keep_cols(d):
    cols = [
        "subject","run","year","pe","data_type",
        "label","base","method","param",
        "tSNR","lag1","ADEV_short","ADEV_long",
        "drift_slope_pct_per_min",
        "LFP_0p005","LFP_0p01",
        "log10_ratio_lt0p005_to_0p005_0p01","log10_ratio_lt0p01_to_0p01_0p1",
        "rz_ADEV_long","rz_tSNR","rz_lag1",
        "flag","tsv_path"
    ]
    return [c for c in cols if c in d.columns]

def write_group(d, outname):
    if d.empty:
        return None
    d = add_derived(d)
    d = d[keep_cols(d)].sort_values(["year","subject","run"], na_position="last")
    outpath = os.path.join(OutputFolder, outname)
    d.to_csv(outpath, sep="\t", index=False)
    return outpath, d

# ---- plot helpers ----
def save_hist(d, col, fname, title):
    if col not in d.columns: return
    x = pd.to_numeric(d[col], errors="coerce")
    x = x[np.isfinite(x)]
    if len(x) == 0: return
    plt.figure(figsize=(7,4))
    plt.hist(x, bins=20)
    plt.xlabel(col); plt.ylabel("count"); plt.title(title)
    plt.tight_layout()
    plt.savefig(os.path.join(plots_dir, fname), dpi=150)
    plt.close()

def save_scatter_year(d, col, fname, title):
    if col not in d.columns or "year" not in d.columns: return
    dd = d[["year",col]].copy()
    dd[col] = pd.to_numeric(dd[col], errors="coerce")
    dd = dd[np.isfinite(dd[col]) & np.isfinite(dd["year"])]
    if dd.empty: return
    plt.figure(figsize=(7,4))
    plt.scatter(dd["year"], dd[col])
    plt.xlabel("year"); plt.ylabel(col); plt.title(title)
    plt.grid(True, alpha=0.3)
    plt.tight_layout()
    plt.savefig(os.path.join(plots_dir, fname), dpi=150)
    plt.close()

def save_box_by(d, col, by, fname, title):
    if col not in d.columns or by not in d.columns: return
    dd = d[[col,by]].copy()
    dd[col] = pd.to_numeric(dd[col], errors="coerce")
    dd = dd[np.isfinite(dd[col])]
    if dd.empty: return
    cats = [c for c in sorted(dd[by].dropna().unique()) if str(c)!=""]
    if not cats: return
    data = [dd.loc[dd[by]==c, col].to_numpy() for c in cats]
    plt.figure(figsize=(max(7, 0.7*len(cats)),4))
    plt.boxplot(data, labels=[str(c) for c in cats], showfliers=True)
    plt.xlabel(by); plt.ylabel(col); plt.title(title)
    plt.xticks(rotation=45, ha="right")
    plt.tight_layout()
    plt.savefig(os.path.join(plots_dir, fname), dpi=150)
    plt.close()

def make_plots(prefix, d):
    save_hist(d, "ADEV_long",  f"{prefix}_hist_ADEV_long.png",  f"{prefix}: ADEV_long distribution")
    save_hist(d, "ADEV_short", f"{prefix}_hist_ADEV_short.png", f"{prefix}: ADEV_short distribution")
    save_hist(d, "tSNR",       f"{prefix}_hist_tSNR.png",       f"{prefix}: tSNR distribution")
    save_hist(d, "lag1",       f"{prefix}_hist_lag1.png",       f"{prefix}: lag1 distribution")
    save_scatter_year(d, "ADEV_long", f"{prefix}_year_ADEV_long.png", f"{prefix}: ADEV_long vs year")
    save_scatter_year(d, "tSNR",      f"{prefix}_year_tSNR.png",      f"{prefix}: tSNR vs year")
    save_scatter_year(d, "lag1",      f"{prefix}_year_lag1.png",      f"{prefix}: lag1 vs year")
    save_box_by(d, "ADEV_long","pe", f"{prefix}_box_pe_ADEV_long.png", f"{prefix}: ADEV_long by PE")
    save_box_by(d, "tSNR","pe",      f"{prefix}_box_pe_tSNR.png",      f"{prefix}: tSNR by PE")
    save_box_by(d, "lag1","pe",      f"{prefix}_box_pe_lag1.png",      f"{prefix}: lag1 by PE")

# ---- baseline groups ----
# in-vivo non-clean
nonclean = select_baseline(invivo_full, False)
if nonclean.empty:
    raise SystemExit(f"[ERROR] Baseline not found (non-clean): label contains '{label_sub}', base={base_req}, method={method_req}, param={param_req}")

out_nc = write_group(nonclean, "group_qc.tsv")
nc_df = out_nc[1]
make_plots("nonclean", nc_df)

# in-vivo clean (optional)
clean_df = None
if include_clean:
    clean = select_baseline(invivo_full, True)
    if not clean.empty:
        out_cl = write_group(clean, "group_qc_clean.tsv")
        clean_df = out_cl[1]
        make_plots("clean", clean_df)

# phantom baseline (if provided)
phantom_nc_df = None
phantom_cl_df = None
if has_phantom and not phantom_full.empty:
    ph_nc = select_baseline(phantom_full, False)
    if not ph_nc.empty:
        out_ph = write_group(ph_nc, "group_qc_phantom.tsv")
        phantom_nc_df = out_ph[1]
        make_plots("phantom_nonclean", phantom_nc_df)

    if include_clean:
        ph_cl = select_baseline(phantom_full, True)
        if not ph_cl.empty:
            out_ph_cl = write_group(ph_cl, "group_qc_phantom_clean.tsv")
            phantom_cl_df = out_ph_cl[1]
            make_plots("phantom_clean", phantom_cl_df)


# ---- condition label ----
def _cond_label(r):
    base = str(r.get("base",""))
    method = str(r.get("method",""))
    param = str(r.get("param",""))
    if param.lower() in ("", "na", "nan"):
        return f"{method} ({base})"
    return f"{method}{param} ({base})"

# ---- ADEV columns ----
adev_cols = [c for c in full.columns if re.match(r"^adevN_tau\d+", str(c))]
def _tau_num(c):
    m = re.search(r"tau(\d+)", c)
    return int(m.group(1)) if m else 10**9
adev_cols = sorted(adev_cols, key=_tau_num)

def _mean_sem(x):
    x = pd.to_numeric(x, errors="coerce")
    x = x[np.isfinite(x)]
    n = len(x)
    if n == 0:
        return np.nan, np.nan, 0
    mean = float(np.mean(x))
    sem = float(np.std(x, ddof=1) / np.sqrt(n)) if n > 1 else 0.0
    return mean, sem, n

def _mean_sd(x):
    x = pd.to_numeric(x, errors="coerce")
    x = x[np.isfinite(x)]
    n = len(x)
    if n == 0:
        return np.nan, np.nan, 0
    mean = float(np.mean(x))
    sd = float(np.std(x, ddof=1)) if n > 1 else 0.0
    return mean, sd, n

def summarize_conditions(df, out_tsv):
    rows = []
    for cond, g in df.groupby("cond"):
        rec = {"cond": cond, "n": int(g.shape[0])}
        for col in ["tSNR","lag1","log10_ratio_lt0p005_to_0p005_0p01","log10_ratio_lt0p01_to_0p01_0p1"]:
            if col in g.columns:
                m, s, n = _mean_sem(g[col])
                rec[f"{col}_mean"] = m
                rec[f"{col}_sem"] = s
        for col in adev_cols:
            m, s, n = _mean_sem(g[col])
            rec[f"{col}_mean"] = m
            rec[f"{col}_sem"] = s
        rows.append(rec)
    out = pd.DataFrame(rows).sort_values("cond")
    out.to_csv(out_tsv, sep="\t", index=False)
    return out

# ============================================================
# Phantom reference stats
# ============================================================
def compute_phantom_adev_band(ph_df):
    """Return dict: tau_int -> (mean, sd)"""
    band = {}
    for col in adev_cols:
        tau = _tau_num(col)
        m, sd, n = _mean_sd(ph_df[col])
        band[tau] = (m, sd)
    return band

def compute_phantom_scalar_ref(ph_df, col):
    """Return (mean, sd) for a scalar column"""
    if col not in ph_df.columns:
        return None
    m, sd, n = _mean_sd(ph_df[col])
    if not np.isfinite(m):
        return None
    return (m, sd)

ph_nc_adev_band = None
ph_nc_refs = {}
ph_cl_adev_band = None
ph_cl_refs = {}

if has_phantom and phantom_nc_df is not None and not phantom_nc_df.empty:
    ph_nc_adev_band = compute_phantom_adev_band(phantom_nc_df)
    for col in ["lag1", "log10_ratio_lt0p005_to_0p005_0p01", "tSNR"]:
        ref = compute_phantom_scalar_ref(phantom_nc_df, col)
        if ref is not None:
            ph_nc_refs[col] = ref

if has_phantom and phantom_cl_df is not None and not phantom_cl_df.empty:
    ph_cl_adev_band = compute_phantom_adev_band(phantom_cl_df)
    for col in ["lag1", "log10_ratio_lt0p005_to_0p005_0p01", "tSNR"]:
        ref = compute_phantom_scalar_ref(phantom_cl_df, col)
        if ref is not None:
            ph_cl_refs[col] = ref

# ============================================================
# Summary plots with phantom overlay
# ============================================================
def plot_adev_meansem(df, fname, title, phantom_band=None):
    if not adev_cols:
        return
    conds = sorted(df["cond"].dropna().unique())
    if len(conds) == 0:
        return
    taus = [_tau_num(c) for c in adev_cols]
    plt.figure(figsize=(8,5))

    # phantom reference band
    if phantom_band is not None:
        ph_y = np.array([phantom_band.get(t, (np.nan, np.nan))[0] for t in taus], dtype=float)
        ph_sd = np.array([phantom_band.get(t, (np.nan, np.nan))[1] for t in taus], dtype=float)
        valid = np.isfinite(ph_y)
        if np.any(valid):
            t_arr = np.array(taus, dtype=float)
            plt.fill_between(t_arr[valid],
                             (ph_y - ph_sd)[valid],
                             (ph_y + ph_sd)[valid],
                             alpha=0.2, color="gray", label="phantom (mean\u00b1SD)")
            plt.plot(t_arr[valid], ph_y[valid], "--", color="gray", linewidth=1.5)

    for cond in conds:
        g = df[df["cond"] == cond]
        y = []
        e = []
        for col in adev_cols:
            m, s, n = _mean_sem(g[col])
            y.append(m); e.append(s)
        if not np.any(np.isfinite(y)):
            continue
        plt.errorbar(taus, y, yerr=e, marker="o", linewidth=1, capsize=2, label=cond)
    plt.xscale("log")
    plt.xlabel("tau (s)")
    plt.ylabel("normalized Allan deviation (adevN)")
    plt.title(title)
    plt.grid(True, alpha=0.3)
    plt.legend(fontsize=7, ncol=2)
    plt.tight_layout()
    plt.savefig(os.path.join(plots_dir, fname), dpi=160)
    plt.close()

def plot_bar_meansem(df, col, fname, title, phantom_ref=None):
    if col not in df.columns:
        return
    conds = sorted(df["cond"].dropna().unique())
    if len(conds) == 0:
        return
    means = []
    sems = []
    for cond in conds:
        m, s, n = _mean_sem(df.loc[df["cond"]==cond, col])
        means.append(m); sems.append(s)
    plt.figure(figsize=(max(8, 0.55*len(conds)),4))
    x = np.arange(len(conds))

    if phantom_ref is not None:
        ph_m, ph_sd = phantom_ref
        if np.isfinite(ph_m):
            plt.axhspan(ph_m - ph_sd, ph_m + ph_sd, alpha=0.2, color="gray",
                        label="phantom (mean\u00b1SD)")
            plt.axhline(ph_m, color="gray", linestyle="--", linewidth=1)

    plt.bar(x, means, yerr=sems, capsize=2)
    plt.xticks(x, conds, rotation=45, ha="right")
    plt.ylabel(col)
    plt.title(title)
    if phantom_ref is not None:
        plt.legend(fontsize=8)
    plt.tight_layout()
    plt.savefig(os.path.join(plots_dir, fname), dpi=160)
    plt.close()


# ---- Generate summary plots (in-vivo, with phantom overlay) ----
all_nc = invivo_full[~invivo_full["is_clean"]].copy()
all_nc["cond"] = all_nc.apply(_cond_label, axis=1)
summary_nc = summarize_conditions(all_nc, os.path.join(OutputFolder, "group_summary_noclean.tsv"))
plot_adev_meansem(all_nc, "summary_adev_vs_tau_meansem_noclean.png",
                  "In-vivo: adevN vs tau (mean\u00b1SEM) [non-clean]",
                  phantom_band=ph_nc_adev_band)
plot_bar_meansem(all_nc, "lag1", "summary_lag1_meansem_noclean.png",
                 "In-vivo: lag1 (mean\u00b1SEM) [non-clean]",
                 phantom_ref=ph_nc_refs.get("lag1"))
plot_bar_meansem(all_nc, "log10_ratio_lt0p005_to_0p005_0p01",
                 "summary_ratio_meansem_noclean.png",
                 "In-vivo: log10_ratio (mean\u00b1SEM) [non-clean]",
                 phantom_ref=ph_nc_refs.get("log10_ratio_lt0p005_to_0p005_0p01"))

summary_clean = None
if include_clean:
    all_cl = invivo_full[invivo_full["is_clean"]].copy()
    if not all_cl.empty:
        all_cl["cond"] = all_cl.apply(_cond_label, axis=1)
        summary_clean = summarize_conditions(all_cl, os.path.join(OutputFolder, "group_summary_clean.tsv"))
        plot_adev_meansem(all_cl, "summary_adev_vs_tau_meansem_clean.png",
                          "In-vivo: adevN vs tau (mean\u00b1SEM) [clean]",
                          phantom_band=ph_cl_adev_band)
        plot_bar_meansem(all_cl, "lag1", "summary_lag1_meansem_clean.png",
                         "In-vivo: lag1 (mean\u00b1SEM) [clean]",
                         phantom_ref=ph_cl_refs.get("lag1"))
        plot_bar_meansem(all_cl, "log10_ratio_lt0p005_to_0p005_0p01",
                         "summary_ratio_meansem_clean.png",
                         "In-vivo: log10_ratio (mean\u00b1SEM) [clean]",
                         phantom_ref=ph_cl_refs.get("log10_ratio_lt0p005_to_0p005_0p01"))

# ============================================================
# Biological Excess Index (BEI): Phantom vs In-vivo
# ============================================================
def compute_bei_adev(invivo_df, ph_adev_band, suffix, title_suffix):
    """
    BEI(tau) = (ADEV_invivo - ADEV_phantom_mean) / ADEV_phantom_mean
    """
    if ph_adev_band is None or not adev_cols:
        return

    conds = sorted(invivo_df["cond"].dropna().unique())
    if len(conds) == 0:
        return

    taus = [_tau_num(c) for c in adev_cols]
    rows = []
    plt.figure(figsize=(8,5))

    for cond in conds:
        g = invivo_df[invivo_df["cond"] == cond]
        bei_means = []
        bei_sems = []
        for col in adev_cols:
            tau = _tau_num(col)
            ph_m, ph_sd = ph_adev_band.get(tau, (np.nan, np.nan))
            if not np.isfinite(ph_m) or ph_m <= 0:
                bei_means.append(np.nan)
                bei_sems.append(np.nan)
                continue
            vals = pd.to_numeric(g[col], errors="coerce")
            vals = vals[np.isfinite(vals)]
            if len(vals) == 0:
                bei_means.append(np.nan)
                bei_sems.append(np.nan)
                continue
            bei_vals = (vals.values - ph_m) / ph_m
            bei_m = float(np.mean(bei_vals))
            bei_s = float(np.std(bei_vals, ddof=1) / np.sqrt(len(bei_vals))) if len(bei_vals) > 1 else 0.0
            bei_means.append(bei_m)
            bei_sems.append(bei_s)

        rec = {"cond": cond}
        for i, col in enumerate(adev_cols):
            tau = _tau_num(col)
            rec[f"BEI_tau{tau}_mean"] = bei_means[i]
            rec[f"BEI_tau{tau}_sem"] = bei_sems[i]
        rows.append(rec)

        if np.any(np.isfinite(bei_means)):
            plt.errorbar(taus, bei_means, yerr=bei_sems,
                         marker="o", linewidth=1, capsize=2, label=cond)

    bei_df = pd.DataFrame(rows)
    bei_df.to_csv(os.path.join(OutputFolder, f"phantom_comparison_BEI_adev_{suffix}.tsv"),
                  sep="\t", index=False)

    plt.axhline(0, color="gray", linestyle="--", linewidth=1, alpha=0.7)
    plt.xscale("log")
    plt.xlabel("tau (s)")
    plt.ylabel("BEI\n(ADEV_invivo \u2212 ADEV_phantom) / ADEV_phantom")
    plt.title(f"Biological Excess Index: ADEV vs tau {title_suffix}")
    plt.grid(True, alpha=0.3)
    plt.legend(fontsize=7, ncol=2)
    plt.tight_layout()
    plt.savefig(os.path.join(plots_dir, f"phantom_BEI_adev_{suffix}.png"), dpi=160)
    plt.close()


def compute_bei_scalar(invivo_df, ph_refs, suffix, title_suffix):
    """BEI for scalar metrics: bar plot per condition."""
    if not ph_refs:
        return

    conds = sorted(invivo_df["cond"].dropna().unique())
    if len(conds) == 0:
        return

    for col, (ph_m, ph_sd) in ph_refs.items():
        if not np.isfinite(ph_m) or ph_m == 0:
            continue

        bei_means = []
        bei_sems = []
        valid_conds = []
        for cond in conds:
            vals = pd.to_numeric(invivo_df.loc[invivo_df["cond"]==cond, col], errors="coerce")
            vals = vals[np.isfinite(vals)]
            if len(vals) == 0:
                continue
            bei = (vals.values - ph_m) / abs(ph_m)
            valid_conds.append(cond)
            bei_means.append(float(np.mean(bei)))
            bei_sems.append(float(np.std(bei, ddof=1) / np.sqrt(len(bei))) if len(bei) > 1 else 0.0)

        if not valid_conds:
            continue

        plt.figure(figsize=(max(8, 0.55*len(valid_conds)),4))
        x = np.arange(len(valid_conds))
        plt.bar(x, bei_means, yerr=bei_sems, capsize=2)
        plt.axhline(0, color="gray", linestyle="--", linewidth=1, alpha=0.7)
        plt.xticks(x, valid_conds, rotation=45, ha="right")
        plt.ylabel(f"BEI ({col})")
        plt.title(f"Biological Excess Index: {col} {title_suffix}")
        plt.tight_layout()
        plt.savefig(os.path.join(plots_dir, f"phantom_BEI_{col}_{suffix}.png"), dpi=160)
        plt.close()


if has_phantom:
    compute_bei_adev(all_nc, ph_nc_adev_band, "noclean", "[non-clean]")
    compute_bei_scalar(all_nc, ph_nc_refs, "noclean", "[non-clean]")

    if include_clean:
        all_cl_cond = invivo_full[invivo_full["is_clean"]].copy()
        if not all_cl_cond.empty:
            all_cl_cond["cond"] = all_cl_cond.apply(_cond_label, axis=1)
            compute_bei_adev(all_cl_cond, ph_cl_adev_band, "clean", "[clean]")
            compute_bei_scalar(all_cl_cond, ph_cl_refs, "clean", "[clean]")


# ============================================================
# report.html
# ============================================================
report = os.path.join(OutputFolder, "report.html")

with open(report, "w") as f:
    f.write("""
<html>
<head>
<meta charset='utf-8'>
<title>fmri_gqc summary</title>
<style>
body { font-family: Arial, sans-serif; }
.row { display: flex; flex-direction: row; gap: 30px; }
.col { flex: 1; }
img { max-width: 100%; height: auto; }
h2 { margin-top: 40px; }
h3 { color: #555; }
.phantom-section { background: #f0f6ff; padding: 15px; border-radius: 8px; margin: 20px 0; }
</style>
</head>
<body>
""")

    plot_dir_abs = os.path.join(OutputFolder, "plots")

    def embed_side_by_side(pattern_non, pattern_clean, title):
        f.write(f"<h2>{title}</h2>")
        f.write("<div class='row'>")

        f.write("<div class='col'>")
        f.write("<h3>Non-stripped</h3>")
        for img in sorted(os.listdir(plot_dir_abs)):
            if pattern_non in img:
                f.write(f"<img src='plots/{img}'>")
        f.write("</div>")

        f.write("<div class='col'>")
        f.write("<h3>Dynamic stripped</h3>")
        for img in sorted(os.listdir(plot_dir_abs)):
            if pattern_clean in img:
                f.write(f"<img src='plots/{img}'>")
        f.write("</div>")

        f.write("</div>")

    # In-vivo summary (with phantom overlay if available)
    embed_side_by_side(
        "summary_adev_vs_tau_meansem_noclean",
        "summary_adev_vs_tau_meansem_clean",
        "Normalized Allan Deviation vs Tau"
    )

    embed_side_by_side(
        "summary_ratio_meansem_noclean",
        "summary_ratio_meansem_clean",
        "Ultra-low Frequency Power Ratio"
    )

    embed_side_by_side(
        "summary_lag1_meansem_noclean",
        "summary_lag1_meansem_clean",
        "Lag-1 Autocorrelation"
    )

    # ---- Phantom comparison section ----
    if has_phantom:
        f.write("<hr>")
        f.write("<div class='phantom-section'>")
        f.write("<h2>Phantom Reference Comparison</h2>")
        f.write(f"<p>Phantom scans: {len(phantom_tsvs)} | In-vivo scans: {len(invivo_tsvs)}</p>")

        if phantom_nc_df is not None:
            f.write(f"<p>Phantom baseline rows (non-clean): {len(phantom_nc_df)}</p>")
        if phantom_cl_df is not None:
            f.write(f"<p>Phantom baseline rows (clean): {len(phantom_cl_df)}</p>")

        f.write("<p><b>Biological Excess Index (BEI)</b>: "
                "(metric<sub>in-vivo</sub> &minus; metric<sub>phantom</sub>) / metric<sub>phantom</sub>. "
                "BEI = 0 means identical to phantom (scanner noise floor); "
                "BEI &gt; 0 indicates biological signal above scanner noise.</p>")

        embed_side_by_side(
            "phantom_BEI_adev_noclean",
            "phantom_BEI_adev_clean",
            "BEI: Allan Deviation vs Tau"
        )

        for col_label, col_pattern in [
            ("lag1", "phantom_BEI_lag1"),
            ("log10 ratio", "phantom_BEI_log10_ratio"),
            ("tSNR", "phantom_BEI_tSNR"),
        ]:
            has_any = any(col_pattern in img for img in os.listdir(plot_dir_abs))
            if has_any:
                embed_side_by_side(
                    f"{col_pattern}_noclean",
                    f"{col_pattern}_clean",
                    f"BEI: {col_label}"
                )

        f.write("</div>")

    f.write("<BR><BR><BR><BR><BR><BR><BR><BR></body></html>")

PY

indexhtml="$OutputFolder/index.html"
cat <<EOF > $indexhtml
<HTML>
 <HEAD>
   <link REL="stylesheet" TYPE="text/css" href=".files/fsl.css">
   <TITLE>FMRI_GQC REPORT</TITLE>
   <link rel="icon" type="image/x-icon" href=".files/favicon.ico">
   <style type="text/css">
    html, body{
      overflow: hidden;
    }
   .water {
      height: 30px;
      background-color:#EBF4FA;
    }
   </style>
   <link href=".files/lightbox.css" rel="stylesheet">
   <script src=".files/jquery.min.js"></script>
   <script src=".files/lightbox.min.js" type="text/javascript"></script>
   <link type=".files/magnifier.css" rel="stylesheet">
   <script type="text/javascript" src=".files/magnifier.js"></script>
 </HEAD>
 <BODY BGCOLOR="#FFFFFF" TEXT="#151515">
  <hr>
  <img src=".files/BCIL_1.png" style="border:none; width:125px;height:50px;float:right;">
  <img src=".files/bmb.png" style="border:none; width:180px;height:50px;float:right;">
  <img src=".files/hcplogo1.jpg" style="border:none; width:150px;height:50px;float:right;">
  <img src=".files/fsl-logo-x2.png" style="border:none; width:80px;height:50px;float:right;">
  <img src=".files/freesurfer.png" style="border:none; width:100px;height:50px;float:right;">
  <B>FMRI_GQC REPORT</B><BR>
  <FONT size=1>Version 1.0 &copy;2006-2026</FONT><BR>
  <Font size=2>Output directory: $OutputFolder </FONT><BR><BR>
  <center>
   <div class="water">
    <B><span style="color:gray"> </span></B>
   </div>
  </center>
  <iframe src="./report.html" frameborder="0" style="overflow:hidden; height:100%; width:100%" class="fullheight" scrolling="auto"></iframe>
 </BODY>
</HTML>
EOF

echo ""
echo "[OK] Outputs in: $OutputFolder"
echo " - $OutputFolder/index.html"
echo " - $OutputFolder/report.html"
echo " - $OutputFolder/group_qc.tsv"
echo " - $OutputFolder/group_qc_full.tsv"
echo " - $OutputFolder/plots/"
if [[ "$INCLUDE_CLEAN" -eq 1 ]]; then
  echo " - (if present) $OutputFolder/group_qc_clean.tsv"
fi
if [[ "$HAS_PHANTOM" -eq 1 ]]; then
  echo " - $OutputFolder/group_qc_phantom.tsv"
  echo " - $OutputFolder/phantom_comparison_BEI_*.tsv"
fi
