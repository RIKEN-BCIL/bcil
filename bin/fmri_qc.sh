#! /bin/bash

# fMRI QC
# Takuya Hayashi, RIKEN BDR
# Copyright (c) 2022- RIKEN


set -euo pipefail

Usage () {
echo ""
echo "Usage: $(basename $0) <fmri> [options]"
echo ""
echo "Output is <fmri>.qc by default."
echo ""
echo "Options:"
echo " -m <mask>                  : user specied mask (default is to estimate brain extraction with species label)"
echo " -r <num remove vols>       : number of removing initial volumes (default 10)"
echo " -o <output folder>         : user specified output folder name (instead of default, <fmri>.qc)"
echo " -p <order,order,..>        : high pass filtering with polynomial detrending with an order of :"
echo "                              1 (linear detrend), 2 (quadratic), 3 (cubic) (defulat: 1,2)"
echo " -b <sigma,sigma,..>        : high pass filtering with bandpass of sigma (default 200)"
echo " -i <TRUE, FALSE>           : phantom simulation by dynamic stripping (default TRUE)"
echo " -d <dim>                   : dimension of dynamic stripping (default: auto; or specify integer)"
echo " -w <width>                 : width of png picture in report.html (default 1200)"
echo " --species=<species label>  : species label of :"
echo "                              1: Human (default), 2: Macaque, 3: Marmoset, 4: Night Monkey, 5: Rat, 6: Mouse"
echo " -h                         : show help"
echo ""
exit 1
}
[ "${1:-}" = "" ] && Usage

PWD=$(pwd)
inputfmri=$1
if [ -e $inputfmri ] ; then 
 fmri=$(realpath -se $inputfmri)
elif [ $(imglob -extension $inputfmri ) != "" ] ; then
 fmri=$(realpath -se $(imglob -extension $inputfmri))
else
 echo "ERROR: cannnot find $inputfmri"
 exit
fi

command="$0 $@"
shift 1

## Setting #################
export HCPPIPEDIR=/mnt/temp_data1/HCP/HCPpipelines-5/HCPpipelines
numDelVol=10
OutputFolder=$(remove_ext $fmri).qc
fmriname=fmri
outroot=${OutputFolder}/${fmriname}
Width=1200
species=1
pddegs=1,2
hps=200
ICA=TRUE
DIM=auto
mask=""
LFP_FREQS="0.001 0.002 0.003 0.004 0.005 0.006 0.007 0.008 0.009 0.01"
ALLAN_TAUS="10 30 60 120 200 300 600 1200"
############################

die() { echo "ERROR: $*" >&2; exit 1; }

is_int() { [[ "$1" =~ ^-?[0-9]+$ ]]; }

normalize_bool() {
  # accept TRUE/FALSE/true/false/1/0/yes/no
  local v="$(echo "$1" | tr '[:lower:]' '[:upper:]')"
  case "$v" in
    TRUE|T|1|YES|Y)  echo "TRUE" ;;
    FALSE|F|0|NO|N)  echo "FALSE" ;;
    *) die "Invalid boolean: $1 (use TRUE/FALSE)" ;;
  esac
}

# Require GNU getopt
getopt --test >/dev/null
if [ $? -ne 4 ]; then
  die "GNU getopt is required but not available on this system."
fi

# Note: -b is "sigma list", -p is "order list"

OPTS=$(getopt -o m:r:o:w:p:b:i:d:h \
  --long mask:,remove-vols:,out:,width:,porder:,sigma:,ica:,dim:,species:,help \
  -n "$(basename "$0")" -- "$@") || Usage

eval set -- "$OPTS"

while true; do
  case "$1" in
    -m|--mask)
      mask="$2"; shift 2 ;;
    -r|--remove-vols)
      numDelVol="$2"; shift 2 ;;
    -o|--out)
      OutputFolder="$2"
      outroot=${OutputFolder}/${fmriname}
      shift 2 ;;
    -w|--width)
      Width="$2"; shift 2 ;;
    -p|--porder)
      pddegs="$2"; shift 2 ;;
    -b|--sigma)
      hps="$2"; shift 2 ;;
    -i|--ica)
      ICA="$(normalize_bool "$2")"; shift 2 ;;
    -d|--dim)
      DIM="$2"; shift 2 ;;
    --species)
      species="$2"; shift 2 ;;
    -h|--help)
      Usage ;;
    --)
      shift; break ;;
    *)
      die "Internal parse error near: $1" ;;
  esac
done

# --- validate numeric args ---
is_int "$numDelVol" || die "remove-vols must be an integer: $numDelVol"
[ "$numDelVol" -ge 0 ] || die "remove-vols must be >= 0: $numDelVol"

is_int "$Width" || die "width must be an integer: $Width"
[ "$Width" -ge 100 ] || die "width seems too small: $Width"

if [ "$DIM" = "auto" ]; then
  MELODIC_DIM=0
else
  is_int "$DIM" || die "dim must be 'auto' or a positive integer: $DIM"
  [ "$DIM" -ge 1 ] || die "dim must be >= 1: $DIM"
  MELODIC_DIM="$DIM"
fi

is_int "$species" || die "species must be an integer label: $species"
[ "$species" -ge 1 ] && [ "$species" -le 6 ] || die "species must be 1..6: $species"

# --- parse list args safely into arrays (same semantics as original) ---
# Allow comma-separated or space-separated lists
pddegs="$(echo "$pddegs" | tr ',' ' ')"
hps="$(echo "$hps" | tr ',' ' ')"

pddegs=($pddegs)
hps=($hps)

# Validate that pddegs are 1..3 (per help)
for x in "${pddegs[@]}"; do
  is_int "$x" || die "porder contains non-integer: $x"
  [ "$x" -ge 1 ] && [ "$x" -le 3 ] || die "porder must be 1..3: $x"
done

# Validate that hps are positive numbers (allow float)
for x in "${hps[@]}"; do
  [[ "$x" =~ ^[0-9]+([.][0-9]+)?$ ]] || die "sigma contains invalid number: $x"
done

#echo pddegs: ${pddegs[@]}
#echo hps: ${hps[@]}

tsv2html () {
#convert a TSV file to an HTML file
#author: Donald L. Merand

#note: this is most useful in conjunction with bcat, which sends results to the browser
# you can get bcat on OS X using homebrew
# http://mxcl.github.com/homebrew/
# then type "brew install bcat"

#also note: this script only creates an HTML snippet - you'll probably want to wrap
# it in some pretty CSS, not to mention <html> and <body> tags

#convert Mac line endings, if any
perl -p -e 's/\r/\n/g' |

#now do the conversion
awk '
BEGIN {
  FS="\t"
  printf "<table>\n"
}
{
  printf "\n\n<tr>"
  for (i=1;i<=NF;i++) {
    printf "<td>%s</td>", $i
  }
  printf "</tr>"
}
END {
  printf "\n</table>"
}'

}

fmristatistics () {
in=$1
refvar=$2
mask=$3

fslmaths ${in} -Tstd -sqr ${in}_var
fslmaths ${refvar} -sub ${in}_var ${in}_var      # '${in}_var' is variance that is included in the $refval but not explained by ${in} 
slicer ${in}_var $mask -s 4 -a ${in}_var.png
fslmaths ${in}_var -div $refvar ${in}_var_ratio  # '${in}_var_ratio' is a ratio of'${in}_var' to variance of $refval
fslmaths ${in} -Tstd ${in}_std
fslmaths ${in} -Tmean ${in}_mean
fslmaths ${in}_mean -div ${in}_std ${in}_tsnr
slicer ${in}_tsnr $mask -s 4 -a ${in}_tsnr.png
echo $(fslstats ${in}_var_ratio -k $mask -m) $(fslstats ${in}_tsnr -k $mask -M)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
}

calc_lfp () {
  # usage: calc_lfp <ts.txt> <TR> <f0_Hz>
  local tsfile="$1"
  local tr="$2"
  local f0="$3"
  python3 - << 'PY' "$tsfile" "$tr" "$f0"
import sys, numpy as np
tsfile, tr, f0 = sys.argv[1], float(sys.argv[2]), float(sys.argv[3])
x = np.loadtxt(tsfile)
x = x - np.mean(x)
n = x.size
if n < 8:
    print("nan"); raise SystemExit
fft = np.fft.rfft(x)
p = (np.abs(fft)**2) / n
f = np.fft.rfftfreq(n, d=tr)
ptot = p[(f >= 0)].sum()
plow = p[(f >= 0) & (f <= f0)].sum()
print(plow/ptot if ptot > 0 else float("nan"))
PY
}

lfp_row () {
  # usage: lfp_row <ts.txt> <TR>
  local tsfile="$1"
  local tr="$2"
  for f0 in $LFP_FREQS ; do
    calc_lfp "$tsfile" "$tr" "$f0"
  done | paste -sd $'\t' -
}

# --- Lag-1 autocorrelation on mean TS ---
calc_lag1 () {
  # usage: calc_lag1 <ts.txt>
  python3 - <<'PY' "$1"
import sys, numpy as np
x = np.loadtxt(sys.argv[1]).astype(float)
x = x - np.mean(x)
if x.size < 3:
    print("nan"); raise SystemExit
x0 = x[:-1]; x1 = x[1:]
den = np.sqrt(np.sum(x0*x0) * np.sum(x1*x1))
print(float(np.sum(x0*x1)/den) if den > 0 else float("nan"))
PY
}

# --- PSD log10 ratios (drift sensitivity proxies) ---
calc_psd_log10_ratios () {
  # usage: calc_psd_log10_ratios <ts.txt> <TR>
  # outputs: log10_ratio_lt0p005_to_0p005_0p01  log10_ratio_lt0p01_to_0p01_0p1
  python3 - <<'PY' "$1" "$2"
import sys, numpy as np
tsfile, tr = sys.argv[1], float(sys.argv[2])
x = np.loadtxt(tsfile).astype(float)
x = x - np.mean(x)
n = x.size
if n < 8:
    print("nan\tnan"); raise SystemExit

f = np.fft.rfftfreq(n, d=tr)
P = (np.abs(np.fft.rfft(x))**2) / n

# drop DC
f = f[1:]; P = P[1:]

def band_power(flo, fhi):
    m = (f >= flo) & (f < fhi)
    return float(P[m].sum()) if np.any(m) else 0.0

p_lt_005   = band_power(0.0,   0.005)
p_005_001  = band_power(0.005, 0.01)
p_lt_001   = band_power(0.0,   0.01)
p_001_01   = band_power(0.01,  0.1)

eps = 1e-30
r1 = np.log10((p_lt_005 + eps) / (p_005_001 + eps))
r2 = np.log10((p_lt_001 + eps) / (p_001_01 + eps))
print(f"{r1}\t{r2}")
PY
}

# --- Band-limited PSD log-ratio (robust low-frequency attenuation metric) ---
calc_band_logratio () {
  # usage: calc_band_logratio <before_ts.txt> <after_ts.txt> <TR> <f_lo> <f_hi>
  python3 - <<'PY' "$1" "$2" "$3" "$4" "$5"
import sys, numpy as np

before, after, tr = sys.argv[1], sys.argv[2], float(sys.argv[3])
flo, fhi = float(sys.argv[4]), float(sys.argv[5])

x0 = np.loadtxt(before); x0 -= x0.mean()
x1 = np.loadtxt(after);  x1 -= x1.mean()

n = x0.size
f = np.fft.rfftfreq(n, d=tr)
P0 = (np.abs(np.fft.rfft(x0))**2) / n
P1 = (np.abs(np.fft.rfft(x1))**2) / n

# drop DC
f, P0, P1 = f[1:], P0[1:], P1[1:]

mask = (f >= flo) & (f <= fhi)
if not np.any(mask):
    print("nan\tnan")
    raise SystemExit

ratio = P1[mask] / np.maximum(P0[mask], 1e-30)
logr  = np.log10(ratio)

print(f"{np.mean(logr)}\t{np.median(logr)}")
PY
}

# --- Drift slope on mean TS ---
calc_drift_slope () {
  # usage: calc_drift_slope <ts.txt> <TR>
  python3 - <<'PY' "$1" "$2"
import sys, numpy as np
ts = np.loadtxt(sys.argv[1]).astype(float)
tr = float(sys.argv[2])
t  = np.arange(ts.size)*tr
ts = ts - np.mean(ts) + 1.0  # stabilize mean for pct; optional
A = np.vstack([t, np.ones_like(t)]).T
a,b = np.linalg.lstsq(A, ts, rcond=None)[0]
mean = float(np.mean(ts))
slope_pct_per_min = 100.0 * (a*60.0)/mean if mean != 0 else float("nan")
print(f"{a}\t{slope_pct_per_min}")
PY
}

# --- Allan deviation for multiple taus (seconds) ---
calc_allan_dev_multi () {
  # usage: calc_allan_dev_multi <ts.txt> <TR> "<TAUS_SEC...>"
  # outputs: normalized Allan deviation (ADEV/mean) for each tau, tab-separated
  python3 - <<'PY' "$1" "$2" "$3"
import sys, numpy as np

ts = np.loadtxt(sys.argv[1]).astype(float)
tr = float(sys.argv[2])
taus = [float(x) for x in sys.argv[3].split()]

# normalize scale by mean of original signal (before de-mean)
mean_signal = float(np.mean(ts))
if not np.isfinite(mean_signal) or mean_signal <= 0:
    # All outputs become NaN if mean is invalid
    print("\t".join(["nan"] * len(taus)))
    sys.exit(0)

# de-mean (Allan is typically defined on zero-mean sequence)
x = ts - mean_signal
n = x.size

def allan_dev_norm(x, m):
    # overlapping Allan deviation using m-point averages, normalized by mean(ts)
    # tau = m*tr
    if m < 1 or 2*m >= n:
        print(f"[WARN] Allan m={m}, N={n}, need N>2m. Returning NaN.", file=sys.stderr)
        return np.nan
    c = np.cumsum(np.insert(x, 0, 0.0))
    y = (c[m:] - c[:-m]) / m
    d = y[m:] - y[:-m]
    adev = np.sqrt(0.5 * np.mean(d * d))
    return adev / mean_signal

out = []
for tau in taus:
    m = int(round(tau / tr))
    out.append(allan_dev_norm(x, m))

print("\t".join("nan" if np.isnan(v) else f"{v}" for v in out))
PY
}

# --- NEW: DelVol effect indices (DII/DIS) + bar plots ---
make_delvol_indices () {
  # usage: make_delvol_indices <metrics_tsv> <outprefix>
  local metrics_tsv="$1"
  local outprefix="$2"

  python3 - <<'PY' "$metrics_tsv" "$outprefix"
import sys, re
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

metrics = sys.argv[1]
outp = sys.argv[2]

df = pd.read_csv(metrics, sep="\t", dtype=str)

# normalize column names
if "label" not in df.columns and "filter" in df.columns:
    df = df.rename(columns={"filter":"label"})

# numeric coercion for non-key columns
for c in df.columns:
    if c in ("label","base","method","param"):
        continue
    df[c] = pd.to_numeric(df[c], errors="coerce")

# detect clean/non-clean by label
df["is_clean"] = df["label"].astype(str).str.contains(r"_clean$", regex=True)

# columns present?
def have(cols):
    return [c for c in cols if c in df.columns]

short_cols = have(["adevN_tau10","adevN_tau30","adevN_tau60"])
long_cols  = have(["adevN_tau200","adevN_tau300","adevN_tau600","adevN_tau1200"])
if len(short_cols) == 0 and len(long_cols) == 0:
    # nothing to do
    sys.exit(0)

rows = []
# group by condition excluding base
for (method, param, is_clean), g in df.groupby(["method","param","is_clean"], dropna=False):
    if method is None:
        continue
    # need both raw and DelVol
    g_raw = g[g["base"].astype(str).str.lower().eq("raw")]
    g_del = g[g["base"].astype(str).str.lower().str.contains("delvol")]
    if len(g_raw) == 0 or len(g_del) == 0:
        continue

    # if multiple rows exist (rare), take first
    r = g_raw.iloc[0]
    d = g_del.iloc[0]

    def safe_vals(row, cols):
        v = []
        for c in cols:
            x = row.get(c)
            v.append(np.nan if x is None else float(x) if np.isfinite(x) else np.nan)
        return np.array(v, dtype=float)

    raw_s = safe_vals(r, short_cols)
    del_s = safe_vals(d, short_cols)
    raw_l = safe_vals(r, long_cols)
    del_l = safe_vals(d, long_cols)

    # DII: mean relative improvement on long taus
    DII = np.nan
    if len(long_cols) > 0:
        with np.errstate(divide="ignore", invalid="ignore"):
            rel = (raw_l - del_l) / raw_l
        DII = np.nanmean(rel)

    # DIS: 1 - mean abs log-ratio on short taus (closer to 1 is less invasive)
    DIS = np.nan
    if len(short_cols) > 0:
        with np.errstate(divide="ignore", invalid="ignore"):
            lr = np.log(del_s / raw_s)
        DIS = 1.0 - np.nanmean(np.abs(lr))

    rows.append({
        "method": method,
        "param": param,
        "is_clean": int(bool(is_clean)),
        "raw_label": str(r.get("label","")),
        "delvol_label": str(d.get("label","")),
        "DII_longtau": DII,
        "DIS_shorttau": DIS,
    })

outdf = pd.DataFrame(rows)
out_tsv = outp + "_delvol_indices.tsv"
outdf.to_csv(out_tsv, sep="\t", index=False)

# ---- plots ----
if len(outdf) == 0:
    sys.exit(0)

# nice x label
outdf["cond"] = outdf["method"].astype(str) + ":" + outdf["param"].astype(str) + (outdf["is_clean"].map({0:"",1:"_clean"}))

# Plot DII (higher better)
d = outdf.dropna(subset=["DII_longtau"]).copy()
if len(d) > 0:
    plt.figure(figsize=(10,4))
    plt.bar(d["cond"], d["DII_longtau"])
    plt.xticks(rotation=45, ha="right")
    plt.ylabel("DII (mean relative improvement, long tau)")
    plt.title("DelVol Improvement Index (DII)")
    plt.tight_layout()
    plt.savefig(outp + "_DII_bar.png", dpi=150)

# Plot DIS (closer to 1 better)
d = outdf.dropna(subset=["DIS_shorttau"]).copy()
if len(d) > 0:
    plt.figure(figsize=(10,4))
    plt.bar(d["cond"], d["DIS_shorttau"])
    plt.xticks(rotation=45, ha="right")
    plt.ylabel("DIS (1 - mean abs log-ratio, short tau)")
    plt.title("DelVol Invasiveness Score (DIS)  (higher ~ less invasive)")
    plt.tight_layout()
    plt.savefig(outp + "_DIS_bar.png", dpi=150)

PY
}

# --- NEW: summary plots from metrics TSV (ADEV curves, band log-ratio, lag1), split by clean/non-clean ---
make_summary_plots () {
  # usage: make_summary_plots <metrics_tsv> <outprefix>
  local metrics_tsv="$1"
  local outprefix="$2"
  python3 - <<'PY' "$metrics_tsv" "$outprefix"
import sys, re
import pandas as pd
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

metrics = sys.argv[1]
outp = sys.argv[2]

df = pd.read_csv(metrics, sep="\t", dtype=str)
# numeric coercion for all non-key columns
for c in df.columns:
    if c in ("label","base","method","param","filter"):
        continue
    df[c] = pd.to_numeric(df[c], errors="coerce")

# normalize column names: allow either "label" or "filter"
if "label" not in df.columns and "filter" in df.columns:
    df = df.rename(columns={"filter":"label"})
if "method" not in df.columns:
    # best effort: derive from label
    df["method"] = df["label"].astype(str).str.replace(r".*_(hppd\d+|hp\d+).*", r"\1", regex=True)

# clean flag
df["is_clean"] = df["method"].astype(str).str.contains("clean", case=False, regex=False) | df["label"].astype(str).str.contains("_clean")

# helper for plotting styles
def base_marker(b):
    b = str(b).lower()
    return "o" if "delvol" in b else "s"

# color by "method family" (poly vs bptf vs raw/other)
def method_family(m):
    m = str(m).lower()
    if "poly" in m: return "poly"
    if "bptf" in m or "hp" in m: return "bptf"
    if m in ("raw","na","none",""): return "raw"
    return m

families = sorted(df["method"].map(method_family).unique())
cmap = plt.get_cmap("tab10")
colors = {fam: cmap(i % 10) for i, fam in enumerate(families)}

def plot_adev(sub, suffix):
    # accept adevN_tau* or adev_tau*
    adev_cols = [c for c in sub.columns if re.match(r"adevN?_tau\d+$", c)]
    if not adev_cols:
        return None
    taus = np.array([int(re.findall(r"\d+", c)[0]) for c in adev_cols], float)
    order = np.argsort(taus)
    taus = taus[order]
    adev_cols = [adev_cols[i] for i in order]

    plt.figure(figsize=(9,5))
    for _, r in sub.iterrows():
        y = r[adev_cols].to_numpy(dtype=float)
        fam = method_family(r.get("method",""))
        mk = base_marker(r.get("base","raw"))
        lbl = str(r.get("label",""))
        plt.plot(taus, y, marker=mk, linewidth=1.5, label=lbl, color=colors.get(fam, None), alpha=0.9)
    plt.xscale("log")
    plt.yscale("log")
    plt.xlabel("tau (s)")
    plt.ylabel("Allan deviation (normalized by mean)")
    plt.title(f"ADEV vs tau ({suffix})")
    plt.grid(True, which="both", alpha=0.3)
    plt.legend(fontsize=8, ncol=2, frameon=False)
    out = f"{outp}_adev_vs_tau_{suffix}.png"
    plt.tight_layout()
    plt.savefig(out, dpi=150)
    plt.close()
    return out

def plot_bar(sub, col, suffix, title, ylabel):
    if col not in sub.columns:
        return None
    sub = sub.copy()
    sub["fam"] = sub["method"].map(method_family)
    sub = sub.sort_values(["fam","base","param","label"], na_position="last")
    x = np.arange(len(sub))
    plt.figure(figsize=(max(9, 0.6*len(sub)), 4.5))
    for i, (_, r) in enumerate(sub.iterrows()):
        plt.bar(i, r[col], color=colors.get(r["fam"], None), alpha=0.85)
    plt.xticks(x, sub["label"].astype(str), rotation=45, ha="right", fontsize=8)
    plt.ylabel(ylabel)
    plt.title(f"{title} ({suffix})")
    plt.grid(True, axis="y", alpha=0.3)
    # legend (family colors)
    handles = [plt.Line2D([0],[0], color=colors[f], lw=6) for f in families]
    plt.legend(handles, families, title="method", fontsize=8, frameon=False, ncol=len(families))
    out = f"{outp}_{col}_{suffix}.png"
    plt.tight_layout()
    plt.savefig(out, dpi=150)
    plt.close()
    return out

outs = {}
for suffix, sub in (("noclean", df[~df["is_clean"]]), ("clean", df[df["is_clean"]])):
    if sub.empty:
        continue
    outs[f"adev_{suffix}"] = plot_adev(sub, suffix)

    # ratio column preference (new -> legacy)
    ratio_col = None
    ratio_title = None
    for cand, title in [
        ("log10_ratio_lt0p005_to_0p005_0p01", "log10 ratio: <0.005 vs 0.005–0.01"),
        ("median_log10_ratio_0p001_0p01", "median log10 ratio: 0.001–0.01 (legacy)"),
        ("mean_log10_ratio_0p001_0p01", "mean log10 ratio: 0.001–0.01 (legacy)"),
    ]:
        if cand in sub.columns:
            ratio_col = cand
            ratio_title = title
            break
    if ratio_col is not None:
        outs[f"ratio_{suffix}"] = plot_bar(sub, ratio_col, suffix, ratio_title, ratio_col)

    outs[f"lag1_{suffix}"] = plot_bar(sub, "lag1", suffix, "lag-1 autocorrelation", "lag1")

print("\t".join([v for v in outs.values() if v]))
PY
}

# ============================================================
# Run fmriqc
# ============================================================
BCILDIR=$(cd $(dirname $0); cd ..; pwd)
source $BCILDIR/bcilconf/settings.sh
if [ -e $OutputFolder ] ; then
	echo "Found output folder: ${OutputFolder} - please remove it before running"
	exit
fi

mkdir -p $OutputFolder/.files

echo "" >> $OutputFolder/command.txt
echo "--------------------" >> $OutputFolder/command.txt
echo "$(date -R)" >> $OutputFolder/command.txt
echo "$command" >> $OutputFolder/command.txt
echo "" >> $OutputFolder/command.txt

cp ${FSLDIR}/doc/fsl.css ${BCILDIR}/doc/images/BCIL_1.png ${BCILDIR}/doc/images/fsl-logo-x2.png ${BCILDIR}/doc/images/hcplogo1.jpg ${BCILDIR}/doc/images/freesurfer.png ${BCILDIR}/doc/images/bmb.png ${BCILDIR}/doc/images/favicon.ico ${BCILDIR}/doc/hcppipe_qc/magnifier.css ${BCILDIR}/doc/hcppipe_qc/magnifier.js ${BCILDIR}/doc/hcppipe_qc/jquery.min.js ${BCILDIR}/doc/hcppipe_qc/lightbox.css ${BCILDIR}/doc/hcppipe_qc/lightbox.min.js $OutputFolder/.files/

reporthtml=${OutputFolder}/report.html

cat << EOF > $reporthtml
<HTML><HEAD>
<link REL="stylesheet" TYPE="text/css" href=".files/fsl.css"><TITLE>FMRIQC REPORT</TITLE><link rel="icon" type="image/x-icon" href=".files/favicon.ico"> <link href=".files/lightbox.css" rel=stylesheet>
 <script src=".files/jquery.min.js"></script>
 <script src=".files/lightbox.min.js" type="text/javascript"></script>
 <link type="text/css" href=".files/magnifier.css" rel="stylesheet"><script type="text/javascript" src=".files/magnifier.js"></script>
 <style type="text/css">
    .button {
      display       : inline-block;
      border-radius : 20%;     
      font-size     : 8pt;    
      text-align    : center; 
      cursor        : pointer;
      padding       : 6px;
      background    : #ccc;
      color         : #000000;
      line-height   : 1em;    
      transition    : .3s;
      border: solid 1px;
      text-decoration: none;
      margin: 0px 10px;
    }
    .button.disabled {
      display       : inline-block;
      border-radius : 20%;     
      font-size     : 8pt;    
      text-align    : center; 
      cursor        : pointer;
      padding       : 6px;
      background    : #ccc;
      color         : #808B96;
      line-height   : 1em;    
      transition    : .3s;
      border: solid 1px;
      text-decoration: none;
      margin: 0px 10px;
    }
 </style> 
</HEAD>
<BODY BGCOLOR="#FFFFFF" TEXT="#151515">
EOF

tr=$(fslval $fmri pixdim4)
dim4=$(fslval $fmri dim4)
pixdim1=$(fslval $fmri pixdim1)
pixdim2=$(fslval $fmri pixdim2)
pixdim3=$(fslval $fmri pixdim3)
spatialsmoothingsigma=$(echo "$pixdim1 / ( 2 * ( sqrt ( 2 * l ( 2 ) ) ) )" | bc -l | awk '{printf("%0.2f",$1)}')

echo ""
echo "Start fmriqc"
echo " HCPPIPEDIR: $HCPPIPEDIR"
echo " Current directory: $PWD"
echo " Input fmri: $inputfmri"
echo " Realpath to fmri: $fmri" 
echo " TR: $tr"
echo " Number of volumes: $dim4"
echo " Spatial resolution: $pixdim1 x $pixdim2 x $pixdim3" 
echo " Number of initial volume deletion: $numDelVol"
echo " polynomial detrend order: ${pddegs[@]}"
echo " hp:${hps[@]}"
echo " spatial smoothing sigma: $spatialsmoothingsigma"
echo " Output directory: $OutputFolder"
echo " Running ICA: $ICA"
echo " ICA dimension: $DIM (MELODIC -d $MELODIC_DIM)" 

if [ ! -z "$mask" ] ; then 
	if [ ! -e $mask ] ; then
		echo "ERROR: cannot find $mask"
		exit 1;
	fi
	echo " user specified mask: $mask"
fi
echo ''

echo '<b>fMRI name</b>:'$(basename $inputfmri)'<br>' >> $reporthtml 
echo '<b>Path to fMRI</b>:'$fmri'<br>' >> $reporthtml
echo '<b>TR</b>:'$tr'<br>' >> $reporthtml
echo "<b>Number of volumes</b>: $dim4<br>" >> $reporthtml
echo "<b>Spatial resolution</b>: $pixdim1 x $pixdim2 x $pixdim3<br>" >> $reporthtml 
echo '<b>Number of initial volume removal</b>:'$numDelVol'<br>' >> $reporthtml
echo '<b>Order of polynomial detrend</b>:'${pddegs[@]}'<br>' >> $reporthtml
echo '<b>Sigma of high pass filter</b>:'${hps[@]}'<br><hr>' >> $reporthtml

fslmaths $fmri $outroot
# ============================================================
# TODO: motion correction
# ============================================================
# scale volume 
# do motion correction
# rescale volume 

# ============================================================
# brain masking
# ============================================================
fslroi $outroot ${outroot}_example 0 1
if [ -z "$mask" ] ; then
	echo " running brain extraction"
	bet4animal ${outroot}_example ${outroot}_example_brain -z $species -m -f 0.2
else
	imcp $mask ${outroot}_example_brain_mask
fi
mask=${outroot}_example_brain_mask
slicer ${outroot}_example $mask -s 4 -a ${outroot}_example.png
echo '<b>'${fmriname}'_example + mask</b></br><img src="'${fmriname}'_example.png" WIDTH='$Width'<br><br><br>' >> $reporthtml
		
# ============================================================
# raw fmri qc metrics
# ============================================================

echo " calculating mean and variance maps"
fslmaths $outroot -Tmean ${outroot}_mean
fslmaths $outroot -Tstd -sqr ${outroot}_var
fslmaths $outroot -Tstd ${outroot}_std
fslmaths ${outroot}_mean -div ${outroot}_std ${outroot}_tsnr

slicer ${outroot}_mean $mask -s 4 -a ${outroot}_mean.png
echo '<b>'${fmriname}' mean</b></br><img src="'${fmriname}'_mean.png" WIDTH='$Width'<br><br><br>' >> $reporthtml
slicer ${outroot}_var $mask -s 4 -a ${outroot}_var.png
echo '<b>'${fmriname}' total variance</b></br><img src="'${fmriname}'_var.png" WIDTH='$Width'<br><br><br>' >> $reporthtml

var=$(fslstats ${outroot}_mean -k $mask -M)
echo "$var" | awk '{printf "Mean signal of '${fmriname}' \t%1.2f\n",$1}' > ${outroot}_stats.tsv
var=$(fslstats ${outroot}_var -k $mask -M)
echo "$var" | awk '{printf "Variance of '${fmriname}' \t%1.2f\n",$1}' >> ${outroot}_stats.tsv
echo " variance of $(basename ${outroot}) : ${var}"
tsnr=$(fslstats ${outroot}_tsnr -k $mask -M)
echo "$tsnr" | awk '{printf "tSNR of '${fmriname}' \t%1.2f\n",$1}' >> ${outroot}_stats.tsv
echo " tSNR of $(basename ${outroot}) : ${tsnr}"	

echo " creating metrics_tsv file" 
metrics_tsv="${outroot}_filter_metrics.tsv"
ALLAN_HDR=""; 
for tau in $ALLAN_TAUS; 
   do ALLAN_HDR="${ALLAN_HDR}\tadevN_tau${tau}"; 
done

echo -e "label\tbase\tmethod\tparam\
\tmean_log10_ratio_0p001_0p01\
\tmedian_log10_ratio_0p001_0p01\
\tLFP_0p005\tLFP_0p01\
\ttSNR\
\tdrift_slope_per_sec\
\tdrift_slope_pct_per_min\
\tlag1\
\tlog10_ratio_lt0p005_to_0p005_0p01\
\tlog10_ratio_lt0p01_to_0p01_0p1\
${ALLAN_HDR}" > "$metrics_tsv"

echo " extracting ${fmriname} ts"
fslmeants -i $fmri -m $mask -o ${outroot}_ts.txt
tslist="${outroot}_ts.txt,"
labellist="$(basename ${outroot}_ts),"
DelVoltslist=""
DelVollabellist=""

# qa metrics
lfp005=$(calc_lfp "${outroot}_ts.txt" "$tr" 0.005)
lfp010=$(calc_lfp "${outroot}_ts.txt" "$tr" 0.01)
read slope slope_pctmin < <(calc_drift_slope "${outroot}_ts.txt" "$tr")
adev=$(calc_allan_dev_multi "${outroot}_ts.txt" "$tr" "$ALLAN_TAUS")
label=$(basename "${outroot}")
base="raw"
method="raw"
param="NA"
mlog="nan"	# band-log-ratio is not defined for raw (no before/after) -> nan
medlog="nan"
lag1=$(calc_lag1 "${outroot}_ts.txt")
read r005 r01 < <(calc_psd_log10_ratios "${outroot}_ts.txt" "$tr")
echo -e "${label}\t${base}\t${method}\t${param}\t${mlog}\t${medlog}\t${lfp005}\t${lfp010}\t${tsnr}\t${slope}\t${slope_pctmin}\t${lag1}\t${r005}\t${r01}\t${adev}" >> "$metrics_tsv"

# ============================================================
# qc metrics for volumes deleted inivial volumes (DelVol)
# ============================================================
echo " deleting initial ${numDelVol} volumes from fmri"
fslmaths $fmri ${outroot}
fslroi ${outroot} ${outroot}_DelVol $numDelVol -1

fslroi ${outroot} ${outroot}_initvol 0 $numDelVol
fslmaths ${outroot}_initvol -Tmean ${outroot}_initvol_mean
slicer ${outroot}_initvol_mean $mask -s 4 -a ${outroot}_initvol_mean.png
echo '<b>'${fmriname}' initvol mean</b></br><img src="'${fmriname}'_initvol_mean.png" WIDTH='$Width'<br><br><br>' >> $reporthtml
echo " calculating variance maps of initial volumes"
fslmaths ${outroot}_DelVol -Tmean ${outroot}_DelVol_mean
fslmaths ${outroot}_DelVol -Tstd -sqr ${outroot}_DelVol_var
fslmaths ${outroot}_var -sub ${outroot}_DelVol_var ${outroot}_initvol_var

fslmaths "${outroot}_DelVol.nii.gz" -Tstd  "${outroot}_DelVol_std"
fslmaths "${outroot}_DelVol_mean" -div "${outroot}_DelVol_std" "${outroot}_DelVol_tsnr"
tsnr=$(fslstats "${outroot}_DelVol_tsnr" -k "$mask" -M)

var=$(fslstats ${outroot}_DelVol_mean -k $mask -M)
echo "$var" | awk '{printf "Mean signal of '${fmriname}_DelVol' \t%1.2f\n",$1}' > ${outroot}_stats.tsv
var=$(fslstats ${outroot}_DelVol_var -k $mask -M)
echo "$var" | awk '{printf "Variance of '${fmriname}_DelVol' \t%1.2f\n",$1}' >> ${outroot}_stats.tsv
echo " variance of $(basename ${outroot})_DelVol : ${var}"
tsnr=$(fslstats ${outroot}_DelVol_tsnr -k $mask -M)
echo "$tsnr" | awk '{printf "tSNR of '${fmriname}_DelVol' \t%1.2f\n",$1}' >> ${outroot}_stats.tsv
echo " tSNR of $(basename ${outroot})_DelVol : ${tsnr}"	

slicer ${outroot}_initvol_var $mask -s 4 -a ${outroot}_initvol_var.png
echo '<b>'${fmriname}' initvol variance</b></br><img src="'${fmriname}'_initvol_var.png" WIDTH='$Width'<br><br><br>' >> $reporthtml
fslmaths ${outroot}_initvol_var -div ${outroot}_var ${outroot}_initvol_var_ratio
varratio=$(fslstats ${outroot}_initvol_var_ratio -k $mask -M)
echo " variance ratio of initial $numDelVol volumes : $varratio"
echo "$varratio" | awk '{printf "Noise variance ratio of initial '$numDelVol' volumes \t%1.2f\n",$1}' >> ${outroot}_stats.tsv

echo " append init volumes and extracting ${fmriname}_DelVol ts"
i=1; vol=""
while [ $i -le $numDelVol ] ; do
 vol="$vol ${outroot}_DelVol_mean"
 i=$((i+1))
done

fslmerge -t ${outroot}_DelVol_plot $vol ${outroot}_DelVol
fslmeants -i ${outroot}_DelVol_plot -m $mask -o ${outroot}_DelVol_plot_ts.txt
fsl_tsplot -i ${outroot}_ts.txt,${outroot}_DelVol_plot_ts.txt -o ${outroot}_ts.png -x Second -u $tr -a $(basename ${outroot})_ts,$(basename ${outroot})_DelVol_ts
echo '<b>'${fmriname}'_ts and '${fmriname}'_DelVol_plot_ts</b></br><img src="'${fmriname}'_ts.png" WIDTH='$Width'<br><br><br>' >> $reporthtml
DelVoltslist+="${outroot}_DelVol_plot_ts.txt,"
DelVollabellist+="$(basename ${outroot}_DelVol_plot_ts),"

# qa metrics
fslmeants -i ${outroot}_DelVol -m $mask -o ${outroot}_DelVol_ts.txt
lfp005=$(calc_lfp "${outroot}_DelVol_ts.txt" "$tr" 0.005)
lfp010=$(calc_lfp "${outroot}_DelVol_ts.txt" "$tr" 0.01)
read slope slope_pctmin < <(calc_drift_slope "${outroot}_DelVol_ts.txt" "$tr")
adev=$(calc_allan_dev_multi "${outroot}_DelVol_ts.txt" "$tr" "$ALLAN_TAUS")
label=$(basename "${outroot}_DelVol")
base="DelVol"
method="raw"
param="NA"
mlog="nan"	# band-log-ratio is not defined for raw (no before/after) -> nan
medlog="nan"
lag1=$(calc_lag1 "${outroot}_DelVol_ts.txt")
read r005 r01 < <(calc_psd_log10_ratios "${outroot}_DelVol_ts.txt" "$tr")
echo -e "${label}\t${base}\t${method}\t${param}\t${mlog}\t${medlog}\t${lfp005}\t${lfp010}\t${tsnr}\t${slope}\t${slope_pctmin}\t${lag1}\t${r005}\t${r01}\t${adev}" >> "$metrics_tsv"

# ============================================================
# detrending with polynomial function
# ============================================================
echo "<hr><b>Detrending with polynomial function</b><br><br>" >> $reporthtml
if [[ -L "$0" ]]
then
    this_script_dir=$(dirname "$(readlink "$0")")
else
    this_script_dir=$(dirname "$0")
fi

for pddeg in ${pddegs[@]}; do

	echo " polynomial detrend (deg=${pddeg}) of fmri"
	ML_PATHS="addpath('${HCPPIPEDIR}/global/fsl/etc/matlab'); addpath('${this_script_dir}');"
	matlab -nodesktop -nosplash <<< "${ML_PATHS} detrendpolynomial('${outroot}',$tr,$pddeg)"
	echo ""
	fslmaths ${outroot}_hppd${pddeg} -add ${outroot}_mean ${outroot}_hppd${pddeg}
	in="${outroot}_hppd${pddeg}"
	vars=($(fmristatistics ${in} ${outroot}_var $mask))
	echo " variance ratio of $(basename ${in}) : ${vars[0]}"
	echo " tSNR of $(basename ${in}) : ${vars[1]}"	
	echo "${vars[0]}" | awk '{printf "Noise variance ratio of '$(basename ${in})' \t%1.2f\n",$1}' >> ${outroot}_stats.tsv
	echo "${vars[1]}" | awk '{printf "tSNR of '$(basename ${in})' \t%1.2f\n",$1}' >> ${outroot}_stats.tsv

	fslmeants -i ${in} -m $mask -o ${in}_ts.txt
	tslist+="${outroot}_hppd${pddeg}_ts.txt,"
	labellist+="$(basename ${outroot}_hppd${pddeg}_ts),"

	# --- TrendVarRatio from MATLAB (if produced) ---
	if [ -e ${outroot}_hppd${pddeg}_metrics.tsv ] ; then
		# metrics.tsv has header line: metric \t value
		tvr_mean=$(awk -F'\t' '$1=="trend_var_ratio_mean"{print $2}' ${outroot}_hppd${pddeg}_metrics.tsv)
		tvr_med=$(awk -F'\t' '$1=="trend_var_ratio_median"{print $2}' ${outroot}_hppd${pddeg}_metrics.tsv)
		echo "$tvr_mean" | awk '{printf "TrendVarRatio mean of '$(basename ${in})' \t%1.6f\n",$1}' >> ${outroot}_stats.tsv
		echo "$tvr_med"  | awk '{printf "TrendVarRatio median of '$(basename ${in})' \t%1.6f\n",$1}' >> ${outroot}_stats.tsv
	fi

	# --- TrendVarRatio map preview in report (if produced) ---
	if [ -e ${outroot}_hppd${pddeg}_trendVarRatio.nii.gz ] ; then
    		slicer ${outroot}_hppd${pddeg}_trendVarRatio $mask -s 4 -a ${outroot}_hppd${pddeg}_trendVarRatio.png
    		echo '<i>TrendVarRatio map (var(trend)/var(original)):</i></br><img src="'${fmriname}'_hppd'${pddeg}'_trendVarRatio.png" WIDTH='$Width'<br><br><br>' >> $reporthtml
	fi

	read mlog medlog < <(calc_band_logratio \
	  "${outroot}_ts.txt" \
	  "${in}_ts.txt" \
	  "$tr" 0.001 0.01)
	lfp005=$(calc_lfp "${in}_ts.txt" "$tr" 0.005)
	lfp010=$(calc_lfp "${in}_ts.txt" "$tr" 0.01)
	tsnr=$(awk -F'\t' '$1 ~ /tSNR of '"$(basename "$in")"'/{print $2}' "${outroot}_stats.tsv")
	read slope slope_pctmin < <(calc_drift_slope "${in}_ts.txt" "$tr")
	adev=$(calc_allan_dev_multi "${in}_ts.txt" "$tr" "$ALLAN_TAUS")
	label=$(basename "${in}")
	base="raw"
	method="poly"
	param="$pddeg"
	lag1=$(calc_lag1 "${in}_ts.txt")
	read r005 r01 < <(calc_psd_log10_ratios "${in}_ts.txt" "$tr")
	echo -e "${label}\t${base}\t${method}\t${param}\t${mlog}\t${medlog}\t${lfp005}\t${lfp010}\t${tsnr}\t${slope}\t${slope_pctmin}\t${lag1}\t${r005}\t${r01}\t${adev}" >> "$metrics_tsv"

	
	echo '<b>'${fmriname}'_hppd'$pddeg'</b><br>' >> $reporthtml
	echo '<i>Independent component analysis:</i>' >> $reporthtml
	if [ $ICA = TRUE ] ; then
		echo " running ICA"
		ICAin=${outroot}_hppd${pddeg}
		if [ -e "${ICAin}_s${spatialsmoothingsigma}.ica" ] ; then 
			rm -rf "${ICAin}_s${spatialsmoothingsigma}.ica"
		fi
		fslmaths ${ICAin}.nii.gz -s $pixdim1 ${ICAin}_s${spatialsmoothingsigma} 
		melodic -i ${ICAin}_s${spatialsmoothingsigma} --Oall -m $mask --nobet --report -o ${ICAin}.ica -d $MELODIC_DIM --tr=$tr
			ACTUAL_DIM=$(head -1 "${ICAin}.ica/melodic_mix" | awk '{print NF}')
			echo " estimated ICA dimension: $ACTUAL_DIM" 
		printf '<b><a target="Change" href="./%s.ica/report/00index.html" class="button">MELODIC Report</a></b><br>\n' "$(basename "${ICAin}")" >> "$reporthtml"

		echo " dynamic stripping"
		filt=$(seq -s, 1 $ACTUAL_DIM)
		fsl_regfilt -i "${ICAin}_s${spatialsmoothingsigma}.nii.gz" \
 		-d "${ICAin}.ica/melodic_mix" \
  		-f "$filt" \
  		-o "${ICAin}_clean.nii.gz"
  
		in="${ICAin}_clean"
		vars=($(fmristatistics ${in} ${outroot}_var $mask))
		echo " variance ratio of $(basename ${in}) : ${vars[0]}"
		echo " tSNR of $(basename ${in}) : ${vars[1]}"	
		echo "${vars[0]}" | awk '{printf "Noise variance ratio of '$(basename ${in})' \t%1.2f\n",$1}' >> ${outroot}_stats.tsv
		echo "${vars[1]}" | awk '{printf "tSNR of '$(basename ${in})' \t%1.2f\n",$1}' >> ${outroot}_stats.tsv

		fslmeants -i "${in}" -m $mask -o "${in}_ts.txt"
		read mlog medlog < <(calc_band_logratio \
		  "${outroot}_ts.txt" \
		  "${in}_ts.txt" \
		  "$tr" 0.001 0.01)
		lfp005=$(calc_lfp "${in}_ts.txt" "$tr" 0.005)
		lfp010=$(calc_lfp "${in}_ts.txt" "$tr" 0.01)
		tsnr=$(awk -F'\t' '$1 ~ /tSNR of '"$(basename "$in")"'/{print $2}' "${outroot}_stats.tsv")
		read slope slope_pctmin < <(calc_drift_slope "${in}_ts.txt" "$tr")
		adev=$(calc_allan_dev_multi "${in}_ts.txt" "$tr" "$ALLAN_TAUS")
		label=$(basename "${in}")
		base="raw"
		method="poly-clean"
		param="$pddeg"
		lag1=$(calc_lag1 "${in}_ts.txt")
		read r005 r01 < <(calc_psd_log10_ratios "${in}_ts.txt" "$tr")
		echo -e "${label}\t${base}\t${method}\t${param}\t${mlog}\t${medlog}\t${lfp005}\t${lfp010}\t${tsnr}\t${slope}\t${slope_pctmin}\t${lag1}\t${r005}\t${r01}\t${adev}" >> "$metrics_tsv"

	else
		printf '<b><a class="button disabled">MELODIC Report</a></b><br>\n' >> "$reporthtml"
	fi
	echo '<i>high-pass variance map:</i></br><img src="'${fmriname}'_hppd'$pddeg'_var.png" WIDTH='$Width'<br><br><br>' >> $reporthtml

	# --- calculate delvol ---
	echo " polynomial detrend (deg=${pddeg}) of ${fmriname} DelVol"
	ML_PATHS="addpath('${HCPPIPEDIR}/global/fsl/etc/matlab'); addpath('${this_script_dir}');"
	matlab -nodesktop -nosplash <<< "${ML_PATHS} detrendpolynomial('${outroot}_DelVol',$tr,$pddeg)"
	echo ""
	fslmaths ${outroot}_DelVol_hppd${pddeg} -add ${outroot}_mean ${outroot}_DelVol_hppd${pddeg}

	in="${outroot}_DelVol_hppd${pddeg}"
	vars=($(fmristatistics ${in} ${outroot}_var $mask))
	echo " variance ratio of $(basename ${in}) : ${vars[0]}"
	echo " tSNR of $(basename ${in}) : ${vars[1]}"	
	echo "${vars[0]}" | awk '{printf "Noise variance ratio of '$(basename ${in})' \t%1.2f\n",$1}' >> ${outroot}_stats.tsv
	echo "${vars[1]}" | awk '{printf "tSNR of '$(basename ${in})' \t%1.2f\n",$1}' >> ${outroot}_stats.tsv

	fslmeants -i ${in} -m $mask -o ${in}_ts.txt

        # --- TrendVarRatio from MATLAB (if produced) ---
        if [ -e ${outroot}_hppd${pddeg}_metrics.tsv ] ; then
                # metrics.tsv has header line: metric \t value
                tvr_mean=$(awk -F'\t' '$1=="trend_var_ratio_mean"{print $2}' ${outroot}_hppd${pddeg}_metrics.tsv)
                tvr_med=$(awk -F'\t' '$1=="trend_var_ratio_median"{print $2}' ${outroot}_hppd${pddeg}_metrics.tsv)
                echo "$tvr_mean" | awk '{printf "TrendVarRatio mean of '$(basename ${in})' \t%1.6f\n",$1}' >> ${outroot}_stats.tsv
                echo "$tvr_med"  | awk '{printf "TrendVarRatio median of '$(basename ${in})' \t%1.6f\n",$1}' >> ${outroot}_stats.tsv
        fi

        # --- TrendVarRatio map preview in report (if produced) ---
        if [ -e ${outroot}_hppd${pddeg}_trendVarRatio.nii.gz ] ; then
                slicer ${outroot}_hppd${pddeg}_trendVarRatio $mask -s 4 -a ${outroot}_hppd${pddeg}_trendVarRatio.png
                echo '<i>TrendVarRatio map (var(trend)/var(original)):</i></br><img src="'${fmriname}'_hppd'${pddeg}'_trendVarRatio.png" WIDTH='$Width'<br><br><br>' >> $reporthtml
        fi

	read mlog medlog < <(calc_band_logratio \
	  "${outroot}_DelVol_ts.txt" \
	  "${in}_ts.txt" \
	  "$tr" 0.001 0.01)
	lfp005=$(calc_lfp "${in}_ts.txt" "$tr" 0.005)
	lfp010=$(calc_lfp "${in}_ts.txt" "$tr" 0.01)
	tsnr=$(awk -F'\t' '$1 ~ /tSNR of '"$(basename "$in")"'/{print $2}' "${outroot}_stats.tsv")
	read slope slope_pctmin < <(calc_drift_slope "${in}_ts.txt" "$tr")
	adev=$(calc_allan_dev_multi "${in}_ts.txt" "$tr" "$ALLAN_TAUS")
	label=$(basename "${in}")
	base="DelVol"
	method="poly"
	param="$pddeg"
	lag1=$(calc_lag1 "${in}_ts.txt")
	read r005 r01 < <(calc_psd_log10_ratios "${in}_ts.txt" "$tr")
	echo -e "${label}\t${base}\t${method}\t${param}\t${mlog}\t${medlog}\t${lfp005}\t${lfp010}\t${tsnr}\t${slope}\t${slope_pctmin}\t${lag1}\t${r005}\t${r01}\t${adev}" >> "$metrics_tsv"

	echo '<b>'${fmriname}'_DelVol_hppd'$pddeg'</b><br>' >> $reporthtml
	echo '<i>Independent component analysis:</i>' >> $reporthtml
	if [ $ICA = TRUE ] ; then
		echo " running ICA"
		ICAin=${outroot}_DelVol_hppd${pddeg}
		if [ -e "${ICAin}_s${spatialsmoothingsigma}.ica" ] ; then 
			rm -rf "${ICAin}_s${spatialsmoothingsigma}.ica"
		fi
		fslmaths ${ICAin}.nii.gz -s $pixdim1 ${ICAin}_s${spatialsmoothingsigma} 
		melodic -i ${ICAin}_s${spatialsmoothingsigma} --Oall -m $mask --nobet --report -o ${ICAin}.ica -d $MELODIC_DIM --tr=$tr
			ACTUAL_DIM=$(head -1 "${ICAin}.ica/melodic_mix" | awk '{print NF}')
			echo " estimated ICA dimension: $ACTUAL_DIM" 
		printf '<b><a target="Change" href="./%s.ica/report/00index.html" class="button">MELODIC Report</a></b><br>\n' \
  "$(basename "${ICAin}")" >> "$reporthtml"

		echo " dynamic stripping"
		filt=$(seq -s, 1 $ACTUAL_DIM)
		fsl_regfilt -i "${ICAin}_s${spatialsmoothingsigma}.nii.gz" \
 		-d "${ICAin}.ica/melodic_mix" \
  		-f "$filt" \
  		-o "${ICAin}_clean.nii.gz"
		in="${ICAin}_clean"
		vars=($(fmristatistics ${in} ${outroot}_var $mask))
		echo " variance ratio of $(basename ${in}) : ${vars[0]}"
		echo " tSNR of $(basename ${in}) : ${vars[1]}"	
		echo "${vars[0]}" | awk '{printf "Noise variance ratio of '$(basename ${in})' \t%1.2f\n",$1}' >> ${outroot}_stats.tsv
		echo "${vars[1]}" | awk '{printf "tSNR of '$(basename ${in})' \t%1.2f\n",$1}' >> ${outroot}_stats.tsv

		fslmeants -i ${in} -m $mask -o "${in}_ts.txt"
		fslmeants -i ${ICAin} -m $mask -o "${ICAin}_ts.txt"
		read mlog medlog < <(calc_band_logratio \
		  "${ICAin}_ts.txt" \
		  "${in}_ts.txt" \
		  "$tr" 0.001 0.01)
		lfp005=$(calc_lfp "${in}_ts.txt" "$tr" 0.005)
		lfp010=$(calc_lfp "${in}_ts.txt" "$tr" 0.01)
		tsnr=$(awk -F'\t' '$1 ~ /tSNR of '"$(basename "$in")"'/{print $2}' "${outroot}_stats.tsv")
		read slope slope_pctmin < <(calc_drift_slope "${in}_ts.txt" "$tr")
		adev=$(calc_allan_dev_multi "${in}_ts.txt" "$tr" "$ALLAN_TAUS")
		label=$(basename "${in}")
		base="DelVol"
		method="poly-clean"
		param="$pddeg"
		lag1=$(calc_lag1 "${in}_ts.txt")
		read r005 r01 < <(calc_psd_log10_ratios "${in}_ts.txt" "$tr")
		echo -e "${label}\t${base}\t${method}\t${param}\t${mlog}\t${medlog}\t${lfp005}\t${lfp010}\t${tsnr}\t${slope}\t${slope_pctmin}\t${lag1}\t${r005}\t${r01}\t${adev}" >> "$metrics_tsv"

	else
		printf '<b><a class="button disabled">MELODIC Report</a></b><br>\n' >> "$reporthtml"
	fi
	echo '<i>high-pass variance map:</i></br><img src="'${fmriname}'_DelVol_hppd'$pddeg'_var.png" WIDTH='$Width'<br><br><br>' >> $reporthtml

	echo " extracting ${fmriname}_DelVol_hppd${pddeg} ts"
	fslmaths ${outroot}_DelVol_hppd${pddeg} -Tmean ${outroot}_DelVol_hppd${pddeg}_mean
	i=1; vol=""
	while [ $i -le $numDelVol ] ; do
		vol="$vol ${outroot}_DelVol_hppd${pddeg}_mean"
		i=$((i+1))
	done
	fslmerge -t ${outroot}_DelVol_hppd${pddeg}_plot $vol ${outroot}_DelVol_hppd${pddeg}
	fslmeants -i ${outroot}_DelVol_hppd${pddeg}_plot -m $mask -o ${outroot}_DelVol_hppd${pddeg}_plot_ts.txt
	fsl_tsplot -i ${outroot}_hppd${pddeg}_ts.txt,${outroot}_DelVol_hppd${pddeg}_plot_ts.txt -o  ${outroot}_hppd${pddeg}_ts.png -x Second -u $tr -a $(basename ${outroot})_hppd${pddeg}_ts,$(basename ${outroot})_DelVol_hppd${pddeg}_plot_ts
	echo '<b>'${fmriname}'_hppd'${pddeg}'_ts and '${fmriname}'_DelVol_hppd'${pddeg}'_plot_ts</b></br><img src="'${fmriname}'_hppd'${pddeg}'_ts.png" WIDTH='$Width'<br><br><br>' >> $reporthtml
	DelVoltslist+="${outroot}_DelVol_hppd${pddeg}_plot_ts.txt,"
	DelVollabellist+="$(basename ${outroot}_DelVol_hppd${pddeg}_plot),"

done

# ============================================================
# high pass filtering with bandpass
# ============================================================
echo "<hr><b>High-pass filtering with bandpass</b><br><br>" >> $reporthtml
for hp in ${hps[@]} ; do
	echo " high pass filtering with sigma=$hp (sec)"
	fslmaths  ${outroot} -bptf $(echo "0.5*$hp/$tr" | bc -l) 0 ${outroot}_hp${hp}
	fslmaths ${outroot}_hp${hp} -add ${outroot}_mean ${outroot}_hp${hp}
	in="${outroot}_hp${hp}"
	vars=($(fmristatistics ${in} ${outroot}_var $mask))
	echo " variance ratio of $(basename ${in}) : ${vars[0]}"
	echo " tSNR of $(basename ${in}) : ${vars[1]}"	
	echo "${vars[0]}" | awk '{printf "Noise variance ratio of '$(basename ${in})' \t%1.2f\n",$1}' >> ${outroot}_stats.tsv
	echo "${vars[1]}" | awk '{printf "tSNR of '$(basename ${in})' \t%1.2f\n",$1}' >> ${outroot}_stats.tsv
	fslmeants -i ${in} -m $mask -o ${in}_ts.txt
	tslist+="${in}_ts.txt,"
	labellist+="$(basename ${in}_ts),"

	read mlog medlog < <(calc_band_logratio \
	  "${outroot}_ts.txt" \
	  "${in}_ts.txt" \
	  "$tr" 0.001 0.01)
	lfp005=$(calc_lfp "${in}_ts.txt" "$tr" 0.005)
	lfp010=$(calc_lfp "${in}_ts.txt" "$tr" 0.01)
	tsnr=$(awk -F'\t' '$1 ~ /tSNR of '"$(basename "$in")"'/{print $2}' "${outroot}_stats.tsv")
	read slope slope_pctmin < <(calc_drift_slope "${in}_ts.txt" "$tr")
	adev=$(calc_allan_dev_multi "${in}_ts.txt" "$tr" "$ALLAN_TAUS")
	label=$(basename "${in}")
	base="raw"
	method="bptf"
	param="$hp"
	lag1=$(calc_lag1 "${in}_ts.txt")
	read r005 r01 < <(calc_psd_log10_ratios "${in}_ts.txt" "$tr")
	echo -e "${label}\t${base}\t${method}\t${param}\t${mlog}\t${medlog}\t${lfp005}\t${lfp010}\t${tsnr}\t${slope}\t${slope_pctmin}\t${lag1}\t${r005}\t${r01}\t${adev}" >> "$metrics_tsv"

	echo '<b>'${fmriname}'_hp'${hp}'</b><br>' >> $reporthtml
	echo '<i>Independent component analysis:</i>' >> $reporthtml
	if [ $ICA = TRUE ] ; then
		echo " running ICA"
		ICAin=${outroot}_hp${hp}
		if [ -e "${ICAin}_s${spatialsmoothingsigma}.ica" ] ; then 
			rm -rf "${ICAin}_s${spatialsmoothingsigma}.ica"
		fi

		fslmaths ${ICAin}.nii.gz -s $pixdim1 ${ICAin}_s${spatialsmoothingsigma} 
		melodic -i ${ICAin}_s${spatialsmoothingsigma} --Oall -m $mask --nobet --report -o ${ICAin}.ica -d $MELODIC_DIM --tr=$tr
			ACTUAL_DIM=$(head -1 "${ICAin}.ica/melodic_mix" | awk '{print NF}')
			echo " estimated ICA dimension: $ACTUAL_DIM" 
		printf '<b><a target="Change" href="./%s.ica/report/00index.html" class="button">MELODIC Report</a></b><br>\n' "$(basename "${ICAin}")" >> "$reporthtml"

		echo " dynamic stripping"
		filt=$(seq -s, 1 $ACTUAL_DIM)
		fsl_regfilt -i "${ICAin}_s${spatialsmoothingsigma}.nii.gz" \
 		-d "${ICAin}.ica/melodic_mix" \
  		-f "$filt" \
  		-o "${ICAin}_clean.nii.gz"
		in="${ICAin}_clean"
		vars=($(fmristatistics ${in} ${outroot}_var $mask))
		echo " variance ratio of $(basename ${in}) : ${vars[0]}"
		echo " tSNR of $(basename ${in}) : ${vars[1]}"	
		echo "${vars[0]}" | awk '{printf "Noise variance ratio of '$(basename ${in})' \t%1.2f\n",$1}' >> ${outroot}_stats.tsv
		echo "${vars[1]}" | awk '{printf "tSNR of '$(basename ${in})' \t%1.2f\n",$1}' >> ${outroot}_stats.tsv

		fslmeants -i ${ICAin} -m $mask -o "${ICAin}_ts.txt"
		fslmeants -i ${in} -m $mask -o "${in}_ts.txt"
		read mlog medlog < <(calc_band_logratio \
		  "${ICAin}_ts.txt" \
		  "${in}_ts.txt" \
		  "$tr" 0.001 0.01)
		lfp005=$(calc_lfp "${in}_ts.txt" "$tr" 0.005)
		lfp010=$(calc_lfp "${in}_ts.txt" "$tr" 0.01)
		tsnr=$(awk -F'\t' '$1 ~ /tSNR of '"$(basename "$in")"'/{print $2}' "${outroot}_stats.tsv")
		read slope slope_pctmin < <(calc_drift_slope "${in}_ts.txt" "$tr")
		adev=$(calc_allan_dev_multi "${in}_ts.txt" "$tr" "$ALLAN_TAUS")
		label=$(basename "${in}")
		base="raw"
		method="bptf-clean"
		param="$hp"
		lag1=$(calc_lag1 "${in}_ts.txt")
		read r005 r01 < <(calc_psd_log10_ratios "${in}_ts.txt" "$tr")
		echo -e "${label}\t${base}\t${method}\t${param}\t${mlog}\t${medlog}\t${lfp005}\t${lfp010}\t${tsnr}\t${slope}\t${slope_pctmin}\t${lag1}\t${r005}\t${r01}\t${adev}" >> "$metrics_tsv"

	else
		printf '<b><a class="button disabled">MELODIC Report</a></b><br>\n' >> "$reporthtml"
	fi
	echo '<i>high-pass variance map:</i></br><img src="'${fmriname}'_hp'${hp}'_var.png" WIDTH='$Width'<br><br><br>' >> $reporthtml

	# --- calculate delvol ---
	echo " high pass filtering ${fmriname}_DelVol with sigma=${hp}"
	fslmaths  ${outroot}_DelVol -bptf $(echo "0.5*$hp/$tr" | bc -l) 0 ${outroot}_DelVol_hp${hp}
	fslmaths ${outroot}_DelVol_hp${hp} -add ${outroot}_DelVol_mean ${outroot}_DelVol_hp${hp}
	in="${outroot}_DelVol_hp${hp}"
	vars=($(fmristatistics ${in} ${outroot}_var $mask))
	echo " variance ratio of $(basename ${in}) : ${vars[0]}"
	echo " tSNR of $(basename ${in}) : ${vars[1]}"	
	echo "${vars[0]}" | awk '{printf "Noise variance ratio of '$(basename ${in})' \t%1.2f\n",$1}' >> ${outroot}_stats.tsv
	echo "${vars[1]}" | awk '{printf "tSNR of '$(basename ${in})' \t%1.2f\n",$1}' >> ${outroot}_stats.tsv

	fslmeants -i ${in} -m $mask -o ${in}_ts.txt

	read mlog medlog < <(calc_band_logratio \
	  "${outroot}_DelVol_ts.txt" \
	  "${in}_ts.txt" \
	  "$tr" 0.001 0.01)
	lfp005=$(calc_lfp "${in}_ts.txt" "$tr" 0.005)
	lfp010=$(calc_lfp "${in}_ts.txt" "$tr" 0.01)
	tsnr=$(awk -F'\t' '$1 ~ /tSNR of '"$(basename "$in")"'/{print $2}' "${outroot}_stats.tsv")
	read slope slope_pctmin < <(calc_drift_slope "${in}_ts.txt" "$tr")
	adev=$(calc_allan_dev_multi "${in}_ts.txt" "$tr" "$ALLAN_TAUS")
	label=$(basename "${in}")
	base="DelVol"
	method="bptf"
	param="$hp"
	lag1=$(calc_lag1 "${in}_ts.txt")
	read r005 r01 < <(calc_psd_log10_ratios "${in}_ts.txt" "$tr")
	echo -e "${label}\t${base}\t${method}\t${param}\t${mlog}\t${medlog}\t${lfp005}\t${lfp010}\t${tsnr}\t${slope}\t${slope_pctmin}\t${lag1}\t${r005}\t${r01}\t${adev}" >> "$metrics_tsv"
		
	echo '<b>'${fmriname}'_DelVol_hp'${hp}'</b><br>' >> $reporthtml
	echo '<i>Independent component analysis:</i>' >> $reporthtml
	if [ $ICA = TRUE ] ; then
		echo " running ICA"
		ICAin=${outroot}_DelVol_hp${hp}
		if [ -e "${ICAin}_s${spatialsmoothingsigma}.ica" ] ; then 
			rm -rf "${ICAin}_s${spatialsmoothingsigma}.ica"
		fi

		fslmaths ${ICAin}.nii.gz -s $pixdim1 ${ICAin}_s${spatialsmoothingsigma} 
		melodic -i ${ICAin}_s${spatialsmoothingsigma} --Oall -m $mask --nobet --report -o ${ICAin}.ica -d $MELODIC_DIM --tr=$tr
			ACTUAL_DIM=$(head -1 "${ICAin}.ica/melodic_mix" | awk '{print NF}')
			echo " estimated ICA dimension: $ACTUAL_DIM" 
		printf '<b><a target="Change" href="./%s.ica/report/00index.html" class="button">MELODIC Report</a></b><br>\n' "$(basename "${ICAin}")" >> "$reporthtml"

		echo " dynamic stripping"
		filt=$(seq -s, 1 $ACTUAL_DIM)
		fsl_regfilt -i "${ICAin}_s${spatialsmoothingsigma}.nii.gz" \
 		-d "${ICAin}.ica/melodic_mix" \
  		-f "$filt" \
  		-o "${ICAin}_clean.nii.gz"
		in="${ICAin}_clean"
		vars=($(fmristatistics ${in} ${outroot}_var $mask))
		echo " variance ratio of $(basename ${in}) : ${vars[0]}"
		echo " tSNR of $(basename ${in}) : ${vars[1]}"	
		echo "${vars[0]}" | awk '{printf "Noise variance ratio of '$(basename ${in})' \t%1.2f\n",$1}' >> ${outroot}_stats.tsv
		echo "${vars[1]}" | awk '{printf "tSNR of '$(basename ${in})' \t%1.2f\n",$1}' >> ${outroot}_stats.tsv

		fslmeants -i "${in}" -m $mask -o "${in}_ts.txt"
		read mlog medlog < <(calc_band_logratio \
		  "${outroot}_DelVol_ts.txt" \
		  "${in}_ts.txt" \
		  "$tr" 0.001 0.01)
		lfp005=$(calc_lfp "${in}_ts.txt" "$tr" 0.005)
		lfp010=$(calc_lfp "${in}_ts.txt" "$tr" 0.01)
		tsnr=$(awk -F'\t' '$1 ~ /tSNR of '"$(basename "$in")"'/{print $2}' "${outroot}_stats.tsv")
		read slope slope_pctmin < <(calc_drift_slope "${in}_ts.txt" "$tr")
		adev=$(calc_allan_dev_multi "${in}_ts.txt" "$tr" "$ALLAN_TAUS")
		label=$(basename "${in}")
		base="DelVol"
		method="bptf-clean"
		param="$hp"
		lag1=$(calc_lag1 "${in}_ts.txt")
		read r005 r01 < <(calc_psd_log10_ratios "${in}_ts.txt" "$tr")
		echo -e "${label}\t${base}\t${method}\t${param}\t${mlog}\t${medlog}\t${lfp005}\t${lfp010}\t${tsnr}\t${slope}\t${slope_pctmin}\t${lag1}\t${r005}\t${r01}\t${adev}" >> "$metrics_tsv"

	else
		printf '<b><a class="button disabled">MELODIC Report</a></b><br>\n' >> "$reporthtml"
	fi
	echo '<i>high-pass variance map:</i></br><img src="'${fmriname}'_DelVol_hp'${hp}'_var.png" WIDTH='$Width'<br><br><br>' >> $reporthtml

	echo " extracting ${fmriname}_DelVol_hp${hp} ts"	
	fslmaths ${outroot}_DelVol_hp${hp} -Tmean ${outroot}_DelVol_hp${hp}_mean
	i=1; vol=""
	while [ $i -le $numDelVol ] ; do
 	 vol="$vol ${outroot}_DelVol_hp${hp}_mean"
	 i=$((i+1))
	done
	fslmerge -t ${outroot}_DelVol_hp${hp}_plot $vol ${outroot}_DelVol_hp${hp}
	fslmeants -i ${outroot}_DelVol_hp${hp}_plot -m $mask -o ${outroot}_DelVol_hp${hp}_plot_ts.txt
	fsl_tsplot -i  ${outroot}_hp${hp}_ts.txt,${outroot}_DelVol_hp${hp}_plot_ts.txt -o ${outroot}_hp${hp}_ts.png -x Second -u $tr -a $(basename ${outroot})_hp${hp}_ts,$(basename ${outroot})_hp${hp}_DelVol_plot_ts
	echo '<b>'${fmriname}'_hp'${hp}'_ts and '${fmriname}'_DelVol_hp'${hp}'_plot_ts</b><br><img src="'${fmriname}'_hp'${hp}'_ts.png" WIDTH='$Width'<br><br><br>' >> $reporthtml
	DelVoltslist+="${outroot}_DelVol_hp${hp}_plot_ts.txt,"
	DelVollabellist+="$(basename ${outroot}_DelVol_hp${hp}_plot),"
	#imrm ${outroot}_hp${hp} ${outroot}_hp${hp}_var

done

# ============================================================
# fmri statistics
# ============================================================
echo "<hr><b>fMRI statistics</b><br><br>" >> $reporthtml
cat ${outroot}_stats.tsv | tsv2html >> $reporthtml

fsl_tsplot -i ${tslist} -o ${outroot}_all_ts.png -x Second -u $tr -a $labellist
echo '<b>'${fmriname}'_all_ts</b><br><img src="'${fmriname}'_all_ts.png" WIDTH='$Width'<br><br><br>' >> $reporthtml
fsl_tsplot -i ${DelVoltslist} -o ${outroot}_DelVol_all_ts.png -x Second -u $tr -a $DelVollabellist
echo '<b>'${fmriname}'_DelVol_all_ts</b><br><img src="'${fmriname}'_DelVol_all_ts.png" WIDTH='$Width'<br><br><br><hr>' >> $reporthtml

# ============================================================
# Create plots
# ============================================================
# --- summary plots (ADEV vs tau, band log-ratio, lag1) split by clean/non-clean ---
metrics_tsv=""
for f in "${OutputFolder}/fmri_filter_metrics.tsv" "${OutputFolder}/filter_metrics.tsv" "${outroot}_filter_metrics.tsv" ; do
  if [[ -f "$f" ]]; then metrics_tsv="$f"; break; fi
done

if [[ -n "$metrics_tsv" ]]; then
  summary_prefix="${outroot}_summary"
  make_summary_plots "$metrics_tsv" "$summary_prefix" >/dev/null || true

  # --- DelVol indices (DII/DIS) ---
  delvol_prefix="${outroot}_delvol"
  make_delvol_indices "$metrics_tsv" "$delvol_prefix" >/dev/null || true

  if [[ -f "${delvol_prefix}_delvol_indices.tsv" ]]; then
    echo "<hr><b>DelVol effect summary (DII/DIS)</b><br><br>" >> "$reporthtml"
    cat "${delvol_prefix}_delvol_indices.tsv" | tsv2html >> "$reporthtml"
    echo "<br>" >> "$reporthtml"
  fi
  if [[ -f "${delvol_prefix}_DII_bar.png" ]]; then
    echo "<b>DII bar</b><br><img src=\"$(basename "${delvol_prefix}_DII_bar.png")\" WIDTH=${Width}><br><br>" >> "$reporthtml"
  fi
  if [[ -f "${delvol_prefix}_DIS_bar.png" ]]; then
    echo "<b>DIS bar</b><br><img src=\"$(basename "${delvol_prefix}_DIS_bar.png")\" WIDTH=${Width}><br><br>" >> "$reporthtml"
  fi

  echo "<hr><b>Summary for Deleting initial volume, Detrending, Filtering and Stripping</b><br><br>" >> $reporthtml
  cat $metrics_tsv | tsv2html >> $reporthtml
  
  for suffix in noclean clean ; do
    if [[ -f "${summary_prefix}_adev_vs_tau_${suffix}.png" ]]; then
      echo "<b>ADEV vs tau (${suffix})</b><br><img src=\"$(basename "${summary_prefix}_adev_vs_tau_${suffix}.png")\" WIDTH=${Width}><br><br>" >> "$reporthtml"
    fi
    for cand in \
      "${summary_prefix}_log10_ratio_lt0p005_to_0p005_0p01_${suffix}.png" \
      "${summary_prefix}_median_log10_ratio_0p001_0p01_${suffix}.png" \
      "${summary_prefix}_mean_log10_ratio_0p001_0p01_${suffix}.png" ; do
      if [[ -f "$cand" ]]; then
        echo "<b>Band ratio (${suffix})</b><br><img src=\"$(basename "$cand")\" WIDTH=${Width}><br><br>" >> "$reporthtml"
        break
      fi
    done
    if [[ -f "${summary_prefix}_lag1_${suffix}.png" ]]; then
      echo "<b>lag1 (${suffix})</b><br><img src=\"$(basename "${summary_prefix}_lag1_${suffix}.png")\" WIDTH=${Width}><br><br>" >> "$reporthtml"
    fi
  done
else
  echo "<hr><i>[INFO] metrics TSV not found - skipping summary plots.</i><br>" >> "$reporthtml"
fi

echo "<BR><BR><BR><BR><BR><BR><BR><BR></BODY></HTML>" >>  $reporthtml

indexhtml="$OutputFolder/index.html"
cat <<EOF > $indexhtml
<HTML>
 <HEAD>
   <link REL="stylesheet" TYPE="text/css" href=".files/fsl.css">
   <TITLE>FMRI_QC REPORT</TITLE>
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
  <B>FMRI_QC REPORT</B><BR>
  <FONT size=1>Version 1.0 &copy;2006-2026</FONT><BR>
  <Font size=2>Output directory: $OutputFolder </FONT><BR><BR>
  <center>
   <div class="water">
    <B><span style="color:gray">$(basename $fmri) </span></B>
   </div>
  </center>
  <iframe src="./report.html" frameborder="0" style="overflow:hidden; height:100%; width:100%" class="fullheight" scrolling="auto"></iframe>
 </BODY>
</HTML>
EOF

#imrm ${outroot}_DelVol ${outroot}_DelVol_mean ${outroot}_DelVol_var ${outroot}_mean ${outroot}_example
#if [ -e ${outroot}_example_brain.nii.gz ] ; then imrm ${outroot}_example_brain ; fi

echo ""
echo "Finished. To view, point your web browser at"
echo "file: $indexhtml"

exit 0

