#!/usr/bin/env Rscript
# bcil_qcsummary_base_v2.R
# Minimal-dependency QC summary generator (no tidyverse/readr/vroom/DT).
# Adds: qc_baseline_by_site.tsv (Protocol/Class + Modality + Metric baseline stats)
#
# Inputs:
#   1) qc_summary_wide.tsv  (must contain: Class, Modality, Run, SubjectFolder + metric columns)
#   2) outdir
#   3) k (optional, default 3)  # MAD outlier threshold
#   4) pdf_max_rows (optional, default 200)
#
# Outputs (in outdir):
#   - qc_summary_flags.tsv
#   - qc_summary.html
#   - qc_summary_flags.pdf
#   - qc_baseline_by_site.tsv   <-- NEW

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  cat("Usage: bcil_qcsummary_base_v2.R <qc_summary_wide.tsv> <outdir> [k=3] [pdf_max_rows=200]\n")
  quit(save="no", status=1, runLast=FALSE)
}
infile <- args[1]
outdir <- args[2]
k <- if (length(args) >= 3) as.numeric(args[3]) else 3
pdf_max_rows <- if (length(args) >= 4) as.integer(args[4]) else 200
gsf    <- if (length(args) >= 5) args[5] else ""     # GroupStudyFolder (to locate <Class>/<Subject>/RawData for the site-overview table)
n_good <- if (length(args) >= 6) as.integer(args[6]) else 20L
n_bad  <- if (length(args) >= 7) as.integer(args[7]) else 10L
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

need <- c("Class","Modality","Run","SubjectFolder")
wide <- tryCatch(
  utils::read.delim(infile, sep="\t", header=TRUE, check.names=FALSE, stringsAsFactors=FALSE),
  error=function(e){ stop(paste("Failed to read:", infile, "\n", e$message)) }
)
miss <- setdiff(need, names(wide))
if (length(miss) > 0) stop(paste("Missing required columns:", paste(miss, collapse=", ")))

# Coerce metric columns to numeric where possible
metric_cols <- setdiff(names(wide), need)
for (cc in metric_cols) {
  suppressWarnings({ x <- as.numeric(wide[[cc]]) })
  if (sum(!is.na(x)) > 0) wide[[cc]] <- x else wide[[cc]] <- NA_real_
}
metric_cols <- metric_cols[sapply(wide[metric_cols], is.numeric)]
if (length(metric_cols) == 0) stop("No numeric metric columns found.")

mad1 <- function(x) stats::mad(x, constant=1, na.rm=TRUE)

# ---------- Baseline stats by site/protocol (Class) + Modality + Metric ----------
baseline_rows <- list()
rowi <- 1
keys <- unique(paste(wide$Class, wide$Modality, sep="\t"))
for (kk in keys) {
  parts <- strsplit(kk, "\t")[[1]]
  cls <- parts[1]; mod <- parts[2]
  idx <- which(wide$Class==cls & wide$Modality==mod)
  sub <- wide[idx, , drop=FALSE]
  for (m in metric_cols) {
    x <- sub[[m]]
    n_valid <- sum(!is.na(x))
    if (n_valid == 0) next
    med <- stats::median(x, na.rm=TRUE)
    md  <- mad1(x)
    q05 <- as.numeric(stats::quantile(x, probs=0.05, na.rm=TRUE, names=FALSE, type=7))
    q95 <- as.numeric(stats::quantile(x, probs=0.95, na.rm=TRUE, names=FALSE, type=7))
    baseline_rows[[rowi]] <- data.frame(
      Protocol = cls,
      Modality = mod,
      Metric = m,
      n_valid = n_valid,
      median = med,
      mad = md,
      p05 = q05,
      p95 = q95,
      stringsAsFactors=FALSE
    )
    rowi <- rowi + 1
  }
}
baseline <- if (length(baseline_rows)>0) do.call(rbind, baseline_rows) else data.frame()
out_baseline <- file.path(outdir, "qc_baseline_by_site.tsv")
if (nrow(baseline) > 0) {
  baseline <- baseline[order(baseline$Protocol, baseline$Modality, baseline$Metric), ]
  utils::write.table(baseline, out_baseline, sep="\t", row.names=FALSE, quote=FALSE)
} else {
  utils::write.table(data.frame(Protocol=character(),Modality=character(),Metric=character(),
                                n_valid=integer(),median=numeric(),mad=numeric(),p05=numeric(),p95=numeric()),
                     out_baseline, sep="\t", row.names=FALSE, quote=FALSE)
}

# ---------- Precompute med & mad per Class+Modality for scoring ----------
stats_map <- new.env(parent=emptyenv())
for (kk in keys) {
  parts <- strsplit(kk, "\t")[[1]]
  cls <- parts[1]; mod <- parts[2]
  idx <- which(wide$Class==cls & wide$Modality==mod)
  sub <- wide[idx, , drop=FALSE]
  med <- vapply(metric_cols, function(mm) stats::median(sub[[mm]], na.rm=TRUE), numeric(1))
  md  <- vapply(metric_cols, function(mm) mad1(sub[[mm]]), numeric(1))
  n_valid <- vapply(metric_cols, function(mm) sum(!is.na(sub[[mm]])), integer(1))
  assign(kk, list(med=med, mad=md, n=n_valid), envir=stats_map)
}

# ---------- Compute flags per row ----------
missing_n <- integer(nrow(wide))
outlier_n <- integer(nrow(wide))
worst_metric <- character(nrow(wide))
worst_score <- numeric(nrow(wide))

for (i in seq_len(nrow(wide))) {
  cls <- wide$Class[i]; mod <- wide$Modality[i]
  kk <- paste(cls, mod, sep="\t")
  st <- get(kk, envir=stats_map, inherits=FALSE)
  vals <- wide[i, metric_cols, drop=FALSE]
  missing_n[i] <- sum(is.na(vals))

  scores <- rep(NA_real_, length(metric_cols)); names(scores) <- metric_cols
  for (m in metric_cols) {
    v <- wide[[m]][i]
    if (is.na(v)) next
    if (is.na(st$mad[m]) || st$mad[m]==0 || st$n[m] < 3) next
    scores[m] <- abs(v - st$med[m]) / st$mad[m]
  }

  if (all(is.na(scores))) {
    outlier_n[i] <- 0
    worst_metric[i] <- ""
    worst_score[i] <- NA_real_
  } else {
    outlier_n[i] <- sum(scores > k, na.rm=TRUE)
    wm <- names(which.max(replace(scores, is.na(scores), -Inf)))
    worst_metric[i] <- wm
    worst_score[i] <- scores[wm]
  }
}

flags <- data.frame(
  Class = wide$Class,
  Modality = wide$Modality,
  Run = wide$Run,
  SubjectFolder = wide$SubjectFolder,
  missing_n = missing_n,
  outlier_n = outlier_n,
  worst_metric = worst_metric,
  worst_score = worst_score,
  stringsAsFactors=FALSE
)
flags <- flags[order(-flags$outlier_n, -flags$missing_n, flags$Class, flags$Modality, flags$Run, flags$SubjectFolder), ]

out_tsv <- file.path(outdir, "qc_summary_flags.tsv")
utils::write.table(flags, out_tsv, sep="\t", row.names=FALSE, quote=FALSE)

# --- HTML with DataTables CDN (no DT package) ---
out_html <- file.path(outdir, "qc_summary.html")
max_miss <- max(flags$missing_n, na.rm=TRUE); if (!is.finite(max_miss)) max_miss <- 0
max_out  <- max(flags$outlier_n, na.rm=TRUE); if (!is.finite(max_out)) max_out <- 0
t1_m <- max(1, floor(max_miss*0.33)); t2_m <- max(1, floor(max_miss*0.66))
t1_o <- max(1, floor(max_out*0.33));  t2_o <- max(1, floor(max_out*0.66))

esc <- function(x){
  x <- gsub("&","&amp;", x, fixed=TRUE)
  x <- gsub("<","&lt;", x, fixed=TRUE)
  x <- gsub(">","&gt;", x, fixed=TRUE)
  x <- gsub("\"","&quot;", x)
  x
}

th <- vapply(names(flags), function(nm){
  nm2 <- esc(nm)
  if (nm=="missing_n" || nm=="outlier_n") sprintf("<th name=\"%s\">%s</th>", nm2, nm2) else sprintf("<th>%s</th>", nm2)
}, character(1))
header <- paste0("<thead><tr>", paste(th, collapse=""), "</tr></thead>")

rows <- apply(flags, 1, function(r){
  paste0("<tr>", paste(sprintf("<td>%s</td>", esc(as.character(r))), collapse=""), "</tr>")
})
tbody <- paste0("<tbody>\n", paste(rows, collapse="\n"), "\n</tbody>")

html <- sprintf('<!doctype html>
<html>
<head>
<meta charset="utf-8"/>
<title>QC Summary Flags</title>
<link rel="stylesheet" href="https://cdn.datatables.net/1.13.8/css/jquery.dataTables.min.css"/>
<script src="https://code.jquery.com/jquery-3.7.1.min.js"></script>
<script src="https://cdn.datatables.net/1.13.8/js/jquery.dataTables.min.js"></script>
<style>
body { font-family: sans-serif; margin: 20px; }
table.dataTable thead th { white-space: nowrap; }
tr.issue { background-color: #fff8e1 !important; }
td.miss_low { background: #deebf7; font-weight: bold; }
td.miss_mid { background: #fffde7; font-weight: bold; }
td.miss_hi  { background: #ffebee; font-weight: bold; }
td.out_low { background: #deebf7; font-weight: bold; }
td.out_mid { background: #fffde7; font-weight: bold; }
td.out_hi  { background: #ffebee; font-weight: bold; }
</style>
</head>
<body>
<h2>QC Summary Flags</h2>
<p>Sortable/searchable table. Outliers are computed within Protocol(Class)+Modality using MAD (k=%s).</p>
<p>Baseline table: <code>qc_baseline_by_site.tsv</code></p>
<table id="qctab" class="display" style="width:100%%">
%s
%s
</table>
<script>
$(document).ready(function() {
  var t = $("#qctab").DataTable({ pageLength: 50, scrollX: true });

  var missCol = null, outCol = null;
  $("#qctab thead th").each(function(i){
    var nm = $(this).attr("name");
    if (nm=="missing_n") missCol=i;
    if (nm=="outlier_n") outCol=i;
  });

  t.rows().every(function(){
    var node = this.node();
    var d = this.data();

    var miss = (missCol===null)?0:parseFloat(d[missCol]);
    var out  = (outCol===null)?0:parseFloat(d[outCol]);
    if (isNaN(miss)) miss = 0;
    if (isNaN(out)) out = 0;

    if (miss > 0 || out > 0) $(node).addClass("issue");

    if (missCol!==null){
      var missTd = $(node).find("td").eq(missCol);
      if (miss >= %d) missTd.addClass("miss_hi");
      else if (miss >= %d) missTd.addClass("miss_mid");
      else if (miss > 0) missTd.addClass("miss_low");
    }

    if (outCol!==null){
      var outTd = $(node).find("td").eq(outCol);
      if (out >= %d) outTd.addClass("out_hi");
      else if (out >= %d) outTd.addClass("out_mid");
      else if (out > 0) outTd.addClass("out_low");
    }
  });
});
</script>
</body>
</html>', k, header, tbody, t2_m, t1_m, t2_o, t1_o)

# --- scanner model per Site/Project + CVD-safe color map (used to color site labels in the figures) ---
model_family <- function(subj){
  s <- sub("^_+", "", subj); t <- strsplit(s, "_", fixed=TRUE)[[1]]
  mu <- toupper(if (length(t) >= 2) t[2] else "")
  if (grepl("MR750", mu)) "MR750"
  else if (grepl("PREMIER", mu)) "Premier"
  else if (grepl("SIGNA", mu)) "Signa"
  else if (grepl("PRISMA", mu)) "Prisma"
  else if (grepl("SKYRA", mu)) "Skyra"
  else if (grepl("VERIO", mu)) "Verio"
  else if (grepl("TRIO", mu)) "Trio"
  else if (nzchar(mu)) tools::toTitleCase(tolower(mu)) else NA_character_
}
site_model <- tapply(wide$SubjectFolder, wide$Class, function(v){
  fam <- vapply(v, model_family, character(1)); fam <- fam[!is.na(fam)]
  if (!length(fam)) NA_character_ else names(sort(table(fam), decreasing=TRUE))[1]
})
okabe <- c("#0072B2","#E69F00","#009E73","#CC79A7","#D55E00","#56B4E9","#F0E442","#000000")
mods_present <- sort(unique(as.character(site_model[!is.na(site_model)])))
model_cols <- setNames(okabe[(seq_along(mods_present)-1) %% length(okabe) + 1], mods_present)
site_lab_col <- function(sites) unname(ifelse(is.na(site_model[sites]), "#000000", model_cols[site_model[sites]]))
site_lab_txt <- function(sites) unname(ifelse(is.na(site_model[sites]), sites, paste0(sites, " (", site_model[sites], ")")))
model_legend_html <- if (length(model_cols)) paste0(
  "<p style=\"margin:4px 0\"><b>Site/Project label color = scanner model:</b> ",
  paste(sprintf("<span style=\"color:%s;font-weight:bold\">&#9632; %s</span>", model_cols, names(model_cols)), collapse=" &nbsp; "),
  "</p>\n") else ""
# scanner-model family table (model -> color + Site/Projects using it)
model_table_html <- ""
if (length(model_cols)) {
  mrows <- vapply(names(model_cols), function(m){
    ss <- sort(names(site_model)[!is.na(site_model) & site_model == m])
    paste0("<tr><td style=\"color:", model_cols[m], ";font-weight:bold\">&#9632; ", m,
           "</td><td>", length(ss), "</td><td>", paste(ss, collapse=", "), "</td></tr>")
  }, character(1))
  model_table_html <- paste0(
    "<h3>Scanner models</h3>\n",
    "<table border=\"1\" cellpadding=\"4\" style=\"border-collapse:collapse\">\n",
    "<thead><tr><th>Scanner model</th><th>#Site/Projects</th><th>Site/Projects</th></tr></thead>\n<tbody>\n",
    paste(mrows, collapse="\n"), "\n</tbody></table><br>\n")
}

# --- rotating 3D "cityscape" GIF of the connectivity matrix (base R persp + ImageMagick; offline-capable) ---
# co: sites x sites (diagonal already 0); scol: per-site model color. Returns the gif basename or "".
render_conn_gif <- function(co, scol, outdir, nf=48L){
  if (!nzchar(Sys.which("convert"))) return("")
  ns <- nrow(co); if (ns < 2) return("")
  mix_hex <- function(a,b){ m<-round((grDevices::col2rgb(a)+grDevices::col2rgb(b))/2); sprintf("#%02X%02X%02X",m[1],m[2],m[3]) }
  darken  <- function(c,f){ m<-round(grDevices::col2rgb(c)*f); sprintf("#%02X%02X%02X",m[1],m[2],m[3]) }
  maxh <- max(co, 1L); bw <- 0.40; phi<-20; rr<-2.6; dd<-1.8; expand<-0.90
  sky<-"#FFFFFF"; floorcol<-"#E3E6E9"; gridcol<-"#CFD3D7"
  pp <- function(th) persp(seq_len(ns), seq_len(ns), matrix(0,ns,ns), zlim=c(0,maxh),
        theta=th, phi=phi, r=rr, d=dd, expand=expand, border=NA, col=NA, box=FALSE, axes=FALSE, scale=TRUE)
  corn <- expand.grid(x=c(0.5,ns+0.5), y=c(0.5,ns+0.5), z=c(0,maxh))
  gx<-c(Inf,-Inf); gy<-c(Inf,-Inf)
  grDevices::png(tempfile(), width=480, height=480)
  for (fr in seq_len(nf)) { pm<-pp(360*(fr-1)/nf); tp<-trans3d(corn$x,corn$y,corn$z,pm)
    gx[1]<-min(gx[1],tp$x); gx[2]<-max(gx[2],tp$x); gy[1]<-min(gy[1],tp$y); gy[2]<-max(gy[2],tp$y) }
  grDevices::dev.off()
  half<-max(diff(gx),diff(gy))/2*1.06; fusr<-c(mean(gx)-half,mean(gx)+half,mean(gy)-half,mean(gy)+half)
  quad <- function(pm,x,y,z,col,border=NA,lwd=1){ tp<-trans3d(x,y,z,pm); polygon(tp$x,tp$y,col=col,border=border,lwd=lwd) }
  idx <- which(co>0, arr.ind=TRUE)
  fdir <- file.path(outdir, ".connframes"); unlink(fdir, recursive=TRUE); dir.create(fdir, showWarnings=FALSE)
  for (fr in seq_len(nf)) {
    theta <- 360*(fr-1)/nf
    grDevices::png(sprintf("%s/f_%03d.png", fdir, fr), width=480, height=480, res=100)
    par(mar=c(0,0,1.4,0), bg=sky); pm<-pp(theta); par(usr=fusr)
    quad(pm, c(0.3,ns+0.7,ns+0.7,0.3), c(0.3,0.3,ns+0.7,ns+0.7), rep(0,4), col=floorcol)
    for (g in 0:ns){ tp<-trans3d(c(0.5,ns+0.5),c(g+0.5,g+0.5),c(0,0),pm); lines(tp$x,tp$y,col=gridcol,lwd=0.5)
      tp<-trans3d(c(g+0.5,g+0.5),c(0.5,ns+0.5),c(0,0),pm); lines(tp$x,tp$y,col=gridcol,lwd=0.5) }
    th<-theta*pi/180; depth<-idx[,1]*sin(th)-idx[,2]*cos(th); ord<-order(depth)
    sang<- -0.60+th; Lh<-0.17; ux<-cos(sang); uy<-sin(sang)       # light fixed to viewer: counter-rotate shadow
    for (k in ord){ i<-idx[k,1]; j<-idx[k,2]; h<-co[i,j]; sx<-h*Lh*ux; sy<-h*Lh*uy
      px<-c(i-bw,i+bw,i+bw,i-bw,i-bw+sx,i+bw+sx,i+bw+sx,i-bw+sx); py<-c(j-bw,j-bw,j+bw,j+bw,j-bw+sy,j-bw+sy,j+bw+sy,j+bw+sy)
      hp<-grDevices::chull(px,py); quad(pm,px[hp],py[hp],rep(0,length(hp)),col=grDevices::adjustcolor("#2A2E33",alpha.f=0.18)) }
    for (k in ord){ i<-idx[k,1]; j<-idx[k,2]; h<-co[i,j]; base<-mix_hex(scol[i],scol[j])
      top<-grDevices::adjustcolor(base,alpha.f=0.80); s1<-grDevices::adjustcolor(darken(base,0.86),alpha.f=0.80); s2<-grDevices::adjustcolor(darken(base,0.72),alpha.f=0.80)
      quad(pm,c(i-bw,i+bw,i+bw,i-bw),c(j-bw,j-bw,j-bw,j-bw),c(0,0,h,h),s1,"#2B2F3333",.4)
      quad(pm,c(i-bw,i+bw,i+bw,i-bw),c(j+bw,j+bw,j+bw,j+bw),c(0,0,h,h),s1,"#2B2F3333",.4)
      quad(pm,c(i-bw,i-bw,i-bw,i-bw),c(j-bw,j+bw,j+bw,j-bw),c(0,0,h,h),s2,"#2B2F3333",.4)
      quad(pm,c(i+bw,i+bw,i+bw,i+bw),c(j-bw,j+bw,j+bw,j-bw),c(0,0,h,h),s2,"#2B2F3333",.4)
      quad(pm,c(i-bw,i+bw,i+bw,i-bw),c(j-bw,j-bw,j+bw,j+bw),c(h,h,h,h),top,"#2B2F3355",.4) }
    title("Inter-site connectivity (shared HARP subjects)", cex.main=0.9, col.main="#1b2a3a")
    legend("bottomleft", inset=c(0.03,0.02), legend=names(model_cols), fill=model_cols,
           border=NA, bty="n", cex=0.6, title="Scanner model", title.adj=0, text.col="#1b2a3a")
    grDevices::dev.off()
  }
  gif <- file.path(outdir, "site_connectivity_3d.gif")
  ok <- tryCatch(system2("convert", c("-delay","26","-loop","0", sort(list.files(fdir, full.names=TRUE)),
         "-resize","480x480","-fuzz","3%","-layers","optimize", gif))==0, error=function(e) FALSE)
  unlink(fdir, recursive=TRUE)
  if (ok && file.exists(gif)) "site_connectivity_3d.gif" else ""
}

# --- Site overview table (manufacturer / model / coil / gradient / protocol per site) ---
site_html <- ""
if (nzchar(gsf)) {
  read_kv <- function(path){
    if (!file.exists(path)) return(setNames(character(0), character(0)))
    ln <- tryCatch(readLines(path, warn=FALSE), error=function(e) character(0))
    setNames(sub("^[^,]*,", "", ln), sub(",.*$", "", ln))
  }
  get_coil <- function(rawdir){
    js <- list.files(file.path(rawdir, "NIFTI"), pattern="\\.json$", full.names=TRUE)
    js <- c(grep("BOLD|bold|REST|rest", js, value=TRUE), js)   # prefer BOLD jsons
    for (j in js){
      ln <- tryCatch(readLines(j, warn=FALSE), error=function(e) character(0))
      for (key in c("ReceiveCoilName", "CoilString")){   # Siemens / GE
        m <- grep(key, ln, value=TRUE)
        if (length(m)) return(sub(paste0('.*"', key, '"[^"]*"([^"]*)".*'), "\\1", m[1]))
      }
    }
    NA_character_
  }
  uc <- function(x){ x <- unique(x[!is.na(x) & nzchar(x) & x != "NONE"]); if (length(x)==0) "NA" else paste(x, collapse=", ") }
  labcol <- c(BLUE="#0072B2", ORANGE="#E69F00", RED="#D55E00")
  # per-site HARP statistics (from folder names): subjects, imaging sessions, repeats, traveling across sites
  is_harp <- function(s) "HARP" %in% toupper(strsplit(s, "_", fixed=TRUE)[[1]])
  id_of   <- function(s){ m <- regmatches(s, regexpr("_[0-9]{4}_", s)); if (length(m)) gsub("_","",m) else NA_character_ }
  sess_of <- function(s) sub(".*_([0-9]+)_MR[0-9]+$", "\\1", s)
  hrow <- data.frame(Class=wide$Class, Subj=wide$SubjectFolder, stringsAsFactors=FALSE)
  hrow <- hrow[vapply(hrow$Subj, is_harp, logical(1)), , drop=FALSE]
  if (nrow(hrow)) {
    hrow$ID <- vapply(hrow$Subj, id_of, character(1)); hrow$Sess <- vapply(hrow$Subj, sess_of, character(1))
    hrow <- unique(hrow[!is.na(hrow$ID), c("Class","ID","Sess"), drop=FALSE])
    spid <- tapply(hrow$Class, hrow$ID, function(x) length(unique(x)))
    travel_ids <- names(spid[spid >= 2])
  } else travel_ids <- character(0)
  parse_age <- function(a){ a <- suppressWarnings(as.integer(sub("Y.*$","",a))); if (is.na(a) || a == 0) NA_integer_ else a }
  parse_sex <- function(x){ x <- toupper(substr(x,1,1)); if (x %in% c("M","F")) x else NA_character_ }
  DEMO <- data.frame(ID=character(0), Age=integer(0), Sex=character(0), stringsAsFactors=FALSE)   # cohort-wide (one row per subject folder; deduped later)
  st <- do.call(rbind, lapply(sort(unique(wide$Class)), function(cl){
    subs <- unique(wide$SubjectFolder[wide$Class == cl]); n <- length(subs)
    manu<-mod<-inst<-grd<-coil<-desc<-proto<-character(0); sidv<-agev<-sexv<-character(0)
    for (s in subs){
      raw <- file.path(gsf, cl, s, "RawData")
      si  <- read_kv(file.path(raw, "Studyinfo.csv"))
      manu<-c(manu,si["Manufacturer"]); mod<-c(mod,si["Model"]); inst<-c(inst,si["Institution"])
      grd<-c(grd,si["Gradient"]); desc<-c(desc,si["Study Description"]); coil<-c(coil,get_coil(raw))
      tok <- strsplit(s, "_", fixed=TRUE)[[1]]; proto <- c(proto, if (length(tok) >= 3) tok[3] else NA)   # protocol = 3rd name token (HARP/CRHD)
      sidv<-c(sidv,id_of(s)); agev<-c(agev,si["Patient's Age"]); sexv<-c(sexv,si["Patient's Sex"])
    }
    # demographics: one age/sex per distinct subject ID (first non-missing)
    # unname(): si["Patient's Age"] returns an NA-named element for subjects lacking the field,
    # and those NA names propagate to data.frame row/column names -> error on the full cohort. - TH
    agn <- unname(vapply(agev, parse_age, integer(1))); sxn <- unname(vapply(sexv, parse_sex, character(1)))
    DEMO <<- rbind(DEMO, data.frame(ID=unname(sidv), Age=agn, Sex=sxn, stringsAsFactors=FALSE))
    uid <- unique(sidv[!is.na(sidv)])
    age_by <- vapply(uid, function(i){ v<-agn[sidv==i & !is.na(agn)]; if (length(v)) v[1] else NA_integer_ }, integer(1))
    sex_by <- vapply(uid, function(i){ v<-sxn[sidv==i & !is.na(sxn)]; if (length(v)) v[1] else NA_character_ }, character(1))
    av <- age_by[!is.na(age_by)]
    AgeStr <- if (length(av)) sprintf("%.0f [%d-%d]", mean(av), min(av), max(av)) else "NA"
    SexStr <- sprintf("%d:%d", sum(sex_by=="M",na.rm=TRUE), sum(sex_by=="F",na.rm=TRUE))
    h <- hrow[hrow$Class == cl, , drop=FALSE]
    nHsubj <- length(unique(h$ID))
    nHsess <- nrow(h)                                              # distinct (ID, session) at this site
    nRepeat <- if (nrow(h)) sum(tapply(h$Sess, h$ID, function(x) length(unique(x))) >= 2) else 0L
    nTrav  <- length(intersect(unique(h$ID), travel_ids))
    lab <- if (n>=n_good) "BLUE" else if (n>=n_bad) "ORANGE" else "RED"
    data.frame(Site=cl, Label=lab, Manufacturer=uc(manu), Model=uc(mod), Institution=uc(inst),
               Description=uc(desc), Protocol=uc(proto), GradientCoil=uc(grd), ReceiveCoil=uc(coil), N=n,
               Age=AgeStr, Sex=SexStr, HARPsubj=nHsubj, HARPsess=nHsess, Repeat=nRepeat, Traveling=nTrav, stringsAsFactors=FALSE)
  }))
  rowsh <- apply(st, 1, function(r){
    paste0("<tr><td>", esc(r[["Site"]]), "</td>",
           "<td style=\"color:", labcol[r[["Label"]]], ";font-weight:bold\">", r[["Label"]], "</td>",
           "<td>", esc(r[["Manufacturer"]]), "</td><td>", esc(r[["Model"]]), "</td><td>", esc(r[["Institution"]]), "</td>",
           "<td>", esc(r[["Description"]]), "</td><td>", esc(r[["Protocol"]]), "</td><td>", esc(r[["GradientCoil"]]), "</td><td>", esc(r[["ReceiveCoil"]]), "</td>",
           "<td>", r[["N"]], "</td><td style=\"white-space:nowrap\">", r[["Age"]], "</td><td style=\"white-space:nowrap\">", r[["Sex"]], "</td>",
           "<td>", r[["HARPsubj"]], "</td><td>", r[["HARPsess"]], "</td><td>", r[["Repeat"]], "</td><td>", r[["Traveling"]], "</td></tr>")
  })
  # --- cohort overview: demographics + inter-site connectivity, shown side by side ---
  demo_html <- ""
  tryCatch({
    ## (A) cohort-wide demographics (unique subjects across all sites)
    DEMO <- DEMO[!is.na(DEMO$ID), , drop=FALSE]
    uids <- unique(DEMO$ID)
    age1 <- vapply(uids, function(i){ v<-DEMO$Age[DEMO$ID==i & !is.na(DEMO$Age)]; if (length(v)) min(v) else NA_integer_ }, integer(1))  # age at first scan
    sex1 <- vapply(uids, function(i){ v<-DEMO$Sex[DEMO$ID==i & !is.na(DEMO$Sex)]; if (length(v)) v[1] else NA_character_ }, character(1))
    av <- age1[!is.na(age1)]; nM <- sum(sex1=="M",na.rm=TRUE); nF <- sum(sex1=="F",na.rm=TRUE)
    age_img <- ""
    if (length(av)) {
      # age histogram (5-year bins) split by sex (CVD-safe: M blue, F orange)
      brk <- seq(floor(min(av)/5)*5, ceiling(max(av)/5)*5, by=5)
      aM <- age1[!is.na(age1) & sex1=="M"]; aF <- age1[!is.na(age1) & sex1=="F"]; aU <- age1[!is.na(age1) & is.na(sex1)]
      hM <- hist(aM, breaks=brk, plot=FALSE)$counts; hF <- hist(aF, breaks=brk, plot=FALSE)$counts
      hU <- if (length(aU)) hist(aU, breaks=brk, plot=FALSE)$counts else rep(0L, length(brk)-1)
      mat <- rbind(M=hM, F=hF, Unknown=hU); colnames(mat) <- paste0(brk[-length(brk)], "-", brk[-1]-1)
      dfile <- "cohort_age_hist.png"
      grDevices::png(file.path(outdir, dfile), width=720, height=500, res=110)
      op <- par(mar=c(4,4,2.4,0.6)+0.1)
      barplot(mat, col=c("#0072B2","#E69F00","#BBBBBB"), border="white",
              xlab="Age (years)", ylab="# subjects", las=2, cex.names=0.75,
              main=sprintf("Cohort age distribution (n=%d)", length(av)))
      legend("topright", fill=c("#0072B2","#E69F00","#BBBBBB"), legend=c("M","F","unknown"), bty="n", cex=0.85)
      par(op); grDevices::dev.off()
      age_img <- paste0("<img src=\"", dfile, "\" style=\"width:100%;max-width:520px;border:1px solid #ccc\"/>")
    }
    ## (B) inter-site connectivity: shared HARP subjects between Site/Projects (site labels colored by scanner model)
    conn_img <- ""; conn3d_html <- ""; gif_html <- ""
    if (exists("hrow") && nrow(hrow)) {
      usites <- sort(unique(hrow$Class)); cids <- unique(hrow$ID)
      inc <- matrix(0L, length(cids), length(usites), dimnames=list(cids, usites))
      inc[cbind(match(hrow$ID, cids), match(hrow$Class, usites))] <- 1L
      co <- crossprod(inc); ns <- length(usites)                 # co[i,j] = #subjects at both; diag = site total
      off <- co; diag(off) <- 0L; vmax <- max(off, 1L)
     if (any(off > 0)) {                                   # only when traveling subjects exist (shared across sites)
      ramp <- grDevices::colorRampPalette(c("#F7FBFF","#C6DBEF","#6BAED6","#2171B5","#08306B"))(100)
      shade <- function(v) ramp[max(1L, min(100L, as.integer(round(v/vmax*99)) + 1L))]
      lcol <- site_lab_col(usites)
      ltxt <- site_lab_txt(usites)
      # tight margins: fit label widths exactly, square plot fills the canvas (no wasted whitespace)
      maxch  <- max(nchar(ltxt))
      left_in <- maxch*0.052 + 0.15; bot_in <- left_in*0.72 + 0.05; top_in <- 0.42; right_in <- 1.25  # right room for colorbar
      matin  <- ns*0.3                                   # 0.3 in per cell
      cf <- "site_connectivity.png"
      grDevices::png(file.path(outdir, cf),
                     width=as.integer((left_in+matin+right_in)*110),
                     height=as.integer((top_in+matin+bot_in)*110), res=110)
      op <- par(mai=c(bot_in,left_in,top_in,right_in), xaxs="i", yaxs="i")
      plot(NA, xlim=c(0.5,ns+0.5), ylim=c(0.5,ns+0.5), xaxt="n", yaxt="n", xlab="", ylab="", bty="n", asp=1)
      for (i in seq_len(ns)) for (j in seq_len(ns)) {
        yv <- ns - i + 1; v <- co[i, j]
        col <- if (i == j) "#E8E8E8" else if (v == 0) "#FFFFFF" else shade(v)
        rect(j-0.5, yv-0.5, j+0.5, yv+0.5, col=col, border="#FFFFFF", lwd=1)
        if (v > 0) text(j, yv, v, cex=0.58, col=if (i != j && v/vmax > 0.6) "white" else "black")
      }
      text(seq_len(ns), 0.35, ltxt, srt=45, adj=1, xpd=NA, cex=0.6, col=lcol)   # bottom labels (model in parens, by model color)
      text(0.35, ns:1, ltxt, adj=1, xpd=NA, cex=0.6, col=lcol)                  # left labels
      # colorbar (right margin): blue ramp = number of shared (traveling) subjects
      nb <- length(ramp); xb0 <- ns+0.95; xb1 <- ns+1.35; yb0 <- 1; yb1 <- ns
      for (b in seq_len(nb)) rect(xb0, yb0+(yb1-yb0)*(b-1)/nb, xb1, yb0+(yb1-yb0)*b/nb, col=ramp[b], border=NA, xpd=NA)
      rect(xb0, yb0, xb1, yb1, border="#888888", xpd=NA)
      ticks <- unique(c(0L, vmax)); for (tv in ticks) text(xb1+0.12, yb0+(yb1-yb0)*tv/vmax, tv, adj=c(0,0.5), cex=0.55, xpd=NA)
      text(xb0, yb1+0.75, "# shared\nsubjects (TS)", adj=c(0,0), cex=0.55, font=2, xpd=NA)
      mtext("Inter-site connectivity — shared traveling subjects (diagonal = site total)", side=3, line=1.4, font=2, cex=0.9)
      par(op); grDevices::dev.off()
      conn_img <- paste0("<img src=\"", cf, "\" style=\"width:100%;max-width:640px;border:1px solid #ccc\"/>")
      utils::write.table(data.frame(Site=rownames(co), co, check.names=FALSE),
                         file.path(outdir, "site_connectivity.tsv"), sep="\t", row.names=FALSE, quote=FALSE)
      ## interactive 3D bar chart (ECharts bar3D) — same "cityscape" style: translucent model-blend bars,
      ## tall skyscrapers, white ground with grid, cast shadows, slow auto-rotation
      scol <- lcol                                        # model color per site
      mix_hex <- function(a, b){ m <- round((grDevices::col2rgb(a) + grDevices::col2rgb(b)) / 2); sprintf("#%02X%02X%02X", m[1], m[2], m[3]) }
      sm <- tapply(DEMO$Sex, DEMO$ID, function(x){ x<-x[!is.na(x)]; if (length(x)) x[1] else NA_character_ })  # ID -> sex
      ejs <- function(s) gsub('"', '', s)
      idxs <- which(co > 0 & row(co) != col(co), arr.ind=TRUE)
      cells <- if (nrow(idxs)) paste(vapply(seq_len(nrow(idxs)), function(k){
                 i <- idxs[k,"row"]; j <- idxs[k,"col"]
                 ids <- rownames(inc)[inc[,i] > 0 & inc[,j] > 0]          # subjects shared by both sites
                 sx <- sm[ids]; nM <- sum(sx=="M", na.rm=TRUE); nF <- sum(sx=="F", na.rm=TRUE)
                 sprintf('{"value":[%d,%d,%d],"mf":"%d:%d","ids":"%s","itemStyle":{"color":"%s","opacity":0.82}}',
                         j-1L, i-1L, co[i,j], nM, nF, ejs(paste(ids, collapse=", ")), mix_hex(scol[i], scol[j]))
               }, character(1)), collapse=",") else ""
      cats   <- paste(sprintf('"%s"', gsub('"','',ltxt)), collapse=",")
      lcolstr <- paste(sprintf('"%s"', lcol), collapse=",")               # per-site label color (by scanner model)
      conn3d_html <- paste0(
        "<div id=\"conn3d-sec\" style=\"display:none\">\n",                 # revealed only when echarts-gl loads (online)
        "<h3>2. Inter-site connectivity (3D)</h3>\n",
        "<p>Bar height = traveling subjects shared between the two Site/Projects (diagonal excluded); ",
        "translucent bars colored by the blend of the two scanner-model colors. ",
        "Drag to rotate (it also auto-rotates), scroll to zoom. <b>Hover a bar</b> for the shared count, the M:F split and the shared subject IDs.</p>\n",
        model_legend_html,
        "<div id=\"conn3d\" style=\"width:100%;height:76vh;min-height:620px;background:#ffffff\"></div>\n",
        "</div>\n",
        "<script src=\"https://cdn.jsdelivr.net/npm/echarts@5.5.1/dist/echarts.min.js\"></script>\n",
        "<script src=\"https://cdn.jsdelivr.net/npm/echarts-gl@2.0.9/dist/echarts-gl.min.js\"></script>\n",
        "<script>(function(){var sec=document.getElementById('conn3d-sec'),el=document.getElementById('conn3d');",
        "if(!el||!window.echarts)return;",                                  # offline / CDN blocked -> 2D + GIF only
        "var cats=[", cats, "];var data=[", cells, "];var lcols=[", lcolstr, "];",
        "var lc=function(v,i){return lcols[i];};",
        "try{sec.style.display='block';var ch=echarts.init(el);ch.setOption({backgroundColor:'#ffffff',",
        "tooltip:{formatter:function(p){var d=p.data||{};return cats[p.value[0]]+' × '+cats[p.value[1]]+'<br>shared: '+p.value[2]+(d.mf?' (M:F='+d.mf+')':'')+(d.ids?'<br>IDs: '+d.ids:'');}},",
        "xAxis3D:{type:'category',data:cats,axisLabel:{interval:0,rotate:40,fontSize:9,color:lc}},",
        "yAxis3D:{type:'category',data:cats,axisLabel:{interval:0,rotate:-40,fontSize:9,color:lc}},",
        "zAxis3D:{type:'value',name:'shared'},",
        "grid3D:{boxWidth:160,boxDepth:160,boxHeight:95,environment:'#ffffff',",
        "axisLine:{lineStyle:{color:'#cccccc'}},splitLine:{lineStyle:{color:'#e3e6e9'}},",
        "light:{main:{intensity:1.2,shadow:true,shadowQuality:'high',alpha:35,beta:35},ambient:{intensity:0.55}},",
        "viewControl:{distance:250,alpha:18,beta:35,autoRotate:true,autoRotateSpeed:5,autoRotateAfterStill:3}},",
        "series:[{type:'bar3D',data:data,shading:'realistic',realisticMaterial:{roughness:0.6,metalness:0},",
        "bevelSize:0.12,label:{show:false},emphasis:{label:{show:true,formatter:function(p){return p.value[2];}}}}]});",
        "setTimeout(function(){ch.resize();},0);window.addEventListener('resize',function(){ch.resize();});",  # fit full container width
        "}catch(e){sec.style.display='none';}})();</script>\n")                 # bar3D unavailable -> hide, keep 2D only
     }
    }
    # Cohort overview = demographics + age histogram only. The inter-site connectivity
    # (2D matrix + interactive 3D) is shown in the Traveling subjects section instead. - TH
    demo_html <- paste0(
      "<h2>Cohort overview</h2>\n",
      "<p><b>", length(uids), " unique subjects</b>",
      if (length(av)) sprintf("; age %.1f &plusmn; %.1f yr (median %d, range %d&ndash;%d; n=%d with age)",
                              mean(av), stats::sd(av), as.integer(stats::median(av)), min(av), max(av), length(av)) else "",
      sprintf("; sex M:F = %d:%d (n=%d with sex).</p>\n", nM, nF, nM+nF),
      "<div style=\"max-width:520px\">", age_img,
      "<div style=\"font-size:0.85em;color:#555\">Age at first scan, 5-year bins, split by sex.</div></div>\n")
    utils::write.table(data.frame(ID=uids, AgeAtFirstScan=age1, Sex=sex1),
                       file.path(outdir, "cohort_demographics.tsv"), sep="\t", row.names=FALSE, quote=FALSE)
  }, error=function(e) message("cohort overview skipped: ", conditionMessage(e)))

  site_html <- paste0(
    "<h2>Site overview</h2>\n",
    "<p style=\"font-size:0.85em;color:#555;margin:2px 0\">Age = mean [min&ndash;max] years, Sex = M:F (one record per subject, from Studyinfo.csv; blank/anonymized entries excluded). HARP columns: #HARP subj = distinct subjects scanned with HARP; #HARP sess = imaging sessions (Nova/standard coil of one session counted once); #Repeat = subjects with &ge;2 sessions here; #Traveling = HARP subjects also scanned at another Site/Project.</p>\n",
    "<table border=\"1\" cellpadding=\"4\" style=\"border-collapse:collapse\">\n",
    "<thead><tr><th>Site/Project</th><th>Label</th><th>Manufacturer</th><th>Model</th><th>Institution</th>",
    "<th>Description</th><th>Protocol</th><th>Gradient coil</th><th>Receive coil</th><th>#Subjects</th>",
    "<th>Age</th><th>Sex (M:F)</th><th>#HARP subj</th><th>#HARP sess</th><th>#Repeat</th><th>#Traveling</th></tr></thead>\n<tbody>\n",
    paste(rowsh, collapse="\n"), "\n</tbody></table><br>\n", model_table_html, demo_html)
  utils::write.table(st, file.path(outdir, "site_overview.tsv"), sep="\t", row.names=FALSE, quote=FALSE)
  if (length(model_cols))
    utils::write.table(data.frame(Model=names(model_cols), Color=unname(model_cols),
      SiteProjects=vapply(names(model_cols), function(m) paste(sort(names(site_model)[!is.na(site_model) & site_model==m]), collapse=","), character(1))),
      file.path(outdir, "model_families.tsv"), sep="\t", row.names=FALSE, quote=FALSE)
}
# --- Traveling subjects: same 4-digit Subject ID present across >1 Site/Project ---
travel_html <- ""
{
  ids <- vapply(wide$SubjectFolder, function(s){ m <- regmatches(s, regexpr("_[0-9]{4}_", s)); if (length(m)) gsub("_","",m) else NA_character_ }, character(1))
  df  <- unique(data.frame(ID=ids, Site=wide$Class, Subject=wide$SubjectFolder, stringsAsFactors=FALSE))
  df  <- df[!is.na(df$ID), , drop=FALSE]
  sp  <- split(df, df$ID)
  cross <- sp[vapply(sp, function(d) length(unique(d$Site)) >= 2, logical(1))]
  if (length(cross)) {
    tv <- do.call(rbind, lapply(cross, function(d){
      d <- d[order(d$Site), ]
      data.frame(ID=d$ID[1], nSite=length(unique(d$Site)),
                 Sites=paste(sprintf("%s(%s)", d$Site, d$Subject), collapse=", "),
                 stringsAsFactors=FALSE)
    }))
    tv <- tv[order(-tv$nSite, tv$ID), , drop=FALSE]
    trows <- apply(tv, 1, function(r)
      paste0("<tr><td>", esc(r[["ID"]]), "</td><td>", r[["nSite"]], "</td><td>", esc(r[["Sites"]]), "</td></tr>"))

    # --- colorful matrix figure: HARP-only traveling subjects, cells shaded by number of HARP sessions ---
    img_html <- ""; stats_html <- ""
    tryCatch({
      is_harp  <- function(subj) "HARP" %in% toupper(strsplit(subj, "_", fixed=TRUE)[[1]])
      sess_of  <- function(subj) sub(".*_([0-9]+)_MR[0-9]+$", "\\1", subj)   # session index (Nova/standard of same session share it)
      dfm <- df[vapply(df$Subject, is_harp, logical(1)), , drop=FALSE]
      dfm$Sess <- vapply(dfm$Subject, sess_of, character(1))
      # traveling (HARP at >=2 Site/Projects), most-traveling first
      nsiteH <- tapply(dfm$Site, dfm$ID, function(x) length(unique(x)))
      ids_ord   <- names(sort(nsiteH[nsiteH >= 2], decreasing=TRUE))
      sites_ord <- sort(unique(dfm$Site))
      nx <- length(ids_ord)                              # columns = subjects (horizontal)
      ny <- length(sites_ord)                            # rows = site/projects (vertical)
      # session-count matrix (rows = sites, cols = subjects); drives both shading and statistics
      cntm <- matrix(0L, ny, nx, dimnames=list(sites_ord, ids_ord))
      for (r in seq_len(ny)) for (c in seq_len(nx)) {
        s0 <- dfm$Sess[dfm$ID == ids_ord[c] & dfm$Site == sites_ord[r]]
        cntm[r, c] <- length(unique(s0))
      }
      # CVD-safe sequential blues by HARP session count (1 / 2 / >=3)
      cnt_cols <- c("#C6DBEF", "#6BAED6", "#08519C"); empty_col <- "#FFFFFF"
      cellpx <- 13L
      W <- max(900L, as.integer(cellpx*nx + 210L))
      H <- max(330L,  as.integer(20L*ny + 150L))
      mfile <- "traveling_matrix.png"
      grDevices::png(file.path(outdir, mfile), width=W, height=H, res=110)
      op <- par(mar=c(4, 13, 4, 0.5) + 0.1, xaxs="i", yaxs="i")
      plot(NA, xlim=c(0.5, nx+0.5), ylim=c(0.5, ny+0.5), xaxt="n", yaxt="n",
           xlab="", ylab="", bty="n", asp=NA)
      for (r in seq_len(ny)) {
        yv <- ny - r + 1                                 # site row 1 at top
        for (c in seq_len(nx)) {
          n <- cntm[r, c]; empty <- (n == 0)
          rect(c-0.48, yv-0.48, c+0.48, yv+0.48,
               col=if (empty) empty_col else cnt_cols[min(n, 3L)],
               border=if (empty) "#ECECEC" else "white", lwd=0.6)
        }
      }
      text(seq_len(nx), 0.4, ids_ord, srt=90, adj=1, xpd=NA, cex=0.5)         # subject IDs (bottom)
      text(0.3, ny:1, site_lab_txt(sites_ord), adj=1, xpd=NA, cex=0.66, col=site_lab_col(sites_ord))   # site labels (left, model in parens, colored by scanner model)
      mtext("HARP traveling subjects — cell shade = number of HARP sessions", side=3, line=2.5, cex=0.9, font=2)
      legend(x=(1+nx)/2, y=ny+0.55, xjust=0.5, yjust=0, horiz=TRUE, xpd=NA, bty="n", cex=0.72,
             legend=c("1 session", "2 sessions", "≥3 sessions"), fill=cnt_cols)
      par(op); grDevices::dev.off()
      img_html <- paste0(
        "<p>HARP traveling-subjects matrix: each column is a 4-digit Subject ID scanned with the HARP protocol ",
        "at &ge;2 Site/Projects; each row a Site/Project; cell shade = number of HARP imaging sessions ",
        "(1 / 2 / &ge;3; colorblind-safe blues).</p>\n",
        "<img src=\"", mfile, "\" style=\"max-width:100%;border:1px solid #ccc\"/><br><br>\n")

      # --- statistics derived from the matrix ---
      sites_per_subj <- colSums(cntm > 0)                # how many Site/Projects each subject visited
      sess_per_subj  <- colSums(cntm)                    # total HARP sessions per subject
      subj_per_site  <- rowSums(cntm > 0)                # traveling subjects scanned at each site
      sess_per_site  <- rowSums(cntm)                    # total HARP sessions at each site
      rpt_per_site   <- rowSums(cntm >= 2)               # subjects with repeat sessions at each site
      tot_sess <- sum(cntm)
      # headline
      head_html <- paste0(
        "<p style=\"margin:4px 0\"><b>Matrix statistics:</b> ", nx, " traveling subjects &times; ", ny,
        " Site/Projects, ", tot_sess, " HARP sessions total; sessions/subject median ",
        stats::median(sess_per_subj), " (max ", max(sess_per_subj),
        "); Site/Projects per subject median ", stats::median(sites_per_subj),
        " (range ", min(sites_per_subj), "&ndash;", max(sites_per_subj), ").</p>\n")
      # breadth distribution: subjects visiting exactly k Site/Projects
      bt <- as.data.frame(table(k=sites_per_subj), stringsAsFactors=FALSE); bt$k <- as.integer(bt$k)
      brows <- paste0("<tr><td>", bt$k, "</td><td>", bt$Freq, "</td></tr>", collapse="\n")
      breadth_html <- paste0(
        "<table border=\"1\" cellpadding=\"4\" style=\"border-collapse:collapse;display:inline-block;vertical-align:top;margin-right:24px\">\n",
        "<caption style=\"font-weight:bold;white-space:nowrap\">Travel breadth</caption>\n",
        "<thead><tr><th># Site/Projects<br>visited</th><th># subjects</th></tr></thead>\n<tbody>\n", brows,
        "\n<tr style=\"font-weight:bold;border-top:2px solid #888\"><td>Total</td><td>", sum(bt$Freq), "</td></tr>",
        "\n</tbody></table>\n",
        "<div style=\"display:inline-block;vertical-align:top;max-width:340px;font-size:0.85em;color:#555\">",
        "How many <i>different</i> Site/Projects each traveling subject was scanned at. ",
        "A row &ldquo;k&rdquo; = the number of subjects scanned at exactly <b>k</b> Site/Projects ",
        "(all &ge;2, since a traveling subject by definition visits two or more). ",
        "E.g. the row <b>2</b> counts subjects scanned at exactly two Site/Projects.</div>\n")
      # (per-site stats now live in the top Site overview table; connectivity matrix is in the Cohort overview block)
      stats_html <- paste0(head_html, breadth_html, "<br><br>\n")
    }, error=function(e) message("traveling matrix skipped: ", conditionMessage(e)))

    # big subject list -> its own page (linked), so it doesn't bloat the summary
    writeLines(paste0("<!doctype html><meta charset=\"utf-8\"><title>Traveling subjects</title>",
      "<body style=\"font-family:system-ui,sans-serif;margin:16px\"><h2>Traveling subjects &mdash; subject list (", nrow(tv), ")</h2>",
      "<table border=\"1\" cellpadding=\"4\" style=\"border-collapse:collapse\"><thead><tr><th>Subject ID</th><th>#Site/Projects</th><th>Site/Project (Subject)</th></tr></thead><tbody>",
      paste(trows, collapse="\n"), "</tbody></table>"), file.path(outdir,"traveling_subjects_table.html"))
    travel_html <- paste0(
      "<h2>Traveling subjects <span style=\"font-weight:normal;font-size:0.8em\">(same 4-digit Subject ID across &ge;2 Site/Projects: ", nrow(tv), ")</span></h2>\n",
      if (exists("model_legend_html")) model_legend_html else "",
      "<h3>1. Site/Project &times; subject map</h3>\n",
      img_html,
      stats_html,
      # (the site x site matrix duplicated the subject x site matrix above, so only the interactive 3D is kept)
      if (exists("conn3d_html")) conn3d_html else "",
      "<h3>3. Subjects</h3>\n",
      "<p><a href=\"traveling_subjects_table.html\" target=\"_blank\">Open the full subject list (", nrow(tv), " subjects) in a new tab &raquo;</a></p>\n")
    utils::write.table(tv, file.path(outdir, "traveling_subjects.tsv"), sep="\t", row.names=FALSE, quote=FALSE)
  }
}
# ---------- BQM domain scores + exclusion candidates (site-aware robust-z) ----------
bqm_html <- ""
excl_thr <- if (length(args) >= 8) suppressWarnings(as.numeric(args[8])) else 3
if (!is.finite(excl_thr)) excl_thr <- 3
# NOTE: BQM domains/exclusion are not finalized yet -> computed block disabled (not published).
# Flip the FALSE to TRUE once the main-metric classification is settled.
if (FALSE) tryCatch({
  MET <- list(
    "fMRI_Movement_RelativeRMS_mean_Movement_RelativeRMS_mean"=c("Motion","hi"),
    "fMRI_Movement_AbsoluteRMS_mean_Movement_AbsoluteRMS_mean"=c("Motion","hi"),
    "fMRI_Movement_Regressors_FD_stats_Q2"=c("Motion","hi"),
    "dMRI_DiffusionQC_abs_motion"=c("Motion","hi"),
    "dMRI_DiffusionQC_rel_motion"=c("Motion","hi"),
    "dMRI_DiffusionQC_SNR"=c("SNR","lo"),
    "dMRI_DiffusionQC_CNR"=c("SNR","lo"),
    "fMRI_RestingStateStats_TSNR"=c("SNR","lo"),
    "fMRI_RestingStateStats_CNR"=c("SNR","lo"),
    "fMRI_ICA-FIX_stats_PercOutlier"=c("Artifact","hi"),
    "dMRI_DiffusionQC_outliers"=c("Artifact","hi"),
    "sMRI_T2wToT1w.mincost_FreeSurfer_BBR_mincost"=c("Registration","hi"),
    "fMRI_EPI2T1w.mincost_FreeSurfer_BBR"=c("Registration","hi"),
    "dMRI_nodif2T1w.mincost_FreeSurfer_BBR_mincost"=c("Registration","hi"),
    "sMRI_SurfaceStats_SDS5"=c("Surface","hi"),
    "sMRI_SurfaceStats_MyelinMap_similarity"=c("Surface","lo"),
    "sMRI_SurfaceStats_thickness_similarity"=c("Surface","lo"),
    "sMRI_SurfaceStats_Myelin_Biasfield_IQR"=c("Surface","hi"))
  MET <- MET[names(MET) %in% names(wide)]
  if (length(MET) >= 3 && all(c("Class","SubjectFolder") %in% names(wide))) {
    doms <- unique(vapply(MET, `[`, character(1), 1)); cls <- as.character(wide$Class)
    Z <- matrix(NA_real_, nrow(wide), length(MET), dimnames=list(NULL, names(MET)))
    for (m in names(MET)) {
      x <- suppressWarnings(as.numeric(wide[[m]])); sgn <- if (MET[[m]][2]=="hi") 1 else -1
      for (cc in unique(cls)) {
        idx <- which(cls==cc & is.finite(x)); if (length(idx) < 5) next
        md <- stats::median(x[idx]); ma <- stats::mad(x[idx]); if (!is.finite(ma) || ma==0) next
        Z[idx, m] <- sgn*(x[idx]-md)/ma
      }
    }
    Z[] <- pmax(pmin(Z, 8), -8)                 # cap so a near-zero-MAD metric can't dominate
    subj <- wide$SubjectFolder; usub <- unique(subj)
    site <- vapply(usub, function(s) cls[which(subj==s)[1]], character(1))
    subjZ <- t(vapply(usub, function(s){ r<-which(subj==s); colMeans(Z[r,,drop=FALSE], na.rm=TRUE) }, numeric(length(MET))))
    colnames(subjZ) <- names(MET)
    dom <- data.frame(SubjectFolder=usub, Site=unname(site), stringsAsFactors=FALSE)
    for (dm in doms) { cc <- names(MET)[vapply(MET,`[`,character(1),1)==dm]; dom[[dm]] <- round(rowMeans(subjZ[,cc,drop=FALSE], na.rm=TRUE),2) }
    dom$Overall <- round(apply(dom[doms],1,function(v) if (all(is.na(v))) NA_real_ else max(v,na.rm=TRUE)),2)
    reason <- apply(dom[doms],1,function(v){ w<-doms[which(v>=excl_thr)]; if (length(w)) paste(sprintf("%s(%.1f)",w,v[v>=excl_thr]),collapse="; ") else "" })
    dom$EXCLUDE <- ifelse(nzchar(reason),"YES","no"); dom$reason <- reason
    dom <- dom[order(dom$EXCLUDE=="no", -dom$Overall),]
    utils::write.table(dom, file.path(outdir,"bqm_domain_scores.tsv"), sep="\t", row.names=FALSE, quote=FALSE)
    utils::write.table(dom[dom$EXCLUDE=="YES",,drop=FALSE], file.path(outdir,"exclusion_candidates.tsv"), sep="\t", row.names=FALSE, quote=FALSE)
    loo <- -3; hio <- 8
    pal <- grDevices::colorRampPalette(c("#2166AC","#4393C3","#F7F7F7","#F4A582","#D6604D","#B2182B","#67001F"))(101)
    cof <- function(v){ v<-pmax(pmin(v,hio),loo); pal[round((v-loo)/(hio-loo)*100)+1] }
    w <- dom[order(-dom$Overall),]; w <- w[is.finite(w$Overall) & w$Overall>=2,,drop=FALSE]; if (nrow(w)>60) w <- w[1:60,]
    hm <- ""
    if (nrow(w) >= 1) {
      WM <- as.matrix(w[,doms]); lab <- sub("_001_MR1$","",w$SubjectFolder)
      grDevices::png(file.path(outdir,"bqm_domain_heatmap.png"), width=820, height=16*nrow(w)+150, res=110)
      op <- par(mai=c(0.2,2.6,1.1,1.2))
      plot(NA,xlim=c(0.5,length(doms)+0.5),ylim=c(0.5,nrow(w)+0.5),xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
      for (i in seq_len(nrow(w))) for (j in seq_along(doms)) { v<-WM[i,j]; yv<-nrow(w)-i+1
        rect(j-0.5,yv-0.5,j+0.5,yv+0.5,col=if (is.na(v)) "#DDDDDD" else cof(v),border="white")
        if (!is.na(v)) text(j,yv,sprintf("%.1f",v),cex=0.5,col=if (!is.na(v)&&v>4) "white" else "black") }
      text(seq_along(doms),nrow(w)+0.9,doms,srt=35,adj=0,xpd=NA,cex=0.7,font=2)
      text(0.4,nrow(w):1,lab,adj=1,xpd=NA,cex=0.5)
      nb<-length(pal); xb<-length(doms)+0.8; for (b in 1:nb) rect(xb,1+(nrow(w)-1)*(b-1)/nb,xb+0.3,1+(nrow(w)-1)*b/nb,col=pal[b],border=NA,xpd=NA)
      for (tv in c(loo,0,3,hio)) text(xb+0.42,1+(nrow(w)-1)*(tv-loo)/(hio-loo),tv,adj=c(0,.5),cex=0.6,xpd=NA)
      mtext(sprintf("BQM domain severity — worst subjects (Overall>=2, n=%d)",nrow(w)),3,line=2.3,font=2,cex=0.8,adj=0)
      par(op); grDevices::dev.off()
      hm <- "<img src=\"bqm_domain_heatmap.png\" style=\"max-width:820px;border:1px solid #ccc\"/><br>\n"
    }
    ex <- dom[dom$EXCLUDE=="YES",,drop=FALSE]
    exrows <- if (nrow(ex)) paste(vapply(seq_len(nrow(ex)), function(i) paste0(
      "<tr><td>", ex$SubjectFolder[i], "</td><td>", ex$Site[i], "</td>",
      paste(sprintf("<td>%.1f</td>", as.numeric(unlist(ex[i,doms]))), collapse=""),
      "<td><b>", sprintf("%.1f",ex$Overall[i]), "</b></td><td>", ex$reason[i], "</td></tr>"), character(1)), collapse="\n") else ""
    bqm_html <- paste0(
      "<h2>BQM domain scores &amp; exclusion candidates</h2>\n",
      "<p style=\"font-size:0.9em;color:#555;margin:2px 0\">Each image-quality metric is turned into a <b>within-site</b> robust z-score (MAD; oriented so higher = worse; capped &plusmn;8), then grouped into domains. ",
      "A <b>domain score</b> is the mean z of its metrics; <b>Overall</b> = the worst (max) domain. <b>EXCLUDE = YES</b> when any domain &ge; ", excl_thr,
      ". Full tables: <code>bqm_domain_scores.tsv</code>, <code>exclusion_candidates.tsv</code>.</p>\n",
      hm,
      "<p style=\"font-weight:bold;margin:8px 0 2px\">Exclusion candidates (", nrow(ex), " / ", nrow(dom), " subjects; any domain &ge; ", excl_thr, ")</p>\n",
      "<table border=\"1\" cellpadding=\"4\" style=\"border-collapse:collapse\">\n",
      "<thead><tr><th>Subject</th><th>Site</th>", paste(sprintf("<th>%s</th>",doms),collapse=""), "<th>Overall</th><th>reason</th></tr></thead>\n<tbody>\n",
      exrows, "\n</tbody></table><br>\n")
  }
}, error=function(e) message("BQM domain scores skipped: ", conditionMessage(e)))

# ---------- Traveling-subject BQM reliability (two-way decomposition) + correlation clustering ----------
ts_html <- ""
tryCatch({
  id_of2 <- function(s){ m<-regmatches(s,regexpr("_[0-9]{4}_",s)); if (length(m)) gsub("_","",m) else NA_character_ }
  BQMc <- grep("^(sMRI|fMRI|dMRI)_", names(wide), value=TRUE)
  BQMc <- BQMc[vapply(BQMc, function(m) sum(is.finite(suppressWarnings(as.numeric(wide[[m]])))) >= 10, logical(1))]
  if (length(BQMc) >= 4 && all(c("Class","SubjectFolder") %in% names(wide))) {
    for (m in BQMc) wide[[m]] <- suppressWarnings(as.numeric(wide[[m]]))
    cls <- as.character(wide$Class); allsites <- sort(unique(cls)); SID <- vapply(wide$SubjectFolder, id_of2, character(1))
    mpolish <- function(sid, site, val){
      grand <- stats::median(val); se <- setNames(rep(0,length(unique(site))), unique(site)); su <- NULL
      for (it in 1:100){ su <- tapply(val-grand-se[site], sid, stats::median)
        ne <- tapply(val-grand-su[sid], site, stats::median); ne <- ne - stats::median(ne)
        if (it>1 && max(abs(ne[names(se)]-se[names(se)]),na.rm=TRUE) < 1e-9){ se<-ne; break }; se<-ne }
      list(grand=grand, subj=su, site=se, resid=val-grand-su[sid]-se[site]) }
    rows <- list(); anom <- list()
    for (m in BQMc){
      x <- wide[[m]]; ok <- is.finite(x) & !is.na(SID); if (sum(ok) < 10) next
      ag <- aggregate(x[ok], by=list(SID=SID[ok], Site=cls[ok]), FUN=stats::median); names(ag)[3] <- "val"
      ts <- names(which(tapply(ag$Site, ag$SID, function(s) length(unique(s))) >= 2)); ag <- ag[ag$SID %in% ts,]
      if (nrow(ag) < 10 || length(unique(ag$Site)) < 2) next
      fit <- mpolish(ag$SID, ag$Site, ag$val)
      noise <- 1.4826*stats::median(abs(fit$resid)); subjsd <- 1.4826*stats::mad(fit$subj); sitesd <- 1.4826*stats::mad(fit$site)
      icc <- if ((subjsd^2+sitesd^2+noise^2)>0) subjsd^2/(subjsd^2+sitesd^2+noise^2) else NA_real_
      rows[[m]] <- c(list(BQM=m, n_subj=length(ts), n_sess=nrow(ag), meas_noise=round(noise,4),
                          subj_spread=round(subjsd,4), site_spread=round(sitesd,4), ICC=round(icc,2)), as.list(round(fit$site[allsites],4)))
      if (is.finite(noise) && noise>0){ z<-fit$resid/noise; for (b in which(abs(z)>3)) anom[[length(anom)+1]] <-
        data.frame(SID=ag$SID[b],Site=ag$Site[b],BQM=m,value=round(ag$val[b],4),resid=round(fit$resid[b],4),resid_z=round(z[b],2),stringsAsFactors=FALSE) }
    }
    if (length(rows) >= 3){
      bias <- do.call(rbind, lapply(rows, function(r) as.data.frame(r, check.names=FALSE, stringsAsFactors=FALSE)))
      utils::write.table(bias, file.path(outdir,"bqm_site_bias.tsv"), sep="\t", row.names=FALSE, quote=FALSE)
      an <- if (length(anom)) do.call(rbind, anom) else data.frame(); if (nrow(an)) an <- an[order(-abs(an$resid_z)),]
      utils::write.table(an, file.path(outdir,"bqm_ts_session_anomalies.tsv"), sep="\t", row.names=FALSE, quote=FALSE)
      short <- sub("^(sMRI|fMRI|dMRI)_","",bias$BQM)
      # variance decomposition plot (subject=ICC, site, noise)
      vs<-bias$subj_spread^2; vt<-bias$site_spread^2; vn<-bias$meas_noise^2; tt<-vs+vt+vn; tt[tt==0]<-NA
      Pm<-cbind(vs/tt, vt/tt, vn/tt); o<-order(bias$ICC); Pm<-Pm[o,,drop=FALSE]
      grDevices::png(file.path(outdir,"bqm_icc.png"), width=1120, height=22*nrow(Pm)+110, res=120)
      op<-par(mai=c(0.6,4.2,0.4,1.0),xpd=NA)
      bpp<-barplot(t(Pm),horiz=TRUE,col=c("#0072B2","#E69F00","#BBBBBB"),border="white",names.arg=short[o],las=1,cex.names=0.78,xlab="proportion of robust variance",xlim=c(0,1))
      text(1.015,bpp,sprintf("%.2f",bias$ICC[o]),adj=0,cex=0.72)
      legend("bottomright",inset=c(0,-0.08),fill=c("#0072B2","#E69F00","#BBBBBB"),legend=c("subject (ICC)","site (harmonizable)","noise"),bty="n",cex=0.6,horiz=TRUE)
      title(sprintf("TS variance decomposition — %d BQM (subject fraction = ICC)",nrow(Pm)),cex.main=0.8,adj=0)
      par(op); grDevices::dev.off()
      # site-bias heatmap
      Sm <- as.matrix(bias[,allsites,drop=FALSE]); Sm <- sweep(Sm,1,ifelse(bias$meas_noise>0,bias$meas_noise,NA),"/"); Sm<-pmax(pmin(Sm,4),-4)
      pal<-grDevices::colorRampPalette(c("#2166AC","#4393C3","#F7F7F7","#F4A582","#D6604D","#B2182B"))(101)
      cof<-function(v) if (is.na(v)) "#DDDDDD" else pal[round((v+4)/8*100)+1]; nb<-nrow(Sm); nm<-ncol(Sm)
      grDevices::png(file.path(outdir,"bqm_site_bias.png"), width=54*nm+480, height=22*nb+150, res=120)
      op<-par(mai=c(1.7,4.2,0.45,0.5))
      plot(NA,xlim=c(0.5,nm+0.5),ylim=c(0.5,nb+0.5),xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
      for(i in 1:nb)for(j in 1:nm){v<-Sm[i,j];yv<-nb-i+1;rect(j-.5,yv-.5,j+.5,yv+.5,col=cof(v),border="white")}
      text(1:nm,0.3,allsites,srt=45,adj=1,xpd=NA,cex=0.78); text(0.4,nb:1,short,adj=1,xpd=NA,cex=0.74)
      mtext(sprintf("Site bias per BQM (%d BQM; units = measurement noise; red=high, blue=low)",nb),3,line=0.3,cex=0.7,font=2,adj=0)
      par(op); grDevices::dev.off()
      # correlation clustering (dedup) + representatives
      cl_html <- ""
      tryCatch({
        sf<-wide$SubjectFolder; us<-unique(sf)
        Mcl<-sapply(bias$BQM,function(m){ tapply(wide[[m]],sf,function(z) if(all(is.na(z))) NA_real_ else stats::median(z,na.rm=TRUE))[us] })
        Cc<-suppressWarnings(stats::cor(Mcl,use="pairwise.complete.obs",method="spearman")); Cc[is.na(Cc)]<-0
        hc<-stats::hclust(stats::as.dist(1-abs(Cc)),method="average"); clid<-stats::cutree(hc,h=0.3)
        iccv<-bias$ICC; repf<-rep("",length(clid))
        for (g in unique(clid)){ ix<-which(clid==g); repf[ix[order(-ifelse(is.na(iccv[ix]),-1,iccv[ix]))][1]]<-"YES" }
        cltab<-data.frame(cluster=as.integer(clid), BQM=short, ICC=iccv, representative=repf, stringsAsFactors=FALSE)
        cltab<-cltab[order(cltab$cluster,-ifelse(is.na(cltab$ICC),-1,cltab$ICC)),]
        utils::write.table(cltab, file.path(outdir,"bqm_clusters.tsv"), sep="\t", row.names=FALSE, quote=FALSE)
        hc$labels<-short
        grDevices::png(file.path(outdir,"bqm_cluster_dendro.png"), width=1120, height=22*length(short)+110, res=120)
        op<-par(mai=c(0.5,0.15,0.5,4.3)); plot(stats::as.dendrogram(hc),horiz=TRUE,nodePar=list(lab.cex=0.72,pch=NA),xlab="1 - |Spearman r|")
        abline(v=0.3,col="#D55E00",lty=2); title(sprintf("BQM correlation clustering (|r|>0.7 -> %d clusters)",length(unique(clid))),cex.main=0.8); par(op); grDevices::dev.off()
        reps<-cltab[cltab$representative=="YES",,drop=FALSE]; reps<-reps[order(-ifelse(is.na(reps$ICC),-1,reps$ICC)),]
        rr<-paste(sprintf("<tr><td>%d</td><td>%s</td><td>%s</td></tr>",reps$cluster,reps$BQM,ifelse(is.na(reps$ICC),"NA",sprintf("%.2f",reps$ICC))),collapse="\n")
        cl_html<-paste0("<h4>Correlation clusters &amp; representatives</h4>\n",
          "<p style=\"font-size:0.9em;color:#555;margin:2px 0\">BQM clustered by |Spearman r| (cut at |r|&gt;0.7). Near-duplicate metrics merge; the representative per cluster is the one with the highest ICC. ",length(unique(clid))," clusters from ",length(short)," BQM. Table: <code>bqm_clusters.tsv</code>.</p>\n",
          "<div style=\"display:flex;flex-wrap:wrap;gap:20px;align-items:flex-start\">",
          "<div><a href=\"bqm_cluster_dendro.png\" target=\"_blank\" title=\"Open full image in a new tab\"><img src=\"bqm_cluster_dendro.png\" style=\"max-width:560px;border:1px solid #ccc;cursor:zoom-in\"/></a></div>",
          "<div><table border=\"1\" cellpadding=\"3\" style=\"border-collapse:collapse;font-size:0.85em\"><thead><tr><th>cluster</th><th>representative BQM</th><th>ICC</th></tr></thead><tbody>\n",rr,"\n</tbody></table></div></div><br>\n")
      }, error=function(e) message("BQM clustering skipped: ", conditionMessage(e)))
      ts_html <- paste0(
        "<h3>4. BQM reliability</h3>\n",
        "<p style=\"font-size:0.9em;color:#555;margin:2px 0\">Using subjects scanned at &ge;2 Site/Projects, each BQM is decomposed (robust two-way median polish) into <b>subject</b> + <b>site</b> + residual. ",
        "<b>ICC</b> = subject fraction of variance (cross-site reproducibility); <b>site bias</b> = each site's systematic offset (in measurement-noise units); residual = measurement noise. ",
        "Tables: <code>bqm_site_bias.tsv</code>, <code>bqm_ts_session_anomalies.tsv</code>.</p>\n",
        "<div style=\"display:flex;flex-wrap:wrap;gap:20px;align-items:flex-start\">",
        "<div><a href=\"bqm_icc.png\" target=\"_blank\" title=\"Open full image in a new tab\"><img src=\"bqm_icc.png\" style=\"max-width:560px;border:1px solid #ccc;cursor:zoom-in\"/></a><div style=\"font-size:0.8em;color:#555\">Variance decomposition (blue=subject=ICC, orange=site, grey=noise). <i>Click to open full size in a new tab.</i></div></div>",
        "<div><a href=\"bqm_site_bias.png\" target=\"_blank\" title=\"Open full image in a new tab\"><img src=\"bqm_site_bias.png\" style=\"max-width:560px;border:1px solid #ccc;cursor:zoom-in\"/></a><div style=\"font-size:0.8em;color:#555\">Site bias per BQM (red=site reads high, blue=low). <i>Click to open full size in a new tab.</i></div></div>",
        "</div><br>\n", cl_html)
    }
  }
}, error=function(e) message("TS BQM reliability skipped: ", conditionMessage(e)))

if (nzchar(site_html) || nzchar(travel_html) || nzchar(bqm_html) || nzchar(ts_html))
  html <- sub("<h2>QC Summary Flags</h2>", paste0(site_html, travel_html, bqm_html, ts_html, "<h2>QC Summary Flags</h2>"), html, fixed=TRUE)

writeLines(html, out_html)

# --- PDF: simple multi-page text table (base R only) ---
out_pdf <- file.path(outdir, "qc_summary_flags.pdf")
to_print <- flags
if (nrow(to_print) > pdf_max_rows) to_print <- to_print[seq_len(pdf_max_rows), , drop=FALSE]

txt <- capture.output(print(to_print, row.names=FALSE))
lines_per_page <- 60

grDevices::pdf(out_pdf, width=11.7, height=8.3)
op <- par(mar=c(1,1,1,1))
n <- length(txt)
start <- 1
while (start <= n) {
  end <- min(n, start + lines_per_page - 1)
  plot.new()
  text(0, 1, paste(txt[start:end], collapse="\n"), adj=c(0,1), family="mono", cex=0.6)
  start <- end + 1
}
par(op)
dev.off()

cat("Wrote:\n", out_tsv, "\n", out_html, "\n", out_pdf, "\n", out_baseline, "\n")
