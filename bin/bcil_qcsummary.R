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
td.miss_low { background: #e8f5e9; font-weight: bold; }
td.miss_mid { background: #fffde7; font-weight: bold; }
td.miss_hi  { background: #ffebee; font-weight: bold; }
td.out_low { background: #e8f5e9; font-weight: bold; }
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
