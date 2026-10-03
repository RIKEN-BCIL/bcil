#!/usr/bin/env Rscript
# bcil_qcsiteplots_base_v2.R
# Fix: show ALL sites/protocols on x-axis even if a metric is missing (n=0) for some sites.
# Base R only. Generates boxplot + jittered points and HTML index.
#
# Usage: bcil_qcsiteplots_base_v2.R <qc_summary_wide.tsv> <outdir> [n_good=20] [n_bad=10]

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  cat("Usage: bcil_qcsiteplots_base_v2.R <qc_summary_wide.tsv> <outdir> [n_good=20] [n_bad=10]\n")
  quit(save="no", status=1, runLast=FALSE)
}
infile <- args[1]
outdir <- args[2]
n_good <- if (length(args) >= 3) as.integer(args[3]) else 20
n_bad  <- if (length(args) >= 4) as.integer(args[4]) else 10

if (!dir.exists(outdir)) dir.create(outdir, recursive=TRUE, showWarnings=FALSE)
plotdir <- file.path(outdir, "siteplots")
if (!dir.exists(plotdir)) dir.create(plotdir, recursive=TRUE, showWarnings=FALSE)

need <- c("Class","Modality","Run","SubjectFolder")
wide <- tryCatch(
  utils::read.delim(infile, sep="\t", header=TRUE, check.names=FALSE, stringsAsFactors=FALSE),
  error=function(e){ stop(paste("Failed to read:", infile, "\n", e$message)) }
)
miss <- setdiff(need, names(wide))
if (length(miss) > 0) stop(paste("Missing required columns:", paste(miss, collapse=", ")))

# numeric coercion
metric_cols <- setdiff(names(wide), need)
for (cc in metric_cols) {
  suppressWarnings({ x <- as.numeric(wide[[cc]]) })
  if (sum(!is.na(x)) > 0) wide[[cc]] <- x else wide[[cc]] <- NA_real_
}
metric_cols <- metric_cols[sapply(wide[metric_cols], is.numeric)]
if (length(metric_cols) == 0) stop("No numeric metric columns found.")

# sanitize filename
safe_name <- function(x){
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("_+", "_", x)
  x
}

# reliability colors by n
rel_color <- function(n){
  ifelse(n >= n_good, "#2e7d32", ifelse(n >= n_bad, "#f9a825", "#c62828"))
}

modalities <- unique(wide$Modality)
sites_all <- sort(unique(wide$Class))

index_rows <- list()
ri <- 1

png_width <- 1400
png_height <- 900

for (mod in modalities) {
  subm <- wide[wide$Modality == mod, , drop=FALSE]
  if (nrow(subm) == 0) next

  for (met in metric_cols) {
    y_all <- subm[[met]]
    if (sum(!is.na(y_all)) < 2) next

    dat <- subm[, c("Class", met), drop=FALSE]
    names(dat)[2] <- "Y"

    # n per site (include 0)
    n_by_site <- sapply(sites_all, function(s){
      sum(!is.na(dat$Y[dat$Class == s]))
    })
    nvec <- as.integer(n_by_site)
    lab_cols <- rel_color(nvec)
    lab_txt <- paste0(sites_all, " (n=", nvec, ")")

    # median per site for ordering (sites with no data get NA, go to end)
    med_by_site <- sapply(sites_all, function(s){
      v <- dat$Y[dat$Class == s]
      if (sum(!is.na(v)) == 0) return(NA_real_)
      stats::median(v, na.rm=TRUE)
    })
    ord <- order(med_by_site, na.last=TRUE)
    sites <- sites_all[ord]
    nvec <- nvec[ord]
    lab_cols <- lab_cols[ord]
    lab_txt <- lab_txt[ord]
    pos <- seq_along(sites)

    # build list of values per site
    vals_list <- lapply(sites, function(s){
      v <- dat$Y[dat$Class == s]
      v[!is.na(v)]
    })
    names(vals_list) <- sites
    nonempty <- sapply(vals_list, function(v) length(v) > 0)

    # file
    fn <- paste0("box_", safe_name(mod), "_", safe_name(met), ".png")
    fpath <- file.path(plotdir, fn)

    grDevices::png(fpath, width=png_width, height=png_height, res=150)
    op <- par(mar=c(10,5,4,2) + 0.1)

    # set y limits using all available values
    yy <- dat$Y[!is.na(dat$Y)]
    ylim <- range(yy, finite=TRUE)
    if (!all(is.finite(ylim)) || diff(ylim)==0) {
      ylim <- c(0, 1)
    } else {
      pad <- 0.05 * diff(ylim)
      ylim <- c(ylim[1]-pad, ylim[2]+pad)
    }

    # start empty plot
    plot(NA, xlim=c(0.5, length(sites)+0.5), ylim=ylim, xaxt="n",
         xlab="Site/Protocol (label color indicates reliability)",
         ylab=met, main=paste0(mod, " / ", met))

    # draw boxplots only for sites with data, but keep positions
    if (any(nonempty)) {
      bp <- boxplot(vals_list[nonempty], plot=FALSE, outline=FALSE)
      bxp(bp, at=pos[nonempty], add=TRUE, outline=FALSE, axes=FALSE)
    }

    # jittered points for all available values
    if (sum(!is.na(dat$Y)) > 0) {
      cls_to_pos <- match(dat$Class, sites)
      keep <- !is.na(dat$Y) & !is.na(cls_to_pos)
      xj <- jitter(cls_to_pos[keep], amount=0.18)
      points(xj, dat$Y[keep], pch=16, cex=0.5, col=grDevices::rgb(0,0,0,0.25))
    }

    # axis labels with colored text
    axis(1, at=pos, labels=FALSE)
    usr <- par("usr")

    text(
      x = pos,
      y = usr[3] - 0.06*(usr[4]-usr[3]),
      labels = lab_txt,
      srt = 0, adj = c(0.5, 1),
      xpd = NA, cex = 0.8, col = lab_cols
         )

    legend("topright",
           legend=c(paste0("GREEN: n>=", n_good),
                    paste0("ORANGE: ", n_bad, "<=n<", n_good),
                    paste0("RED: n<", n_bad)),
           col=c("#2e7d32", "#f9a825", "#c62828"),
           pch=15, pt.cex=1.2, bty="n", cex=0.85)

    par(op)
    grDevices::dev.off()

    index_rows[[ri]] <- data.frame(
      Modality=mod,
      Metric=met,
      Sites=length(sites),
      MinN=min(nvec),
      MedianN=as.integer(stats::median(nvec)),
      MaxN=max(nvec),
      PlotFile=file.path("siteplots", fn),
      stringsAsFactors=FALSE
    )
    ri <- ri + 1
  }
}

index <- if (length(index_rows)>0) do.call(rbind, index_rows) else data.frame()
out_html <- file.path(outdir, "qc_siteplots.html")

esc <- function(x){
  x <- gsub("&","&amp;", x, fixed=TRUE)
  x <- gsub("<","&lt;", x, fixed=TRUE)
  x <- gsub(">","&gt;", x, fixed=TRUE)
  x <- gsub("\"","&quot;", x, fixed=TRUE)
  x
}

if (nrow(index) == 0) {
  writeLines("<html><body><h2>No plots generated</h2></body></html>", out_html)
  cat("No plots generated. Check input.\n")
  quit(save="no", status=0, runLast=FALSE)
}

th <- paste0("<thead><tr>",
             "<th>Modality</th><th>Metric</th><th>#Sites</th><th>Min n</th><th>Median n</th><th>Max n</th><th>Plot</th>",
             "</tr></thead>")
rows <- apply(index, 1, function(r){
  mod <- esc(r[["Modality"]]); met <- esc(r[["Metric"]])
  link <- esc(r[["PlotFile"]])
  paste0("<tr>",
         "<td>", mod, "</td>",
         "<td>", met, "</td>",
         "<td>", r[["Sites"]], "</td>",
         "<td>", r[["MinN"]], "</td>",
         "<td>", r[["MedianN"]], "</td>",
         "<td>", r[["MaxN"]], "</td>",
         "<td><a href=\"", link, "\" target=\"_blank\">open</a></td>",
         "</tr>")
})
tbody <- paste0("<tbody>\n", paste(rows, collapse="\n"), "\n</tbody>")

html <- paste0('<!doctype html>
<html>
<head>
<meta charset="utf-8"/>
<title>QC Site/Protocol Plots</title>
<link rel="stylesheet" href="https://cdn.datatables.net/1.13.8/css/jquery.dataTables.min.css"/>
<script src="https://code.jquery.com/jquery-3.7.1.min.js"></script>
<script src="https://cdn.datatables.net/1.13.8/js/jquery.dataTables.min.js"></script>
<style>
body { font-family: sans-serif; margin: 20px; }
table.dataTable thead th { white-space: nowrap; }
</style>
</head>
<body>
<h2>QC Site/Protocol Distribution Plots</h2>
<p>Per metric: boxplot across sites/protocols within each modality.</p>
<p>All sites/protocols are shown on the x-axis; sites with <code>n=0</code> have no box but remain labeled.</p>
<p>X-axis labels include per-site <code>n_valid</code>; label color indicates reliability:</p>
<ul>
<li><span style="color:#2e7d32;font-weight:bold;">GREEN</span>: n &ge; ', n_good, '</li>
<li><span style="color:#f9a825;font-weight:bold;">ORANGE</span>: ', n_bad, ' &le; n &lt; ', n_good, '</li>
<li><span style="color:#c62828;font-weight:bold;">RED</span>: n &lt; ', n_bad, '</li>
</ul>

<table id="tab" class="display" style="width:100%">
', th, '
', tbody, '
</table>

<script>
$(document).ready(function() {
  $("#tab").DataTable({ pageLength: 25, order: [[0,"asc"],[1,"asc"]] });
});
</script>
</body>
</html>')

writeLines(html, out_html)
cat("Wrote:\n", out_html, "\nPlots in:\n", plotdir, "\n")
