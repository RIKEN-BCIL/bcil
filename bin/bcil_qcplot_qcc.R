#! /usr/bin/env Rscript
#
# Create Shewhart control charts using qcc (limits/statistics) + ggplot2 (plotting)
# bcil_qcplot_qcc_runcolor.R
#
# Required libraries: ggplot2, qcc
#
# Usage:
#   bcil_qcplot_qcc.R <input.csv> <output.png> [options]
#
# PNG: writes four rasters (same stem): <stem>_p2.png, <stem>_p5.png, <stem>_p10.png, <stem>_p50.png
#   Height fixed (2 in at ref dpi = former "medium" height). Width = sum_c max(n_subj*c*mult, min_strip_px)
#   + margin, with mult 2 / 5 / 10 / 50 px per subject (count = all rows per Class in CSV, including NA Y).
#   Facet panel widths follow plotted x range (space="free_x").
# PDF: single vector file at <output> if path ends in .pdf
#
# Input CSV format (compatible with the old script):
#   Column 1: Class  (must be named 'Class')
#   Column 2: X      (can be numeric OR character; e.g., subject id, time, index)
#   Column 3: Y      (numeric; continuous for -x/-m, counts for -c)
#   Optional:
#     Column named 'Run' (if present, points are colored by Run)
#
# Options:
#   -x          Individuals (x) chart (default)
#   -m          Moving range (mR) chart (computed from successive differences)
#   -c          c-chart (count chart)
#   --no-label  disable labeling of out-of-control points
#   --point-size <value> set point size (default: 1.0)
#   --nsigmas <n> number of sigmas for control limits (default: 3)
#   --show-1n2  show 1- and 2-sigma lines (default: on for -x, off otherwise)
#
suppressPackageStartupMessages({
  library(ggplot2)
  library(qcc)
})

args <- commandArgs(trailingOnly = TRUE)

stop_quietly <- function(status = 1) {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  quit(save = "no", status = status, runLast = FALSE)
}

if (length(args) == 0) {
  cat("\nCreate Shewhart control chart using qcc + ggplot2.\n\n")
  cat("Usage: bcil_qcplot_qcc_runcolor.R <input.csv> <output.(pdf|png)> [options]\n\n")
  cat("Chart types:\n")
  cat("  -x: Individuals (x) chart (default)\n")
  cat("  -m: Moving range (mR) chart\n")
  cat("  -c: c-chart (counts)\n\n")
  cat("Options:\n")
  cat("  --no-label           Disable auto labeling (default: enabled)\n")
  cat("  --point-size <val>   Point size (default: 1.0)\n")
  cat("  --nsigmas <n>        Sigma width for limits (default: 3)\n")
  cat("  --show-1n2           Show 1- and 2-sigma lines (default: on for -x)\n\n")
  stop_quietly(0)
}

if (length(args) < 2) {
  cat("ERROR: Please provide <input.csv> and <output.(pdf|png)>\n")
  stop_quietly()
}

datfile <- args[1]
outimg  <- args[2]

# defaults
chart_type <- "-x"
auto_label <- TRUE
point_size <- 1.0
nsigmas <- 3
show_1n2 <- NA  # auto: TRUE for -x, FALSE otherwise

# parse options
if (length(args) >= 3) {
  i <- 3
  while (i <= length(args)) {
    if (args[i] %in% c("-x", "-m", "-c")) {
      chart_type <- args[i]
      i <- i + 1
    } else if (args[i] == "--no-label") {
      auto_label <- FALSE
      i <- i + 1
    } else if (args[i] == "--point-size") {
      if (i + 1 <= length(args)) {
        point_size <- as.numeric(args[i + 1])
        if (is.na(point_size) || point_size <= 0) {
          cat("ERROR: Invalid point size value:", args[i + 1], "\n")
          stop_quietly()
        }
        i <- i + 2
      } else {
        cat("ERROR: --point-size option requires a value\n")
        stop_quietly()
      }
    } else if (args[i] == "--nsigmas") {
      if (i + 1 <= length(args)) {
        nsigmas <- as.integer(args[i + 1])
        if (is.na(nsigmas) || nsigmas < 1) {
          cat("ERROR: Invalid nsigmas value:", args[i + 1], "\n")
          stop_quietly()
        }
        i <- i + 2
      } else {
        cat("ERROR: --nsigmas option requires a value\n")
        stop_quietly()
      }
    } else if (args[i] == "--show-1n2") {
      show_1n2 <- TRUE
      i <- i + 1
    } else {
      cat("WARNING: Unknown option:", args[i], "\n")
      i <- i + 1
    }
  }
}

if (is.na(show_1n2)) {
  show_1n2 <- (chart_type == "-x")
}

dat <- read.csv(datfile, check.names = FALSE, stringsAsFactors = FALSE)

if (!("Class" %in% names(dat))) {
  cat("ERROR: Input must contain a column named 'Class'.\n")
  stop_quietly()
}
if (ncol(dat) < 3) {
  cat("ERROR: Input must have at least 3 columns: Class, X, Y\n")
  stop_quietly()
}

Xval <- names(dat)[2]
Yval <- names(dat)[3]
has_run <- ("Run" %in% names(dat))
has_mm <- "MetricMissing" %in% names(dat)

# Keep X as-is (can be character); Y must be numeric (empty CSV cells -> NA)
dat[[Yval]] <- suppressWarnings(as.numeric(dat[[Yval]]))
if (has_mm) {
  mm0 <- suppressWarnings(as.integer(as.numeric(dat$MetricMissing)))
} else {
  mm0 <- rep(NA_integer_, nrow(dat))
}
# Rows per Class before dropping NA-Y rows (width budget includes NA subjects)
cnt_tbl <- table(dat$Class[!is.na(dat$Class)], dnn = NULL)
cls_width_counts <- setNames(as.integer(cnt_tbl), names(cnt_tbl))

# Keep all rows with a Class (including Y == NA / NaN when MetricMissing was not set — e.g. TSV "NaN");
# otherwise leading subjects vanish from the x-axis until the first finite Y.
keep <- !is.na(dat$Class)
dat <- dat[keep, , drop = FALSE]

if (nrow(dat) < 1L) {
  cat("ERROR: No valid rows after filtering. Check that Y column is numeric.\n")
  stop_quietly()
}

# Decide ordering within each Class:
# - If X is mostly numeric -> order by numeric X
# - Else -> keep file order but create an index per Class
x_num <- suppressWarnings(as.numeric(dat[[Xval]]))
numeric_ratio <- mean(!is.na(x_num))
use_numeric_x <- is.finite(numeric_ratio) && numeric_ratio >= 0.8

dat$X_label <- as.character(dat[[Xval]])

# Create plotting x coordinate (numeric index) per Class
dat <- dat[order(dat$Class), , drop = FALSE]
dat <- do.call(rbind, lapply(split(dat, dat$Class), function(d) {
  if (use_numeric_x) {
    d$X_num <- suppressWarnings(as.numeric(d[[Xval]]))
    d <- d[order(is.na(d$X_num), d$X_num), , drop = FALSE]
  }
  d$X_idx <- seq_len(nrow(d))
  d
}))

# --- helper: build limits using qcc ---
qcc_limits <- function(y, type, nsigmas = 3) {
  q <- qcc::qcc(y, type = type, nsigmas = nsigmas, plot = FALSE)
  center <- unname(q$center)
  lcl <- unname(q$limits[1])
  ucl <- unname(q$limits[2])
  sigma <- (ucl - center) / nsigmas
  list(center = center, lcl = lcl, ucl = ucl, sigma = sigma)
}

# --- MR chart limits for n=2 ---
mr_limits <- function(mr) {
  mrbar <- mean(mr, na.rm = TRUE)
  lcl <- 0
  ucl <- 3.267 * mrbar
  list(center = mrbar, lcl = lcl, ucl = ucl, sigma = NA_real_)
}

# Line trace: interpolate between observed points only; rule=1 leaves NA outside [min,max] x of observed
# (no back-extrapolation of the first value across leading missing subjects).
approx_line_y <- function(x, y) {
  k <- which(!is.na(y))
  if (length(k) == 0L) {
    return(y)
  }
  stats::approx(x[k], y[k], xout = x, rule = 1L)$y
}

# Per-Class limits: qcc when possible; heuristic when <2 non-missing Y (still plot)
limits_df <- do.call(rbind, lapply(split(dat, dat$Class), function(d) {
  y <- d[[Yval]]
  y_obs <- y[!is.na(y)]
  cl <- unique(d$Class)[1]
  if (chart_type == "-x") {
    if (length(y_obs) >= 2L) {
      lim <- qcc_limits(y_obs, type = "xbar.one", nsigmas = nsigmas)
    } else if (length(y_obs) == 1L) {
      c <- y_obs[1]
      eps <- if (is.finite(c) && c != 0) max(abs(c) * 0.02, 1e-12) else 1e-6
      cat("WARNING: Class ", cl, ": only one non-missing Y; heuristic sigma for limits.\n", sep = "")
      lim <- list(
        center = c, lcl = c - nsigmas * eps, ucl = c + nsigmas * eps, sigma = eps
      )
    } else {
      cat("WARNING: Class ", cl, ": no non-missing Y; limit lines omitted.\n", sep = "")
      lim <- list(center = NA_real_, lcl = NA_real_, ucl = NA_real_, sigma = NA_real_)
    }
  } else if (chart_type == "-c") {
    if (length(y_obs) >= 2L) {
      lim <- qcc_limits(y_obs, type = "c", nsigmas = nsigmas)
    } else if (length(y_obs) == 1L) {
      c <- y_obs[1]
      eps <- max(sqrt(max(c, 0.25, na.rm = TRUE)), 0.5)
      cat("WARNING: Class ", cl, ": only one count; heuristic spread for limits.\n", sep = "")
      lim <- list(
        center = c,
        lcl = max(c - nsigmas * eps, 0),
        ucl = c + nsigmas * eps,
        sigma = eps
      )
    } else {
      cat("WARNING: Class ", cl, ": no non-missing counts; limit lines omitted.\n", sep = "")
      lim <- list(center = NA_real_, lcl = NA_real_, ucl = NA_real_, sigma = NA_real_)
    }
  } else if (chart_type == "-m") {
    mr <- abs(diff(y))
    mr <- mr[!is.na(mr)]
    if (length(mr) >= 1L) {
      lim <- mr_limits(mr)
    } else {
      cat("WARNING: Class ", cl, ": no mR pairs; limits at zero.\n", sep = "")
      lim <- list(center = 0, lcl = 0, ucl = 0, sigma = NA_real_)
    }
  } else {
    stop("Unknown chart type")
  }
  data.frame(
    Class = cl,
    center = lim$center,
    lcl = lim$lcl,
    ucl = lim$ucl,
    sigma = lim$sigma,
    stringsAsFactors = FALSE
  )
}))

# Join limits (restore row order — merge can permute rows within Class)
dat$.merge_ord <- seq_len(nrow(dat))
dat <- merge(dat, limits_df, by = "Class", all.x = TRUE, sort = FALSE)
dat <- dat[order(dat$.merge_ord), , drop = FALSE]
dat$.merge_ord <- NULL

# Build plotting y depending on chart type
if (chart_type == "-m") {
  dat <- dat[order(dat$Class, dat$X_idx), , drop = FALSE]
  dat <- do.call(rbind, lapply(split(dat, dat$Class), function(d) {
    d$mr <- c(NA_real_, abs(diff(d[[Yval]])))   # align MR with current point
    d
  }))
  plot_y <- "mr"
  ylab_txt <- "mr-chart"
} else if (chart_type == "-x") {
  plot_y <- Yval
  ylab_txt <- "x-chart"
} else {
  plot_y <- Yval
  ylab_txt <- "c-chart"
}

if (has_mm) {
  mm <- suppressWarnings(as.integer(as.numeric(dat$MetricMissing)))
} else {
  mm <- rep(NA_integer_, nrow(dat))
}

# Points: NA at MetricMissing; line uses plot_y_line (interpolated) so the trace is not broken
dat$plot_y_vis <- dat[[plot_y]]
if (has_mm) {
  miss1 <- !is.na(mm) & mm == 1L
  dat$plot_y_vis[miss1] <- NA_real_
} else {
  miss1 <- rep(FALSE, nrow(dat))
}

dat <- dat[order(dat$Class, dat$X_idx), , drop = FALSE]
dat <- do.call(rbind, lapply(split(dat, dat$Class), function(d) {
  d$plot_y_line <- approx_line_y(d$X_idx, d$plot_y_vis)
  d
}))

# Out-of-control flags for labeling (need finite limits)
dat$out_of_control <- !is.na(dat[[plot_y]]) &
  is.finite(dat$ucl) & is.finite(dat$lcl) &
  (dat[[plot_y]] > dat$ucl | dat[[plot_y]] < dat$lcl)
if (has_mm) {
  dat$out_of_control <- dat$out_of_control | (!is.na(mm) & mm == 1L)
}
# -m first row has mr == NA by construction; only flag missing Y on x/c charts
if (chart_type != "-m") {
  dat$out_of_control <- dat$out_of_control | is.na(dat[[Yval]])
}
# --- label text for outliers (prefer SubjectFolder when non-empty; else X) ---
label_txt <- as.character(dat$X_label)
if ("SubjectFolder" %in% names(dat)) {
  sf <- as.character(dat$SubjectFolder)
  take_sf <- !is.na(sf) & nzchar(sf)
  label_txt[take_sf] <- sf[take_sf]
}
if ("Run" %in% names(dat)) {
  r <- as.character(dat$Run)
  has_r <- !is.na(r) & nzchar(r)
  label_txt <- ifelse(has_r, paste0(label_txt, ":", r), label_txt)
}
label_txt <- ifelse(is.na(label_txt), "", label_txt)

dat$label <- ifelse(dat$out_of_control, label_txt, "")
dat$label <- ifelse(dat$out_of_control & !nzchar(dat$label), as.character(dat$X_label), dat$label)
dat$label[is.na(dat$label)] <- ""

# x-axis ticks: every 10 indices
x_max <- max(dat$X_idx, na.rm = TRUE)
x_breaks <- unique(seq(0, x_max, by = 10))
if (length(x_breaks) == 0) x_breaks <- NULL

# Base plot: line follows interpolated trace; points use observed y only
p <- ggplot(dat, aes(x = X_idx, y = plot_y_line)) +
  geom_line(na.rm = TRUE) +
  scale_x_continuous(
    breaks = x_breaks,
    expand = ggplot2::expansion(mult = c(0.02, 0.05), add = c(0.65, 0.65))
  ) +
  facet_grid(. ~ Class, scales = "free_x", space = "free_x") +
  ylab(ylab_txt) +
  xlab(Xval) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA),
    axis.text.x = element_text(angle = 90, size = 8, face = "italic")
  ) +
  geom_hline(aes(yintercept = center)) +
  geom_hline(aes(yintercept = ucl), linetype = "dashed") +
  geom_hline(aes(yintercept = lcl), linetype = "dashed")

# Points: skip MetricMissing rows (no dot on NA); color by Run if present
pt_dat <- if (has_mm) dat[!miss1, , drop = FALSE] else dat
if (has_run) {
  dat$Run <- as.factor(dat$Run); pt_dat$Run <- as.factor(pt_dat$Run)
  if (nrow(pt_dat) > 0L) {
    # colorblind-safe (Okabe-Ito, blue-first, no green) palette for Run - TH
    cvd_pal <- c("#0072B2","#E69F00","#D55E00","#56B4E9","#CC79A7","#F0E442","#000000","#999999")
    p <- p + geom_point(data = pt_dat, aes(x = X_idx, y = plot_y_vis, color = Run), size = point_size) +
      ggplot2::scale_colour_manual(values = rep(cvd_pal, length.out = max(nlevels(pt_dat$Run), 1L)),
                                   na.value = "#999999")
  }
} else {
  if (nrow(pt_dat) > 0L) {
    p <- p + geom_point(data = pt_dat, aes(x = X_idx, y = plot_y_vis), size = point_size)
  }
}

# 1- and 2-sigma lines for Individuals chart (skip classes with non-finite sigma)
if (show_1n2 && chart_type == "-x") {
  ds <- dat[is.finite(dat$sigma) & is.finite(dat$center), , drop = FALSE]
  ds <- ds[!duplicated(ds$Class), , drop = FALSE]
  if (nrow(ds) > 0L) {
    p <- p +
      geom_hline(data = ds, aes(yintercept = center + 1 * sigma), linetype = "dotted") +
      geom_hline(data = ds, aes(yintercept = center - 1 * sigma), linetype = "dotted") +
      geom_hline(data = ds, aes(yintercept = center + 2 * sigma), linetype = "dotdash") +
      geom_hline(data = ds, aes(yintercept = center - 2 * sigma), linetype = "dotdash")
  }
}

# labeling (missing metric: place orange text at center line)
# geom_text(check_overlap = TRUE): labels are drawn in data order; if a label's bounding box
#   would overlap one already drawn, it is skipped. Many missing metrics share similar y_txt
#   (e.g. center line) so not every out-of-control / missing subject name will appear.
if (auto_label) {
  lab_dat <- dat[dat$out_of_control & dat$label != "", , drop = FALSE]
  if ("MetricMissing" %in% names(lab_dat)) {
    mm_lab <- suppressWarnings(as.integer(as.numeric(lab_dat$MetricMissing)))
    lab_is_miss <- (!is.na(mm_lab) & mm_lab == 1L) |
      (chart_type != "-m" & is.na(lab_dat[[Yval]]))
  } else {
    lab_is_miss <- chart_type != "-m" & is.na(lab_dat[[Yval]])
  }
  lab_dat$y_txt <- ifelse(
    is.na(lab_dat$plot_y_vis),
    ifelse(is.finite(lab_dat$center), lab_dat$center, 0),
    lab_dat$plot_y_vis
  )
  col_miss_lab <- "#ea580c"
  col_ooc_lab <- "#111111"
  if (any(lab_is_miss)) {
    p <- p + geom_text(
      data = lab_dat[lab_is_miss, , drop = FALSE],
      aes(x = X_idx, y = y_txt, label = label),
      inherit.aes = FALSE,
      color = col_miss_lab,
      vjust = -0.5,
      size = 2.5,
      check_overlap = TRUE
    )
  }
  if (any(!lab_is_miss)) {
    p <- p + geom_text(
      data = lab_dat[!lab_is_miss, , drop = FALSE],
      aes(x = X_idx, y = y_txt, label = label),
      inherit.aes = FALSE,
      color = col_ooc_lab,
      vjust = -0.5,
      size = 2.5,
      check_overlap = TRUE
    )
  }
}

# PNG widths: px per subject 2 / 5 / 10 / 50; height = former "medium" (2 in at ref dpi)
dpi_ref <- 2300 / 18
chart_h_in <- 2
min_strip_px <- 72
outer_margin_px <- 180
png_px_per_subj <- c(p2 = 2, p5 = 5, p10 = 10, p50 = 50)
max_png_width_px <- 30000

plot_width_in <- function(mult) {
  cls_u <- unique(dat$Class)
  ch <- as.character(cls_u)
  n_vec <- cls_width_counts[ch]
  miss <- is.na(n_vec)
  if (any(miss)) {
    nr <- vapply(split(dat, dat$Class), nrow, integer(1))
    n_vec[miss] <- unname(nr[ch[miss]])
  }
  panel_px <- pmax(as.numeric(n_vec) * mult, min_strip_px)
  total_w_px <- sum(panel_px) + outer_margin_px
  total_w_px / dpi_ref
}

clamp_png_width_in <- function(w_in, lev) {
  max_w_in <- max_png_width_px / dpi_ref
  if (!is.finite(w_in) || w_in <= 0) {
    return(max_w_in)
  }
  if (w_in > max_w_in) {
    cat(
      "WARNING: ", lev, " width ", sprintf("%.1f", w_in * dpi_ref),
      " px exceeds PNG device limit; clamped to ", max_png_width_px, " px.\n",
      sep = ""
    )
    return(max_w_in)
  }
  w_in
}

if (grepl("\\.[Pp][Dd][Ff]$", outimg)) {
  w_in <- plot_width_in(10)
  ggsave(
    filename = outimg, plot = p, width = w_in, height = chart_h_in,
    dpi = dpi_ref, bg = "transparent", limitsize = FALSE
  )
} else {
  out_stem <- file.path(dirname(outimg), tools::file_path_sans_ext(basename(outimg)))
  for (lev in names(png_px_per_subj)) {
    mult <- unname(png_px_per_subj[[lev]])
    outp <- paste0(out_stem, "_", lev, ".png")
    w_in <- plot_width_in(mult)
    w_in <- clamp_png_width_in(w_in, lev)
    ggsave(
      filename = outp, plot = p, dpi = dpi_ref, width = w_in, height = chart_h_in,
      bg = "transparent", limitsize = FALSE
    )
  }
}
