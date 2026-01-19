#! /usr/bin/env Rscript
#
# Create Shewhart control charts using qcc (limits/statistics) + ggplot2 (plotting)
# bcil_qcplot_qcc_runcolor.R
#
# Required libraries: ggplot2, qcc
#
# Usage:
#   bcil_qcplot_qcc_runcolor.R <input.csv> <output.(pdf|png)> [options]
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

# Keep X as-is (can be character); Y must be numeric
dat[[Yval]] <- suppressWarnings(as.numeric(dat[[Yval]]))

# Remove rows with missing essentials (do NOT drop based on X)
dat <- dat[!is.na(dat$Class) & !is.na(dat[[Yval]]), , drop = FALSE]

if (nrow(dat) < 2) {
  cat("ERROR: Not enough valid rows after filtering (need >=2). Check that Y column is numeric.\n")
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

# compute per-Class limits
limits_df <- do.call(rbind, lapply(split(dat, dat$Class), function(d) {
  y <- d[[Yval]]
  if (chart_type == "-x") {
    lim <- qcc_limits(y, type = "xbar.one", nsigmas = nsigmas)
  } else if (chart_type == "-c") {
    lim <- qcc_limits(y, type = "c", nsigmas = nsigmas)
  } else if (chart_type == "-m") {
    mr <- abs(diff(y))
    lim <- mr_limits(mr)
  } else {
    stop("Unknown chart type")
  }
  data.frame(
    Class = unique(d$Class)[1],
    center = lim$center,
    lcl = lim$lcl,
    ucl = lim$ucl,
    sigma = lim$sigma,
    stringsAsFactors = FALSE
  )
}))

# Join limits
dat <- merge(dat, limits_df, by = "Class", all.x = TRUE, sort = FALSE)

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

# Out-of-control flags for labeling
dat$out_of_control <- !is.na(dat[[plot_y]]) & (dat[[plot_y]] > dat$ucl | dat[[plot_y]] < dat$lcl)
# --- label text for outliers ---
label_txt <- dat$X_label
if ("SubjectFolder" %in% names(dat)) {
  label_txt <- dat$SubjectFolder
}
if ("Run" %in% names(dat)) {
  label_txt <- paste0(label_txt, ":", dat$Run)
}

dat$label <- ifelse(dat$out_of_control, label_txt, "")

# x-axis ticks: every 10 indices
x_max <- max(dat$X_idx, na.rm = TRUE)
x_breaks <- unique(seq(0, x_max, by = 10))
if (length(x_breaks) == 0) x_breaks <- NULL

# Base plot: keep the line uncolored so colors only convey Run (when present)
p <- ggplot(dat, aes(x = X_idx, y = .data[[plot_y]])) +
  geom_line() +
  scale_x_continuous(breaks = x_breaks) +
  facet_grid(. ~ Class, scales = "free_x") +
  ylab(ylab_txt) +
  xlab(Xval) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA),
    axis.text.x = element_text(angle = 90, size = 8, face = "italic")
  ) +
  geom_hline(aes(yintercept = center), linewidth = 0.6) +
  geom_hline(aes(yintercept = ucl), linetype = "dashed", linewidth = 0.6) +
  geom_hline(aes(yintercept = lcl), linetype = "dashed", linewidth = 0.6)

# Points: color by Run if present
if (has_run) {
  dat$Run <- as.factor(dat$Run)
  p <- p + geom_point(aes(color = Run), size = point_size)
} else {
  p <- p + geom_point(size = point_size)
}

# 1- and 2-sigma lines for Individuals chart
if (show_1n2 && chart_type == "-x") {
  p <- p +
    geom_hline(aes(yintercept = center + 1 * sigma), linetype = "dotted", linewidth = 0.4) +
    geom_hline(aes(yintercept = center - 1 * sigma), linetype = "dotted", linewidth = 0.4) +
    geom_hline(aes(yintercept = center + 2 * sigma), linetype = "dotdash", linewidth = 0.4) +
    geom_hline(aes(yintercept = center - 2 * sigma), linetype = "dotdash", linewidth = 0.4)
}

# labeling
if (auto_label) {
  p <- p + geom_text(
    data = dat[dat$out_of_control & dat$label != "", , drop = FALSE],
    aes(label = label),
    vjust = -0.5,
    size = 2.5,
    check_overlap = TRUE
  )
}

ggsave(filename = outimg, plot = p, dpi = 150, width = 18, height = 2, bg = "transparent")
