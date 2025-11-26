rm(list = ls())

# ---- packages ----
required_packages <- c("tidyverse", "tcltk", "ggplot2", "lubridate", "scales",
                       "ggh4x", "grid", "ggrepel")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# ------------------------------------------------------------
# Unregulated peak-flow timeline with broken (faceted) x-axis
# ------------------------------------------------------------

# ===== CONFIG =====
gap_years <- 50   # gap that starts a new era

# ===== START DIR + FILE PICKER =====
# start_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) getwd())
# pick_file <- function() {
#   old_wd <- getwd(); on.exit(setwd(old_wd), add = TRUE); setwd(start_dir)
#   tryCatch(file.choose(), error = function(e) NA_character_)
# }
# csv_path <- pick_file()
# stopifnot(!is.na(csv_path))
csv_path <- "D:/0.RMC/Willamette/2025-Report/data/BestFit_input/CGR_3day_Input_Paleo_BB_nopsi.csv"

# ===== READ RAW (two header rows) =====
raw <- readr::read_csv(csv_path, col_names = FALSE, na = c("", "NA"), show_col_types = FALSE)
stopifnot(nrow(raw) >= 2)

header_row  <- as.character(raw[2, ])

find_block <- function(headers, pattern) {
  n <- length(headers); k <- length(pattern)
  for (i in seq_len(n - k + 1)) {
    if (all(tolower(trimws(headers[i:(i + k - 1)])) == tolower(trimws(pattern)))) {
      return(i:(i + k - 1))
    }
  }
  integer(0)
}

# Expected column-name sequences (row 2)
exact_pat     <- c("year", "flow", "plotting position")
interval_pat  <- c("year", "lower", "most likely", "upper", "plotting position")
threshold_pat <- c("start year", "end year", "flow")
rex_pat       <- c("year", "most likely", "upper")  # right exceedance (optional)

exact_idx     <- find_block(header_row, exact_pat)
interval_idx  <- find_block(header_row, interval_pat)      # optional
threshold_idx <- find_block(header_row, threshold_pat)
rex_idx       <- find_block(header_row, rex_pat)           # optional

# Require exact + threshold; allow interval & right-exceed optional
if (!length(exact_idx) || !length(threshold_idx)) {
  stop("Missing required blocks. Row 2 must include:\n",
       "  exact: year, flow, plotting position\n",
       "  threshold: start year, end year, flow\n",
       "Optional blocks:\n",
       "  interval: year, lower, most likely, upper, plotting position\n",
       "  right exceedance: year, most likely, upper")
}

dat_rows <- if (nrow(raw) > 2) (3:nrow(raw)) else integer(0)
stopifnot(length(dat_rows) > 0)

block_df <- function(cols, names) {
  out <- raw[dat_rows, cols, drop = FALSE]
  names(out) <- names
  out
}

exact_df     <- block_df(exact_idx, exact_pat)
threshold_df <- block_df(threshold_idx, threshold_pat)
interval_df  <- if (length(interval_idx)) block_df(interval_idx, interval_pat) else
  tibble(`year`=numeric(), `lower`=numeric(), `most likely`=numeric(), `upper`=numeric(),
         `plotting position`=numeric())
right_ex_df  <- if (length(rex_idx))      block_df(rex_idx, rex_pat) else
  tibble(`year`=numeric(), `most likely`=numeric(), `upper`=numeric())

# ===== COERCE TYPES =====
to_year_num <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXt")) return(lubridate::year(x))
  x <- as.character(x)
  suppressWarnings({
    d <- lubridate::ymd(x, quiet = TRUE)
    out <- ifelse(!is.na(d), lubridate::year(d), NA_integer_)
    tmp <- suppressWarnings(as.integer(x))
    out[is.na(out)] <- tmp[is.na(out)]
    as.integer(out)
  })
}
num <- function(x) suppressWarnings(as.numeric(x))

ams <- exact_df |>
  mutate(year = to_year_num(`year`), flow = num(`flow`)) |>
  select(year, flow) |>
  filter(!is.na(year) & !is.na(flow))

intervals <- interval_df |>
  mutate(
    year        = to_year_num(`year`),
    lower       = num(`lower`),
    most_likely = num(`most likely`),
    upper       = num(`upper`)
  ) |>
  select(year, lower, most_likely, upper) |>
  filter(!is.na(year) & (!is.na(lower) | !is.na(upper) | !is.na(most_likely))) |>
  mutate(mid = dplyr::coalesce(most_likely, 0.5 * (lower + upper)))

thr <- threshold_df |>
  mutate(
    start_year = to_year_num(`start year`),
    end_year   = to_year_num(`end year`),
    threshold  = num(`flow`)
  ) |>
  select(start_year, end_year, threshold) |>
  filter(!is.na(start_year) & !is.na(end_year) & !is.na(threshold)) |>
  mutate(s = pmin(start_year, end_year), e = pmax(start_year, end_year)) |>
  transmute(start_year = s, end_year = e, threshold)

# Right exceedance (most likely + upper only)
right_ex <- right_ex_df |>
  mutate(
    year        = to_year_num(`year`),
    most_likely = num(`most likely`),
    upper       = num(`upper`)
  ) |>
  transmute(year, mid = most_likely, upper) |>
  filter(!is.na(year) & !is.na(mid) & !is.na(upper))

# ===== AUTO-ERA GENERATION =====
year_pts <- c(ams$year, intervals$year, thr$start_year, thr$end_year, right_ex$year)
year_pts <- sort(unique(year_pts[!is.na(year_pts)]))
if (length(year_pts) == 0) stop("No usable years found in the CSV.")

gaps <- diff(year_pts)
break_after <- which(gaps > gap_years)
chunk_bounds <- c(1, break_after + 1, length(year_pts))
era_ranges <- purrr::map2(chunk_bounds[-length(chunk_bounds)], chunk_bounds[-1],
                          ~c(year_pts[.x], year_pts[.y]))
era_tbl <- tibble(
  era_id   = seq_along(era_ranges),
  era_start= vapply(era_ranges, `[[`, numeric(1), 1),
  era_end  = vapply(era_ranges, `[[`, numeric(1), 2),
  era_label= paste0(era_start, "–", era_end)
)

assign_era <- function(y) {
  idx <- rep(NA_integer_, length(y))
  for (i in seq_len(nrow(era_tbl))) {
    idx[y >= era_tbl$era_start[i] & y <= era_tbl$era_end[i]] <- i
  }
  factor(idx, levels = era_tbl$era_id, labels = era_tbl$era_label)
}
if (nrow(ams))       ams$era       <- assign_era(ams$year)
if (nrow(intervals)) intervals$era <- assign_era(intervals$year)
if (nrow(right_ex))  right_ex$era  <- assign_era(right_ex$year)

# Split thresholds by era
split_thresh_by_era <- function(thr, era_tbl) {
  if (!nrow(thr)) return(thr[0, ])
  out <- list()
  for (i in seq_len(nrow(thr))) {
    s <- thr$start_year[i]; e <- thr$end_year[i]; h <- thr$threshold[i]
    for (j in seq_len(nrow(era_tbl))) {
      xs <- max(s, era_tbl$era_start[j])
      xe <- min(e, era_tbl$era_end[j])
      # old: if (!is.na(xs) && !is.na(xe) && xs <= xe) {
      if (!is.na(xs) && !is.na(xe) && xs < xe) {   # <- exclude zero-width pieces
        out[[length(out) + 1]] <- tibble(
          start_year = xs, end_year = xe, threshold = h,
          era = factor(era_tbl$era_id[j], levels = era_tbl$era_id, labels = era_tbl$era_label)
        )
      }
    }
  }
  bind_rows(out)
}
thr_split <- split_thresh_by_era(thr, era_tbl)

# Keep only rows with an era
if ("era" %in% names(ams))       ams       <- dplyr::filter(ams,       !is.na(era))
if ("era" %in% names(intervals)) intervals <- dplyr::filter(intervals, !is.na(era))
if ("era" %in% names(thr_split)) thr_split <- dplyr::filter(thr_split, !is.na(era))
if ("era" %in% names(right_ex))  right_ex  <- dplyr::filter(right_ex,  !is.na(era))

# ===== LEGEND LABELS (POR range) & exceedance label text =====
era_order <- era_tbl |> dplyr::arrange(era_end) |> dplyr::pull(era_label)
relevel <- function(f) factor(f, levels = era_order)
if (nrow(ams))       ams$era       <- relevel(ams$era)
if (nrow(intervals)) intervals$era <- relevel(intervals$era)
if (nrow(thr_split)) thr_split$era <- relevel(thr_split$era)
if (nrow(right_ex))  right_ex$era  <- relevel(right_ex$era)

por_label <- if (nrow(ams)) {
  sprintf("Systematic POR %d to %d", min(ams$year, na.rm = TRUE), max(ams$year, na.rm = TRUE))
} else "Systematic POR"
int_label <- "Historical Intervals"
thr_label <- "Perception Thresholds"

if (nrow(right_ex)) {
  right_ex <- right_ex |>
    mutate(rex_label = paste0("1 Exceedance: ", scales::comma(mid), " cfs to \u221E"))
}

# ===== PLOT =====
n_panels <- length(era_order)
weights  <- c(rep(1, n_panels - 1), 4)
interval_lwd <- 1.0

# 1) Era info
era_info <- era_tbl %>% dplyr::select(era_label, era_start, era_end)

# 2) Derive an era-level thr_end_era from threshold_df (`start year`, `end year`)
thr_end_by_era <- threshold_df %>%
  dplyr::transmute(thr_end = as.integer(`end year`)) %>%
  dplyr::filter(!is.na(thr_end)) %>%
  tidyr::crossing(era_info) %>%                                   # cross join eras × thr_end values
  dplyr::filter(thr_end >= era_start, thr_end <= era_end) %>%     # keep thr_end within era window
  dplyr::group_by(era_label) %>%
  dplyr::summarise(thr_end_era = max(thr_end), .groups = "drop")  # one value per era

# 3) Build intervals with caps:
iv <- if (nrow(intervals)) {
  intervals %>%
    dplyr::filter(!is.na(lower) & !is.na(upper)) %>%
    dplyr::mutate(era_label = as.character(era)) %>%
    dplyr::left_join(era_info, by = "era_label") %>%
    dplyr::left_join(thr_end_by_era, by = "era_label") %>%
    dplyr::mutate(
      span_end   = dplyr::if_else(!is.na(thr_end_era) & thr_end_era < era_end, thr_end_era, era_end),
      span_width = pmax(1, span_end - era_start),
      
      cap_pct = dplyr::case_when(
        span_width <   10 ~ 0.001,
        span_width <   75 ~ 0.010,
        span_width <  500 ~ 0.020,
        span_width < 2000 ~ 0.040,
        span_width < 5000 ~ 0.040,
        TRUE              ~ 0.080
      ),
      cap_w     = pmax(1, pmin(500, cap_pct * span_width)),
      cap_half  = cap_w / 2,
      xleft     = year - cap_half,
      xright    = year + cap_half
    )
} else tibble()

# Observed facet limits (+ tiny pad)
last_lab <- tail(era_order, 1)
in_last  <- function(f) !is.na(f) & as.character(f) == last_lab
x_obs <- range(c(
  ams$year[in_last(ams$era)],
  if (nrow(intervals)) intervals$year[in_last(intervals$era)],
  right_ex$year[in_last(right_ex$era)],
  thr_split$start_year[in_last(thr_split$era)],
  thr_split$end_year[in_last(thr_split$era)]
), na.rm = TRUE)
pad_years <- max(1, round(diff(x_obs) * 0.01))
x_obs_pad <- c(x_obs[1] - pad_years, x_obs[2] + pad_years)

# === Build label rows: one per (era, threshold), even if multiple rectangles ===
# Rule: if there are several rectangles with the same threshold in an era, keep the LONGEST.
ymax_all <- max(
  c(ams$flow, intervals$upper, thr_split$threshold),
  na.rm = TRUE
)
label_pad_cfs <- 0.16 * ymax_all   # change percentage of total y-axis range to move the threshold up and down

if (nrow(thr_split)) {
  thr_labels <- thr_split %>%
    dplyr::mutate(span = end_year - start_year) %>%
    dplyr::group_by(era, threshold) %>%
    dplyr::arrange(dplyr::desc(span), start_year, end_year, .by_group = TRUE) %>%
    dplyr::slice(1) %>%                        # keep only one per (era, threshold)
    dplyr::ungroup() %>%
    dplyr::mutate(
      # place label near top of the rectangle
      xmid = pmin(end_year, pmax(start_year, 0.5 * (start_year + end_year))),
      ylab = pmax(5, threshold - label_pad_cfs),
      thr_txt = paste0(
        "Threshold Flow: \n", scales::comma(threshold), " to 0 cfs\n",
        "From Year \n", start_year, " to ", end_year
      )
    )
} else {
  thr_labels <- tibble()
}

p <- ggplot() +
  # Perception thresholds (mapped to legend via fill)
  { if (nrow(thr_split)) geom_rect(
    data = transform(thr_split, src = thr_label),
    aes(xmin = start_year, xmax = end_year, ymin = 0, ymax = threshold, fill = src),
    alpha = 0.5, color = NA
  ) } +
  # Threshold labels (no duplicates; repel to avoid overlaps within each facet)
  { if (nrow(thr_labels)) ggrepel::geom_text_repel(
    data = thr_labels,
    aes(x = xmid, y = ylab, label = thr_txt),
    size = 4.0,             
    box.padding = 0.4,      # space around text
    point.padding = 0,      # we aren't repelling from points
    segment.color = NA,     # no leader line
    min.segment.length = 0,
    force = 5,              # push a bit if crowded
    direction = "y",        # move mostly vertically to stay in panel
    max.overlaps = Inf,
    seed = 42
  ) } +
    # AMS points (mapped to legend via color)
  { if (nrow(ams)) geom_point(
    data = transform(ams, src = por_label),
    aes(x = year, y = flow, color = src),
    size = 2, stroke = 1.1, shape = 21, fill = "black"
  ) } +
  # Right exceedance: blue dotted + arrow, point, label (no legend)
  { if (nrow(right_ex)) geom_segment(
    data = right_ex,
    aes(x = year, xend = year, y = mid, yend = upper),
    color = "#1f78b4", linetype = "dotted", linewidth = 0.9, lineend = "round"
  ) } +
    # 2) short solid segment with the arrowhead
  { if (nrow(right_ex)) geom_segment(
    data = right_ex,
    aes(x = year, xend = year, y = upper - 1, yend = upper),
    color = "#1f78b4", linewidth = 0.9, lineend = "round",
    arrow = grid::arrow(length = unit(6, "pt"), type = "closed", ends = "last", angle = 25)
  ) } +
  { if (nrow(right_ex)) geom_point(
    data = right_ex,
    aes(x = year, y = mid),
    shape = 21, size = 1.5, stroke = 1.1, fill = "#1f78b4", color = "#1f78b4"

  ) } +
  { if (nrow(right_ex)) geom_text(
    data = right_ex,
    aes(x = year, y = upper, label = rex_label),
    vjust = -0.6, hjust = 0, size = 4.0, color = "#1f78b4"
  ) } +
  # Historical intervals whiskers + caps (mapped to legend via linetype)
  # { if (nrow(iv)) geom_segment(
  #   data = transform(iv, src = int_label),
  #   aes(x = year, xend = year, y = lower, yend = upper, linetype = src),
  #   linewidth = interval_lwd, lineend = "butt", color = "black"
  # ) } +
  # { if (nrow(iv)) geom_segment(
  #   data = transform(iv, src = int_label),
  #   aes(x = xleft, xend = xright, y = lower, yend = lower, linetype = src),
  #   linewidth = interval_lwd, lineend = "butt", color = "black"
  # ) } +
  # { if (nrow(iv)) geom_segment(
  #   data = transform(iv, src = int_label),
  #   aes(x = xleft, xend = xright, y = upper, yend = upper, linetype = src),
  #   linewidth = interval_lwd, lineend = "butt", color = "black"
  # ) } +
  { if (nrow(iv)) geom_errorbar(
    data = transform(iv, src = int_label),
    aes(x = year, ymax = lower, ymin = upper, linetype = src),
    width = 5, lineend = "butt", color = "black"
  ) } +
  # (keep midpoint but out of legend)
  { if (nrow(intervals)) geom_point(
    data = intervals,
    aes(x = year, y = mid),
    size = 2, stroke = 1.1, shape = 21, fill = "lightblue2", color = "black",
    show.legend = FALSE
  ) } +
  # Legend scales (names exactly as requested)
  scale_color_manual(
    name = NULL,
    values = setNames("black", por_label),
    breaks = por_label
  ) +
  scale_linetype_manual(
    name = NULL,
    values = setNames("solid", int_label),
    breaks = int_label
  ) +
  scale_fill_manual(
    name = NULL,
    values = setNames("lightcoral", thr_label),
    breaks = thr_label
  ) +
  guides(
    color    = guide_legend(order = 1, override.aes = list(shape = 21, fill = "black", size = 2)),
    linetype = guide_legend(order = 2, override.aes = list(color = "black", linewidth = 1.2)),
    fill     = guide_legend(order = 3, override.aes = list(alpha = 0.5))
  ) +
  theme(legend.position = c(0.98, 0.98), legend.justification = c(1, 1)) +
  # Faceting & axes
  ggh4x::facet_grid2(~ era, scales = "free_x", space = "free_x") +
  scale_y_continuous("4-Day Volume (cfs)", labels = scales::comma, #Modify y-axis label
                     breaks = scales::breaks_width(100000),
                     expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(ylim = c(0, NA), clip = "off") +
  xlab("Water Year") +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major = element_line(colour = "grey80", linewidth = 0.1),
    panel.grid.minor = element_line(colour = "grey90", linewidth = 0.1),
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 16, colour = "black"),
    axis.text.y  = element_text(size = 16, colour = "black"),
    axis.title.x = element_text(size = 20, colour = "black"),
    axis.title.y = element_text(size = 20, colour = "black"),
    strip.text = element_blank(),
    legend.position      = c(0.98, 0.985),   # inside top-right
    legend.justification = c(1, 1),
 #   legend.box.background = element_rect(fill = "white", colour = "gray70", linewidth = 1.0),
    legend.background     = element_rect(fill = "white", colour = NA),
    legend.key            = element_rect(fill = "white", colour = NA),
    legend.margin        = margin(4, 6, 4, 6),
    legend.text  = element_text(size = 16),
    aspect.ratio   = 3.5,   # kept as in your script
    panel.spacing.x = unit(6, "pt"),
    plot.margin     = margin(6, 12, 6, 8)
  )

# Panel widths 1,1,4
p <- p + ggh4x::force_panelsizes(cols = unit(weights, "null"), respect = TRUE)

# Observed facet: dense ticks + zero side padding
x_scales <- replicate(n_panels - 1, NULL, simplify = FALSE)
x_scales[[n_panels]] <- scale_x_continuous(
  limits       = x_obs_pad,
  breaks       = seq(floor(x_obs_pad[1] / 10) * 10, ceiling(x_obs_pad[2] / 10) * 10, by = 10),
  minor_breaks = seq(floor(x_obs_pad[1] / 5)  * 5,  ceiling(x_obs_pad[2] / 5)  * 5,  by = 5),
  expand       = expansion(mult = c(0, 0))
)
p <- p + ggh4x::facetted_pos_scales(x = x_scales)

print(p)


# 
# # ===== EXPORT (preview + save with real size) =====
# w_in  <- 12       # width in inches
# h_in  <- 6.8      # height in inches  (increase for taller)
# dpi   <- 300
# out_dir <- file.path(start_dir, "figures")
# dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
# png_file <- file.path(out_dir, "unregulated_timeline.png")
# pdf_file <- file.path(out_dir, "unregulated_timeline.pdf")
# 
# # Preview at true size
# if (.Platform$OS.type == "windows") {
#   windows(width = w_in, height = h_in); print(p)
# } else if (Sys.info()[["sysname"]] == "Darwin") {
#   quartz(width = w_in, height = h_in);  print(p)
# } else {
#   x11(width = w_in, height = h_in);     print(p)
# }
# 
# # Save PNG + PDF (use ggplot directly so size is honored)
# ggsave(filename = png_file, plot = p, width = w_in, height = h_in, units = "in", dpi = dpi)
# ggsave(filename = pdf_file, plot = p, width = w_in, height = h_in, units = "in", device = cairo_pdf)
# 
# message("Saved:\n  ", png_file, "\n  ", pdf_file)

