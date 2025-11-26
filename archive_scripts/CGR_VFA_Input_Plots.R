rm(list = ls())

# Adapted from Allen's script. Most stuff is hard coded for Willamette/Cougar ###

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

# Read CSV Files ---------------------------------------------------------------
cgr_paleo_input <- read.csv("D:/0.RMC/Willamette/2025-Report/data/BestFit_input/ss/CGR_3day_paleo_best_nopsi.csv",header = T)

ditch_flow_commas <- function(flow_vect) {
  as.numeric(gsub(",", "", flow_vect))
}

cgr_syst <- cgr_paleo_input %>%
  filter(Type == "Systematic") %>%
  select(WY_start, Inflow_3day, Type) %>%
  rename(WY = WY_start)

cgr_int <- cgr_paleo_input %>%
  filter(Type == "Interval") %>%
  select(WY_start, Inflow_3day, Upper, Lower, Type) %>%
  rename(WY = WY_start)

cgr_thresh <- cgr_paleo_input %>%
  filter(Type == "Threshold") %>%
  select(WY_start,WY_end,Inflow_3day, Type) %>% 
  mutate(WY_Check = WY_start <= WY_end)

# Define Eras ------------------------------------------------------------------
# Identify where to split
gap_years <- 200
input_data_years <- c(cgr_syst$WY, cgr_int$WY, cgr_thresh$WY_start, cgr_thresh$WY_end)
year_pts <- sort(unique(input_data_years[!is.na(input_data_years)]))

gaps <- diff(year_pts, lag = 1)
break_after <- which(gaps > gap_years)
chunk_bounds <- c(1, break_after + 1, length(year_pts))
era_ranges <- purrr::map2(chunk_bounds[-length(chunk_bounds)], chunk_bounds[-1],
                          ~c(year_pts[.x], year_pts[.y]))

era_tbl <- tibble(era_id    = seq_along(era_ranges),
                  era_start = map_dbl(era_ranges, 1),
                  era_end   = map_dbl(era_ranges, 2),
                  era_label = paste0(map_dbl(era_ranges, 1), "–", map_dbl(era_ranges, 2)))

# Hard code break at 1811
era_tbl <- tibble(era_id    = c(1,2,3),
                  era_start = c(-300,1274,1811),
                  era_end   = c(1274,1811,2021),
                  era_label = paste0(era_start, "–", era_end))

assign_era <- function(y) {
  idx <- rep(NA_integer_, length(y))
  for (i in seq_len(nrow(era_tbl))) {
    idx[y >= era_tbl$era_start[i] & y <= era_tbl$era_end[i]] <- i
  }
  factor(idx, levels = era_tbl$era_id, labels = era_tbl$era_label)
}

cgr_syst <- cgr_syst %>% 
  mutate(Era = assign_era(WY))

cgr_int <- cgr_int %>% 
  mutate(Era = assign_era(WY))

# Split thresholds by era - using all data
cgr_input <- cgr_paleo_input %>% 
  crossing(era_tbl) %>%                         
  mutate(xs = pmax(WY_start , era_start),
         xe = pmin(WY_end , era_end)) %>%
  filter(ifelse(Type == "Threshold",
                !is.na(xs) & !is.na(xe) & xs < xe,
                !is.na(xs) & !is.na(xe)& xs <= xe)) %>%
  mutate(Era = factor(era_id,levels = era_tbl$era_id,labels = era_tbl$era_label)) %>%
  select(WY_start, WY_end, Inflow_3day, Upper, Lower, Type, era_label, Era) %>%
  arrange(WY_start)

# Legend Labels ----------------------------------------------------------------
era_order <- era_tbl %>%
  dplyr::arrange(era_end) %>% 
  dplyr::pull(era_label)

relevel <- function(f) factor(f, levels = era_order)
cgr_input$Era <- relevel(cgr_input$Era)

minPOR <- cgr_paleo_input %>%
  filter(Type == "Systematic") %>% 
  pull(WY_start) %>% 
  min(., na.rm = TRUE)

maxPOR <- cgr_paleo_input %>%
  filter(Type == "Systematic") %>% 
  pull(WY_start) %>% 
  max(., na.rm = TRUE)

por_label <- sprintf("Systematic POR: %d to %d", minPOR, maxPOR)
int_label <- "Historic Flood (Dec 1861)"
thr_label <- "Perception Thresholds"

# === Era info ===
era_info <- era_tbl %>%
  select(era_label, era_start, era_end)

max_3day <- ceiling(max(cgr_input$Inflow_3day, cgr_input$Upper, na.rm = TRUE) / 1000) * 1000
# seq.int(0,max_3day,length.out = 7)

# Plot function
plot_era <- function(df) {
  ggplot() +
    # thresholds
    geom_rect(data = df %>% filter(Type == "Threshold"),
              aes(xmin = WY_start, xmax = WY_end, ymin = 0, ymax = Inflow_3day, fill = thr_label), alpha = 0.5) +
    # intervals
    geom_errorbar(data = df %>% filter(Type == "Interval"),
                  aes(x = WY_start, ymin = Lower, ymax = Upper, linetype = int_label), width = 5, color = "black") +
    geom_point(data = df %>% filter(Type == "Interval"),
               aes(x = WY_start, y = Inflow_3day),
               shape = 21, fill = "lightblue2", color = "black",size = 1.5) +
    # systematic
    geom_point(data = df %>% filter(Type == "Systematic"),
               aes(x = WY_start, y = Inflow_3day, color = por_label),size = 1) +
    scale_y_continuous(breaks = seq(0, max_3day*2, 10000),
                       minor_breaks = seq(0, max_3day*2 , 1000),
                       limits = c(0, max_3day),
                       labels = scales::comma) + 
    labs(x = "Year", y = "3-Day Inflow (cfs)") +
    theme_bw() +
    theme(panel.grid.major = element_line(colour = "grey80", linewidth = 0.1),
          panel.grid.minor = element_line(colour = "grey90", linewidth = 0.1),
          axis.text.x  = element_text(angle = 45, hjust = 1, size = 8, colour = "black"),
          axis.text.y  = element_text(size = 8, colour = "black"),
          axis.title.x = element_text(size = 10, colour = "black"),
          axis.title.y = element_text(size = 10, colour = "black"))
}

# build one plot per era
# plots <- cgr_input %>%
#   group_split(Era) %>%
#   lapply(plot_era)

plots <- lapply(seq_along(era_order), function(i) {
  era_name <- era_order[i]
  df_era <- cgr_input %>% filter(Era == era_name)
  
  p <- plot_era(df_era) +
    theme(
      axis.title.x = element_blank(),
      legend.position = if (i == length(era_order)) "inside" else "none",
      axis.title.y = if (i == 1) element_text() else element_blank(),
      axis.text.y  = if (i == 1) element_text() else element_blank(),
      axis.ticks.y = if (i == 1) element_line() else element_blank()
    )
  
  # Set x-axis limits based on era length
  if(i == length(era_order)){
    era_span <- ceiling(max(df_era$WY_end)/5)*5 - floor(min(df_era$WY_start)/5)*5
    year_brk <- dplyr::case_when(
      era_span <   10 ~ 1,
      era_span <   150 ~ 10,
      era_span <  500 ~ 25,
      era_span < 1000 ~ 100,
      era_span < 5000 ~ 500,
      era_span < 10000 ~ 1000)
    start_year <- floor(min(df_era$WY_start)/5)*5
    end_year <- ceiling(max(df_era$WY_end)/5)*5
    
  }else{
    era_span <- ceiling((max(df_era$WY_end) - min(df_era$WY_start))/10)*10
    year_brk <- dplyr::case_when(
      era_span <   10 ~ 1,
      era_span <   100 ~ 10,
      era_span <  300 ~ 100,
      era_span < 1000 ~ 200,
      era_span < 5000 ~ 500,
      era_span < 10000 ~ 1000)
    start_year <- floor(min(df_era$WY_start)/10)*10
    end_year <- ceiling(max(df_era$WY_end)/10)*10
  }
  
  p <- p + scale_x_continuous(
    limits = c(start_year, end_year),
    breaks = seq(start_year, end_year, by = year_brk),
    expand = expansion(mult = c(0.01, 0)))
  
  # Only add legend scales to the last plot
  if (i == length(era_order)) {
    p <- p +
      scale_color_manual(name = NULL, values = setNames("black", por_label), breaks = por_label) +
      scale_linetype_manual(name = NULL, values = setNames("solid", int_label), breaks = int_label) +
      scale_fill_manual(name = NULL, values = setNames("lightcoral", thr_label), breaks = thr_label) +
      guides(
        color    = guide_legend(order = 1, override.aes = list(shape = 21, fill = "black")),
        linetype = guide_legend(order = 2, override.aes = list(color = "black", linewidth = 0.7)),
        fill     = guide_legend(order = 3, override.aes = list(alpha = 0.5,size = .1))) +
      theme(legend.position.inside = c(0.73,0.8),
            legend.text = element_text(size = 6, colour = "black"),
            legend.margin = margin(0,0,0,0))
  }
  
  return(p)
})

# Remove extra y-axes - this could probably be added to loop above
# plots <- lapply(seq_along(plots), function(i) {
#   if (i == 1) {
#     plots[[i]] +
#       theme(axis.title.y = element_text(),
#             axis.text.y  = element_text(),
#             axis.ticks.y = element_line())
#   } else {
#     plots[[i]] +
#       theme(axis.title.y = element_blank(),
#             axis.text.y  = element_blank(),
#             axis.ticks.y = element_blank())
#   }
# })

# get spans (for widths)
era_spans <- cgr_input %>%
  group_by(Era) %>%
  summarise(span = max(WY_end) - min(WY_start), .groups = "drop") %>%
  pull(span)

# patchwork
n_panels <- length(era_order)
weights  <- c(rep(1, n_panels - 1), 4)
patchwork::wrap_plots(plots, nrow = 1) + patchwork::plot_layout(widths = weights) + 
  patchwork::plot_annotation(caption = "Water Year",
                             theme = theme(plot.caption = element_text(hjust = 0.5, size = 10)))

outfile <- "D:/0.RMC/Willamette/2025-Report/Figures/BestFit/Input_Data/CGR_Paleo_ggplot.png"
ggsave(outfile, plot = last_plot(),height = 5, width = 7, dpi = 300)



