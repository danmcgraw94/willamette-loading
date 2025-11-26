## 2024 PMF Inflow Volumes
## Dan McGraw

library(tidyverse)
library(lubridate)
library(gt)
theme_set(theme_bw())

out_dir <- "D:/0.RMC/JohnMartin/DQC/Figures/Standalone_Figures/"

# READ PMF INFLOW --------------------------------------------------------------
pmf_1hr  <- read_csv("D:/0.RMC/JohnMartin/DQC/data/Peak_to_Vol/PMF_RAS_hourly.csv")
#pmf_15min <- read_csv("D:/0.RMC/JohnMartin/DQC/data/Peak_to_Vol/PMF_15min.csv")

# Peak 
peak_pmf <- max(pmf_1hr$Flow_cfs, na.rm = TRUE)

# 1- to 5-day inflow vols ------------------------------------------------------
pmf_1hr <- pmf_1hr %>% 
  mutate(Q_1day = zoo::rollmean(Flow_cfs,k = 24,fill = NA, align = "right"),
         Q_2day = zoo::rollmean(Flow_cfs,k = 48,fill = NA, align = "right"),
         Q_3day = zoo::rollmean(Flow_cfs,k = 72,fill = NA, align = "right"),
         Q_4day = zoo::rollmean(Flow_cfs,k = 96,fill = NA, align = "right"),
         Q_5day = zoo::rollmean(Flow_cfs,k = 120,fill = NA, align = "right"))
# 
# pmf_15min <- pmf_15min %>%  
#   mutate(Q_1day = zoo::rollmean(Flow_cfs,k = 96,fill = NA, align = "right"),
#          Q_2day = zoo::rollmean(Flow_cfs,k = 192,fill = NA, align = "right"),
#          Q_3day = zoo::rollmean(Flow_cfs,k = 288,fill = NA, align = "right"),
#          Q_4day = zoo::rollmean(Flow_cfs,k = 384,fill = NA, align = "right"),
#          Q_5day = zoo::rollmean(Flow_cfs,k = 480,fill = NA, align = "right"))

# MAX INFLOW VOLS --------------------------------------------------------------
pmf_inflow_vol <- pmf_1hr %>% 
  summarise(PeakQ = round(peak_pmf,-2),
            Max_1day = round(max(Q_1day, na.rm = TRUE),-2),
            Max_2day = round(max(Q_2day, na.rm = TRUE),-2),
            Max_3day = round(max(Q_3day, na.rm = TRUE),-2),
            Max_4day = round(max(Q_4day, na.rm = TRUE),-2),
            Max_5day = round(max(Q_5day, na.rm = TRUE),-2))

pmf_inflow_vol

# GT TABLE ---------------------------------------------------------------------
pmf_sum_gt <- pmf_inflow_vol %>% 
  gt() %>%
  # Header
  tab_header(title = "2024 PMF Inflow Volumes",subtitle = "Recommended PMF") %>%
  # Column labels
  cols_label(PeakQ = "Peak Inflow (cfs)",
             Max_1day = "Max 1-Day",
             Max_2day = "Max 2-Day",
             Max_3day = "Max 3-Day",
             Max_4day = "Max 4-Day",
             Max_5day = "Max 5-Day") %>%
  tab_spanner(label = "Inflow Volume (cfs)", columns = -c(PeakQ)) %>% 
  # Format Inflows
  fmt_number(columns = everything(),decimals = 0,use_seps = T) %>%
  # Alignment
  cols_align(align = "center",columns = everything()) %>%
  # Make column headers bold
  tab_style(style = cell_text(weight = "bold"),locations = cells_column_labels(everything())) %>%
  # Make titles bold
  tab_style(style = cell_text(weight = "bold"),locations = cells_title(groups = c("title", "subtitle"))) %>%
  # Make spanner bold
  tab_style(style = cell_text(weight = "bold"),locations = cells_column_spanners(spanners = everything())) %>%
  # Table styling
  tab_options(
    table.font.size = px(11),
    table.border.top.style = "solid",
    table.border.top.width = px(2),
    table.border.top.color = "#4a5f4a",
    table.border.bottom.style = "solid",
    table.border.bottom.width = px(2),
    table.border.bottom.color = "#4a5f4a",
    heading.background.color = "#4a5f4a",
    heading.align = "left",
    column_labels.background.color = "#e8e8e8",
    data_row.padding = px(8),
    column_labels.padding = px(8)
  ) %>%
  # Set column widths
  cols_width(PeakQ ~ px(75),
             Max_1day ~ px (75),
             Max_2day ~ px(75),
             Max_3day ~ px(75),
             Max_4day ~ px(75),
             Max_5day ~ px(75))

pmf_sum_gt %>% gtsave(paste0(out_dir,"PMF Inflow Volume Summary.html"))
pmf_sum_gt %>% gtsave(paste0(out_dir,"PMF Inflow Volume Summary.docx"))
