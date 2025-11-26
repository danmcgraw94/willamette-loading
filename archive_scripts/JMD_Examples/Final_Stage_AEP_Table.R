library(tidyverse)
library(gt)

table_e1 <- tibble(
  crit_res_elev = c("Upper PMF", "Recommended PMF","Top of Dam", "Flood Control Pool", "Maximum Observed Reservoir Stage (1999)", "Spillway Crest"),
  Elev_navd88 = c(3893.8, 3890.9, 3881.8, 3871.8, 3862.2, 3841.8),
  AEP = c(9.1E-06, 1.5E-05,5.4E-05,1.8E-03,8.7E-03,2.6E-01),
  RT = round(1/AEP,-2))

table_e1 %>% 
  gt() %>% 
  tab_header(
    title = md("**Annual exceedance probabilities for critical reservoir elevations at John Martin Dam**")
  ) %>%
  cols_label(
    crit_res_elev = "Critical Reservoir Elevation",
    Elev_navd88 = "Elev. ft-NAVD88",
    AEP = "AEP",
    RT = "Return Period"
  ) %>%
  fmt_number(
    columns = c(Elev_navd88),
    decimals = 1,
    use_seps = TRUE
  ) %>%
  fmt_number(
    columns = c(RT),
    decimals = 0,
    use_seps = TRUE
  ) %>%
  fmt_scientific(
    columns = AEP,
    exp_style = "E"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.font.size = px(12),
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
  )
