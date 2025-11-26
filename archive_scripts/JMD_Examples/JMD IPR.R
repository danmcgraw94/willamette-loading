library(tidyverse)
library(lubridate)
# Tables and presentation
library(gt)

jmd_pert <- tibble(Field = c("Completion Date","Watershed","Drainage Area (sq. mi.)",
                             "Contributing Drainage Area (sq. mi.)","Design Crest Elevation",
                             "Minimum Surveyed Crest Elevation","Pool of Record Elevation",
                             "Upper PMF (2025)","Rec. PMF (2025)","SDF"),
                   Value = c("1948", "Arkansas", "18,915" , "18,130" ,
                             "3,881.8-ft" , "3,881.5-ft" , "3,862.2-ft (1999)", 
                             "3,893.8-ft","3,890.9-ft","3,889.2-ft"))

jmd_pert %>% 
  gt() %>%
  tab_header(
    title = md("**Pertinent Data**"),
    subtitle = "John Martin Dam"
  ) %>%
  cols_label(
    Field = "Description",
    Value = "Value",
  ) %>%
  cols_align(
    align = "left",
    columns = Field
  ) %>%
  cols_align(
    align = "center",
    columns = Value
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
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
  cols_width(
    Field ~ px(200),
    Value ~ px(150)
  )


# Log normal QPs --------------------------
# Original data (mean and SD of the lognormal distribution)
params <- tibble(
  aep = c(0.01, 0.001, 0.0001),
  mean_ln = c(69000, 138700, 278900),
  sd_ln = c(54100, 98100, 187300)
)

# Convert mean and SD to meanlog and sdlog parameters
params <- params %>%
  mutate(
    meanlog = log(mean_ln^2 / sqrt(sd_ln^2 + mean_ln^2)),
    sdlog = sqrt(log(1 + (sd_ln^2 / mean_ln^2)))
  )

# Generate x values for plotting
x_vals <- seq(0, 500000, length.out = 1000)

# Pivot longer and generate density curves
plot_data <- params %>%
  rowwise() %>%
  mutate(
    x = list(x_vals),
    density = list(dlnorm(x_vals, meanlog = meanlog, sdlog = sdlog))
  ) %>%
  unnest(c(x, density))

# Plot
ggplot(plot_data, aes(x = x, y = density, color = as.factor(aep))) +
  geom_line(linewidth = 1) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Lognormal Distributions by AEP",
    x = "Value",
    y = "Density",
    color = "AEP"
  ) +
  theme_bw()
