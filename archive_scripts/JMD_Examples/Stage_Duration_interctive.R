# Stage Duration interactive plots
pacman::p_load(
  tidyverse,
  zoo,
  patchwork,
  ggsci,
  ggtext,
  plotly,
  future, furrr,
  memoise,
  paletteer,
  plotly,
  dygraphs,
  xts,
  highcharter,
  this.path
)

theme_set(theme_bw())

# Load the og file
stage_dur_Rfile <- paste0(this.dir(),"/Stage_Duration.R")
source(stage_dur_Rfile)

# Convert to Plotly ------------------------------------------------------------
# Create the ggplot
stage_dur <- ggplot(stage_duration_long, aes(x = Prob, y = Elev))+
  geom_line(aes(color = Month),linewidth = 0.65) +
  geom_line(data = stage, aes(x = Prob, y = Stage), color = "black",linetype = "dashed", linewidth = 0.7) +
  scale_colour_paletteer_d("ggthemes::Classic_Cyclic")+
  labs(x = "Percen of Time Exceeded",
       y = "Elevation (ft-NAVD88)")

# Convert to interactive plotly
strage_dur_inter <- ggplotly(stage_dur)
htmlwidgets::saveWidget(strage_dur_inter, "D:/0.RMC/JohnMartin/DQC/Figures/stage_exceedance.html")

# Native Plotly ----------------------------------------------------------------
# Get unique months for creating traces
months <- unique(stage_duration_long$Month)

# Create empty plot
p <- plot_ly()

# Add a trace for each month
for(month in months) {
  month_data <- stage_duration_long %>% filter(Month == month)
  
  p <- p %>% add_trace(
    data = month_data,
    x = ~Prob,
    y = ~Elev,
    type = 'scatter',
    mode = 'lines',
    name = month,
    line = list(width = 2),
    hovertemplate = paste(
      '<b>Month:</b>', month, '<br>',
      '<b>Exceedance:</b> %{x:.2f}%<br>',
      '<b>Elevation:</b> %{y:.2f} ft<br>',
      '<extra></extra>'
    )
  )
}

# Add overall stage-duration curve (black dashed)
p <- p %>% add_trace(
  data = stage,
  x = ~Prob,
  y = ~Stage,
  type = 'scatter',
  mode = 'lines',
  name = 'Overall',
  line = list(color = 'black', dash = 'dash', width = 2),
  hovertemplate = paste(
    '<b>Overall</b><br>',
    '<b>Exceedance:</b> %{x:.2f}%<br>',
    '<b>Stage:</b> %{y:.2f} ft<br>',
    '<extra></extra>'
  )
)

# Layout
p <- p %>% layout(
  title = "Stage Duration Curves by Month",
  xaxis = list(title = "Percent of Time Exceeded (%)"),
  yaxis = list(title = "Elevation (ft, NAVD88)"),
  hovermode = 'closest',
  legend = list(x = 0.02, y = 0.98)
)

htmlwidgets::saveWidget(p, "D:/0.RMC/JohnMartin/DQC/Figures/stage_exceedance_native.html")
# Highcharter ------------------------------------------------------------------
# Create highchart
hc <- highchart() %>%
  hc_chart(zoomType = "xy")

# Add each month
for(month in unique(stage_duration_long$Month)) {
  month_data <- stage_duration_long %>% filter(Month == month)
  
  hc <- hc %>%
    hc_add_series(
      data = month_data,
      type = "line",
      hcaes(x = Prob, y = Elev),
      name = month
    )
}

# Add overall curve
hc <- hc %>%
  hc_add_series(
    data = stage,
    type = "line",
    hcaes(x = Prob, y = Stage),
    name = "Overall",
    dashStyle = "Dash",
    color = "black",
    lineWidth = 2
  ) %>%
  hc_xAxis(title = list(text = "Percent of Time Exceeded (%)")) %>%
  hc_yAxis(title = list(text = "Elevation (ft, NAVD88)")) %>%
  hc_title(text = "Stage Duration Curves by Month") %>%
  hc_tooltip(shared = FALSE, crosshairs = TRUE)

hc
htmlwidgets::saveWidget(hc, "D:/0.RMC/JohnMartin/DQC/Figures/stage_exceedance_highchart.html")
