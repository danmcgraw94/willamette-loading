# Stage Duration
# ---- Package Management ----
pacman::p_load(
  # Core
  tidyverse,
  zoo,
  patchwork,                      # Combine plots
  ggsci,                          # Scientific color palettes (optional)
  ggtext,                         # Rich text in plots
  plotly,                         # Interactive plots
  # Performance
  future, furrr,                  # Parallel processing
  memoise,                         # Caching
  paletteer
)

theme_set(theme_bw())

# Read Data --------------------------------------------------------------------
stage_duration <- fs::dir_ls("D:/0.RMC/JohnMartin/DQC/data/RFA_Stage_Duration/", glob = "*Duration.csv") %>% 
  read_csv()

stage_duration_long <- stage_duration %>% 
  pivot_longer(cols = -Prob, names_to = "Month", values_to = "Elev")

# Use month.name base vector
stage_duration_long <- stage_duration_long %>% 
  mutate(Month = factor(Month, levels = month.name))

# Stage Timeseries -------------------------------------------------------------
stage <- fs::dir_ls("D:/0.RMC/JohnMartin/DQC/data/RFA_Stage_Duration/", glob = "*timeseries.csv") %>% 
  read_csv()

stage <- stage %>%
  arrange(desc(Stage)) %>% 
  mutate(Rank = row_number(),
         Prob = (Rank/(n()+1)))

# Duration Curve -----------------------
stage_dur <- ggplot(stage_duration_long, aes(x = Prob, y = Elev))+
  geom_line(aes(color = Month),linewidth = 0.65) +
  geom_line(data = stage, aes(x = Prob, y = Stage), color = "black",linetype = "dashed", linewidth = 0.7) +
  scale_colour_paletteer_d("ggthemes::Classic_Cyclic")+
  labs(x = "Percen of Time Exceeded",
       y = "Elevation (ft-NAVD88)")

plot_dir <-"D:/0.RMC/JohnMartin/DQC/Figures/Standalone_Figures/"
#ggsave(paste0(plot_dir,"Stage_Duration.png"), stage_dur, height = 4, width = 6, dpi = 500)




