# Seasonality
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
  memoise                         # Caching
)

theme_set(theme_bw())

# Read Data --------------------------------------------------------------------
Seasonality <- fs::dir_ls("D:/0.RMC/JohnMartin/DQC/data/RFA_Seasonality/", glob = "*Seasonality.csv") %>% 
  read_csv()

months <- c("January","February","March","April",
            "May","June","July","August", "September",
            "October","November","December")

# Use month.name base vector
Seasonality <- Seasonality %>% 
  mutate(Month = factor(Month, levels = month.name))

# Histogram -----------------------
seasonality <- ggplot(Seasonality, aes(x = Month, y = Relative_Frequency))+
  geom_col(fill = "steelblue") + 
  geom_text(aes(label = ifelse(Frequency > 0, Frequency, "")), 
            vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Month",
       y = "Relative Frequency") + 
  scale_y_continuous(breaks = seq(0,0.4,0.1),limits = c(0,.35))

plot_dir <-"D:/0.RMC/JohnMartin/DQC/Figures/Standalone_Figures/"
ggsave(paste0(plot_dir,"Seaonality.png"), seasonality, height = 4, width = 6, dpi = 500)
