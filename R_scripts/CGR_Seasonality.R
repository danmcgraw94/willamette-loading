# Cougar Stage and Flood Seasonallity  -----------------------------------------
# RMC Flood Hazards - Dan McGraw

# ---- Package Management ----
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse, ggplot2, scales, ggh4x, ggrepel,patchwork,fs, 
  ggsci, ggtext, glue, cli, assertthat, future, furrr, memoise)


theme_set(theme_bw())
plotdir <- "/outputs/Figures/Seasonality/"

# Seasonality Tables ------------------------------------------------------------
season_stage <- tibble(
  Month = factor(month.name, levels = month.name),
  Frequency = c(0,0,1,1,16,25,6,0,0,4,0,0),
  Rel_Freq = Frequency/sum(Frequency),
  Cume_Rel_Freq = cumsum(Rel_Freq))

season_inflow <- tibble(
  Month = factor(month.name, levels = month.name),
  Frequency = c(12,6,2,3,0,0,0,0,0,0,3,11),
  Rel_Freq = Frequency/sum(Frequency),
  Cume_Rel_Freq = cumsum(Rel_Freq))

# Histograms
inflow_szn <- ggplot(season_inflow, aes(x = Month, y = Rel_Freq))+
  geom_col(fill = "steelblue") + 
  geom_text(aes(label = ifelse(Frequency > 0, Frequency, "")), 
            vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Month",
       y = "Relative Frequency") + 
  scale_y_continuous(breaks = seq(0,0.8,0.1),limits = c(0,.35))

stage_szn <- ggplot(season_stage, aes(x = Month, y = Rel_Freq))+
  geom_col(fill = "steelblue") + 
  geom_text(aes(label = ifelse(Frequency > 0, Frequency, "")), 
            vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Month",
       y = "Relative Frequency") + 
  scale_y_continuous(breaks = seq(0,0.8,0.1),limits = c(0,.5))

# Save
ggsave(paste0(getwd(),plotdir,"Inflow_Seasonality.png"), inflow_szn, height = 5, width = 7, dpi=300)
ggsave(paste0(getwd(),plotdir,"Stage_Seasonality.png"), stage_szn, height = 5, width = 7, dpi=300)