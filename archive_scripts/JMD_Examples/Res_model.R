# Reservoir Model
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
res_models <- fs::dir_ls("D:/0.RMC/JohnMartin/DQC/data/RFA_Res_Model/", glob = "*Res_models.csv") %>% 
  read_csv()

final_res <- res_models %>% filter(model == "0_IES Res Ops with OT Updated - No OW")
sensitivities <- res_models %>% filter(model != "0_IES Res Ops with OT Updated - No OW")

# Stage - Discharge ------------
stage_discharge <- ggplot(sensitivities, aes(x = discharge, y = stage))+
  geom_line(aes(color = model),linewidth = 0.5) +
  #scale_x_log10(breaks = scales::log_breaks(n=5,base = 10), minor_breaks = scales::minor_breaks_log(detail = 1))+
  geom_line(data = final_res, aes(x = discharge, y = stage,linetype = "Final - Forecast with No OW"), color = "black", linewidth = 0.8) +
  ggsci::scale_color_tron()+
  scale_x_continuous(breaks = seq(0,5e6,1e6), minor_breaks = seq(0,5e6,1e5),labels = scales::comma)+
  scale_y_continuous(breaks = seq(3700,3900,20), minor_breaks = seq(3700,3900,5),labels = scales::comma)+
  scale_linetype_manual(values = c("Final - Forecast with No OW" = "solid"), breaks = c("Final - Forecast with No OW")) +
  labs(x = "Discharge (cfs)",
       y = "Elevation (ft-NAVD88)",
       color = "Sensitivity Models",
       linetype = "Final Model") +
  theme(
    legend.position = "inside",
    legend.justification = c(0.95, 0.1))

plot_dir <-"D:/0.RMC/JohnMartin/DQC/Figures/Standalone_Figures/"
ggsave(paste0(plot_dir,"Stage_Discharge.png"), stage_discharge, height = 4, width = 6, dpi = 500)

# Stage - Discharge ------------
stage_discharge_final <- ggplot(sensitivities, aes(x = discharge, y = stage))+
  #geom_line(aes(color = model),linewidth = 0.65) +
  #scale_x_log10(breaks = scales::log_breaks(n=5,base = 10), minor_breaks = scales::minor_breaks_log(detail = 1))+
  geom_line(data = final_res, aes(x = discharge, y = stage,linetype = "Final - Forecast with No OW"), color = "black", linewidth = 0.8) +
  ggsci::scale_color_tron()+
  scale_x_continuous(breaks = seq(0,5e6,1e6), minor_breaks = seq(0,5e6,1e5),labels = scales::comma)+
  scale_y_continuous(breaks = seq(3700,3900,20), minor_breaks = seq(3700,3900,5),labels = scales::comma)+
  scale_linetype_manual(values = c("Final - Forecast with No OW" = "solid"), breaks = c("Final - Forecast with No OW")) +
  labs(x = "Discharge (cfs)",
       y = "Elevation (ft-NAVD88)",
       color = "Sensitivity Models",
       linetype = "Final Model") +
  theme(
    legend.position = "inside",
    legend.justification = c(0.95, 0.1))

plot_dir <-"D:/0.RMC/JohnMartin/DQC/Figures/Standalone_Figures/"
ggsave(paste0(plot_dir,"Stage_Discharge_finalonly.png"), stage_discharge_final, height = 4, width = 6, dpi = 500)
