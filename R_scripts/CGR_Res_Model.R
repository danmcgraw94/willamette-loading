# Cougar Reservoir Models
# ---- Package Management ----
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse, ggplot2, scales, ggh4x, ggrepel,patchwork,fs, 
  ggsci, ggtext, glue, cli, assertthat, future, furrr, 
  memoise,lubridate, purrr,zoo)


# Theme and plot directory
theme_set(theme_bw())
plotdir <- "/outputs/Figures/ResModel/"

# Read Data --------------------------------------------------------------------
cgr_res_models <- dir_ls("data/Cougar/",glob = "*CGR_Res_Model.csv*",recurse = T) %>% read_csv()

# clean up model names
model_names <- tibble(
  Model = unique(cgr_res_models$Model),
  Model_Names = c("Debris-No Gates",
                  "Best Est",
                  "Best Est-Delayed",
                  "Debris-100p",
                  "Debris-10p",
                  "Debris-25p",
                  "Debris-25p - Delayed",
                  "Debris-50p",
                  "Debris-50p - Delayed",
                  "Debris-75p",
                  "Max Capacity"),
  Model_colors = c("#E41A1C","black","#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E","cyan2","#A6761D","#666666","#377EB8"))

cgr_res_models <- cgr_res_models %>% 
  left_join(model_names, join_by(Model))

plot_colors <- setNames(model_names$Model_colors,model_names$Model_Names)

# CGR elevations
cgr_elevs <- tibble(
  Level = c("Top of Dam","Full Pool", "Max. Con Pool", "Top of Spillway Gates","Spillway Crest","Min. Con Pool","Min. Power Pool"),
  Elev_29 = c(1705,1699,1690,1699,1656.75,1532,1516),
  Elev_88 = Elev_29 + 4.38,
  Storage_acft = (c(NA,200,189,200,151.2,52,43.5))*1000) %>%
  filter(Level != c("Full Pool")) %>% 
  arrange(desc(Elev_88))

EL_TOD <- cgr_elevs %>% filter(Level == "Top of Dam") %>% pull(Elev_88)
EL_FC <- cgr_elevs %>% filter(Level == "Max. Con Pool") %>% pull(Elev_88)
EL_SPILL <- cgr_elevs %>% filter(Level == "Spillway Crest") %>% pull(Elev_88)

# Check Stage-Storage ----------------------------------------------------------
ggplot(cgr_res_models) +
  geom_line(aes(x = Stage_FT, y = Storage_ACFT, color = Model))+
  # these are so stupid
  scale_color_rickandmorty()

# Show all stage-discharge -----------------------------------------------------
# CHECK
ggplot(cgr_res_models) +
  geom_line(aes(x = Discharge_CFS, y = Stage_FT, color = Model))+
  # but use a new one for fun
  scale_color_futurama()

# Stage-Discharge --------------------------------------------------------------
# CHECK
ggplot(cgr_res_models) +
  # Top of dam
  geom_hline(yintercept = EL_TOD)+
  # Flood Control
  geom_hline(yintercept = EL_FC)+
  geom_line(aes(x = Discharge_CFS, y = Stage_FT, color = Model_Names)) +
  labs(x = "Discharge (CFS)", y = "Stage (ft-NAVD88)")+
  scale_color_futurama()+
  theme(legend.position = "inside", legend.position.inside = c(.85,.25)) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                minor_breaks = scales::minor_breaks_log()) + 
  scale_y_continuous(breaks = seq(1200,1800,20),minor_breaks = seq(1200,1800,5)) + 
  coord_cartesian(xlim = c(1E1,1e6), ylim = c(1500,1730))+
  theme_classic()

# Best-Estimate, BE-Delayed, Max Capacity, Debris No Gates ---------------------
selectedcurves <- c("Best Est","Best Est-Delayed","Debris-No Gates","Max Capacity")

best_est <- filter(cgr_res_models, Model_Names == "Best Est")
other_models <- filter(cgr_res_models, Model_Names %in% selectedcurves, Model_Names != "Best Est")

stage_discharge_bestest <- ggplot() +
  # Shaded zones
  annotate("rect", 
           xmin = -100000, xmax = 3e5, 
           ymin = EL_FC, ymax = EL_TOD, 
           alpha = 0.1, fill = "#fc8d59") +
  annotate("rect", 
           xmin = -100000, xmax = 3e5, 
           ymin = EL_SPILL, ymax = EL_FC, 
           alpha = 0.1, fill = "#91bfdb") +
  
  # Reference lines for key elevations
  geom_hline(yintercept = EL_TOD, linetype = "dashed", 
             color = "grey40", linewidth = 0.3) +
  geom_hline(yintercept = EL_FC, linetype = "dashed", 
             color = "grey40", linewidth = 0.3) +
  geom_hline(yintercept = EL_SPILL, linetype = "dashed", 
             color = "grey40", linewidth = 0.3) +
  
  # Other model curves
  geom_line(data = other_models, 
            aes(x = Discharge_CFS, y = Stage_FT, color = Model_Names),
            linewidth = 0.8) +
  
  # Best estimate curve (black, thicker)
  geom_line(data = best_est, 
            aes(x = Discharge_CFS, y = Stage_FT),
            color = "black", linewidth = 1.2) +
  
  # Elevation labels
  annotate("text", x = 200000, y = EL_TOD + 3, 
           label = paste0("Top of Dam = ", round(EL_TOD, 1), " ft"),
           size = 3, hjust = 1, fontface = "bold", color = "grey20") +
  
  annotate("text", x = 200000, y = EL_FC - 4, 
           label = paste0("Max. Conservation Pool = ", round(EL_FC, 1), " ft"),
           size = 3, hjust = 1, color = "grey20") +
  
  annotate("text", x = 200000, y = EL_SPILL + 4, 
           label = paste0("Spillway Crest = ", round(EL_SPILL, 1), " ft"),
           size = 3, hjust = 1, color = "grey20") +
  
  # Scales
  scale_x_continuous(
    breaks = seq(0, 300000, 50000),
    minor_breaks = seq(0, 200000, 10000),
    labels = scales::comma) +
  scale_y_continuous(
    breaks = seq(1500, 1730, 20),
    minor_breaks = seq(1500, 1730, 5),
    limits = c(1500, 1730),
    labels = scales::comma,
    expand = c(0,0)) +
  scale_color_manual(values = plot_colors,
                     name = "Alternative Curves") +
  # Labels and theme
  labs(x = "Discharge (cfs)", 
       y = "Stage (ft, NAVD88)",
       caption = "Bold black line indicates best estimate rating curve") +
  theme_bw() +
  theme(
    panel.grid.minor = element_line(color = "grey90", linewidth = 0.25),
    panel.grid.major = element_line(color = "grey80", linewidth = 0.4),
    legend.position = "inside",
    legend.justification = c(0.8, 0.2),
    legend.background = element_rect(fill = "white", color = "grey50"),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    plot.caption = element_text(hjust = 0, face = "italic", size = 9),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9)
  )+
  coord_cartesian(xlim = c(0,200000))

ggsave(paste0(getwd(),plotdir,"Best Estimate Stage-Discharge.png"), stage_discharge_bestest, height = 5, width = 7, dpi = 500)

# Stage-Storage-----------------------------------------------------------------
best_est_storage <- cgr_res_models %>% 
  filter(Model_Names == "Best Est")

stage_storage_bestest <- ggplot() +
  # Shaded zones
  annotate("rect", 
           xmin = 0, xmax = max(best_est_storage$Storage_ACFT, na.rm = TRUE), 
           ymin = EL_FC, ymax = EL_TOD, 
           alpha = 0.1, fill = "#fc8d59") +
  annotate("rect", 
           xmin = 0, xmax = max(best_est_storage$Storage_ACFT, na.rm = TRUE), 
           ymin = EL_SPILL, ymax = EL_FC, 
           alpha = 0.1, fill = "#91bfdb") +
  
  # Reference lines for key elevations
  geom_hline(yintercept = EL_TOD, linetype = "dashed", 
             color = "grey40", linewidth = 0.3) +
  geom_hline(yintercept = EL_FC, linetype = "dashed", 
             color = "grey40", linewidth = 0.3) +
  geom_hline(yintercept = EL_SPILL, linetype = "dashed", 
             color = "grey40", linewidth = 0.3) +
  
  # Storage-stage curve (black)
  geom_line(data = best_est_storage,
            aes(x = Storage_ACFT, y = Stage_FT),
            color = "black", linewidth = 1.2) +
  
  # Elevation labels
  annotate("text", x = 50000, 
           y = EL_TOD + 4, 
           label = paste0("Top of Dam = ", round(EL_TOD, 1), " ft"),
           size = 3, hjust = 0.5, fontface = "bold", color = "grey20") +
  
  annotate("text", x = 50000, 
           y = EL_FC - 4, 
           label = paste0("Max. Conservation Pool = ", round(EL_FC, 1), " ft"),
           size = 3, hjust = 0.5, color = "grey20") +
  
  annotate("text", x = 50000, 
           y = EL_SPILL + 4, 
           label = paste0("Spillway Crest = ", round(EL_SPILL, 1), " ft"),
           size = 3, hjust = 0.5, color = "grey20") +
  
  # Scales
  scale_x_continuous(
    breaks = seq(0, 300000, 50000),
    minor_breaks = seq(0, 300000, 10000),
    labels = scales::comma,
    expand = c(0, 0)) +
  scale_y_continuous(
    breaks = seq(1500, 1730, 20),
    minor_breaks = seq(1500, 1730, 5),
    limits = c(1500, 1730),
    labels = scales::comma,
    expand = c(0, 0)) +
  # Labels and theme
  labs(x = "Storage (ac-ft)", 
       y = "Stage (ft, NAVD88)") +
  theme_bw() +
  theme(
    panel.grid.minor = element_line(color = "grey90", linewidth = 0.25),
    panel.grid.major = element_line(color = "grey80", linewidth = 0.4),
    plot.caption = element_text(hjust = 0, face = "italic", size = 9),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9)
  )

print(stage_storage_bestest)

ggsave(paste0(getwd(),plotdir,"Best Estimate Stage-Storage.png"), stage_storage_bestest, height = 5, width = 7, dpi = 500)

# Debris Reduction--------------------------------------------------------------
# Define order and display labels
selectedcurves_colors <- c(
  "Best Est" = "#2166ac",
  "Debris-10p" = "#67a9cf",
  "Debris-25p" = "#fddbc7",
  "Debris-50p" = "#ef8a62",
  "Debris-75p" = "#d6604d",
  "Debris-100p" = "#b2182b")

legend_order <- c("Best Est", "Debris-10p", "Debris-25p", 
                  "Debris-50p", "Debris-75p", "Debris-100p")

legend_labels <- c("0% spillway reduction",
                   "10% spillway reduction",
                   "25% spillway reduction",
                   "50% spillway reduction",
                   "75% spillway reduction",
                   "100% spillway reduction")

# Prepare data
debris_data <- cgr_res_models %>% 
  filter(Model_Names %in% legend_order) %>% 
  mutate(Model_Names = factor(Model_Names, levels = rev(legend_order))) %>% 
  arrange(Model_Names, Discharge_CFS)

# plot
stage_discharge_debris <- ggplot() +
  # Shaded zones
  annotate("rect", 
           xmin = -100000, xmax = 3e5, 
           ymin = EL_FC, ymax = EL_TOD, 
           alpha = 0.1, fill = "#fc8d59") +
  annotate("rect", 
           xmin = -100000, xmax = 3e5, 
           ymin = EL_SPILL, ymax = EL_FC, 
           alpha = 0.1, fill = "#91bfdb") +
  # Reference lines for key elevations
  geom_hline(yintercept = EL_TOD, linetype = "dashed", 
             color = "grey40", linewidth = 0.3) +
  geom_hline(yintercept = EL_FC, linetype = "dashed", 
             color = "grey40", linewidth = 0.3) +
  geom_hline(yintercept = EL_SPILL, linetype = "dashed", 
             color = "grey40", linewidth = 0.3) +
  
  # Debris sensitivity curves
  geom_line(data = debris_data, 
            aes(x = Discharge_CFS, y = Stage_FT, color = Model_Names),
            linewidth = 0.8) +
  
  # Elevation labels
  annotate("text", x = 200000, y = EL_TOD + 3, 
           label = paste0("Top of Dam = ", round(EL_TOD, 1), " ft"),
           size = 3, hjust = 1, fontface = "bold", color = "grey20") +
  
  annotate("text", x = 200000, y = EL_FC - 4, 
           label = paste0("Max. Conservation Pool = ", round(EL_FC, 1), " ft"),
           size = 3, hjust = 1, color = "grey20") +
  
  annotate("text", x = 200000, y = EL_SPILL + 4, 
           label = paste0("Spillway Crest = ", round(EL_SPILL, 1), " ft"),
           size = 3, hjust = 1, color = "grey20") +
  
  # Scales
  scale_x_continuous(
    breaks = seq(0, 300000, 50000),
    minor_breaks = seq(0, 200000, 10000),
    labels = scales::comma) +
  scale_y_continuous(
    breaks = seq(1500, 1730, 20),
    minor_breaks = seq(1500, 1730, 5),
    limits = c(1500, 1730),
    labels = scales::comma,
    expand = c(0, 0)) +
  scale_color_manual(
    values = selectedcurves_colors,
    breaks = legend_order,
    labels = legend_labels,
    name = "Debris Sensitivity"
  ) +
  
  # Labels and theme
  labs(x = "Discharge (cfs)", 
       y = "Stage (ft, NAVD88)",
       caption = "Curves show impact of spillway capacity reduction due to debris") +
  theme_bw() +
  theme(
    panel.grid.minor = element_line(color = "grey90", linewidth = 0.25),
    panel.grid.major = element_line(color = "grey80", linewidth = 0.4),
    legend.position = "inside",
    legend.justification = c(0.8, 0.2),
    legend.background = element_rect(fill = "white", color = "grey50"),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    plot.caption = element_text(hjust = 0, face = "italic", size = 9),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9)
  )+
  coord_cartesian(xlim = c(0,200000))

print(stage_discharge_debris)
ggsave(paste0(getwd(),plotdir,"DebrisReduction_ResModels.png"), stage_discharge_debris, height = 5, width = 7, dpi = 500)

# Incremental Discharge for Best Est -------------------------------------------
weir_c <- 3
dam_L <- 1500

best_est <- best_est %>% 
  mutate(Inc_Discharge_CFS = Discharge_CFS - lag(Discharge_CFS,n=1)) %>% 
  mutate(OT_Depth = ifelse(Stage_FT <= EL_TOD,0,Stage_FT - EL_TOD),
         OT_Q = weir_c*dam_L*(OT_Depth^1.5))

big_changes <- best_est$Inc_Discharge_CFS >0 & best_est$Inc_Discharge_CFS >= 750

# Plot only best estimate discharge function ----------------------------------------
# Best Estimate Only -----------------------------------------------------------
best_est <- cgr_res_models %>% 
  filter(Model_Names == "Best Est")

stage_discharge_only_bestest <- ggplot() +
  # Shaded zones
  annotate("rect", 
           xmin = -100000, xmax = 3e5, 
           ymin = EL_FC, ymax = EL_TOD, 
           alpha = 0.1, fill = "#fc8d59") +
  annotate("rect", 
           xmin = -100000, xmax = 3e5, 
           ymin = EL_SPILL, ymax = EL_FC, 
           alpha = 0.1, fill = "#91bfdb") +
  # Reference lines for key elevations
  geom_hline(yintercept = EL_TOD, linetype = "dashed", 
             color = "grey40", linewidth = 0.3) +
  geom_hline(yintercept = EL_FC, linetype = "dashed", 
             color = "grey40", linewidth = 0.3) +
  geom_hline(yintercept = EL_SPILL, linetype = "dashed", 
             color = "grey40", linewidth = 0.3) +
  
  # Best estimate curve (black, thicker)
  geom_line(data = best_est, 
            aes(x = Discharge_CFS, y = Stage_FT),
            color = "black", linewidth = 1.2) +
  
  # Elevation labels
  annotate("text", x = 190000, y = EL_TOD + 3, 
           label = paste0("Top of Dam = ", round(EL_TOD, 1), " ft"),
           size = 3, hjust = 1, fontface = "bold", color = "grey20") +
  
  annotate("text", x = 190000, y = EL_FC - 4, 
           label = paste0("Max. Conservation Pool = ", round(EL_FC, 1), " ft"),
           size = 3, hjust = 1, color = "grey20") +
  
  annotate("text", x = 190000, y = EL_SPILL + 4, 
           label = paste0("Spillway Crest = ", round(EL_SPILL, 1), " ft"),
           size = 3, hjust = 1, color = "grey20") +
  
  # Scales
  scale_x_continuous(
    breaks = seq(0, 200000, 50000),
    minor_breaks = seq(0, 200000, 10000),
    labels = scales::comma) +
  scale_y_continuous(
    breaks = seq(1500, 1730, 20),
    minor_breaks = seq(1500, 1730, 5),
    limits = c(1500, 1730),
    labels = scales::comma,
    expand = c(0, 0)) +
  
  # Labels and theme
  labs(x = "Discharge (cfs)", 
       y = "Stage (ft, NAVD88)",
       caption = "Best estimate stage-discharge rating curve") +
  theme_bw() +
  theme(
    panel.grid.minor = element_line(color = "grey90", linewidth = 0.25),
    panel.grid.major = element_line(color = "grey80", linewidth = 0.4),
    plot.caption = element_text(hjust = 0, face = "italic", size = 9),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9))+
  coord_cartesian(xlim = c(0,200000))

print(stage_discharge_only_bestest)

ggsave(paste0(getwd(), plotdir, "Best_Estimate_Stage_Discharge.png"), 
       stage_discharge_only_bestest, height = 5, width = 7, dpi = 500, bg = "white")
