# Cougar Reservoir Models
# 8/13/2025
# Libraries
library(tidyverse)
library(lubridate)
library(scales)

# Theme set
theme_set(theme_bw())
source("D:/R/theme_USACE.r")
today = format(Sys.Date(),"%d-%b-%Y")
plot_height = 5
plot_width = 7

# Res Models --------------------------------------------------------------------
cgr_res_models <- read.csv("D:/0.RMC/Willamette/2025-Report/data/ResModel/CGR_Res_Model.csv",header = T)

reservoir_features <- tibble(
  Feature = c("Top of Dam","Spillway","IDF"),
  Stage_Ft = c(1710.9,1661.1,1710.2)
)
top_of_dam <- reservoir_features$Stage_Ft[reservoir_features$Feature == "Top of Dam"]

# Check Stage-Storage ----------------------------------------------------------
ggplot(cgr_res_models) +
  geom_line(aes(x = Stage_FT, y = Storage_ACFT, color = Model))

# Show all stage-discharge -----------------------------------------------------
ggplot(cgr_res_models) +
  geom_line(aes(x = Discharge_CFS, y = Stage_FT, color = Model))

# Clean up Legend --------------------------------------------------------------
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

# Stage-Discharge --------------------------------------------------------------
ggplot(cgr_res_models) +
  geom_hline(yintercept = reservoir_features$Stage_Ft[reservoir_features$Feature!="IDF"])+
  geom_line(aes(x = Discharge_CFS, y = Stage_FT, color = Model_Names)) +
  labs(x = "Discharge (CFS)", y = "Stage (ft-NAVD88)")+
  scale_color_manual(values = plot_colors)+
  theme(legend.position = "inside", legend.position.inside = c(.85,.25)) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                minor_breaks = scales::minor_breaks_log()) + 
  scale_y_continuous(breaks = seq(1200,1800,20),minor_breaks = seq(1200,1800,5)) + 
  coord_cartesian(xlim = c(1E1,1e6), ylim = c(1500,1730))

# Best-Estimate, BE-Delayed, Max Capacity, Debris No Gates ---------------------
selectedcurves <- c("Best Est","Best Est-Delayed","Debris-No Gates","Max Capacity")

best_est <- filter(cgr_res_models, Model_Names == "Best Est")
other_models <- filter(cgr_res_models, Model_Names %in% selectedcurves, Model_Names != "Best Est")

ggplot() +
  geom_hline(yintercept = reservoir_features$Stage_Ft[reservoir_features$Feature != "IDF"]) +
  geom_line(data = other_models,aes(x = Discharge_CFS, y = Stage_FT, color = Model_Names)) +
  geom_line(data = best_est,aes(x = Discharge_CFS, y = Stage_FT, color = Model_Names),linewidth = 1) +
  labs(x = "Discharge (CFS)", y = "Stage (ft-NAVD88)", color = "Model") +
  scale_color_manual(values = plot_colors) +
  theme(legend.position = "inside", legend.position.inside = c(.8, .25)) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                minor_breaks = scales::minor_breaks_log(), limits = c(1E2,1E6)) +
  scale_y_continuous(breaks = seq(1200,1800,20),minor_breaks = seq(1200,1800,5),limits =  c(1500,1730))+
  annotate("text", x = 2e2,
           y = reservoir_features$Stage_Ft[reservoir_features$Feature != "IDF"],
           label = reservoir_features$Feature[reservoir_features$Feature != "IDF"],
           vjust = -.5,
           size = 3)+
  theme(axis.title = element_text(size = 9),axis.text = element_text(size = 8))

fig_name <- "D:/0.RMC/Willamette/2025-Report/Figures/ResModel/Best_Est_ResModels_log.png"
ggsave(fig_name,plot = last_plot(),height = plot_height, width = plot_width,dpi = 300)

ggplot() +
  geom_hline(yintercept = reservoir_features$Stage_Ft[reservoir_features$Feature != "IDF"]) +
  geom_line(data = other_models,aes(x = Discharge_CFS, y = Stage_FT, color = Model_Names)) +
  geom_line(data = best_est,aes(x = Discharge_CFS, y = Stage_FT, color = Model_Names),linewidth = 1) +
  labs(x = "Discharge (CFS)", y = "Stage (ft-NAVD88)", color = "Model") +
  scale_color_manual(values = plot_colors) +
  theme(legend.position = "inside", legend.position.inside = c(.8, .25)) +
  # scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
  #               minor_breaks = scales::minor_breaks_log(), limits = c(1E2,1E6)) + 
  scale_x_continuous(breaks = seq(0,1e6,5e4),minor_breaks = seq(0,1e6,1e4),limits =  c(0,2e5))+
  scale_y_continuous(breaks = seq(1200,1800,20),minor_breaks = seq(1200,1800,5),limits =  c(1500,1730))+
  annotate("text", x = 1.5e5,
           y = reservoir_features$Stage_Ft[reservoir_features$Feature != "IDF"],
           label = reservoir_features$Feature[reservoir_features$Feature != "IDF"],
           vjust = 1.25,
           size = 3)+
  theme(axis.title = element_text(size = 9),axis.text = element_text(size = 8))

fig_name <- "D:/0.RMC/Willamette/2025-Report/Figures/ResModel/Best_Est_ResModels.png"
ggsave(fig_name,plot = last_plot(),height = plot_height, width = plot_width,dpi = 300)

# Stage-Storage-----------------------------------------------------------------
feature_labs <- paste0(reservoir_features$Feature[reservoir_features$Feature != "IDF"]," = ",reservoir_features$Stage_Ft[reservoir_features$Feature != "IDF"],"-ft")

cgr_res_models %>% 
  filter(Model_Names == "Best Est") %>% 
  ggplot() +
  #geom_hline(yintercept = reservoir_features$Stage_Ft[reservoir_features$Feature != "IDF"]) +
  geom_line(aes(x = Storage_ACFT, y = Stage_FT),color = "blue2",size = 1)+
  scale_x_continuous(breaks = seq(0,300000,50000),minor_breaks = seq(0,300000,10000),labels = scales::comma) +
  scale_y_continuous(breaks = seq(1200,1800,20),minor_breaks = seq(1200,1800,5),limits =  c(1500,1730),labels = scales::comma) +
  # annotate("text", x = 75000,
  #          y = reservoir_features$Stage_Ft[reservoir_features$Feature != "IDF"],
  #          label = feature_labs,
  #          vjust = -.5,
  #          size = 3)+
  labs(x = "Storage (ac-ft)", y = "Stage (ft-NAVD88)")

fig_name <- "D:/0.RMC/Willamette/2025-Report/Figures/ResModel/ResModel_Storage.png"
ggsave(fig_name,plot = last_plot(),height = plot_height, width = plot_width,dpi = 300)

# Debris Reduction--------------------------------------------------------------
# Define order and display labels
selectedcurves_colors <- c(
  "Best Est" = "#1b9e77",
  "Debris-10p" = "#d95f02",
  "Debris-25p" = "#e66101",
  "Debris-50p" = "#fdae61",
  "Debris-75p" = "#fee08b",
  "Debris-100p" = "#fee06a")

legend_order <- c("Best Est",
                  "Debris-10p",
                  "Debris-25p",
                  "Debris-50p",
                  "Debris-75p",
                  "Debris-100p")

legend_labels <- c("0% spillway reduction",
                   "10% spillway reduction",
                   "25% spillway reduction",
                   "50% spillway reduction",
                   "75% spillway reduction",
                   "100% spillway reduction")

debris_data <- dplyr::filter(cgr_res_models, Model_Names %in% legend_order) %>% 
  dplyr::mutate(Model_Names = factor(Model_Names, levels = rev(legend_order))) %>% 
  dplyr::arrange(Model_Names, Discharge_CFS)

ggplot(debris_data, aes(x = Discharge_CFS, y = Stage_FT, color = Model_Names)) +
  geom_hline(yintercept = reservoir_features$Stage_Ft[reservoir_features$Feature != "IDF"]) +
  geom_line(linewidth = 0.5) +
  labs(x = "Discharge (CFS)",
       y = "Stage (ft-NAVD88)",
       color = NULL) +
  scale_color_manual(values = selectedcurves_colors,
                     breaks = legend_order,
                     labels = legend_labels) +
  theme(legend.position = "inside",
        legend.position.inside = c(.8, .25),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)) +
  scale_x_continuous(breaks = seq(0, 1e6, 5e4),
                     minor_breaks = seq(0, 1e6, 1e4),
                     limits = c(0, 2e5)) +
  scale_y_continuous(breaks = seq(1200, 1800, 20),
                     minor_breaks = seq(1200, 1800, 5),
                     limits = c(1500, 1730)) +
  annotate("text",x = 1.5e5,
           y = reservoir_features$Stage_Ft[reservoir_features$Feature != "IDF"],
           label = reservoir_features$Feature[reservoir_features$Feature != "IDF"],
           vjust = 1.25,
           size = 3)

fig_name <- "D:/0.RMC/Willamette/2025-Report/Figures/ResModel/DebrisReduction_ResModels.png"
ggsave(fig_name,plot = last_plot(),height = plot_height, width = plot_width,dpi = 300)

# Incremental Discharge for Best Est -------------------------------------------
weir_c <- 3
dam_L <- 1500

best_est <- best_est %>% 
  mutate(Inc_Discharge_CFS = Discharge_CFS - lag(Discharge_CFS,n=1)) %>% 
  mutate(OT_Depth = ifelse(Stage_FT <= top_of_dam,0,Stage_FT - top_of_dam),
         OT_Q = weir_c*dam_L*(OT_Depth^1.5))

big_changes <- best_est$Inc_Discharge_CFS >0 & best_est$Inc_Discharge_CFS >= 750

# Plot best estimate discharge function ----------------------------------------
minor_log_brks <- function(x){
  (2:9)*10^(x)
}

Qbreaks <- 10^(1:7)
Qbreaks_minor <- (as.vector(sapply(c(0,1:7),minor_log_brks)))

# log scale ----
ggplot(best_est, aes(x = Discharge_CFS, y = Stage_FT)) +
  geom_hline(yintercept = reservoir_features$Stage_Ft[reservoir_features$Feature != "IDF"]) +
  geom_line(linewidth = 1) +
  labs(x = "Discharge (CFS)",
       y = "Stage (ft-NAVD88)") +
  theme(legend.position = "inside",
        legend.position.inside = c(.8, .25),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)) +
  scale_x_log10(breaks = Qbreaks,
                minor_breaks = Qbreaks_minor,
                labels = scales::comma) +
  # scale_x_continuous(breaks = seq(0, 1e6, 5e4),
  #                    minor_breaks = seq(0, 1e6, 1e4),
  #                    limits = c(0, 2e5)) +
  scale_y_continuous(breaks = seq(1200, 1800, 20),
                     minor_breaks = seq(1200, 1800, 5),
                     limits = c(1500, 1730)) +
  annotate("text",x = 1e1,
           y = reservoir_features$Stage_Ft[reservoir_features$Feature != "IDF"],
           label = reservoir_features$Feature[reservoir_features$Feature != "IDF"],
           vjust = 1.25,
           size = 3)+
  coord_cartesian(xlim = c(1,2e5))

fig_name <- "D:/0.RMC/Willamette/2025-Report/Figures/ResModel/Best_Discharge_log_ResModels.png"
ggsave(fig_name,plot = last_plot(),height = plot_height, width = plot_width,dpi = 300)

# not log scale -----
ggplot(best_est, aes(x = Discharge_CFS, y = Stage_FT)) +
  geom_hline(yintercept = reservoir_features$Stage_Ft[reservoir_features$Feature != "IDF"]) +
  geom_line(linewidth = 1) +
  labs(x = "Discharge (CFS)",
       y = "Stage (ft-NAVD88)") +
  theme(legend.position = "inside",
        legend.position.inside = c(.8, .25),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)) +
  # scale_x_log10(breaks = Qbreaks,
  #               minor_breaks = Qbreaks_minor) +
  scale_x_continuous(breaks = seq(0, 1e6, 5e4),
                     minor_breaks = seq(0, 1e6, 1e4),
                     limits = c(0, 2e5),
                     labels = scales::comma) +
  scale_y_continuous(breaks = seq(1200, 1800, 20),
                     minor_breaks = seq(1200, 1800, 5),
                     limits = c(1500, 1730)) +
  annotate("text",x = 1.5e5,
           y = reservoir_features$Stage_Ft[reservoir_features$Feature != "IDF"],
           label = reservoir_features$Feature[reservoir_features$Feature != "IDF"],
           vjust = 1.25,
           size = 3)

fig_name <- "D:/0.RMC/Willamette/2025-Report/Figures/ResModel/Best_Discharge_ResModels.png"
ggsave(fig_name,plot = last_plot(),height = plot_height, width = plot_width,dpi = 300)
