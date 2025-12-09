# Final Stage-Frequency Curve --------------------------------------------------
# ---- Package Management ----
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  tidyverse, ggplot2, scales, ggh4x, ggrepel,patchwork,fs, 
  ggsci, ggtext, glue, cli, assertthat, future, furrr, 
  memoise,lubridate, purrr,zoo,janitor,gt)

# Theme and plot directory
theme_set(theme_bw())
plotdir <- file.path(getwd(),"outputs","Figures","Stage_Frequency")
dir_create(plotdir)

# Load Final Stage-Frequency Curve ---------------------------------------------
rfa_2025 <- dir_ls("data/Cougar/",glob = "*Final_FullUncert.csv*",recurse = T) %>%
  read_csv()

# Load Final 2018 Stage-Frequency Curve ----------------------------------------
rfa_2018 <- dir_ls("data/Cougar/",glob = "*StageFreq_2018.csv*",recurse = T) %>%
  read_csv()

# remove unessecary NA data
rfa_2018 <- rfa_2018 %>% filter(!is.na(Upper))

# Empirical Stages -------------------------------------------------------------
empirical_stages <- dir_ls("data/Cougar/",glob = "*CGR_Empirical_StageFreq.csv*",recurse = T) %>%
  read_csv() %>%
  rename(AEP = Plotting_Position) %>% 
  mutate(DT = mdy(Date))

# Add Z- and Gumbel-variate ----------------------------------------------------
rfa_2025 <- rfa_2025 %>%
  mutate(Z_AEP = qnorm(1-AEP),
         Gumbel_AEP = -log(-log(1-AEP)),.before = everything())

rfa_2018 <- rfa_2018 %>%
  mutate(Z_AEP = qnorm(1-AEP),
         Gumbel_AEP = -log(-log(1-AEP)),.before = everything())

empirical_stages <- empirical_stages %>%
  mutate(Z_AEP = qnorm(1-AEP),
         Gumbel_AEP = -log(-log(1-AEP)),.before = everything())

# Critical Elevs ---------------------------------------------------------------
pmf_ngvd29 <- 1710.2 - 4.38

cgr_elevs <- tibble(
  Level = c("Top of Dam","PMF","Full Pool", "Max. Con. Pool", "Top of Spillway Gates","Spillway Crest","Min. Con. Pool","Min. Power Pool"),
  Elev_29 = c(1705,pmf_ngvd29,1699,1690,1699,1656.75,1532,1516),
  Elev_88 = Elev_29 + 4.38) %>% 
  filter(!Level %in% c("Full Pool","Min. Con. Pool","Min. Power Pool")) %>% 
  arrange(desc(Elev_88)) %>%
  mutate(elev_label = paste0(Level, ": ",round(Elev_88,1),"-ft"))

# Estimate Crtical Elevation AEPs ----------------------------------------------
# rfa_2025_AEPs <- 1 - pnorm(approx(x = rfa_2025$Expected, y = rfa_2025$Z_AEP, xout = cgr_elevs$Elev_88)$y)
# rfa_2018_AEPs <- 1 - pnorm(approx(x = rfa_2018$Expected, y = rfa_2018$Z_AEP, xout = cgr_elevs$Elev_88)$y)

cgr_elevs <- cgr_elevs %>% 
  mutate(rfa_2025_AEPs = 1 - pnorm(approx(x = rfa_2025$Expected, y = rfa_2025$Z_AEP, xout = cgr_elevs$Elev_88)$y),
         rfa_2018_AEPs = 1 - pnorm(approx(x = rfa_2018$Expected, y = rfa_2018$Z_AEP, xout = cgr_elevs$Elev_88)$y)) %>% 
  mutate(rfa_2025_RTs = round(1/rfa_2025_AEPs,0),
         rfa_2018_RTs = round(1/rfa_2018_AEPs,0)) %>%
  mutate(AEP_Label_2025 = paste0(Level,": ",
                                 sprintf("%.2E",rfa_2025_AEPs),
                                 " (",
                                 scales::number(1/rfa_2025_AEPs, accuracy = 1000, big.mark = ","),
                                 " yrs)"))

# Pivot Data Longer for plotting -----------------------------------------------
rfa_2025_long <- rfa_2025 %>% 
  pivot_longer(cols = -c(Gumbel_AEP, Z_AEP, AEP),
               values_to = "Stage_ft",
               names_to = "Curve") %>% 
  mutate(nice_name = case_when(
    Curve == "Upper" ~ "90% Uncertainity Bounds",
    Curve == "Lower" ~ "90% Uncertainity Bounds",
    Curve == "Expected" ~ "3-Day Expected Stage-Frequency Curve",
    Curve == "Median" ~ "3-Day Median Stage-Frequency Curve"))

rfa_2018_long <- rfa_2018 %>% 
  select(-c(Sensitivity,Description)) %>% 
  pivot_longer(cols = -c(Gumbel_AEP, Z_AEP, AEP),
               values_to = "Stage_ft",
               names_to = "Curve") %>% 
  mutate(nice_name = case_when(
    Curve == "Upper" ~ "90% Uncertainity Bounds",
    Curve == "Lower" ~ "90% Uncertainity Bounds",
    Curve == "Expected" ~ "3-Day Expected Stage-Frequency Curve",
    Curve == "Median" ~ "3-Day Median Stage-Frequency Curve"))


# FINAL STAGE FREQ PLOT --------------------------------------------------------
# Mimics CEDALS template plotter

# 1. Set up Axes - use Gumbel Variate for AEP (x-axis)
aep_breaks <- c(9.9e-1, 9e-1, 5e-1, 1e-1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7, 1e-8, 1e-9, 1e-10)
minor_aep_breaks <- unlist(lapply(2:9, function(i) i * 10^-(1:10)))

# Gumbel breaks
Gumbel_AEP_breaks <- -log(-log(1-aep_breaks))
Gumbel_AEP_breaks_minor <- -log(-log(1-minor_aep_breaks))

# Z breaks
z_breaks <- qnorm(1-aep_breaks)
z_breaks_minor <- qnorm(1-minor_aep_breaks)

# X-limits
gumb_limit1 <- -log(-log(1-(9.999e-1)))
gumb_limit2 <- -log(-log(1-(9e-8)))

# Y-limits
stage_1 <- 1658
stage_2 <- 1722

# Critical Elevation label - X coordinate
label_aep <- 0.025

# 2. Define colors & line types -----
final_fill <- "grey80"

plot_colors <- c("3-Day Expected Stage-Frequency Curve" = "#3B4992FF",
                 "90% Uncertainity Bounds" = "#3B4992FF")

plot_colors_brks <- c("3-Day Expected Stage-Frequency Curve",
                      "90% Uncertainity Bounds")

plot_linetype <- c("3-Day Expected Stage-Frequency Curve" = "solid",
                 "90% Uncertainity Bounds" = "dashed")

plot_linetype_brks <- c("3-Day Expected Stage-Frequency Curve",
                        "90% Uncertainity Bounds")

pmf <- "#FF0000"
top_of_dam <- "black"

# 3. Plot ----------------------------------------------------------------------
final_stagefreq <- ggplot() +
  geom_ribbon(data = rfa_2025, 
              aes(x = Gumbel_AEP, ymin =  Lower, ymax = Upper), fill = final_fill, alpha = 0.35) +
  geom_line(data = rfa_2025, 
            aes(x = Gumbel_AEP,
                y = Expected, 
                color = "3-Day Expected Stage-Frequency Curve",
                linetype = "3-Day Expected Stage-Frequency Curve"), linewidth = 0.85) +
  geom_line(data = rfa_2025_long %>% filter(nice_name == "90% Uncertainity Bounds"), 
            aes(x = Gumbel_AEP, 
                y = Stage_ft, 
                group = Curve, 
                color = "90% Uncertainity Bounds",
                linetype = "90% Uncertainity Bounds"), linewidth = 0.4) +
  scale_color_manual(values = plot_colors, breaks = plot_colors_brks) +
  scale_linetype_manual(values = plot_linetype, breaks = plot_linetype_brks) +
  
  # Observed Stage points
  geom_point(data = empirical_stages, aes(x = Gumbel_AEP, y = Stage_ft, shape = "Obs. Stages (Summer AMS 1971-2001)"), size = 1.5, alpha = 0.7) +
  scale_shape_manual(values = c("Obs. Stages (Summer AMS 1971-2001)" = 16)) +
  
  # Scales
  scale_x_continuous(breaks = Gumbel_AEP_breaks, minor_breaks = Gumbel_AEP_breaks_minor, labels = aep_breaks) +
  scale_y_continuous(breaks = seq(1600, 1800, 10), minor_breaks = seq(1600, 1800, 5), labels = scales::comma) +
  
  # Labs
  labs(x = "Annual Exceedance Probability (AEP)",
       y = "Elevation (ft-NAVD88)",
       title = "Cougar Dam - IES Stage Frequency Curve",
       linetype = "Curve",
       color = "Curve",
       shape = "Observations") +
  
  # Legend Guides
  guides(color = guide_legend(order = 1), linetype = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  
  # Crit Elevation lines - Make PMF Red
  geom_hline(yintercept = cgr_elevs$Elev_88[cgr_elevs$Level != "PMF"]) +
  geom_hline(yintercept = cgr_elevs$Elev_88[cgr_elevs$Level == "PMF"], color = "#BB0021FF", alpha = 0.6,linetype = "dashed") +
  annotate("text", x = -log(-log(1 - label_aep)),
           y = cgr_elevs$Elev_88[cgr_elevs$Level != "PMF"],
           label = cgr_elevs$elev_label[cgr_elevs$Level != "PMF"], 
           size = 2.75, fontface = "bold.italic", vjust = -0.75) +
  annotate("text", x = -log(-log(1 - 0.0001)),
           y = cgr_elevs$Elev_88[cgr_elevs$Level == "PMF"],
           label = cgr_elevs$elev_label[cgr_elevs$Level == "PMF"], 
           size = 2.75, fontface = "bold.italic", vjust = -0.75, color = "#BB0021FF") +
  
  annotate("label", x = -log(-log(1 - 2E-5)), y = 1690,
           label = paste(cgr_elevs$AEP_Label_2025[1:3], collapse = "\n"),
           size = 2.75, fontface = "bold.italic", vjust = 1, hjust = 0,
           fill = "beige") +
  # Theme
  theme_bw() +
  theme(legend.position = "inside",
        legend.justification = c(0.95, 0.1),
        legend.text = element_text(size = 7),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.key.width = unit(1, "cm"),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 11, face = "bold",hjust = 0.5)) +
  coord_cartesian(xlim = c(gumb_limit1, gumb_limit2), ylim = c(stage_1, stage_2),expand = F)

# Save -------------------------------------------------------------------------
final_stagefreq

ggsave(file.path(plotdir,"Final_Stage_Frequency.png"), final_stagefreq, height = 6, width = 10, dpi = 500)

# 6x8 - relocate label
final_stagefreq_nolab <- final_stagefreq
final_stagefreq_nolab$layers[[9]] <- NULL
final_stagefreq_6x8 <- final_stagefreq_nolab + annotate("label", x = -log(-log(1 - 6.5E-5)), y = 1690,
                                label = paste(cgr_elevs$AEP_Label_2025[1:3], collapse = "\n"),
                                size = 2.75, fontface = "bold.italic", vjust = 1, hjust = 0,
                                fill = "beige")

ggsave(file.path(plotdir,"Final_Stage_Frequency_6x8.png"), final_stagefreq_6x8, height = 6, width = 8, dpi = 500)
# PREVIOUS STUDY ---------------------------------------------------------------
prv_plot_colors <- c("3-Day Expected Stage-Frequency Curve" = "#3B4992FF",
                 "90% Uncertainity Bounds" = "#3B4992FF",
                 "2018 3-Day Expected" = "#008B45FF",
                 "2018 90% Uncertainity Bounds" =  "#008B45FF")

prv_plot_colors_brks <- c("3-Day Expected Stage-Frequency Curve",
                          "90% Uncertainity Bounds",
                          "2018 3-Day Expected",
                          "2018 90% Uncertainity Bounds")

prv_plot_linetype <- c("3-Day Expected Stage-Frequency Curve" = "solid",
                       "90% Uncertainity Bounds" = "dashed",
                       "2018 3-Day Expected" = "solid",
                       "2018 90% Uncertainity Bounds" =  "dotted")

prv_plot_linetype_brks <- c("3-Day Expected Stage-Frequency Curve",
                            "90% Uncertainity Bounds",
                            "2018 3-Day Expected",
                            "2018 90% Uncertainity Bounds")

previous_stagefreq <- ggplot() +
  geom_ribbon(data = rfa_2025, 
              aes(x = Gumbel_AEP, ymin =  Lower, ymax = Upper), fill = final_fill, alpha = 0.35) +
  geom_line(data = rfa_2025, 
            aes(x = Gumbel_AEP,
                y = Expected, 
                color = "3-Day Expected Stage-Frequency Curve",
                linetype = "3-Day Expected Stage-Frequency Curve"), linewidth = 0.85) +
  geom_line(data = rfa_2025_long %>% filter(nice_name == "90% Uncertainity Bounds"), 
            aes(x = Gumbel_AEP, 
                y = Stage_ft, 
                group = Curve, 
                color = "90% Uncertainity Bounds",
                linetype = "90% Uncertainity Bounds"), linewidth = 0.4) +
  geom_line(data = rfa_2018, 
            aes(x = Gumbel_AEP,
                y = Expected, 
                color = "2018 3-Day Expected",
                linetype = "2018 3-Day Expected"), linewidth = 0.85) +
  geom_line(data = rfa_2018_long %>% filter(nice_name == "90% Uncertainity Bounds"), 
            aes(x = Gumbel_AEP, 
                y = Stage_ft, 
                group = Curve, 
                color = "2018 90% Uncertainity Bounds",
                linetype = "2018 90% Uncertainity Bounds"), linewidth = 0.4) +
  scale_color_manual(values = prv_plot_colors, breaks = prv_plot_colors_brks) +
  scale_linetype_manual(values = prv_plot_linetype, breaks = prv_plot_linetype_brks) +
  
  # Observed Stage points
  geom_point(data = empirical_stages, aes(x = Gumbel_AEP, y = Stage_ft, shape = "Obs. Stages (Summer AMS 1971-2001)"), size = 1.5, alpha = 0.7) +
  scale_shape_manual(values = c("Obs. Stages (Summer AMS 1971-2001)" = 16)) +
  
  # Scales
  scale_x_continuous(breaks = Gumbel_AEP_breaks, minor_breaks = Gumbel_AEP_breaks_minor, labels = aep_breaks) +
  scale_y_continuous(breaks = seq(1600, 1800, 10), minor_breaks = seq(1600, 1800, 5), labels = scales::comma) +
  
  # Labs
  labs(x = "Annual Exceedance Probability (AEP)",
       y = "Elevation (ft-NAVD88)",
       title = "Cougar Dam - IES Stage Frequency Curve",
       subtitle = "Full Uncertainty included from 2018 Study",
       linetype = "Curve",
       color = "Curve",
       shape = "Observations") +
  
  # Legend Guides
  guides(color = guide_legend(order = 1), linetype = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  
  # Crit Elevation lines - Make PMF Red
  geom_hline(yintercept = cgr_elevs$Elev_88[cgr_elevs$Level != "PMF"]) +
  geom_hline(yintercept = cgr_elevs$Elev_88[cgr_elevs$Level == "PMF"], color = "#BB0021FF", alpha = 0.6,linetype = "dashed") +
  annotate("text", x = -log(-log(1 - label_aep)),
           y = cgr_elevs$Elev_88[cgr_elevs$Level != "PMF"],
           label = cgr_elevs$elev_label[cgr_elevs$Level != "PMF"], 
           size = 2.75, fontface = "bold.italic", vjust = -0.75) +
  annotate("text", x = -log(-log(1 - 0.0001)),
           y = cgr_elevs$Elev_88[cgr_elevs$Level == "PMF"],
           label = cgr_elevs$elev_label[cgr_elevs$Level == "PMF"], 
           size = 2.75, fontface = "bold.italic", vjust = -0.75, color = "#BB0021FF") +
  
  # annotate("label", x = -log(-log(1 - 2E-5)), y = 1690,
  #          label = paste(cgr_elevs$AEP_Label_2025[1:3], collapse = "\n"),
  #          size = 2.75, fontface = "bold.italic", vjust = 1, hjust = 0,
  #          fill = "beige") +
  # Theme
  theme_bw() +
  theme(legend.position = "inside",
        legend.justification = c(0.95, 0.1),
        legend.text = element_text(size = 7),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.key.width = unit(1, "cm"),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 11, face = "bold",hjust = 0.5),
        plot.subtitle = element_text(size = 9,hjust = 0.5)) +
  coord_cartesian(xlim = c(gumb_limit1, gumb_limit2), ylim = c(stage_1, stage_2),expand = F)

# Save -------------------------------------------------------------------------
previous_stagefreq

ggsave(file.path(plotdir,"PreviousStudy_Stage_Frequency.png"), previous_stagefreq, height = 6, width = 10, dpi = 500)

ggsave(file.path(plotdir,"PreviousStudy_Stage_Frequency_6x8.png"), previous_stagefreq, height = 6, width = 8, dpi = 500)

# Critical Elevation AEPs - GT Table -------------------------------------------
cgr_elevs_nice <- cgr_elevs %>% 
  mutate(RFA_2025 = sprintf("%.2E",rfa_2025_AEPs),
         RFA_2018 = sprintf("%.2E",rfa_2018_AEPs),.before = rfa_2025_AEPs) %>% 
  select(-c(elev_label,AEP_Label_2025)) %>% 
  mutate(OOM_Shift = log10(rfa_2018_AEPs) - log10(rfa_2025_AEPs),.before = rfa_2025_RTs)

elevation_AEPS <- cgr_elevs_nice %>% 
  select(-c(RFA_2025,RFA_2018)) %>% 
  gt() %>% 
  tab_header(
    title = md("**Cougar Dam Critical Elevations**"),
    subtitle = "Associated AEPs from 2025 and 2018 Stage-Frequency Estimates"
  ) %>%
  cols_label(
    Level = "Name",
    Elev_29  = "Elev-NGVD29",
    Elev_88  = "Elev-NAVD88",
    rfa_2025_AEPs = "AEP: 2025",
    rfa_2018_AEPs = "AEP: 2018",
    OOM_Shift = "Order of Magnitude Shift from 2018",
    rfa_2025_RTs = "1/Yrs: 2025",
    rfa_2018_RTs = "1/Yrs: 2018"
  ) %>%
  fmt_number(
    columns = OOM_Shift,
    decimals = 2,
    use_seps = T
  ) %>%
  fmt_number(
    columns = c(rfa_2025_RTs, rfa_2018_RTs),
    decimals = 0,
    use_seps = T
  ) %>%
  fmt_number(
    columns = c(Elev_29,Elev_88),
    decimals = 1,
    use_seps = T
  ) %>%
  fmt_scientific(
    columns = c(rfa_2025_AEPs,rfa_2018_AEPs),
    decimals = 2,
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
  )

elevation_AEPS

# SAVE -------------------------------------------------------------------------
outfile <- file.path(getwd(),"outputs","Stage_AEPs.html")
gtsave(elevation_AEPS, outfile)
