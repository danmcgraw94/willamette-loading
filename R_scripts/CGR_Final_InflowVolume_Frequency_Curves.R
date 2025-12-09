# Final Inflow-Volume Frequency Curve --------------------------------------------------
# ---- Package Management ----
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  tidyverse, ggplot2, scales, ggh4x, ggrepel,patchwork,fs, 
  ggsci, ggtext, glue, cli, assertthat, future, furrr, 
  memoise,lubridate, purrr,zoo,janitor,gt)

# Theme and plot directory
theme_set(theme_bw())
plotdir <- file.path(getwd(),"outputs","Figures","Volume_Frequency")
dir_create(plotdir)

# Read Data --------------------------------------------------------------------
# Distribution summaries
bestfit_sum <- dir_ls("data/Cougar/",glob = "*BF_analysis_summary.csv*",recurse = T) %>%
  read_csv() %>% 
  janitor::clean_names()

# Volume-Frequency Curves
vfcs <- dir_ls("data/Cougar/",glob = "*BF_Sensitivities.csv*",recurse = T) %>%
  read_csv()

# Add Z field
vfcs <- vfcs %>% 
  mutate(Z_aep = qnorm(1 - Probability),.before = everything())

# Get 2025 Final ---------------------------------------------------------------
final_sum <- bestfit_sum %>% filter(sensitivity == "Final")
final_2025 <- vfcs %>% 
  filter(Analysis == final_sum$analysis) %>% 
  mutate(nice_name = "2025 Final: Syst + Hist + Paleo-Ext (No PSI) + PF")

# Get 2018 Final ---------------------------------------------------------------
# vfc_2018 <- dir_ls("data/Cougar/",glob = "*VFC_2018.csv*",recurse = T) %>%
#   read_csv()
sum_2018 <- bestfit_sum %>% filter(analysis == "8c - CGR_3day_annual_PF_2018")
final_2018 <- vfcs %>%
  filter(Analysis == sum_2018$analysis) %>% 
  mutate(nice_name = "2018 Final: Syst + Hist + PF")

# Best Fit Input data ----------------------------------------------------------
bf_input <- dir_ls("data/Cougar/",glob = "*CGR_3day_Input_Paleo_ext_BB_nopsi.csv*",recurse = T) %>%
  read_csv() %>% 
  filter(Datatype != "Threshold") %>% 
  select(-c(start_year,end_year)) %>% 
  mutate(Datatype = ifelse(flow < 1500,"Low-Outlier",Datatype))

# Add Z field
bf_input <- bf_input %>% 
  mutate(Z_aep = qnorm(1 - plotting_position))

# PF Quantile Priors -----------------------------------------------------------
pf_priors <- tibble(AEP = c(0.01,0.001,0.0001),
                    Mean_Q = c(15400,22000,30000),
                    SD_Q = c(2300,3800,6500),
                    Lower = round(qnorm(0.05,Mean_Q,SD_Q),-2),
                    Flow = Mean_Q,
                    Upper = round(qnorm(0.95,Mean_Q,SD_Q),-2))

# Add Z field
pf_priors <- pf_priors %>% 
  mutate(Z_aep = qnorm(1 - AEP),.before = everything())

# 3-Day PMF Volume -------------------------------------------------------------
pmf_3day <- 42700

# Estimate PMF AEP -------------------------------------------------------------
pmf_aep_2025 <- 1 - pnorm(approx(x = final_2025$Posterior_Predictive, y = final_2025$Z_aep, xout = pmf_3day)$y)
pmf_aep_2018 <- 1 - pnorm(approx(x = final_2018$Posterior_Predictive, y = final_2018$Z_aep, xout = pmf_3day)$y)

vfc_pmf_aep <- tibble(Analysis = c("2025 Final","2018 Final"),
                      Mean_log = c(final_sum$mean_of_log, sum_2018$mean_of_log),
                      SD_log = c(final_sum$sd_of_log, sum_2018$sd_of_log),
                      Skew_log = c(final_sum$skew_of_log, sum_2018$skew_of_log),
                      ERL = c(final_sum$erl,sum_2018$erl),
                      PMF_3day = pmf_3day,
                      AEP_PMF = c(pmf_aep_2025, pmf_aep_2018)) %>% 
  mutate(OOM_Shift = log10(pmf_aep_2018) - log10(AEP_PMF)) %>% 
  # Nice label - AEP and return period (rounded to 1000 years)
  mutate(nice_label = paste0(Analysis," PMF: ", sprintf("%.2E",AEP_PMF),
                             " (",
                             scales::number(1/AEP_PMF, accuracy = 1000, big.mark = ","),
                             " yrs)"))

# Pivot Data Longer for plotting CI --------------------------------------------
final_2025_CI <- final_2025 %>% 
  select(-c(Analysis,nice_name)) %>% 
  pivot_longer(cols = -c(Z_aep, Probability),
               values_to = "Inflow_cfs",
               names_to = "Curve") %>% 
  mutate(nice_name = case_when(
    Curve == "CI_95" ~ "90% Credible Intervals",
    Curve == "CI_5" ~ "90% Credible Intervals",
    Curve == "Posterior_Predictive" ~ "Posterior Predictive",
    Curve == "Posterior_Mode" ~ "Posterior Mode"))

final_2018_CI <- final_2018 %>% 
  select(-c(Analysis,nice_name)) %>% 
  pivot_longer(cols = -c(Z_aep, Probability),
               values_to = "Inflow_cfs",
               names_to = "Curve") %>% 
  mutate(nice_name = case_when(
    Curve == "CI_95" ~ "90% Credible Intervals",
    Curve == "CI_5" ~ "90% Credible Intervals",
    Curve == "Posterior_Predictive" ~ "Posterior Predictive",
    Curve == "Posterior_Mode" ~ "Posterior Mode"))

# FINAL INFLOW VOL FREQ PLOT ---------------------------------------------------
# Mimics CEDALS template plotter

# 1. Set up Axes - use Z Variate for AEP (x-axis)
aep_breaks <- c(9.9e-1, 9e-1, 5e-1, 1e-1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7, 1e-8, 1e-9, 1e-10)
minor_aep_breaks <- unlist(lapply(2:9, function(i) i * 10^-(1:10)))

# Gumbel breaks
Gumbel_AEP_breaks <- -log(-log(1-aep_breaks))
Gumbel_AEP_breaks_minor <- -log(-log(1-minor_aep_breaks))

# Z breaks
z_breaks <- qnorm(1-aep_breaks)
z_breaks_minor <- qnorm(1-minor_aep_breaks)

# X-limits
z_limit1 <- qnorm(1-(9.999e-1))
z_limit2 <- qnorm(1-(9e-8))

# Y-limits
inflow_1 <- 4e2
inflow_2 <- 1.2e5

# Critical Elevation label - X coordinate
label_aep <- 0.99

# 2. Define colors & line types ------------------------------------------------
# final_fill <- "grey80"
final_fill <- "#3B4992FF"

# Colors
plot_colors <- c("Posterior Predictive" = "#3B4992FF",
                 "90% Credible Intervals" = "#3B4992FF")
plot_colors_brks <- names(plot_colors)

# Linetype
plot_linetype <- c("Posterior Predictive" = "solid",
                   "90% Credible Intervals" = "dashed")
plot_linetype_brks <- names(plot_linetype)

# Horizontal PMF Line
pmf <- "#FF0000"

# Point Shapes
pt_shp <- c("Systematic POR (1927-2021)" = 16,
            "Historic Event" = 21,
            "Precip-Freq Quantile Prior" = 22,
            "Low-Outlier" = 0)
pt_shp_brks <- names(pt_shp)

# Labels -------------
all_labs <- c(paste0("Mean (of log): ",vfc_pmf_aep$Mean_log[1]),
             paste0("Std Dev (of log): ",vfc_pmf_aep$SD_log[1]),
             paste0("Skew (of log): ",vfc_pmf_aep$Skew_log[1]),
             paste0("ERL (yrs): ",vfc_pmf_aep$ERL[1]),
             vfc_pmf_aep$nice_label[1])

final_label <- paste(all_labs, collapse = "\n")

# 3. Plot ----------------------------------------------------------------------
final_volfreq <- ggplot() +
  geom_ribbon(data = final_2025, 
              aes(x = Z_aep, ymin =  CI_5, ymax = CI_95), fill = final_fill, alpha = 0.18) +
  geom_line(data = final_2025, 
            aes(x = Z_aep,
                y = Posterior_Predictive, 
                color = "Posterior Predictive",
                linetype = "Posterior Predictive"), linewidth = 0.85) +
  geom_line(data = final_2025_CI %>% filter(nice_name == "90% Credible Intervals"), 
            aes(x = Z_aep, 
                y = Inflow_cfs, 
                group = Curve, 
                color = "90% Credible Intervals",
                linetype = "90% Credible Intervals"), linewidth = 0.4) +
  scale_color_manual(values = plot_colors, breaks = plot_colors_brks) +
  scale_linetype_manual(values = plot_linetype, breaks = plot_linetype_brks) +
  
  # Observed Inflow points
  geom_point(data = bf_input %>% filter(Datatype == "Point"),
             aes(x = Z_aep, y = flow, shape = "Systematic POR (1927-2021)"), size = 1.25, alpha = 0.7) +
  # Historic Interval
  geom_errorbar(data = bf_input %>% filter(Datatype == "Interval"),
                aes(x = Z_aep, ymin = lower_flow, ymax = upper_flow), width = 0.05, color = "black") +
  geom_point(data = bf_input %>% filter(Datatype == "Interval"),
             aes(x = Z_aep, y = flow, shape = "Historic Event"), size = 1.25, alpha = 0.9, fill = "lightblue2",color = "black") +
  # PR Priors
  geom_errorbar(data = pf_priors,
                aes(x = Z_aep, ymin = Lower, ymax = Upper), width = 0.05, color = "black") +
  geom_point(data = pf_priors,
             aes(x = Z_aep, y = Flow, shape = "Precip-Freq Quantile Prior"), size = 1.25, alpha = 0.9, fill = "#991A00FF",color = "black") +
  # Low-Outlier
  geom_point(data = bf_input %>% filter(Datatype == "Low-Outlier"),
             aes(x = Z_aep, y = flow, shape = "Low-Outlier"), size = 1.25, alpha = 0.9,color = "#749B58FF") +
  # Point Legend
  scale_shape_manual(values = pt_shp,breaks = pt_shp_brks) +
  # Scales
  scale_x_continuous(breaks = z_breaks, minor_breaks = z_breaks_minor, labels = aep_breaks) +
  scale_y_log10(breaks = scales::log_breaks(n = 5), minor_breaks = scales::minor_breaks_log(detail = 1), labels = scales::comma) +
  
  # Labs
  labs(x = "Annual Exceedance Probability (AEP)",
       y = "3-Day Inflow-Volume",
       title = "Cougar Dam - IES 3-Day Volume Frequency Curve",
       linetype = "Curve",
       color = "Curve",
       shape = "Observations") +
  
  # Legend Guides
  guides(color = guide_legend(order = 1), linetype = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  
  # 3-Day PMF Inflow Volume
  geom_hline(yintercept = pmf_3day, color = "#BB0021FF", alpha = 0.95,linetype = "dashed")+
  annotate("text", x = qnorm(1-label_aep),
           y = pmf_3day,
           label = "3-Day PMF Volume: 42,700 cfs", 
           size = 2.75, fontface = "bold.italic", vjust = -0.75, hjust = 0, color = "#BB0021FF")+
  
  annotate("label", x = qnorm(1-label_aep), y = 2e4,
           label = final_label,
           size = 2.75, fontface = "bold.italic", vjust = 1, hjust = 0,
           fill = "beige")+
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
        plot.title = element_text(size = 11, face = "bold",hjust = 0.5))

# Save -------------------------------------------------------------------------
final_volfreq
ggsave(file.path(plotdir,"Final_Inflow_Vol_Frequency.png"), final_volfreq, height = 6, width = 8, dpi = 500)


# PREVIOUS STUDY ---------------------------------------------------------------
# Colors
prv_plot_colors <- c("Posterior Predictive" = "#3B4992FF",
                     "90% Credible Intervals" = "#3B4992FF",
                 "2018 Posterior Predictive" = "#008B45FF",
                 "2018 90% Credible Intervals" =  "#008B45FF")

prv_plot_colors_brks <- names(prv_plot_colors)

# linetypes
prv_plot_linetype <- c("Posterior Predictive" = "solid",
                       "90% Credible Intervals" = "dashed",
                       "2018 Posterior Predictive" = "solid",
                       "2018 90% Credible Intervals" =  "dotted")

prv_plot_linetype_brks <- names(prv_plot_linetype)

# Labels ---
prv_labs <- c(paste0("2025 ERL (yrs): ",vfc_pmf_aep$ERL[1]),
              paste0("2018 ERL (yrs): ",vfc_pmf_aep$ERL[2]),
              vfc_pmf_aep$nice_label[1],
              vfc_pmf_aep$nice_label[2])

prv_label <- paste(prv_labs, collapse = "\n")

# PLOT
previous_volfreq <- ggplot() +
  geom_ribbon(data = final_2025, 
              aes(x = Z_aep, ymin =  CI_5, ymax = CI_95), fill = final_fill, alpha = 0.18) +
  # 2025 posterior
  geom_line(data = final_2025, 
            aes(x = Z_aep,
                y = Posterior_Predictive, 
                color = "Posterior Predictive",
                linetype = "Posterior Predictive"), linewidth = 0.85) +
  # 2025 CI
  geom_line(data = final_2025_CI %>% filter(nice_name == "90% Credible Intervals"), 
            aes(x = Z_aep, 
                y = Inflow_cfs, 
                group = Curve, 
                color = "90% Credible Intervals",
                linetype = "90% Credible Intervals"), linewidth = 0.4) +
  # 2018 posterior
  geom_line(data = final_2018, 
            aes(x = Z_aep,
                y = Posterior_Predictive, 
                color = "2018 Posterior Predictive",
                linetype = "2018 Posterior Predictive"), linewidth = 0.7) +
  # 2018 CI
  geom_line(data = final_2018_CI %>% filter(nice_name == "90% Credible Intervals"), 
            aes(x = Z_aep, 
                y = Inflow_cfs, 
                group = Curve, 
                color = "2018 90% Credible Intervals",
                linetype = "2018 90% Credible Intervals"), linewidth = 0.4) +
  scale_color_manual(values = prv_plot_colors, breaks = prv_plot_colors_brks) +
  scale_linetype_manual(values = prv_plot_linetype, breaks = prv_plot_linetype_brks)+
  
  # Observed Inflow points
  geom_point(data = bf_input %>% filter(Datatype == "Point"),
             aes(x = Z_aep, y = flow, shape = "Systematic POR (1927-2021)"), size = 1.25, alpha = 0.7) +
  # Historic Interval
  geom_errorbar(data = bf_input %>% filter(Datatype == "Interval"),
                aes(x = Z_aep, ymin = lower_flow, ymax = upper_flow), width = 0.05, color = "black") +
  geom_point(data = bf_input %>% filter(Datatype == "Interval"),
             aes(x = Z_aep, y = flow, shape = "Historic Event"), size = 1.25, alpha = 0.9, fill = "lightblue2",color = "black") +
  # PR Priors
  geom_errorbar(data = pf_priors,
                aes(x = Z_aep, ymin = Lower, ymax = Upper), width = 0.05, color = "black") +
  geom_point(data = pf_priors,
             aes(x = Z_aep, y = Flow, shape = "Precip-Freq Quantile Prior"), size = 1.25, alpha = 0.9, fill = "#991A00FF",color = "black") +
  # Low-Outlier
  geom_point(data = bf_input %>% filter(Datatype == "Low-Outlier"),
             aes(x = Z_aep, y = flow, shape = "Low-Outlier"), size = 1.25, alpha = 0.9,color = "#749B58FF") +
  # Point Legend
  scale_shape_manual(values = pt_shp,breaks = pt_shp_brks) +
  # Scales
  scale_x_continuous(breaks = z_breaks, minor_breaks = z_breaks_minor, labels = aep_breaks) +
  scale_y_log10(breaks = scales::log_breaks(n = 5), minor_breaks = scales::minor_breaks_log(detail = 1), labels = scales::comma) +
  
  # Labs
  labs(x = "Annual Exceedance Probability (AEP)",
       y = "3-Day Inflow-Volume",
       title = "Cougar Dam - IES 3-Day Volume Frequency Curve",
       subtitle = "Points shown are from 2025 Volume-Frequency Analysis",
       linetype = "Curve",
       color = "Curve",
       shape = "Observations") +
  
  # Legend Guides
  guides(color = guide_legend(order = 1), linetype = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  
  # 3-Day PMF Inflow Volume
  geom_hline(yintercept = pmf_3day, color = "#BB0021FF", alpha = 0.95,linetype = "dashed")+
  annotate("text", x = qnorm(1-label_aep),
           y = pmf_3day,
           label = "3-Day PMF Volume: 42,700 cfs", 
           size = 2.75, fontface = "bold.italic", vjust = -0.75, hjust = 0, color = "#BB0021FF")+
  
  annotate("label", x = qnorm(1-label_aep), y = 2e4,
           label = prv_label,
           size = 2.75, fontface = "bold.italic", vjust = 1, hjust = 0,
           fill = "beige")+
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
        plot.subtitle = element_text(size = 9, hjust = 0.5))

# Save -------------------------------------------------------------------------
previous_volfreq

ggsave(file.path(plotdir,"Previous_Inflow_Vol_Frequency.png"), previous_volfreq, height = 6, width = 8, dpi = 500)

# Comparison - GT Table -------------------------------------------
vol_GT <- vfc_pmf_aep %>% 
  select(-c(nice_label,PMF_3day)) %>% 
  gt() %>% 
  tab_header(
    title = md("**Cougar Dam 3-Day Volume Frequency**"),
    subtitle = "Parameters from 2025 and 2018 Inflow-Volume Frequency Estimates"
  ) %>%
  cols_label(
    Analysis = "Name",
    Mean_log  = "Mean (of log)",
    SD_log  = "SD (of log)",
    Skew_log = "Skew (of log)",
    ERL = "ERL",
    OOM_Shift = "Order of Magnitude Shift from 2018",
    AEP_PMF = "3-Day PMF AEP"
  ) %>%
  fmt_number(
    columns = OOM_Shift,
    decimals = 2,
    use_seps = T
  ) %>%
  fmt_number(
    columns = c(Mean_log, SD_log,Skew_log),
    decimals = 4,
    use_seps = T
  ) %>%
  fmt_scientific(
    columns = AEP_PMF,
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

vol_GT

# SAVE -------------------------------------------------------------------------
outfile <- file.path(getwd(),"outputs","Volume_AEPs.html")
gtsave(vol_GT, outfile)

# RECORD LENGTH EXTENSION ------------------------------------------------------
# Join with Sensitivity Field --------------------------------------------------
rec_len <- vfcs %>% 
  left_join(bestfit_sum, join_by(Analysis == analysis)) %>% 
  filter(sensitivity == "Record Length")

# Colors
reclen_plot_colors <- c("Posterior Predictive" = "#3B4992FF",
                     "90% Credible Intervals" = "#3B4992FF",
                     "3-Day Winter Syst" = "#D51317FF",
                     "3-Day Winter Syst + Hist" = "#F39200FF",
                     "3-Day Winter Syst + Hist + Paleo (w/PSIs)" = "#EFD500FF",
                     "3-Day Winter Syst + Hist + PF" = "#95C11FFF",
                     "3-Day Winter Syst + Hist + Paleo (no PSIs)" = "#007B3DFF")

reclen_plot_colors_brks <- names(reclen_plot_colors)

# linetypes
reclen_plot_linetype <- c("Posterior Predictive" = "solid",
                       "90% Credible Intervals" = "dashed",
                       "3-Day Winter Syst" = "solid",
                       "3-Day Winter Syst + Hist" = "solid",
                       "3-Day Winter Syst + Hist + Paleo (w/PSIs)" = "solid",
                       "3-Day Winter Syst + Hist + PF" = "solid",
                       "3-Day Winter Syst + Hist + Paleo (no PSIs)" = "solid")

reclen_plot_linetype_brks <- names(prv_plot_linetype)

# Labels ---
erl_table <- rec_len %>% 
  group_by(nice_name) %>% 
  summarise(ERL = unique(erl)) %>% 
  ungroup() %>% 
  bind_rows(bestfit_sum %>% 
              filter(sensitivity == "Final") %>% 
              summarise(nice_name = nice_name,
                        ERL = unique(erl))) %>% 
  mutate(nice_label = paste0(nice_name, " ERL: ",ERL, " yrs"))


erl_label <- paste(erl_table$nice_label, collapse = "\n")

# PLOT
reclen_volfreq <- ggplot() +
  geom_ribbon(data = final_2025, 
              aes(x = Z_aep, ymin =  CI_5, ymax = CI_95), fill = final_fill, alpha = 0.18) +
  # record length posterior
  geom_line(data = rec_len, 
            aes(x = Z_aep,
                y = Posterior_Predictive, 
                color = nice_name), linewidth = 0.725) +
  # 2025 posterior
  geom_line(data = final_2025, 
            aes(x = Z_aep,
                y = Posterior_Predictive, 
                color = "Posterior Predictive"), linewidth = 0.85) +
  scale_color_manual(values = reclen_plot_colors, breaks = reclen_plot_colors_brks) +
  
  # Observed Inflow points
  geom_point(data = bf_input %>% filter(Datatype == "Point"),
             aes(x = Z_aep, y = flow, shape = "Systematic POR (1927-2021)"), size = 1.25, alpha = 0.7) +
  # Historic Interval
  geom_errorbar(data = bf_input %>% filter(Datatype == "Interval"),
                aes(x = Z_aep, ymin = lower_flow, ymax = upper_flow), width = 0.05, color = "black") +
  geom_point(data = bf_input %>% filter(Datatype == "Interval"),
             aes(x = Z_aep, y = flow, shape = "Historic Event"), size = 1.25, alpha = 0.9, fill = "lightblue2",color = "black") +
  # PR Priors
  geom_errorbar(data = pf_priors,
                aes(x = Z_aep, ymin = Lower, ymax = Upper), width = 0.05, color = "black") +
  geom_point(data = pf_priors,
             aes(x = Z_aep, y = Flow, shape = "Precip-Freq Quantile Prior"), size = 1.25, alpha = 0.9, fill = "#991A00FF",color = "black") +
  # Low-Outlier
  geom_point(data = bf_input %>% filter(Datatype == "Low-Outlier"),
             aes(x = Z_aep, y = flow, shape = "Low-Outlier"), size = 1.25, alpha = 0.9,color = "#749B58FF") +
  # Point Legend
  scale_shape_manual(values = pt_shp,breaks = pt_shp_brks) +
  # Scales
  scale_x_continuous(breaks = z_breaks, minor_breaks = z_breaks_minor, labels = aep_breaks) +
  scale_y_log10(breaks = scales::log_breaks(n = 5), minor_breaks = scales::minor_breaks_log(detail = 1), labels = scales::comma) +
  
  # Labs
  labs(x = "Annual Exceedance Probability (AEP)",
       y = "3-Day Inflow-Volume",
       title = "Cougar Dam - IES 3-Day Volume Frequency Curve",
       linetype = "Curve",
       color = "Curve",
       shape = "Observations") +
  
  # Legend Guides
  guides(color = guide_legend(order = 1), linetype = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  
  # 3-Day PMF Inflow Volume
  geom_hline(yintercept = pmf_3day, color = "#BB0021FF", alpha = 0.95,linetype = "dashed")+
  annotate("text", x = qnorm(1-label_aep),
           y = pmf_3day,
           label = "3-Day PMF Volume: 42,700 cfs", 
           size = 2.75, fontface = "bold.italic", vjust = -0.75, hjust = 0, color = "#BB0021FF")+
  
  annotate("label", x = qnorm(1-.9), y = 1e3,
           label = erl_label,
           size = 2.75, fontface = "bold.italic", vjust = 1, hjust = 0,
           fill = "beige")+
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
        plot.subtitle = element_text(size = 9, hjust = 0.5))+
  coord_cartesian(ylim = c(100,2e5))
# SAVE -------------------------------------------------------------------------
reclen_volfreq

ggsave(file.path(plotdir,"RecordLength_Inflow_Vol_Frequency.png"), reclen_volfreq, height = 6, width = 8, dpi = 500)

# Comparison GT Table ----------------------------------------------------------
vfc_summary <- bestfit_sum %>% 
  select(c(nice_name,sensitivity,mean_of_log, sd_of_log, skew_of_log, erl)) %>% 
  gt() %>% 
  tab_header(
    title = md("**Cougar Dam 3-Day Volume Frequency**"),
    subtitle = "Parameters from 2025 Inflow-Volume Frequency Estimates"
  ) %>%
  cols_label(
    nice_name = "Analysis",
    sensitivity = "Sensitivity",
    mean_of_log  = "Mean (of log)",
    sd_of_log  = "SD (of log)",
    skew_of_log = "Skew (of log)",
    erl = "ERL",
    # OOM_Shift = "Order of Magnitude Shift from 2018",
    # AEP_PMF = "3-Day PMF AEP"
  ) %>%
  # fmt_number(
  #   columns = OOM_Shift,
  #   decimals = 2,
  #   use_seps = T
  # ) %>%
  fmt_number(
    columns = c(mean_of_log, sd_of_log, skew_of_log),
    decimals = 4,
    use_seps = T
  ) %>%
  # fmt_scientific(
  #   columns = AEP_PMF,
  #   decimals = 2,
  #   exp_style = "E"
  # ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  cols_align(
    align = "left",
    columns = nice_name
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

vfc_summary
# SAVE -------------------------------------------------------------------------
outfile <- file.path(getwd(),"outputs","VFC_Summary.html")
gtsave(vfc_summary, outfile)
