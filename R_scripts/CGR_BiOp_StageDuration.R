# Cougar BiOp Stage-Duration Analysis
# ---- Package Management ----
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  tidyverse, ggplot2, scales, ggh4x, ggrepel,patchwork,fs, 
  ggsci, ggtext, glue, cli, assertthat, future, furrr, 
  memoise,lubridate, purrr,zoo,janitor)

# Theme and plot directory
theme_set(theme_bw())
plotdir <- file.path(getwd(),"outputs","Figures","Stage_Duration","BiOp")
dir_create(plotdir)

# Read Data --------------------------------------------------------------------
# POR
stage_dur <- dir_ls("data/Cougar/",glob = "*CGR_Stage_Duration_BiOp.csv*",recurse = T) %>%
  read_csv() %>% 
  janitor::clean_names()

# Stage Duration Curves --------------------------------------------------------
por_colors <- c("Observed" = "#3B4992FF", 
                "Observed - Truncated" = "#3B4992FF",
                "ResSim - Near Term" = "#EE0000FF",
                "ResSim - Long Term" = "#008B45FF")

por_lines <- c("Observed" = "solid", 
               "Observed - Truncated" = "dashed",
               "ResSim - Near Term" = "solid",
               "ResSim - Long Term" = "solid")

annual_stage_dur <- ggplot(stage_dur, aes(x = percent_time_exceedance, y = elev_ft_88))+
  geom_line(aes(color = operations, linetype = operations),linewidth = 0.85) +
  scale_color_manual(values = por_colors, breaks = names(por_colors)) + 
  scale_linetype_manual(values = por_lines, breaks = names(por_lines)) + 
  scale_y_continuous(breaks = seq(1200,1800,50),labels = scales::comma) +
  scale_x_continuous(breaks = seq(0,100,10),labels = scales::comma) +
  labs(x = "Percent of Time Exceeded",
       y = "Elevation (ft-NAVD88)",
       color = "POR",
       linetype = "POR") +
  theme(legend.position = "bottom")

# Save
ggsave(file.path(plotdir,"Annual_Stage_Duration.png"), annual_stage_dur, height = 6, width = 8, dpi = 500)

# Stage Duration Comparison by Month -------------------------------------------
monthly_stage_dur_longterm <- dir_ls("data/Cougar/",glob = "*AT6_Stage_Duration_biop.csv",recurse = T) %>%
  read_csv()

monthly_stage_dur_shortterm <- dir_ls("data/Cougar/",glob = "*NTOM_Stage_Duration_biop.csv",recurse = T) %>%
  read_csv()

monthly_stage_dur_portrun <- dir_ls("data/Cougar/",glob = "*Stage_Duration.csv",recurse = T) %>%
  read_csv()

monthly_stage_dur_porfull <- dir_ls("data/Cougar/",glob = "*Stage_Duration_FullPOR.csv",recurse = T) %>%
  read_csv()

# Pivot Longer -----------------------------------------------------------------
monthly_portrun_long <- monthly_stage_dur_portrun %>% 
  pivot_longer(cols = -Probability, names_to = "Month", values_to = "Elev") %>% 
  mutate(Operations = "Observed - Truncated")

monthly_porfull_long <- monthly_stage_dur_porfull %>% 
  pivot_longer(cols = -Probability, names_to = "Month", values_to = "Elev") %>% 
  mutate(Operations = "Observed")

monthly_shortterm_long <- monthly_stage_dur_shortterm %>% 
  pivot_longer(cols = -Probability, names_to = "Month", values_to = "Elev") %>% 
  mutate(Operations = "ResSim - Near Term")

monthly_longterm_long <- monthly_stage_dur_longterm %>% 
  pivot_longer(cols = -Probability, names_to = "Month", values_to = "Elev") %>% 
  mutate(Operations = "ResSim - Long Term")

monthy_stage_dur <- bind_rows(monthly_portrun_long, 
                              monthly_porfull_long, 
                              monthly_shortterm_long, 
                              monthly_longterm_long)

# Use month.name base vector
monthy_stage_dur <- monthy_stage_dur %>% 
  mutate(Month = factor(Month, levels = month.name)) %>% 
  mutate(percent_time_exceeded = Probability * 100)

# Facet Plot by Month ----------------------------------------------------------
stage_dur_monthly <- ggplot(monthy_stage_dur, aes(x = percent_time_exceeded, y = Elev)) +
  geom_line(aes(color = Operations, linetype = Operations),linewidth = 0.85) +
  scale_color_manual(values = por_colors, breaks = names(por_colors)) + 
  scale_linetype_manual(values = por_lines, breaks = names(por_lines)) + 
  scale_y_continuous(breaks = seq(1200,1800,50),labels = scales::comma) +
  scale_x_continuous(breaks = seq(0,100,25),labels = scales::comma) +
  labs(x = "Percent of Time Exceeded",
       y = "Elevation (ft-NAVD88)",
       color = "POR",
       linetype = "POR") +
  facet_wrap(vars(Month)) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(size = 6),axis.text.y = element_text(size = 6))

# Save
ggsave(file.path(plotdir,"Monthly_Stage_Duration.png"), stage_dur_monthly, height = 6.5, width = 9, dpi = 500)
