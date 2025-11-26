# Updated Input data to make this easier
# Need to work the knots out to better streamline this for automation

# ---- Package Management ----
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse, ggplot2, scales, ggh4x, ggrepel,patchwork,fs, 
  ggsci, ggtext, glue, cli, assertthat, future, furrr, 
  memoise,lubridate, purrr,zoo,
  tcltk,grid,ggrepel,paletteer)

theme_set(theme_light())
plotdir <- "/outputs/Figures/Volume_Frequency/"

# Read Data --------------------------------------------------------------------
bf_input <- dir_ls("data/Cougar/",glob = "*CGR_3day_Input_Paleo_BB_nopsi.csv*",recurse = T) %>%
  read_csv()

#pal_aaas("default")(10)
#"#3B4992FF" "#EE0000FF" "#008B45FF" "#631879FF" "#008280FF" "#BB0021FF" "#5F559BFF" "#A20056FF" "#808180FF" "#1B1919FF"

# ECDF
bestfit_ecdf <- bf_input %>% filter(Datatype != "Threshold") %>% 
  ggplot() + 
  geom_errorbar(aes(x = plotting_position, ymin = lower_flow ,ymax = upper_flow),width = 0.03,color = "black")+
  geom_point(aes(x = plotting_position, y = flow, fill = Dataseries),shape = 21,size = 2,alpha = 0.75) +
  scale_x_continuous(trans = scales::trans_new("reverse_log", transform = function(x) -log10(x),inverse = function(x) 10^(-x)),
                     breaks = scales::log_breaks(n = 10, base =10),
                     minor_breaks = scales::minor_breaks_log(),
                     labels = scales::label_number(accuracy = 0.001)) +
  scale_y_log10(breaks = scales::log_breaks(),
                minor_breaks = scales::minor_breaks_log(),
                labels = scales::comma) + 
  scale_fill_manual(values = c("Historic" = "lightblue2", "Systematic POR" = "grey30"))+
  labs(y = "3-Day Inflow Volume", x = "Empirical AEP", color = NULL)+
  theme(legend.position = "bottom")

ggsave(paste0(getwd(), plotdir, "Empirical_AEP_3day.png"), 
       bestfit_ecdf, height = 5, width = 7, dpi = 500)

# NEB Labels
year_midpoint <- function(start_year,end_year){
  midyear = (end_year + start_year)/2
}

bf_input <- bf_input %>% mutate(DataLabel = ifelse(Datatype == "Threshold" & end_year <=1861, paste0("NEB: ", scales::comma(flow)," cfs"),""),
                                Paleo_midpoint = ifelse(Datatype == "Threshold"& end_year <=1861,year_midpoint(start_year,end_year),NA))

# remove the 5 year one:
bf_input$DataLabel[bf_input$flow == 37000] <- ""
bf_input$Paleo_midpoint[bf_input$flow == 37000] <- NA

#  -----------------------------------------------------------------------------
# PALEO ERA --------------------------------------------------------------------
#  -----------------------------------------------------------------------------
paleo_era <- ggplot(bf_input) + 
  # NEB Filled blocks
  geom_rect(aes(xmin = start_year, xmax = end_year, ymin = 0, ymax = flow),
            fill = "lightcoral", alpha = 0.5) + 
  # Top of NEB
  geom_segment(aes(x = start_year, xend = end_year, y = flow, yend = flow), 
               color = "lightcoral") +
  
  # Historic intervals (filter and plot separately)
  geom_errorbar(data = bf_input %>% filter(Dataseries == "Historic"), 
                aes(x = year, ymin = lower_flow, ymax = upper_flow), 
                width = 5, lineend = "butt", color = "black") + 
  geom_point(data = bf_input %>% filter(Dataseries == "Historic"),
             aes(x = year, y = flow), 
             size = 1.5, shape = 21, fill = "lightblue2", color = "black", stroke = 0.8) +
  
  # Systematic POR (filter and plot separately)
  geom_point(data = bf_input %>% filter(Dataseries == "Systematic POR"),
             aes(x = year, y = flow), 
             size = 1.5, shape = 21, fill = "grey30", color = "black", stroke = 0.8) +
  
  # Manual legend using dummy data
  geom_rect(data = data.frame(x = 1), aes(xmin = -Inf, xmax = -Inf, ymin = -Inf, ymax = -Inf, fill = "Paleo NEB"), 
            alpha = 0.5) +
  geom_point(data = data.frame(x = 1), aes(x = -10000, y = -10000, fill = "Historic Intervals", color = "Historic Intervals"),
             size = 2.5, shape = 21, stroke = 0.8) +
  geom_point(data = data.frame(x = 1), aes(x = -10000, y = -10000, fill = "Systematic POR 1927-2021", color = "Systematic POR 1927-2021"),
             size = 2.5, shape = 21, stroke = 0.8) +
  # Manual scales
  scale_fill_manual(name = NULL, values = c("Paleo NEB" = "lightcoral",
                                            "Historic Intervals" = "lightblue2",
                                            "Systematic POR 1927-2021" = "grey30"),
                    breaks = c("Paleo NEB", "Historic Intervals", "Systematic POR 1927-2021")) +
  scale_color_manual( name = NULL, values = c("Historic Intervals" = "black",
                                              "Systematic POR 1927-2021" = "black"),
                      breaks = c("Historic Intervals", "Systematic POR 1927-2021"), 
                      guide = "none") +
  # legend guide
  guides(fill = guide_legend(override.aes = list(shape = c(22, 21, 21),
                                                 size = c(4, 3, 3),
                                                 alpha = c(0.5, 1, 1),
                                                 color = c("lightcoral", "black", "black"),
                                                 stroke = c(0.5, 0.8, 0.8)))) +
  # NEB Labels
  # annotate("text", x = bf_input$Paleo_midpoint, y = bf_input$flow - 10000, 
  #          label = bf_input$DataLabel, size = 3) + 
  # Axes
  scale_y_continuous(breaks = seq(0, 70000, 10000), 
                     minor_breaks = seq(0, 70000, 2500), 
                     labels = scales::comma) +
  scale_x_continuous(breaks = c(seq(-500, 3000, 200), -300), 
                     minor_breaks = seq(-500, 3000, 100)) +
  # Axis label
  labs(x = "Year", y = "3-Day Inflow Volume (cfs)") +
  theme_bw() +
  theme(legend.position = "inside",
        legend.justification = c(1, 1),
        legend.position.inside = c(0.95,0.95),
        legend.background = element_rect(fill = "white", color = "grey50", linewidth = 0.5),
        legend.key = element_rect(fill = "white"),
        legend.spacing.y = unit(2, "pt"))+ 
  # theme(legend.position = "none") +  # <-- REMOVE LEGEND
  # Plot Window
  coord_cartesian(xlim = c(-350, 1840), ylim = c(0, 65000), expand = FALSE)

#  -----------------------------------------------------------------------------
# HISTORIC ERA -----------------------------------------------------------------
#  -----------------------------------------------------------------------------
hist_era <- ggplot(bf_input) + 
  # NEB Filled blocks
  geom_rect(aes(xmin = start_year, xmax = end_year, ymin = 0, ymax = flow),
            fill = "lightcoral", alpha = 0.5) + 
  # Top of NEB
  geom_segment(aes(x = start_year, xend = end_year, y = flow, yend = flow), 
               color = "lightcoral") +
  
  # Historic intervals (filter and plot separately)
  geom_errorbar(data = bf_input %>% filter(Dataseries == "Historic"), 
                aes(x = year, ymin = lower_flow, ymax = upper_flow), 
                width = 1, lineend = "butt", color = "black") + 
  geom_point(data = bf_input %>% filter(Dataseries == "Historic"),
             aes(x = year, y = flow), 
             size = 1.5, shape = 21, fill = "lightblue2", color = "black", stroke = 0.8) +
  
  # Systematic POR (filter and plot separately)
  geom_point(data = bf_input %>% filter(Dataseries == "Systematic POR"),
             aes(x = year, y = flow), 
             size = 1.5, shape = 21, fill = "grey30", color = "black", stroke = 0.8) +
  
  # Manual legend using dummy data
  geom_rect(data = data.frame(x = 1), aes(xmin = -Inf, xmax = -Inf, ymin = -Inf, ymax = -Inf, fill = "Paleo NEB"), 
            alpha = 0.5) +
  geom_point(data = data.frame(x = 1), aes(x = -10000, y = -10000, fill = "Historic Intervals", color = "Historic Intervals"),
             size = 2.5, shape = 21, stroke = 0.8) +
  geom_point(data = data.frame(x = 1), aes(x = -10000, y = -10000, fill = "Systematic POR 1927-2021", color = "Systematic POR 1927-2021"),
             size = 2.5, shape = 21, stroke = 0.8) +
  # Manual scales
  scale_fill_manual(name = NULL, values = c("Paleo NEB" = "lightcoral",
                                            "Historic Intervals" = "lightblue2",
                                            "Systematic POR 1927-2021" = "grey30"),
                    breaks = c("Paleo NEB", "Historic Intervals", "Systematic POR 1927-2021")) +
  scale_color_manual( name = NULL, values = c("Historic Intervals" = "black",
                                              "Systematic POR 1927-2021" = "black"),
                      breaks = c("Historic Intervals", "Systematic POR 1927-2021"), 
                      guide = "none") +
  # legend guide
  guides(fill = guide_legend(override.aes = list(shape = c(22, 21, 21),
                                                 size = c(4, 3, 3),
                                                 alpha = c(0.5, 1, 1),
                                                 color = c("lightcoral", "black", "black"),
                                                 stroke = c(0.5, 0.8, 0.8)))) +
  # NEB Labels
  # annotate("text", x = bf_input$Paleo_midpoint, y = bf_input$flow - 10000, 
  #          label = bf_input$DataLabel, size = 3) + 
  # Axes
  scale_y_continuous(breaks = seq(0, 70000, 10000), 
                     minor_breaks = seq(0, 70000, 2500), 
                     labels = scales::comma) +
  scale_x_continuous(breaks = c(seq(-500, 3000, 10)), 
                     minor_breaks = seq(-500, 3000, 5)) +
  # Axis label
  labs(x = "Year", y = "3-Day Inflow Volume (cfs)") +
  theme_bw() +
  theme(legend.position = "inside",
        legend.justification = c(1, 1),
        legend.position.inside = c(0.95,0.95),
        legend.background = element_rect(fill = "white", color = "grey50", linewidth = 0.5),
        legend.key = element_rect(fill = "white"),
        legend.spacing.y = unit(2, "pt"))+ 
  # Plot Window
  coord_cartesian(xlim = c(1855, 2025), ylim = c(0, 65000), expand = FALSE)

bestfit_timeseries <- 
  (paleo_era + theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))) +
  (hist_era + theme(axis.title.y = element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.text.y = element_blank()))

print(bestfit_timeseries)

ggsave(paste0(getwd(), plotdir, "Paleo_3day_timeseries.png"), 
       bestfit_timeseries, height = 6, width = 10, dpi = 500)

                 