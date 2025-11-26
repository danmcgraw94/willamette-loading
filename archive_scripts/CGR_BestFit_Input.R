# Cougar BestFit Inputs - Mostly a check process
# RMC Flood Hazards - Dan McGraw
today = format(Sys.Date(),"%d-%b-%Y")

# ---- Package Management ----
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse, ggplot2, scales, ggh4x, ggrepel,patchwork,fs, 
  ggsci, ggtext, glue, cli, assertthat, future, furrr, memoise)


vida_peak_paleo <- tibble(
  ID = c("NEB", "PSI-1","PSI-2","PSI-3","PSI-4","PSI-5"),
  Q_Lower = c(180000,120000,110000,80000,70000,70000),
  Q_Best = c(290000,220000,180000,135000,120000,90000),
  Q_Upper = c(420000,320000,260000,200000,180000,115000))

# Relationships on excel sheet
vida_peaktovol <- function(peakQ){
  vol_Q <- (peakQ*0.6262) + 1414.3
  return(vol_Q)
}

vida_vol_toCGR <- function(vida_vol){
  cgr_vol <- (vida_vol*0.334) -1491.4
  return(cgr_vol)
}

vida_peak_toCGRvol <-  function(peakQ){
  cgr_vol <- (vida_peaktovol(peakQ)*0.334) -1491.4
  return(cgr_vol)
}

cgrvol_to_vidapeak <- function(cgr_vol){
  vida_peak <- (((cgr_vol+1491.4)/0.334) - 1414.3)/0.6262
  return(vida_peak)
}


# Paleo Flood as Perception Thresholds -----------------------------------------
cgr_paleo <- dir_ls("data/Cougar/",glob = "*BestFit_paleo*",recurse = T) %>% read_csv()

# Peak to Volume Transform Figures ---------------------------------------------
vida_peak_3day <- dir_ls("data/Cougar/",glob = "*Vida_peak*",recurse = T) %>% read_csv()
vida_3day_CGR_3day <- dir_ls("data/Cougar/",glob = "*Vida_3day*",recurse = T) %>% read_csv()


# Linear Regression ------------------------------------------------------------
vida_ptv <- lm(vida_peak_3day$Vida_3day ~ vida_peak_3day$Vida_peak)
CGR_vtv <- lm(vida_3day_CGR_3day$CGR_3day ~ vida_3day_CGR_3day$Vida_3day)

# LM equation for plot
lm_eqn <- function(df, y, x) {
  model <- lm(y ~ x, data = df)
  
  eq <- substitute(
    italic(y) == b*italic(x) + a * "," ~ ~italic(R)^2 == r2,
    list(
      a   = format(unname(coef(model)[1]), digits = 2),
      b   = format(unname(coef(model)[2]), digits = 2),
      r2  = format(summary(model)$r.squared, digits = 3)))
  
  as.character(as.expression(eq))
}

eq_label <- lm_eqn(vida_peak_3day,vida_peak_3day$Vida_3day,vida_peak_3day$Vida_peak)
print(eq_label)

plotdir <- "/outputs/Figures/"

# Plot Vida Peak to 3-day -------------------------------------------------------
# Create a sequence of peak flows for smooth prediction curves
peak_seq <- tibble(
  PeakQ = seq(min(vida_peak_3day$Vida_peak), 
              max(vida_peak_3day$Vida_peak), 
              length.out = 102))

PI_3day <- predict(vida_ptv, newdata = peak_seq, 
                           interval = "prediction", level = 0.95)

CI_3day <- predict(vida_ptv, newdata = peak_seq, 
                   interval = "confidence", level = 0.95)

vida_peak_intervals <- peak_seq %>%
  mutate(
    fit = PI_3day[, "fit"],
    pred_lwr = PI_3day[, "lwr"],
    pred_upr = PI_3day[, "upr"],
    conf_lwr = CI_3day[, "lwr"],
    conf_upr = CI_3day[, "upr"])


ggplot(vida_peak_3day) + 
  # Prediction interval (wider)
  geom_ribbon(data = vida_peak_intervals, 
              aes(x = PeakQ, ymin = pred_lwr, ymax = pred_upr),
              fill = "#46ACC8", alpha = 0.15) +
  # Confidence interval (narrower)
  geom_ribbon(data = vida_peak_intervals,
              aes(x = PeakQ, ymin = conf_lwr, ymax = conf_upr),
              fill = "#46ACC8", alpha = 0.3) +
  geom_point(aes(x = Vida_peak, y = Vida_3day),size = 1) +
  scale_x_continuous(breaks = seq(0,100000,20000),minor_breaks = seq(0,100000,5000),labels = scales::label_comma()) + 
  scale_y_continuous(breaks = seq(0,100000,20000),minor_breaks = seq(0,100000,5000),labels = scales::label_comma()) +
  coord_cartesian(xlim = c(0,100000),ylim = c(0,100000)) +
  geom_smooth(method = "lm",aes(x = Vida_peak, y = Vida_3day),se = F,linewidth = 0.7)+
  labs(x = "Peak Flow at Vida USGS Gage (cfs)", y = "3-Day Volume at Vida USGS Gage (cfs)")+
  annotate(geom = "text",
           x = 60000,
           y = 50000,
           label = eq_label,
           parse = T,
           hjust = 0.5,
           vjust = 0,
           size = 3)

ggsave(paste0(plotdir,"Vida_PVT_",today,".png"), height = 5, width = 7, dpi=300)

# Plot Vida 3-day to CGR 3-day
eq_label_cgr <- lm_eqn(vida_3day_CGR_3day_clip,vida_3day_CGR_3day_clip$CGR_3day,vida_3day_CGR_3day_clip$Vida_3day)
print(eq_label_cgr)

# hardcode
eq_label_cgr <- "italic(y) == \"0.334\" * italic(x) - \"1491.4\" * \",\" ~ ~italic(R)^2 == \"0.9621\""

ggplot(vida_3day_CGR_3day_clip) + 
  geom_point(aes(x = Vida_3day, y = CGR_3day),size = 1) +
  scale_x_continuous(breaks = seq(0,70000,10000),minor_breaks = seq(0,70000,5000),labels = scales::label_comma()) + 
  scale_y_continuous(breaks = seq(0,30000,5000),minor_breaks = seq(0,30000,1000),labels = scales::label_comma()) +
  coord_cartesian(xlim = c(0,70000),ylim = c(0,30000)) +
  geom_smooth(method = "lm",aes(x = Vida_3day, y = CGR_3day),se = F,linewidth = 0.7)+
  labs(x = "3-Day Volume at Vida USGS Gage (cfs)", y = "3-Day Volume at Cougar Dam (cfs)")+
  annotate(geom = "text",
           x = 38000,
           y = 15000,
           label = eq_label_cgr,
           parse = T,
           hjust = 0.5,
           vjust = 0,
           size = 3)

ggsave(paste0(plotdir,"CGR_VT_",today,".png"), height = 5, width = 7, dpi=300)

vida_plot <- ggplot(vida_peak_3day) + 
  geom_point(aes(x = Vida_peak, y = Vida_3day),size = 1) +
  scale_x_continuous(breaks = seq(0,100000,20000),minor_breaks = seq(0,100000,5000),labels = scales::label_comma()) + 
  scale_y_continuous(breaks = seq(0,100000,20000),minor_breaks = seq(0,100000,5000),labels = scales::label_comma()) +
  coord_cartesian(xlim = c(0,100000),ylim = c(0,100000)) +
  geom_smooth(method = "lm",aes(x = Vida_peak, y = Vida_3day),se = F,linewidth = 0.7)+
  labs(x = "Peak Flow at Vida USGS Gage (cfs)", y = "3-Day Volume at Vida USGS Gage (cfs)")+
  annotate(geom = "text",
           x = 60000,
           y = 50000,
           label = eq_label,
           parse = T,
           hjust = 0.5,
           vjust = 0,
           size = 3)

cgr_plot <- ggplot(vida_3day_CGR_3day_clip) + 
  geom_point(aes(x = Vida_3day, y = CGR_3day),size = 1) +
  scale_x_continuous(breaks = seq(0,70000,10000),minor_breaks = seq(0,70000,5000),labels = scales::label_comma()) + 
  scale_y_continuous(breaks = seq(0,30000,5000),minor_breaks = seq(0,30000,1000),labels = scales::label_comma()) +
  coord_cartesian(xlim = c(0,70000),ylim = c(0,30000)) +
  geom_smooth(method = "lm",aes(x = Vida_3day, y = CGR_3day),se = F,linewidth = 0.7)+
  labs(x = "3-Day Volume at Vida USGS Gage (cfs)", y = "3-Day Volume at Cougar Dam (cfs)")+
  annotate(geom = "text",
           x = 38000,
           y = 15000,
           label = eq_label_cgr,
           parse = T,
           hjust = 0.5,
           vjust = 0,
           size = 3)

library(patchwork)
volume_transforms <- vida_plot + cgr_plot
ggsave(paste0(plotdir,"Both_VT_",today,".png"),volume_transforms, height = 5, width = 7, dpi=300)
