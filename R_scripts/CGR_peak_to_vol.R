# Cougar Peak to Volume Transformation -----------------------------------------
# Vida Gage - Peak to 3-day
# Vida 3-day to Cougar 3-day
# Functions used primarily as a check

# RMC Flood Hazards - Dan McGraw
today = format(Sys.Date(),"%d-%b-%Y")

# ---- Package Management ----
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse, ggplot2, scales, ggh4x, ggrepel,patchwork,fs, 
  ggsci, ggtext, glue, cli, assertthat, future, furrr, memoise)

# Hardcoded Paleo data
vida_peak_paleo <- tibble(
  ID = c("NEB", "PSI-1","PSI-2","PSI-3","PSI-4","PSI-5"),
  Q_Lower = c(180000,120000,110000,80000,70000,70000),
  Q_Best = c(290000,220000,180000,135000,120000,90000),
  Q_Upper = c(420000,320000,260000,200000,180000,115000))

# Relationships from excel sheet
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
vida_ptv <- lm(Vida_3day ~ Vida_peak, data = vida_peak_3day)
CGR_vtv <- lm(CGR_3day ~ Vida_3day, data = vida_3day_CGR_3day)

# LM equation for regression figures ----
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

# Function
eq_vida <- lm_eqn(vida_peak_3day,vida_peak_3day$Vida_3day,vida_peak_3day$Vida_peak)
eq_cgr <- lm_eqn(vida_3day_CGR_3day,vida_3day_CGR_3day$CGR_3day,vida_3day_CGR_3day$Vida_3day)

# Hardcode - using cougar eq from excel sheet
eq_vida <- "atop(italic(y) == \"0.63\" * italic(x) + \"1414\", 
                  phantom() ~ italic(R)^2 == \"0.908\")"
eq_cgr <- "atop(italic(y) == \"0.334\" * italic(x) -\"1491.4\", 
                 phantom() ~ italic(R)^2 == \"0.9621\")"

print(eq_vida)
print(eq_cgr)

# Plot dir ----
plotdir <- "/outputs/Figures/Peak_to_Vol/"

# Common Theme ----
common_theme <- theme_bw() +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "white", color = "black"),
        plot.title = element_text(face = "bold", size = 10),
        plot.subtitle = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 10))

# Prediction and Confidence Intervals ------------------------------------------
# Create a sequence of peak flows for smooth prediction curves
peak_seq <- tibble(Vida_peak = seq(min(vida_peak_3day$Vida_peak),
                                   max(vida_peak_3day$Vida_peak),
                                   length.out = 200))

PI_3day <- predict(vida_ptv,
                   newdata = peak_seq, 
                   interval = "prediction",
                   level = 0.95)

CI_3day <- predict(vida_ptv,
                   newdata = peak_seq, 
                   interval = "confidence",
                   level = 0.95)

vida_peak_intervals <- peak_seq %>%
  mutate(fit = PI_3day[, "fit"],
         pred_lwr = PI_3day[, "lwr"],
         pred_upr = PI_3day[, "upr"],
         conf_lwr = CI_3day[, "lwr"],
         conf_upr = CI_3day[, "upr"])

# Create a sequence of 3-day flows for smooth prediction curves
vol_seq <- tibble(Vida_3day = seq(min(vida_3day_CGR_3day$Vida_3day),
                                  max(vida_3day_CGR_3day$Vida_3day),
                                  length.out = 200))

PI_cgr <- predict(CGR_vtv,
                  newdata = vol_seq,
                  interval = "prediction",
                  level = 0.95)

CI_cgr <- predict(CGR_vtv,
                  newdata = vol_seq, 
                  interval = "confidence",
                  level = 0.95)

cgr_intervals <- vol_seq %>%
  mutate(fit = PI_cgr[, "fit"],
         pred_lwr = PI_cgr[, "lwr"],
         pred_upr = PI_cgr[, "upr"],
         conf_lwr = CI_cgr[, "lwr"],
         conf_upr = CI_cgr[, "upr"])

# Plot Vida Peak to 3-day -------------------------------------------------------
vida_3day <- ggplot(vida_peak_3day) + 
  common_theme +
  # 1:1 line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  # Prediction interval (wider)
  geom_ribbon(data = vida_peak_intervals, 
              aes(x = Vida_peak, ymin = pred_lwr, ymax = pred_upr),
              fill = "#46ACC8", alpha = 0.15) +
  # Confidence interval (narrower)
  geom_ribbon(data = vida_peak_intervals,
              aes(x = Vida_peak, ymin = conf_lwr, ymax = conf_upr),
              fill = "#46ACC8", alpha = 0.3) +
  # Fitted line
  geom_line(data = vida_peak_intervals, aes(x = Vida_peak, y = fit),
            color = "#46ACC8", linewidth = 1) +
  # Data points
  geom_point(aes(x = Vida_peak, y = Vida_3day),
             size = 1.5, alpha = 0.7, shape = 16) +
  labs(title = "Vida USGS Gage - Peak to 3-Day Regression",
       subtitle = "95% confidence and prediction intervals",
       x = "Peak Flow (cfs)",
       y = "3-Day Volume (cfs)")+
  scale_x_continuous(breaks = seq(0,100000,20000),minor_breaks = seq(0,100000,5000),labels = scales::label_comma()) + 
  scale_y_continuous(breaks = seq(0,100000,20000),minor_breaks = seq(0,100000,5000),labels = scales::label_comma()) +
  coord_cartesian(xlim = c(0,100000),ylim = c(0,70000),expand = F) +
  annotate("text",
           x = 70000,
           y = 30000, 
           label = eq_vida,
           parse = TRUE,
           hjust = 0.5,
           vjust = 0.5,
           size = 3)

ggsave(paste0(getwd(),plotdir,"Vida_Peak_3day.png"), vida_3day, height = 5, width = 7, dpi=300)

# Plot Vida 3-day to CGR 3-day -------------------------------------------------
cgr_3day <- ggplot(vida_3day_CGR_3day) + 
  common_theme +
  # 1:1 line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  # Prediction interval (wider)
  geom_ribbon(data = cgr_intervals, 
              aes(x = Vida_3day, ymin = pred_lwr, ymax = pred_upr),
              fill = "#DD8D29", alpha = 0.15) +
  # Confidence interval (narrower)
  geom_ribbon(data = cgr_intervals,
              aes(x = Vida_3day, ymin = conf_lwr, ymax = conf_upr),
              fill = "#DD8D29", alpha = 0.3) +
  # Fitted line
  geom_line(data = cgr_intervals, aes(x = Vida_3day, y = fit),
            color = "#DD8D29", linewidth = 1) +
  # Data points
  geom_point(aes(x = Vida_3day, y = CGR_3day),
             size = 1.5, alpha = 0.7, shape = 16) +
  labs(title = "Vida UGSS 3-Day to Cougar 3-Day Regression",
       subtitle = "95% confidence and prediction intervals",
       x = "3-Day Volume - Vida (cfs)",
       y = "3-Day Volume - Cougar (cfs)") +
  scale_x_continuous(breaks = seq(0,100000,20000),minor_breaks = seq(0,100000,5000),labels = scales::label_comma()) + 
  scale_y_continuous(breaks = seq(0,100000,5000),minor_breaks = seq(0,100000,2500),labels = scales::label_comma()) +
  coord_cartesian(xlim = c(0,70000),ylim = c(0,30000),expand = F) +
  annotate("text",
           x = 46000,
           y = 11000, 
           label = eq_cgr,
           parse = TRUE,
           hjust = 0,
           vjust = 0.5,
           size = 3)

ggsave(paste0(getwd(),plotdir,"CGR_3day.png"), cgr_3day, height = 5, width = 7, dpi=300)

# Combine Plots ----------------------------------------------------------------
volume_transforms <- vida_3day + cgr_3day
ggsave(paste0(getwd(),plotdir,"Vida_CGR_3day.png"),volume_transforms, height = 7, width = 10, dpi=300)
