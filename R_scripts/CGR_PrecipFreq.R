# Cougar Peak to Volume Transformation -----------------------------------------
# Vida Gage - Peak to 3-day
# Vida 3-day to Cougar 3-day

# RMC Flood Hazards - Dan McGraw
today = format(Sys.Date(),"%d-%b-%Y")

# ---- Package Management ----
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse, ggplot2, scales, ggh4x, ggrepel,patchwork,fs, 
  ggsci, ggtext, glue, cli, assertthat, future, furrr, memoise)