# Precip-Freq Hyteographs --------------------------------------------------
# ---- Package Management ----
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  tidyverse, ggplot2, scales, ggh4x, ggrepel,patchwork,fs, 
  ggsci, ggtext, glue, cli, assertthat, future, furrr, 
  memoise,lubridate, purrr,zoo,janitor,gt)

# Theme and plot directory
theme_set(theme_bw())
plotdir <- file.path(getwd(),"outputs","Figures","PF")
dir_create(plotdir)

# Load Final Stage-Frequency Curve ---------------------------------------------
rrft_gages <- dir_ls("data/Cougar/",glob = "*CGR_RRFT_Hyetographs.csv*",recurse = T) %>%
  read_csv()

# Subbasin Areas ---------------------------------------------------------------
area_SK02 <- 156.9
area_SK04 <- 47.2

# Areal Weighted Gage Timeseries -----------------------------------------------
rrft_gages <- rrft_gages %>% mutate(Precip_Weighted = ifelse(Gage == "SK02", 
                                               Precip*area_SK02/(area_SK02+area_SK04),
                                               Precip*area_SK04/(area_SK02+area_SK04)))

# Extract by Met & Pivot Wider -------------------------------------------------
# unique(rrft_gages$Met)
dec1964 <- rrft_gages %>% filter(Met == "Dec1964") %>% 
  pivot_wider(names_from = Gage, values_from = c(Precip, Precip_Weighted)) %>% 
  mutate(Precip_basin = Precip_Weighted_SK02 + Precip_Weighted_SK04) %>% 
  mutate(Hour = seq(0,n()-1,1),
         DateTime = dmy_hm(DT, truncated = 3),.before = everything())

feb1996 <- rrft_gages %>% filter(Met == "Feb1996") %>% 
  pivot_wider(names_from = Gage, values_from = c(Precip, Precip_Weighted)) %>% 
  mutate(Precip_basin = Precip_Weighted_SK02 + Precip_Weighted_SK04) %>% 
  mutate(Hour = seq(0,n()-1,1),
         DateTime = dmy_hm(DT, truncated = 3),.before = everything())

nov1996 <- rrft_gages %>% filter(Met == "Nov1996") %>% 
  pivot_wider(names_from = Gage, values_from = c(Precip, Precip_Weighted)) %>% 
  mutate(Precip_basin = Precip_Weighted_SK02 + Precip_Weighted_SK04) %>% 
  mutate(Hour = seq(0,n()-1,1),
         DateTime = dmy_hm(DT, truncated = 3),.before = everything())

# Bind Rows back to a long format ----------------------------------------------
basin_precip <- bind_rows(dec1964 %>% select(Hour, DateTime, Met, Precip_basin),
                          feb1996 %>% select(Hour, DateTime, Met, Precip_basin),
                          nov1996 %>% select(Hour, DateTime, Met, Precip_basin)) %>% 
  group_by(Met) %>% 
  mutate(Precip_basin = ifelse(is.na(Precip_basin),0,Precip_basin),
         Precip_Cume = cumsum(Precip_basin),
         Precip_24hr = rollsum(x = Precip_basin, k = 24, fill = NA, align = "right"),
         Precip_72hr = rollsum(x = Precip_basin, k = 72, fill = NA, align = "right")) %>% 
  ungroup()


# Summarize Max 1-,24-, and 72-hr precip depths  -------------------------------
basin_Summary <- basin_precip %>%  group_by(Met) %>% 
  summarise(Max_1hr = max(Precip_basin, na.rm = T),
            Max_24hr = max(Precip_24hr, na.rm = T),
            Max_72hr = max(Precip_72hr, na.rm = T),
            Cumulative = max(Precip_Cume))

# Hyetographs ------------------------------------------------------------------
# DEC 1964 ---- 
max_cume <- floor((max(basin_precip$Precip_Cume[basin_precip$Met == "Dec1964"])/10))*10
max_inc <- ceiling((max(basin_precip$Precip_basin[basin_precip$Met == "Dec1964"])/.25))*.25
axis2_scale <- max_cume/max_inc

dec1964_hyeto <- 
  basin_precip %>% 
  filter(Met == "Dec1964") %>% 
  ggplot(aes(x = Hour)) +
  geom_col(aes(y = Precip_basin * axis2_scale), fill = "lightblue") +  # scale up incremental
  geom_line(aes(y = Precip_Cume), linewidth = 1, color = "darkblue") +
  labs(title = "December 1964")+
  scale_y_continuous(
    name = "Cumulative Precipitation (inches)",
    sec.axis = sec_axis(~ . * (0.33 / 20.34), 
                        name = "Incremental Precipitation (inches)"))

ggsave(file.path(plotdir,"December_1964.png"), dec1964_hyeto, height = 6, width = 8, dpi = 500)

# FEB 1996 ----
max_cume <- ceiling((max(basin_precip$Precip_Cume[basin_precip$Met == "Feb1996"])/10))*10
max_inc <- ceiling((max(basin_precip$Precip_basin[basin_precip$Met == "Feb1996"])/.25))*.25
axis2_scale <- max_cume/max_inc

feb1996_hyeto <- 
  basin_precip %>% 
  filter(Met == "Feb1996") %>% 
  ggplot(aes(x = Hour)) +
  geom_col(aes(y = Precip_basin * axis2_scale), fill = "lightblue") +  # scale up incremental
  geom_line(aes(y = Precip_Cume), linewidth = 1, color = "darkblue") +
  labs(title = "Feburary 1996")+
  scale_y_continuous(
    name = "Cumulative Precipitation (inches)",
    sec.axis = sec_axis(~ . * (0.33 / 20.34), 
                        name = "Incremental Precipitation (inches)"))

ggsave(file.path(plotdir,"Feburary_1996.png"), feb1996_hyeto, height = 6, width = 8, dpi = 500)

# NOV 1996 ---- 
max_cume <- floor((max(basin_precip$Precip_Cume[basin_precip$Met == "Nov1996"])/10))*10
max_inc <- ceiling((max(basin_precip$Precip_basin[basin_precip$Met == "Nov1996"])/.25))*.25
axis2_scale <- max_cume/max_inc

nov_1996_hyeto <- 
  basin_precip %>% 
  filter(Met == "Nov1996") %>% 
  ggplot(aes(x = Hour)) +
  geom_col(aes(y = Precip_basin * axis2_scale), fill = "lightblue") +  # scale up incremental
  geom_line(aes(y = Precip_Cume), linewidth = 1, color = "darkblue") +
  labs(title = "Novemeber 1996")+
  scale_y_continuous(
    name = "Cumulative Precipitation (inches)",
    sec.axis = sec_axis(~ . * (0.33 / 20.34), 
                        name = "Incremental Precipitation (inches)"))

ggsave(file.path(plotdir,"November_1996.png"), nov_1996_hyeto, height = 6, width = 8, dpi = 500)
