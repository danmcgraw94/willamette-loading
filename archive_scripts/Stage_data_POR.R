# Willamette Stage Data
# Cougar Lake Near Rainbow, OR - 14159400
# Hills Creek Lake Near Oakridge, OR - 14145100
# Blue River Lake Near Blue River, OR - 14162100
# Fall Creek Lake Near Lowell, OR - 14150900
# Lookout Point Lake Near Lowell, OR - 14149000
# Green Peter Lake Near Foster, OR - 14186100
# Foster Lake at Foster, OR - 14186600

library(dataRetrieval)
library(tidyverse)
library(lubridate)
library(jsonlite)

# Theme set
theme_set(theme_bw())
source("D:/R/theme_USACE.r")
today = format(Sys.Date(),"%d-%b-%Y")
plot_height = 5
plot_width = 7

# Site Numbers #################################################################
willamette_sites <- c(14159400, 14145100, 14162100, 14150900, 14149000, 14186100,14186600)
willamette_sites_names <- c("Cougar", "Hills Creek" , "Blue River" , "Fall Creek", "Lookout Point", "Green Peter","Foster")

# POR Date range ###############################################################
start_date <- mdy("10-01-1930")
end_date <- Sys.Date()
# end_date <- mdy("09-30-2000")

# Define parameter code ########################################################
parameter_cd <- "62614"
site_info <- whatNWISdata(siteNumber = "14159400")

# site_data_daily <- readNWISdv(siteNumbers = "14159400",
#                               parameterCd = "62614",
#                               startDate = start_date,
#                               endDate = end_date, statCd = "32400")


# oregon sites with daily reservoir data
OR_sites <- whatNWISsites(stateCd = "OR", 
                          parameterCd = "62614")
nrow(OR_sites)
names(OR_sites)

#library(ggsn)
library(sf)

usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE),
                crs = 4269)

sf_or <- st_as_sf(OR_sites, 
                  coords = c("dec_long_va", "dec_lat_va"),
                  crs = 4269)

sf_willamette <- sf_or %>% filter(site_no %in% willamette_sites)

ggplot() +
  geom_sf(data = usa[ usa$ID == "oregon" ,]) +
  geom_sf(data = sf_or, fill = NA, shape = 1, size = 3) + 
  geom_sf(data = sf_willamette, color = "blue2") + 
  xlab(NULL)+
  ylab(NULL)+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


# Retrieve data#################################################################
all_sites <- list()

for (i in 1:length(willamette_sites)){

  site_data_daily <- readNWISdv(siteNumbers = willamette_sites[i],
                                parameterCd = parameter_cd,
                                startDate = start_date,
                                endDate = end_date,
                                statCd = "32400")
  
  site_data_daily_clean <- renameNWISColumns(site_data_daily)
  
  all_sites[[i]] <-site_data_daily_clean
}

willamette_PORs <- bind_rows(all_sites)
head(willamette_PORs)

# Wrangle Data ################################################################
willamette_PORs <- willamette_PORs %>% 
  rename(Elevft_29 = X_62614_32400)

willamette_PORs <- willamette_PORs %>% 
  mutate(DT = lubridate::ymd_hms(Date, truncated = 3),.before = everything())

willamette_PORs <- willamette_PORs %>% 
  mutate(WY = ifelse(month(DT)>9,year(DT) + 1,year(DT)),.before = Date)

willamette_PORs <- willamette_PORs %>% 
  mutate(WY_decimal = year(Date) + (month(Date)/12) + (day(Date)/365.25))

# Get Annual Max at each site ##################################################
willamette_AMS <- willamette_PORs %>% 
  group_by(site_no, WY) %>%
  slice_max(Elevft_29, n = 1) %>% 
  ungroup()

ggplot(willamette_AMS) +
  geom_point(aes(x=WY_decimal,y=Elevft_29,color = site_no),size = 2) + 
  geom_line(data = willamette_PORs, aes(x=WY_decimal,y=Elevft_29,color = site_no),linewidth = 0.2)

# Export #######################################################################
datums <- tibble(
  site_no = willamette_sites,
  site_num = willamette_sites_names,
  To_navd88 = c(4.38,3.82,3.84,3.78,3.52,3.65,3.65),
  To_ngvd29 = -1*(c(4.38,3.82,3.84,3.78,3.52,3.65,3.65)))

for (i in 1:length(willamette_sites)){
  
  site_data_daily <- readNWISdv(siteNumbers = willamette_sites[i],
                                parameterCd = parameter_cd,
                                startDate = start_date,
                                endDate = end_date,
                                statCd = "32400")
  
  site_data_daily_clean <- site_data_daily %>% 
    select(-(grep("_cd", colnames(site_data_daily))))
  
  elevation_field_cols <- colnames(site_data_daily_clean)
  site_data_daily_clean <- site_data_daily_clean %>% 
    rename(Elev_29 = grep("32400", elevation_field_cols))
  
  datum_conversion <- datums$To_navd88[datums$site_no == willamette_sites[i]]
  
  site_data_daily_clean <- site_data_daily_clean %>% 
    mutate(Elev_NAVD88 = Elev_29 + datum_conversion)
  
  write.csv(site_data_daily_clean,paste0("D:/0.RMC/Willamette/2025-Report/data/Stage Data/USGS/",willamette_sites_names[i],".csv"),row.names = F)

}

# Full POR (RFA + USGS Record) #################################################
allsites_POR <- read.csv("D:/0.RMC/Willamette/2025-Report/data/Stage Data/AllSites_POR.csv",header = T)

sites <- unique(allsites_POR$Site)

allsites_POR <- allsites_POR %>% 
  mutate(DT = lubridate::mdy(Date),.before = Date) %>% 
  mutate(WY = ifelse(month(DT)>9,year(DT) + 1,year(DT)),.before = Stage_ft_NAVD88) %>% 
  mutate(DT_decimal = year(DT) + (month(DT)/12) + (day(DT)/365.25))

allsites_AMS <- allsites_POR %>% 
  group_by(Site,WY) %>% 
  arrange(desc(Stage_ft_NAVD88), DT) %>% # earliest DT tie-break
  slice_max(Stage_ft_NAVD88, n = 1, with_ties = FALSE) %>% 
  ungroup()

for (i in 1:length(sites)){
  filtered_AMS <- allsites_AMS %>% 
    filter(Site == sites[i]) %>% 
    arrange(desc(Stage_ft_NAVD88))
  
  write.csv(filtered_AMS,paste0("D:/0.RMC/Willamette/2025-Report/data/Stage Data/AMS/",sites[i],".csv"),row.names = F)

}
# Color palletes
#  "#984EA3"  "#FFFF33" "#F781BF" "#999999"
# "#1B9E77" "#D95F02" "#7570B3" "#E7298A" "#66A61E" "#E6AB02" "#A6761D" "#666666"
# "Cougar"  "Hills Creek" "Lookout Point" "Fall Creek"  "Blue River"  "Green-Peter" "Foster"

site_colors <- tibble(
  Site = sites,
  Colors = as.character(c("#E41A1C","#7570B3","#FF7F00","#A65628","#377EB8","#4DAF4A","#1B9E77"))
)

for (i in 1:length(sites)){
  filtered_AMS <- allsites_AMS %>% 
    filter(Site == sites[i]) %>% 
    arrange(desc(Stage_ft_NAVD88))
  
  filtered_POR <- allsites_POR %>% 
    filter(Site == sites[i])
  
  site_color <- site_colors$Colors[site_colors$Site == sites[i]]
  
  # X-axis generation
  min_WY = (floor(min(filtered_AMS$WY)/5))*5
  max_WY = (ceiling(max(filtered_AMS$WY)/5))*5
  xbreaks_maj = seq(min_WY,max_WY,by = 5)
  xbreaks_min = seq(min_WY,max_WY,by = 1)
  
  # Elevation range
  delta_elev <- max(filtered_POR$Stage_ft_NAVD88,na.rm = T) - min(filtered_POR$Stage_ft_NAVD88,na.rm = T)
  print(sites[i])
  print(delta_elev)
  elev_break_val <- ifelse(delta_elev >= 100,50,10)
  min_break_val <- ifelse(elev_break_val >= 11,5,5)
  
  # Y-axis generation
  min_elev = (floor(min(filtered_POR$Stage_ft_NAVD88)/elev_break_val))*elev_break_val
  max_elev = (ceiling(max(filtered_POR$Stage_ft_NAVD88)/elev_break_val))*elev_break_val
  ybreaks_maj = seq(min_elev,max_elev,by = elev_break_val)
  ybreaks_min = seq(min_elev,max_elev,by = min_break_val)
  
  # Y-axis extents
  y_ext_max <- ifelse(max(ybreaks_maj) - max(filtered_POR$Stage_ft_NAVD88,na.rm = T) > 15, max(filtered_POR$Stage_ft_NAVD88,na.rm = T) + 15,max(ybreaks_maj))
  y_ext_min <- ifelse(delta_elev < 200,min(ybreaks_maj),y_ext_max - 200)
  print(y_ext_min)
  
  site_stageTS <- ggplot() +
    geom_line(data = filtered_POR, aes(x = DT_decimal, y = Stage_ft_NAVD88), linewidth = 0.25) +
    geom_point(data = filtered_AMS, aes(x = DT_decimal, y = Stage_ft_NAVD88), color = site_color,size = 1) +
    scale_x_continuous(breaks = xbreaks_maj, minor_breaks = xbreaks_min) +
    scale_y_continuous(breaks = ybreaks_maj, minor_breaks = ybreaks_min) +
    coord_cartesian(ylim = c(y_ext_min,y_ext_max))+
    labs(x = "Year", y = "Reservoir Stage (ft-NAVD88)")
  
  # save fig
  filename <- paste0("D:/0.RMC/Willamette/2025-Report/Figures/Stage AMS/",sites[i],".png")
  ggsave(filename,site_stageTS,height = plot_height, width = plot_width, dpi = 300)
}

# Cougar high pools ############################################################
Cougar_POR <- allsites_POR %>% 
  filter(Site == "Cougar")

Cougar_Top10 <- Cougar_POR %>% 
  mutate(Month = month(DT)) %>% 
  group_by(WY,Month) %>% 
  slice_max(Stage_ft_NAVD88, n = 1, with_ties = F) %>% 
  arrange(desc(Stage_ft_NAVD88))

# moving 365 day max filter
Cougar_365day_max <- Cougar_POR %>% 
  mutate(Max_Yr = zoo::rollmax(Stage_ft_NAVD88,365,align = "right",fill = NA))

Cougar_365day_top10 <- Cougar_365day_max %>% 
  slice_max(Stage_ft_NAVD88, n = 10, with_ties = F)

# X-axis generation
min_WY = (floor(min(Cougar_POR$WY)/5))*5
max_WY = (ceiling(max(Cougar_POR$WY)/5))*5
xbreaks_maj = seq(min_WY,max_WY,by = 5)
xbreaks_min = seq(min_WY,max_WY,by = 1)

# Elevation range
delta_elev <- max(Cougar_POR$Stage_ft_NAVD88,na.rm = T) - min(Cougar_POR$Stage_ft_NAVD88,na.rm = T)
print(delta_elev)
elev_break_val <- ifelse(delta_elev >= 100,50,10)
min_break_val <- ifelse(elev_break_val >= 11,5,5)

# Y-axis generation
min_elev = (floor(min(Cougar_POR$Stage_ft_NAVD88)/elev_break_val))*elev_break_val
max_elev = (ceiling(max(Cougar_POR$Stage_ft_NAVD88)/elev_break_val))*elev_break_val
ybreaks_maj = seq(min_elev,max_elev,by = elev_break_val)
ybreaks_min = seq(min_elev,max_elev,by = min_break_val)

p <- ggplot(Cougar_POR) +
  geom_line(aes(x = DT_decimal, y = Stage_ft_NAVD88), linewidth = 0.25) +
  scale_x_continuous(breaks = xbreaks_maj, minor_breaks = xbreaks_min) +
  scale_y_continuous(breaks = ybreaks_maj, minor_breaks = ybreaks_min) +
  coord_cartesian(ylim = c(1350,1710))+
  labs(x = "Year", y = "Reservoir Stage (ft-NAVD88)")

# Truncated ------------------------------------------------------------------
start_trunc <- lubridate::mdy("10-01-1969")
end_trunc <- lubridate::mdy("10-01-2000")

Cougar_trunc <- Cougar_POR %>% 
  dplyr::filter(DT >= start_trunc) %>% 
  dplyr::filter(DT <=end_trunc)

p2 <- p + geom_line(data = Cougar_trunc,aes(x=DT_decimal, y = Stage_ft_NAVD88), color = "green2")

(Cougar_trunc$DT_decimal[length(Cougar_trunc$DT_decimal)] + Cougar_trunc$DT_decimal[1])/2

cgr_por_full <- ggplot(Cougar_POR) +
  geom_line(aes(x = DT_decimal, y = Stage_ft_NAVD88), linewidth = 0.25) +
  scale_x_continuous(breaks = xbreaks_maj, minor_breaks = xbreaks_min) +
  scale_y_continuous(breaks = ybreaks_maj, minor_breaks = ybreaks_min) +
  coord_cartesian(ylim = c(1350,1710))+
  labs(x = "Year", y = "Reservoir Stage (ft-NAVD88)") + 
  geom_vline(xintercept = c(Cougar_trunc$DT_decimal[1],
                            Cougar_trunc$DT_decimal[length(Cougar_trunc$DT_decimal)]),
             color = "green4",linetype = "dashed",linewidth = 0.35) +
  annotate(geom="label",x=1985.336, y=1400,label="Truncated Stage Data",size=3,fill="white",label.size=NA,alpha = .75, hjust = 0.5, vjust = 1) + 
  geom_segment(x = Cougar_trunc$DT_decimal[1],
               y = 1400,
               xend = Cougar_trunc$DT_decimal[length(Cougar_trunc$DT_decimal)],
               yend = 1400,
               arrow = arrow(length = unit(0.02, "npc"), ends = "both"),color = "green4",linewidth = 0.25)

ggsave("D:/0.RMC/Willamette/2025-Report/Figures/Stage AMS/Cougar_POR_all.png",cgr_por_full,height = plot_height, width = plot_width, dpi = 300)

# Empirical Stage of Truncated -------------------------------------------------
weibull_pp <- function(x) {
  n <- length(x)
  i <- rank(x)
  return(i / (n+1))
}

AEPtoZ <- function(AEP) { 
  Z = qnorm(AEP, mean = 0, sd = 1, lower.tail = TRUE)
  return(Z) 
}

Cougar_trunc_AMS <- Cougar_trunc %>% 
  group_by(WY) %>%
  top_n(1, Stage_ft_NAVD88) %>%
  ungroup() %>% 
  mutate(pp = weibull_pp(Stage_ft_NAVD88)) %>%
  mutate(AEP = 1-pp) %>%
  mutate(Z = AEPtoZ(pp))

Cougar_POR_AMS <- Cougar_POR %>% 
  group_by(WY) %>%
  top_n(1, Stage_ft_NAVD88) %>%
  ungroup() %>% 
  mutate(pp = weibull_pp(Stage_ft_NAVD88)) %>%
  mutate(AEP = 1-pp) %>%
  mutate(Z = AEPtoZ(pp))

# plot
maj_brks <- 10^(0:-4)
min_brks <- as.vector(sapply(1:4, function(x) (1:9) * 10^-x))

reservoir_features <- tibble(
  Feature = c("Top of Dam","Spillway"),
  Stage_Ft = c(1710.9,1661.1)) %>% 
  mutate(feature_labs = paste0(reservoir_features$Feature," = ",reservoir_features$Stage_Ft,"-ft"))

emp_colors <- c("Full POR" = "black", "Truncated POR" = "green4")
emp_shps <- c("Full POR" = 15, "Truncated POR" = 16)

ggplot() + 
  geom_hline(yintercept = reservoir_features$Stage_Ft) +
  geom_point(data = Cougar_POR_AMS, aes(x = AEP, y = Stage_ft_NAVD88, shape = "Full POR",color = "Full POR")) + 
  geom_point(data = Cougar_trunc_AMS, aes(x = AEP, y = Stage_ft_NAVD88, shape = "Truncated POR",color = "Truncated POR")) + 
  scale_x_continuous(trans = trans_new("reverse_log", transform = function(x) -log10(x),inverse = function(x) 10^(-x)), 
                     breaks = maj_brks,
                     minor_breaks = min_brks,
                     labels = label_number(accuracy = 0.001),
                     limits = c(1,0.001)) + 
  scale_y_continuous(breaks = seq(1600,1720,10),minor_breaks = seq(1600,1720,5),limits = c(1600,1715)) + 
  scale_color_manual(values = emp_colors, name = NULL) +
  scale_shape_manual(values = emp_shps, name = NULL) +
  annotate("text",x = 3E-2, y=reservoir_features$Stage_Ft,label = reservoir_features$feature_labs,size=3,vjust = -.75) +
  labs(y = "Stage (ft-NAVD88)", x ="Empirical AEP")+
  theme(legend.position = "inside", legend.position.inside = c(0.75,0.2))

ggsave("D:/0.RMC/Willamette/2025-Report/Figures/Stage AMS/Cougar_EmpiricalAMS_both.png",plot = last_plot(),height = plot_height, width = plot_width, dpi = 300)
