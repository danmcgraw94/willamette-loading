# Volume Frequency Figures and Sensitivity
# Willamette
# Libraries
library(tidyverse)

# Theme set
theme_set(theme_bw())
source("D:/R/theme_USACE.r")
today = format(Sys.Date(),"%d-%b-%Y")
plot_height = 5
plot_width = 7

# Import Data ##################################################################
# Systematic Data from Best-Fit
filenames <- dir("D:/0.RMC/Willamette/2025-Report/Figures/CEDALS_Templates/csv/",full.names = T)
syst_data <- read.csv(filenames[5],header = T)
hist_data <- read.csv(filenames[4],header = T)
priors <- read.csv(filenames[1],header = T)
curve <- read.csv(filenames[2],header = T)
sensit_curves <- read.csv(filenames[3],header = T)

# Plotting computations #######################################################
# qnorm = norm.inv from excel
syst_data <- syst_data %>% mutate(z_var = qnorm(Plotting_Position))
hist_data <- hist_data %>%
  mutate(Neg_Error_Bar = Most_Likely - Lower,
         Most_Likely_Pt = Most_Likely,
         Pos_Error_Bar = Upper - Most_Likely,
         z_var = qnorm(Plotting_Position))

priors <- priors %>%
  mutate(Lower = round(qnorm(0.05,mean = Mean, sd = Sdev),-2),
         Upper = round(qnorm(0.95,mean = Mean, sd = Sdev),-2),
         Most_Likely_Pt = Mean,
         Neg_Error_Bar = Most_Likely_Pt - Lower,
         Pos_Error_Bar = Upper - Most_Likely_Pt,
         z_var = qnorm(Quantile_prior))

curve <- curve %>% mutate(z_var = qnorm(AEP))
sensit_curves <- sensit_curves %>% mutate(z_var = qnorm(AEP))
aepbrks <- c(0.99,0.9,0.5,0.1,1e-2,1e-3,1e-4,1e-5,1e-6,1e-7,1e-8)
zvar_brks <- qnorm(aepbrks)

ggplot(syst_data) +
  geom_point(aes(x = 1-z_var, y = Inflow_Volume_cfs)) +
  scale_x_continuous(breaks = zvar_brks,name = "Annual Exceedance Probability",labels = aepbrks)



