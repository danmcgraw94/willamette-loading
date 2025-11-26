df <- read.csv("D:/0.RMC/Willamette/2025-Report/data/PF/example_sheets/AEP_01.csv", header = T)
names(df) <- c("AEP", "Inflow")

# Normal fit
mu <- mean(df$Inflow)
sigma <- sd(df$Inflow)
Qnorm_0.99 <- qnorm(0.99, mean = mu, sd = sigma)

# Log-normal fit
log_inflow <- log(df$Inflow)
mu_log <- mean(log_inflow)
sigma_log <- sd(log_inflow)
Qlnorm_0.99 <- qlnorm(0.99, meanlog = mu_log, sdlog = sigma_log)

Qnorm_0.99
Qlnorm_0.99

# Precip Frequency
rrft_computed <- read.csv("D:/0.RMC/Willamette/2025-Report/data/PF/RRFT/*.csv", header = T)

AEPtoZ <- function(AEP) { 
  Z = qnorm(AEP, mean = 0, sd = 1, lower.tail = TRUE)
  return(Z) 
}

ZtoAEP <- function(Z) { 
  AEP = pnorm(Z, mean = 0, sd = 1, lower.tail = TRUE)
  return(AEP) 
}

ZtoAEP(AEPtoZ(0.1))

rrft_computed_long <- rrft_computed %>% 
  pivot_longer(cols = starts_with("Bin"),
               names_to = "Bin",
               values_to = "Inflow_3day") %>% 
  filter(!is.na(Inflow_3day)) %>% 
  mutate(Z = AEPtoZ(AEP))

aep_maj_brks <- 10^(0:-8)
aep_min_brks <- as.vector(sapply(1:8, function(x) (1:9) * 10^-x))

z_maj_brks <- AEPtoZ(aep_maj_brks)
z_min_brks <- AEPtoZ(aep_min_brks)

ggplot(rrft_computed_long) + 
  geom_point(aes(x = Z, y = Inflow_3day,color = Bin)) +
  scale_x_continuous(transform = "reverse",
                     breaks = z_maj_brks,
                     minor_breaks = z_min_brks,
                     labels = aep_maj_brks) +
  labs(y = "3-Day Inflow", x = "AEP") + 
  theme(legend.position = "none")

ggplot(rrft_computed_long) + 
  geom_point(aes(x = Z, y = Inflow_3day)) +
  scale_x_continuous(transform = "reverse",
                     breaks = z_maj_brks,
                     minor_breaks = z_min_brks,
                     labels = aep_maj_brks) +
  labs(y = "3-Day Inflow", x = "AEP") + 
  theme(legend.position = "none")

# What Are the bins?
rrft_parms <- rrft_computed_long %>% 
  group_by(Bin) %>% 
  summarise(mAEP = mean(AEP),
            mu = mean(Inflow_3day),
            sigma = sd(Inflow_3day),
            mu_log = mean(log(Inflow_3day)),
            sigma_log = sd(log(Inflow_3day))) %>% 
  ungroup() %>% 
  arrange(desc(mAEP))

aep_out <- c(1e-1,1e-2,1e-3,1e-4,1e-5)
approx(x = rrft_parms$mAEP, y = rrft_parms$mu,xout = aep_out)

rrft_parms <- rrft_computed_long %>% 
  group_by(Bin) %>% 
  summarise(mAEP = mean(AEP),
            sdAEP = sd(AEP),
            AEP_50 = qnorm(0.5, mean = mAEP, sd = sdAEP),
            muQ = mean(Inflow_3day),
            sigmaQ = sd(Inflow_3day),
            #mu_log = mean(log(Inflow_3day)),
            #sigma_log = sd(log(Inflow_3day)),
            Q_5 = qnorm(0.05, mean = muQ, sd = sigmaQ),
            Q_25 = qnorm(0.25, mean = muQ, sd = sigmaQ),,
            Q_50 = qnorm(0.5, mean = muQ, sd = sigmaQ),,
            Q_75 = qnorm(0.75, mean = muQ, sd = sigmaQ),,
            Q_95 = qnorm(0.95, mean = muQ, sd = sigmaQ)) %>% 
  ungroup() %>% 
  arrange(desc(AEP_50)) %>% 
  mutate(mZ = AEPtoZ(mAEP))

# Output AEPs
out_AEP <- aep_maj_brks
out_Z <- AEPtoZ(out_AEP)

rrft_approx <- approx(x = rrft_parms$mZ, y = rrft_parms$Q_50, xout = out_Z)

rrft_priors <- tibble(Z = rrft_approx$x,
                      Inflow_3day = rrft_approx$y) %>% 
  mutate(AEP = ZtoAEP(Z)) %>% 
  filter(!is.na(Inflow_3day))


