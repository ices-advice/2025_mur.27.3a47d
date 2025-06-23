## Extract results of interest, write TAF output tables

## Before:
## After:

library(TAF)
library(dplyr)

mkdir("output")

# process landing ratio with survey index

load(file = "model/Chr_advice_2025.Rdata")
index_Q34 <- read.taf("data/GAM_MURQ34_indices1991-2024_tw.csv") 
catch_mur.27.3a47d <- read.csv(file = "data/Catch_summary.csv")

output_summary <- index_Q34 %>% rename(index_up = up, index_lo = lo, index_CV = CV, year = Year) %>%
  mutate(index = index/1000, index_up = index_up/1000, index_lo = index_lo/1000, age = NULL)
  
output_summary <- full_join(output_summary, catch_mur.27.3a47d)  
output_summary$advice[output_summary$advice == 0] <- NA

output_summary <- left_join(output_summary, lc@summary[, c("year", "Lc")]) %>% rename(Lc_year = Lc)
output_summary <- left_join(output_summary, lmean_ref2@summary)
output_summary$Linf <- lref_ref@Linf
output_summary$LFeM <- lref_ref@value

output_summary <- left_join(output_summary, fi_ref2@indicator[,c("year", "indicator")])
output_summary$inv_indicator <- 1/output_summary$indicator
output_summary <- left_join(output_summary, hr@data[,c("year", "harvest_rate")])
output_summary$Fmsy_proxy <- Fp_ref@value
output_summary$Iloss <- bi_ref@Iloss
output_summary$Itarget <- bi_ref@Itrigger

write.taf(output_summary, "output/output_summary.csv")

# LBI table
write.taf(Ind, "output/LBI.csv")
