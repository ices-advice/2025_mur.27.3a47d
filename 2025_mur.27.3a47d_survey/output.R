## Extract results of interest, write TAF output tables

## Before: Combined Q34 surveys in a DATRAS raw object containing LFD par haul, 
## total abundance and biomass as well as exploitable abundance and biomass per 
## Haul Tweedie GAMM run with retrospective and leave-one-out analysis
## After: 

library(TAF)

mkdir("output")

taf.library(surveyIndex)
library(dplyr)
library(tidyr)

# library(gstat)
source("utilities.R")

wg_year <- 2025

# LOAD dataset
load(paste0("data/surveyQ34_MUR", wg_year,".Rdata"))

###############################
# deltaGAM Berg et al. 
###############################

ages <- 0

# LOAD model outputs
output_file <- grep("_lack_data",grep("_loo",list.files(path="model"), invert = T, value = T), invert = T, value = T)

load(file=paste0("model/", output_file))

# extract model diagnosis
model_list <- grep("retro",grep("gam",ls(), value = T), value = T, invert = T)

model_summary <- lapply(model_list, function(x){m <-get(x)
dat <- data.frame(model = x, 
                  formula = as.character(m$pModels[[1]]$formula)[3],
                  family = as.character(m$pModels[[1]]$family$family),
                  nage = length(m$pModels),
                  AIC = AIC.surveyIdx(m), 
                  BIC = AIC.surveyIdx(m, BIC = T),
                  deviance_expl=1-m$pModels[[1]]$deviance/m$pModels[[1]]$null.deviance,
                  edfs = m$edfs)
return(dat)})
model_summary <- do.call(rbind, model_summary)

write.csv(model_summary, file = "output/GAM_summary_MURQ34.csv",row.names = F)

# extract index at age
# best AIC
idx_tw <- gather.idx(gam_Cship_Gear_timeYD_Depth_spy)

write.taf(idx_tw, file = paste0("output/GAM_MURQ34_indices1991-", wg_year-1,"_tw.csv"),row.names = F)

# explore MohnRho 
retro_list <- grep("retro",grep("gam",ls(), value = T), value = T)

retro_summary <- lapply(retro_list, function(x){m <-get(x)
base_m <- get(gsub("retro_","",x)) 
dat <- data.frame(model = gsub("retro_","",x), 
                  formula = as.character(base_m$pModels[[1]]$formula)[3],
                  family = as.character(base_m$pModels[[1]]$family$family),
                  nage = length(base_m$pModels),
                  mRho_Biom = ifelse(length(base_m$pModels)==6,NA,mohn.surveyIdx(m, base_m)[1]))
return(dat)})
retro_summary <- do.call(rbind, retro_summary)
write.csv(retro_summary, file = "output/GAM_MURQ34_retro.csv",row.names = F)

#retrospective
#REDO it standardized by their one mean not the baseline one

#standirdized or not
idx.std <- F

retro.idx_tw <- gather.retro(retro_gam_Cship_Gear_timeYD_Depth_spy)

if(idx.std){
  idx_mage <- group_by(idx_tw, age) %>% summarise(idx_mean = mean(index))
  idx_tw <- group_by(idx_tw, age) %>% summarise(Year=Year, std.idx = index/mean(index), std.up= up/mean(index), std.lo= lo/mean(index))
  retro.idx_tw <- left_join(retro.idx_tw, idx_mage)
  retro.idx_tw <- mutate(retro.idx_tw, std.idx = index/idx_mean,.keep = "unused")
}
idx_tw$retro <- as.character(wg_year-1)
if(idx.std){
  retro.idx_tw$std.lo <- retro.idx_tw$std.up <- NA
} else {
  retro.idx_tw$lo <- retro.idx_tw$up <- NA
}

retro.idx_tw <- retro.idx_tw[,names(idx_tw[,names(idx_tw)!="CV"])]
retro.idx_tw  <- rbind(idx_tw[,names(idx_tw)!="CV"], retro.idx_tw)
retro.idx_tw$Year <- as.numeric(retro.idx_tw$Year)

write.taf(retro.idx_tw, file = paste0("output/GAM_MURQ34_indices1991-", wg_year-1,"_retro_tw.csv"),row.names = F)

# Loo analysis

output_list_loo <- grep("_loo",list.files(path="model"), value = T)

load(file=paste0("model/", output_list_loo))

idx_1 <- gather.idx(loo_BTS)
idx_1$survey <- "BTS"
idx_1$index_std <- idx_1$index/mean(idx_1$index)
idx_1$up_std <- idx_1$up/mean(idx_1$index)
idx_1$lo_std <- idx_1$lo/mean(idx_1$index)
idx_2 <- gather.idx(loo_CGFS)
idx_2$survey <- "CGFS"
idx_2$index_std <- idx_2$index/mean(idx_2$index)
idx_2$up_std <- idx_2$up/mean(idx_2$index)
idx_2$lo_std <- idx_2$lo/mean(idx_2$index)
idx_3 <- gather.idx(loo_IBTS)
idx_3$survey <- "IBTS"
idx_3$index_std <- idx_3$index/mean(idx_3$index)
idx_3$up_std <- idx_3$up/mean(idx_3$index)
idx_3$lo_std <- idx_3$lo/mean(idx_3$index)

idx_tw$retro <- "all"
idx_tw <- idx_tw %>% rename(survey = retro)
idx_tw$index_std <- idx_tw$index/mean(idx_tw$index)
idx_tw$up_std <- idx_tw$up/mean(idx_tw$index)
idx_tw$lo_std <- idx_tw$lo/mean(idx_tw$index)

loo <- bind_rows(idx_tw,idx_1,idx_2,idx_3)

write.taf(loo, file = paste0("output/GAM_MURQ34_indices1991-", wg_year-1,"_loo_tw.csv"),row.names = F)

# extract residuals information

resid_tw <- resid_ln <- survey[['HH']][,c('Year', 'lon', 'lat', 'Gear', 'Country', 'Ship', 'CShip', 'timeOfYear','TimeShotHour', 'Depth')]
resid_tw$res <- gam_Cship_Gear_timeYD_Depth_spy$residuals[[1]]

save(resid_tw, resid_ln, file = "output/residuals.Rdata")

# # missing uk 6 nautical miles missing and UK BTS in 2022 
# output_file <- grep("_lack_data",list.files(path="model"), value = T)
# 
# load(file=paste0("model/", output_file))
# 
# test.idx <- gather.idx(gam_Cship_Gear_timeYD_Depth_spy_test)
# test.idx$year_wouk <- 2021
# 
# retro.idx <- gather.retro(retro_gam_Cship_Gear_timeYD_Depth_spy, bound = T) # look for run without 2022 data to compare
# retro.idx <- subset(retro.idx, retro==2021) 
# retro.idx$year_wouk <- "baseline"
# retro.idx <- retro.idx[,names(test.idx)]
# 
# test.idx <- rbind(test.idx, retro.idx)
# test.idx$Year <- as.numeric(test.idx$Year)
# 
# write.taf(test.idx, file = "output/GAM_MURQ34_indices1991-2021_UK6miles_sensi2021_tw.csv",row.names = F)
