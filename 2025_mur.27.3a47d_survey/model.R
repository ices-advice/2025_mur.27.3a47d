## Run analysis, write model results

## Before:Combined Q34 surveys in a DATRAS raw object containing LFD par haul, 
## total abundance and biomass as well as exploitable abundance and biomass per Haul
## After: Tweedie GAMM run with retrospective and leave-one-out analysis  

# rm(list=ls())
# gc()

#GAM's model
library(icesTAF)
taf.library(DATRAS)
taf.library(surveyIndex)
library(doParallel)
library(RcppAlgos)

mkdir("model")

# # allocated number of core used
if(Sys.info()['sysname'] == "Linux"){
  mc.cores <- 4#detectCores()-3 # leave 1 core free
} else {
  mc.cores <- 1
}

wg_year <- 2025

# LOAD data
load(paste0('data/surveyQ34_MUR', wg_year, '.Rdata'))

##############################################################
### RUN Berg et al. GAMs
##############################################################

# CREATE spatial grid

grid <- getGrid(survey, nLon=200)

# --- GAM LN, time-invariant spatial effect + depth

ages <- 0 # exploitable biomass
cutoff <- 0.1

# SET MAX dimension of smoother for each age group
# SET no. knots spatial splines

kvecP = rep(14 * 14 , length(ages))

# Tweedie (used in 2023)
modelsStatP <- rep("Year+s(lon,lat,k=kvecP[a],bs='ts') +
                    ti(lon,lat,ctime,d=c(2,1),k=c(kvecP[a],5),bs=c('tp','cs')) +
                    Gear + s(CShip,bs='re',by=dum) +
                    s(timeOfYear,bs='cc') +
                    s(TimeShotHour,bs ='cc') +
                    s(Depth, bs ='ts') +
                    offset(log(SweptArea))", length(ages))

# # in 2024 test half number of knot for ti
# modelsStatP <- rep("Year+s(lon,lat,k=kvecP[a],bs='ts') +
#                     ti(lon,lat,ctime,d=c(2,1),k=c(kvecP[a]/2,5),bs=c('tp','cs')) +
#                     Gear + s(CShip,bs='re',by=dum) +
#                     s(timeOfYear,bs='cc') +
#                     s(TimeShotHour,bs ='cc') +
#                     s(Depth, bs ='ts') +
#                     offset(log(SweptArea))", length(ages))

# run models (5h34min42s)
gam_Cship_Gear_timeYD_Depth_spy <- getSurveyIdx(survey, ages = ages,
                                   myids = grid[[3]],
                                   cutOff = cutoff,
                                   fam = rep("Tweedie", length(ages)),
                                   modelP = modelsStatP,
                                   kvecP = kvecP,
                                   mc.cores = mc.cores)

# run retrospective analysisb (20h19min51s)
retro_gam_Cship_Gear_timeYD_Depth_spy <- retro.surveyIdx(gam_Cship_Gear_timeYD_Depth_spy, survey, grid)

save(grid, gam_Cship_Gear_timeYD_Depth_spy, retro_gam_Cship_Gear_timeYD_Depth_spy, file=paste0("model/surveyQ34_MUR_GAM_Cship_Gear_Tyd_D_spY", wg_year,".Rdata"), compress="xz")

## need to do it by hand (issue with gear effect) (5h28min23s)
# load(file="model/surveyQ34_SPiCT_MUR_GAM_Cship_Gear_Tyd_D_spY.Rdata")
# loo <- leaveout.surveyIdx(gam_Cship_Gear_timeYD_Depth_spy, survey, grid, fac = survey$Survey)
# 
# save(grid, gam_Cship_Gear_timeYD_Depth_spy, retro_gam_Cship_Gear_timeYD_Depth_spy, loo, file="model/surveyQ34_SPiCT_MUR_GAM_Cship_Gear_Tyd_D_spY_loo.Rdata", compress="xz")

survey_loo_CGFS <- subset(survey, Survey != "FR-CGFS")

loo_CGFS <- getSurveyIdx(survey_loo_CGFS, ages = ages,
                                   myids = grid[[3]],
                                   cutOff = cutoff,
                                   fam = rep("Tweedie", length(ages)),
                                   modelP = modelsStatP,
                                   kvecP = kvecP,
                                   mc.cores = mc.cores)

survey_loo_IBTS <- subset(survey, Survey != "NS-IBTS")

loo_IBTS <- getSurveyIdx(survey_loo_IBTS, ages = ages,
                         myids = grid[[3]],
                         cutOff = cutoff,
                         fam = rep("Tweedie", length(ages)),
                         modelP = modelsStatP,
                         kvecP = kvecP,
                         mc.cores = mc.cores)

survey_loo_BTS <- subset(survey, Survey != "BTS")

modelsStatP <- rep("Year+s(lon,lat,k=kvecP[a],bs='ts') +
                    ti(lon,lat,ctime,d=c(2,1),k=c(kvecP[a],5),bs=c('tp','cs')) +
                    s(CShip,bs='re',by=dum) +
                    s(timeOfYear,bs='cc') +
                    s(TimeShotHour,bs ='cc') +
                    s(Depth, bs ='ts') +
                    offset(log(SweptArea))", length(ages))

loo_BTS <- getSurveyIdx(survey_loo_BTS, ages = ages,
                         myids = grid[[3]],
                         cutOff = cutoff,
                         fam = rep("Tweedie", length(ages)),
                         modelP = modelsStatP,
                         kvecP = kvecP,
                         mc.cores = mc.cores)

save(grid, loo_CGFS, loo_IBTS, loo_BTS, file=paste0("model/surveyQ34_MUR_GAM_Cship_Gear_Tyd_D_spY_loo", wg_year,".Rdata"), compress="xz")

# # Test lack of UK BTS and 6 nautical miles hauls in CGFS on 2021 dataset
# 
# survey_test <- subset(survey, Year != 2022)
# survey_test <- subset(survey_test, !c(Year == 2021 & !is.na(uk_6miles))) # remove CGFS uk_6miles
# survey_test <- subset(survey_test, !c(Year == 2021 & Survey == "BTS" & Country=="GB")) # remove CGFS uk_6miles
# 
# 
# gam_Cship_Gear_timeYD_Depth_spy_test <- getSurveyIdx(survey_test, ages = ages,
#                                                 myids = grid[[3]],
#                                                 cutOff = cutoff,
#                                                 fam = rep("Tweedie", length(ages)),
#                                                 modelP = modelsStatP,
#                                                 kvecP = kvecP,
#                                                 mc.cores = mc.cores)
# 
# save(grid, survey_test, gam_Cship_Gear_timeYD_Depth_spy_test, file="model/surveyQ34_MUR_GAM_Cship_Gear_Tyd_D_spY2023_lack_data.Rdata", compress="xz")

