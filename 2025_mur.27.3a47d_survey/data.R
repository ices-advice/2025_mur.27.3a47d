## Preprocess data, write TAF data tables

## Before: IBTS, BTS Q3 and CGFS Q4 DATRAS data in exchange format and LFD from IC 
## After: Combined Q34 surveys in a DATRAS raw object containing LFD par haul, 
## total abundance and biomass as well as exploitable abundance and biomass per Haul

library(icesTAF)
# install.packages('devtools') # I guess you also need this
# devtools::install_github("DTUAqua/DATRAS/DATRAS")
# devtools::install_github("casperwberg/surveyIndex/surveyIndex")
# NB: if run for the first time first prepare data folder and packages using:
# taf.bootstrap()

# load R packages from project local library (setup by icesTAF::taf.bootstrap function)
library(icesDatras)
taf.library(DATRAS)
library(maps)
library(mapdata)
library(tweedie)
taf.library(surveyIndex)
library(sp)
library(dplyr)
library(data.table)
library(scam)
library(rnaturalearth) #require('terra', ‘sys’, ‘proxy’, ‘askpass’, ‘e1071’, ‘wk’, ‘curl’, ‘mime’, ‘openssl’, ‘classInt’, ‘DBI’, ‘s2’, ‘units’, ‘httr’, ‘jsonlite’, ‘sf’) 
taf.library(rnaturalearthhires)
# library(rgdal)
# library(rgeos)
library(sf)
library(lwgeom)

wg_year <- 2025
extract <- F # force extraction of data and overwrite previous data compilation

mkdir("data")


# if already exist do not run
source("utilities.R")

if(extract | !file.exists(paste0('data/IBTSQ3_MUR', wg_year, '.Rdata')))
  source("data_IBTSQ3.R")

if(extract | !file.exists(paste0('data/BTSQ3_MUR', wg_year, '.Rdata')))
  source("data_BTSQ3.R")  

if(extract | !file.exists(paste0('data/CGFSQ4_MUR', wg_year, '.Rdata')))
  source("data_CGFSQ4.R")

rm(list=ls()[ls()!="wg_year"])
gc()

load(paste0('data/IBTSQ3_MUR', wg_year, '.Rdata'))

load(paste0('data/BTSQ3_MUR', wg_year, '.Rdata'))

load(paste0('data/CGFSQ4_MUR', wg_year, '.Rdata'))

# match column in DATRASraw object 
survey_IBTSQ3[['HH']]$EEZ <- survey_IBTSQ3[['HH']]$uk_6miles <- NA 
survey_BTSQ3[['HH']]$EEZ <- survey_BTSQ3[['HH']]$uk_6miles <- NA 
survey_CGFSQ4[['HH']]$BeamLength <- NA
survey_IBTSQ3[['HH']]$BeamLength <- NA
survey_IBTSQ3[['HH']] <- survey_IBTSQ3[['HH']][,names(survey_CGFSQ4[['HH']])] 
survey_BTSQ3[['HH']] <- survey_BTSQ3[['HH']][,names(survey_CGFSQ4[['HH']])] 

# match sizeGroup between survey /!\ ALWAYS CHECK WHAT IS INSIDE N_missing
N_missing <- matrix(0, nrow = nrow(survey_IBTSQ3[["HH"]]$N), 
                    ncol = table(colnames(survey_CGFSQ4[["HH"]]$N) %in% colnames(survey_IBTSQ3[["HH"]]$N))["FALSE"],
                    dimnames = list(haul.id=rownames(survey_IBTSQ3[["HH"]]$N), sizeGroup= colnames(survey_CGFSQ4[["HH"]]$N)[-which(colnames(survey_CGFSQ4[["HH"]]$N) %in% colnames(survey_IBTSQ3[["HH"]]$N))]))
survey_IBTSQ3[["HH"]]$N <- cbind(survey_IBTSQ3[["HH"]]$N, N_missing)
N_missing <- matrix(0, nrow = nrow(survey_IBTSQ3[["HH"]]$Nold), 
                    ncol = table(colnames(survey_CGFSQ4[["HH"]]$Nold) %in% colnames(survey_IBTSQ3[["HH"]]$Nold))["FALSE"],
                    dimnames = list(haul.id=rownames(survey_IBTSQ3[["HH"]]$Nold), sizeGroup= colnames(survey_CGFSQ4[["HH"]]$Nold)[-which(colnames(survey_CGFSQ4[["HH"]]$Nold) %in% colnames(survey_IBTSQ3[["HH"]]$Nold))]))
survey_IBTSQ3[["HH"]]$Nold <- cbind(survey_IBTSQ3[["HH"]]$Nold, N_missing)

N_missing <- matrix(0, nrow = nrow(survey_BTSQ3[["HH"]]$N), 
                    ncol = table(colnames(survey_CGFSQ4[["HH"]]$N) %in% colnames(survey_BTSQ3[["HH"]]$N))["FALSE"],
                    dimnames = list(haul.id=rownames(survey_BTSQ3[["HH"]]$N), sizeGroup= colnames(survey_CGFSQ4[["HH"]]$N)[-which(colnames(survey_CGFSQ4[["HH"]]$N) %in% colnames(survey_BTSQ3[["HH"]]$N))]))
survey_BTSQ3[["HH"]]$N <- cbind(survey_BTSQ3[["HH"]]$N, N_missing)
N_missing <- matrix(0, nrow = nrow(survey_BTSQ3[["HH"]]$Nold), 
                    ncol = table(colnames(survey_CGFSQ4[["HH"]]$Nold) %in% colnames(survey_BTSQ3[["HH"]]$Nold))["FALSE"],
                    dimnames = list(haul.id=rownames(survey_BTSQ3[["HH"]]$Nold), sizeGroup= colnames(survey_CGFSQ4[["HH"]]$Nold)[-which(colnames(survey_CGFSQ4[["HH"]]$Nold) %in% colnames(survey_BTSQ3[["HH"]]$Nold))]))
survey_BTSQ3[["HH"]]$Nold <- cbind(survey_BTSQ3[["HH"]]$Nold, N_missing)

# N_missing <- matrix(0, nrow = nrow(survey_CGFSQ4[["HH"]]$N), 
#                     ncol = table(colnames(survey_BTSQ3[["HH"]]$N) %in% colnames(survey_CGFSQ4[["HH"]]$N))["FALSE"],
#                     dimnames = list(haul.id=rownames(survey_CGFSQ4[["HH"]]$N), sizeGroup= colnames(survey_BTSQ3[["HH"]]$N)[-which(colnames(survey_BTSQ3[["HH"]]$N) %in% colnames(survey_CGFSQ4[["HH"]]$N))]))
# survey_CGFSQ4[["HH"]]$N <- cbind(survey_CGFSQ4[["HH"]]$N, N_missing)
# N_missing <- matrix(0, nrow = nrow(survey_CGFSQ4[["HH"]]$Nold), 
#                     ncol = table(colnames(survey_BTSQ3[["HH"]]$Nold) %in% colnames(survey_CGFSQ4[["HH"]]$Nold))["FALSE"],
#                     dimnames = list(haul.id=rownames(survey_CGFSQ4[["HH"]]$Nold), sizeGroup= colnames(survey_BTSQ3[["HH"]]$Nold)[-which(colnames(survey_BTSQ3[["HH"]]$Nold) %in% colnames(survey_CGFSQ4[["HH"]]$Nold))]))
# survey_CGFSQ4[["HH"]]$Nold <- cbind(survey_CGFSQ4[["HH"]]$Nold, N_missing)

# check if it is doing the good thing (place good column together)
survey <- survey_CGFSQ4
survey[['HH']] <- rbind(survey[['HH']], survey_BTSQ3[['HH']], survey_IBTSQ3[['HH']])
survey[['HL']] <- rbind(survey[['HL']], survey_BTSQ3[['HL']], survey_IBTSQ3[['HL']])
survey[['CA']] <- rbind(survey[['CA']], survey_BTSQ3[['CA']], survey_IBTSQ3[['CA']])

survey <- subset(survey, Year %in% c(1991:(wg_year-1)))

# create a continuous time variable
survey$ctime  <-  as.numeric(as.character(survey$Year))

#create interaction variable (Country:vessel)
survey[['HH']]$CShip <- as.factor(paste(survey[['HH']]$Country, survey[['HH']]$Ship, sep = "_"))

#clean up the dataset and remove Norway data as there is no catches of red mullet in the time series
survey <- subset(survey, !is.na(SweptArea))
survey <- subset(survey, Country!="NO") 

save(survey, file=paste0('data/surveyQ34_MUR', wg_year, '.Rdata'))
