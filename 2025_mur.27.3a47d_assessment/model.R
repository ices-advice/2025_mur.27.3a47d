## Run analysis, write model results

## Before:
## After:

library(TAF)

mkdir("model")

assess_year <- 2025
bench_ref_period <- c(1991:2022)

###### Load libraries
# library(remotes)
# install_github("shfischer/cat3advice", force = T, build_vignettes = T)
library(dplyr)
library(tidyr)
library(data.table)
taf.library(cat3advice)

###### Read in data
load(file="data/IC_echantil_length_mur_wgnssk2025.rdata")

### ------------------------------------------------------------------------ ###
### Chr rule: Try cat3advice package from Simon Fisher                      ####  
### ------------------------------------------------------------------------ ###
### get survey index and catches
index_Q34 <- read.taf("data/GAM_MURQ34_indices1991-2024_tw.csv") 
catch_mur.27.3a47d <- read.csv(file = "data/Catch_summary.csv")

A <- cat3advice::A(catch_mur.27.3a47d, units = "tonnes")

### Index
index_Q34 <- index_Q34[,c("Year", "index", "up","lo")] %>% mutate(Year = as.factor(Year))
index <- index_Q34[,c("Year", "index")]
names(index) <- tolower(names(index))
index <- index %>% mutate(year = as.numeric(as.character(year)), index = index/1000)

### get most recent value
idx <- cat3advice::I(index, units = "kg/km2")

### Biomass safeguard

### use same plaice data as before
### application in first year with new calculation of Itrigger
bi_ref <- cat3advice::b(index, units = "kg/km2", Iloss = quantile(subset(idx@idx,year %in% bench_ref_period)$index,0.20)) # set of year used during the benchmark TO USE NEXT YEAR
# bi <- cat3advice::b(index, units = "kg/km2", Iloss = quantile(idx@idx$index,0.20))

### Target harvest rate FproxyMSY
### Annual catch length frequencies
length_mur.27.3a47d <- IC_echantil$Undetermined_length
length_mur.27.3a47d <- length_mur.27.3a47d %>% reshape2::melt(id.vars = "MeanLength", variable.name = "Year", value.name = "numbers") %>%
  rename(length = MeanLength, year = Year) %>% filter(!year %in% c(2008,2013) & numbers!=0) %>%
  mutate(length = as.numeric(length), year = as.numeric(as.character(year)), catch_category = "Landings")

# ### add missing data years
# length_mur.27.3a47d <- rbind(length_mur.27.3a47d,
#                              expand.grid(length = unique(length_mur.27.3a47d$length) , 
#                                          year = c(2008,2013,2017), 
#                                          numbers = NA, catch_category = 'Landings'))

# if only use Lc from benchmark data up to 2022

### Length at first capture Lc : average of yearly Lc
lc_ref <- cat3advice::Lc(length_mur.27.3a47d, average = bench_ref_period) #based on benchmark TO USE NEXT YEAR

lc <- cat3advice::Lc(length_mur.27.3a47d, average = T) # for plotting purposes full dataset 
lc@value <- lc_ref@value # overwrite with Lc calculated from benchmark ref period

### Mean length
# lmean_ref <- cat3advice::Lmean(data = subset(length_mur.27.3a47d, year <= max(bench_ref_period)), Lc = lc_ref, units = "mm")
lmean_ref2 <- cat3advice::Lmean(data = length_mur.27.3a47d, Lc = lc_ref, units = "mm") # TO USE NEXT YEAR

# lmean <- cat3advice::Lmean(data = length_mur.27.3a47d, Lc = lc, units = "mm") #USE FUTURE BENCHMARK

### Reference length (with Linf VB estimates from French age length sampling from 2006-2021)
lref_ref <- cat3advice::Lref(Lc = lc_ref, Linf = 341, units = "mm") # TO USE NEXT YEAR

# lref <- cat3advice::Lref(Lc = lc, Linf = 341, units = "mm") #USE FUTURE BENCHMARK

# Indicator
# fi_ref <- cat3advice::f(Lmean = lmean_ref, Lref = lref_ref, units = "mm")
fi_ref2 <- cat3advice::f(Lmean = lmean_ref2, Lref = lref_ref, units = "mm")  # TO USE NEXT YEAR

# fi <- cat3advice::f(Lmean = lmean, Lref = lref, units = "mm") #USE FUTURE BENCHMARK

### Harvest rate

### combine catch and index data into single data.frame
df <- merge(catch_mur.27.3a47d, index, all = TRUE) # combine catch & index data
### calculate harvest rate
# hr_ref <- cat3advice::HR(subset(df, year <= max(bench_ref_period)), units_catch = "tonnes", units_index = "kg/km2")

hr <- cat3advice::HR(df, units_catch = "tonnes", units_index = "kg/km2")

# Harvest rate target FproxyMSY
### calculate (relative) target harvest rate
Fp_ref <- cat3advice::F(hr, yr_ref = c(2005, 2006, 2009, 2012,2018)) # Based on benchmark
# Fp_ref2 <- cat3advice::F(hr, fi_ref2) # full hr time series # to display all time series NEXT YEAR

# Fp <- cat3advice::F(hr, fi) # use future benchmark

mp <- cat3advice::m(hcr = "chr")

advice_ref <- cat3advice::chr(A = A, I = idx, F = Fp_ref, b = bi_ref, m = mp, discard_rate = 0) # to use NEXT YEAR
# advice_ref2 <- cat3advice::chr(A = A, I = idx, F = Fp_ref2, b = bi_ref, m = mp, discard_rate = 0)

# advice <- cat3advice::chr(A = A, I = idx, F = Fp, b = bi, m = mp, discard_rate = 0) #use future benchmark

# LBI analysis (old code that needs to be optimised)

final <- IC_echantil$Undetermined_length
final$MeanLength <- as.numeric(final$MeanLength)
final <- final[order(final$MeanLength),]
weight <- IC_echantil$Undetermined_weight
weight$MeanLength <- as.numeric(weight$MeanLength)
weight <- weight[order(weight$MeanLength),]
Year <- as.numeric(colnames(final)[2:length(colnames(final))])
  
Ind <- data.frame(matrix(ncol=24, nrow=length(Year))) #Yv
names(Ind) <- c('Year','L75','L25','Lmed', 'L90', 'L95', 'Lmean','Lc','LFeM','Lmaxy' ,'Lmat', 'Lopt','Linf', 'Lmax5',  'Lmean_LFeM','Lc_Lmat','L25_Lmat','Lmean_Lmat','Lmean_Lopt', 'L95_Linf', 'Lmaxy_Lopt','Lmax5_Linf','Pmega','Pmegaref')
#Ind$Year <- startyear:endyear  #Yv
Ind$Year <- Year


Ind$Lmat <- 169
Ind$Linf <- as.numeric(lref_ref@Linf)
Ind$Lopt <- 2/3*Ind$Linf

Ind <- subset(Ind, !Year %in% c(2008,2013))
Ind$Lmean <- subset(lmean_ref2@summary, !year %in% c(2008, 2013, 2017))$Lmean
Ind$Lc <- as.numeric(lc@summary$Lc) # yearly Lc
Ind$LFeM <- 0.75 * as.numeric(lc_ref@value) + 0.25 * Ind$Linf # based on average Lc

Ind$Conservref <- 0.8   
Ind$Ref <- 1   
Ind$Pmegaref <- 0.3   
  
for(jj in (1:length(Ind$Year))+1){
  j <- jj-1 
    
  final2 <- final[,c(1,jj)]
  colnames(final2) <- c("lngth","number")
    
  final2$cumsum <- cumsum(final2[,2])
  final2$cumsum_perc <- final2$cumsum/sum(final2$number)
    
  # find mean top 5%
  numb <- as.data.frame(final2[rev(order(final2$lngth)),c("number","lngth")])    # from largest starting
  numb$cum <- cumsum(numb$number) 
  numb$cumperc <- round(numb$cum/sum(numb$number),5)  
  numb$num5 <- 0
  numb[numb$cumperc<=0.05,"num5"] <- numb[numb$cumperc<=0.05,"number"]
  numb[max(which(numb$cumperc<=0.05))+1,"num5"] <- (0.05-numb[max(which(numb$cumperc<=0.05)),"cumperc"])*sum(numb$number)
  Ind[j,"Lmax5"] <- sum(numb$num5*numb$lngth)/sum(numb$num5)
    
  # indicators
  Ind[j, "L75"] <- min(final2[which(final2$cumsum_perc >= 0.75), "lngth"])
  Ind[j, "L25"] <- min(final2[which(final2$cumsum_perc >= 0.25), "lngth"])
  Ind[j, "Lmed"] <- min(final2[which(final2$cumsum_perc >= 0.5), "lngth"])
  Ind[j, "L95"] <- min(final2[which(final2$cumsum_perc >= 0.95), "lngth"])
  Ind[j, "L90"] <- min(final2[which(final2$cumsum_perc >= 0.90), "lngth"])
    
  final2$biomass <- final2$number * weight[, jj]
  final2$biomass[is.na(final2$biomass)] <- 0
  Ind[j, "Lmaxy"] <- final2[final2$biomass == max(final2$biomass), "lngth"]  # length class with max yield
    
  Lopt <- (2 / 3) * as.numeric(lref_ref@Linf)
    
  Ind[j, "Pmega"] <- sum(final2[which(final2$lngth >= (Lopt + 0.1 * Lopt)),
                                  "number"]) / sum(final2$number)   # proportion larger Lopt+10%
}
  
#calculate various ratios
Ind$Lmaxy_Lopt <- Ind$Lmaxy / Ind$Lopt
Ind$L95_Linf <- Ind$L95 / Ind$Linf
Ind$Lmean_LFeM <- Ind$Lmean / Ind$LFeM
Ind$Lmean_Lmat <- Ind$Lmean / Ind$Lmat
Ind$Lmean_Lopt <- Ind$Lmean / Ind$Lopt
Ind$Lmax5_Linf <- Ind$Lmax5 / Ind$Linf
Ind$Lc_Lmat <- Ind$Lc / Ind$Lmat
Ind$L25_Lmat <- Ind$L25 / Ind$Lmat

save(Ind, A, idx, lc, lc_ref, lmean_ref2, lref_ref, fi_ref2, hr, Fp_ref, bi_ref, mp, advice_ref, file = "model/Chr_advice_2025.Rdata")
