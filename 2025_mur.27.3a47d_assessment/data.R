## Preprocess data, write TAF data tables

## Before:
## After:

library(TAF)
library(data.table)
library(dplyr)
library(stringr)
library(gsubfn)
library(FLCore)
library(openxlsx)
library(stats)


mkdir("data")

source("utilities.R")

assess_year <- 2025

# Explore official landings
taf.unzip("boot/data/HistoricalLandings1950-2010.zip",files = "HistoricalLandings1950-2010/ICES_1950-2010.csv", exdir = "boot/data")
taf.unzip("boot/data/OfficialNominalCatches.zip",files = "ICESCatchDataset2006-2022.csv", exdir = "boot/data")
taf.unzip("boot/data/2023preliminaryCatchStatisticsV2.zip", exdir = "boot/data")
taf.unzip("boot/data/2024preliminaryCatchStatistics.zip", exdir = "boot/data")

official_files<- grep(".csv",list.files("boot/data/",recursive =T), value =T)

HistoricalLandings <- grep("Historical", official_files, value =T)
officialLandings <- grep("ICESCatchDataset", official_files, value =T)
prelPrevYear <- grep("Preliminary", official_files, value =T)
prelData <- grep("joglw5", official_files, value =T)

HistoricalLandings <- read.csv(file.path("boot/data/", HistoricalLandings))
HistoricalLandings <- HistoricalLandings %>% filter(Division %in% c("III a", "IV a", "IV b", "IV c", "IV (not specified)", "VII d") &
                                                    Species %in% c("Red mullet", "Surmullets(=Red mullets) nei")) %>%
  reshape2::melt(id.vars = c("Country", "Species", "Division"), variable.name = "Year", value.name = "AMS.Catch") %>%
  mutate(AMS.Catch = as.numeric(gsub("-|<0.5|[.]", "", AMS.Catch)), 
         Year = as.numeric(gsub("X", "", Year)),
         Area = gsubfn("III a|IV a|IV b|IV c|IV [(]not specified[)]|VII d", 
                           list('III a'="27.3.a",'IV a'="27.4.a",'IV b'="27.4.b", 'IV c'="27.4.c",'IV (not specified)' = "27.4", 
                                'VII d'="27.7.d"), Division),
         Species="MUR",
         Country = gsubfn("Belgium|Denmark|France|Germany|Netherlands|Norway|UK - Eng[+]Wales[+]N[.]Irl[.]|UK - England & Wales|UK - Scotland", 
                          list(Belgium="BE", Denmark="DK",France="FR", Germany="DE", Netherlands= "NL", Norway = "NO", 
                               'UK - Eng+Wales+N.Irl.'="GB", 'UK - England & Wales'="GB", 'UK - Scotland'="GB"), Country),
         Division = NULL, BMS.Catch = NA) %>% filter(Year<2006)
  
officialLandings <- read.csv(file.path("boot/data/", officialLandings))
officialLandings <- officialLandings[,-1] %>% filter(Area %in% c("27.3.a", "27.4.a", "27.4.b", "27.4.c", "27.4_NK", "27.7.d") &
                                                  Species == "MUR") %>%
  reshape2::melt(id.vars = c("Country", "Species", "Area", "Units"), variable.name = "Year", value.name = "AMS.Catch") %>%
  mutate(Year = as.numeric(gsub("X", "", Year)),
         Area = gsub("27.4_NK","27.4" , Area), Units = NULL, BMS.Catch = NA)


prelPrevYear <- read.table(file.path("boot/data", prelPrevYear), sep = ";", dec = ".", header = T, fileEncoding = "UTF-8-BOM")
# prelPrevYear <- prelPrevYear %>% filter(Area %in% c("27_3_A_20", "27_3_A_21", "27_4_A", "27_4_B", "27_4_C", "27_7_D") &
#                                           Species.Latin.Name == "Mullus surmuletus") %>%
#   mutate(Area = gsubfn("27_3_A_20|27_3_A_21|27_4_A|27_4_B|27_4_C|27_7_D", 
#                        list('27_3_A_20'="27.3.a",'27_3_A_21'="27.3.a",'27_4_A'="27.4.a",'27_4_B'="27.4.b", '27_4_C'="27.4.c", 
#                             '27_7_D'="27.7.d"), Area),
#          Species="MUR",
#          BMS.Catch.TLW. = as.numeric(gsub("NULL","" , BMS.Catch.TLW.)),
#          Species.Latin.Name = NULL, AphiaID = NULL) %>% rename(AMS.Catch=AMS.Catch.TLW., BMS.Catch=BMS.Catch.TLW.)
prelPrevYear <- prelPrevYear %>% filter(Area %in% c("27_3_A_20", "27_3_A_21", "27_4_A", "27_4_B", "27_4_C", "27_7_D") &
                      Species.Latin.Name == "Mullus surmuletus") %>%
  mutate(Area = gsubfn("27_3_A_20|27_3_A_21|27_4_A|27_4_B|27_4_C|27_7_D", 
                       list('27_3_A_20'="27.3.a",'27_3_A_21'="27.3.a",'27_4_A'="27.4.a",'27_4_B'="27.4.b", '27_4_C'="27.4.c", 
                            '27_7_D'="27.7.d"), Area),
         Species="MUR",
         Species.Latin.Name = NULL, Species_Latin_Name = NULL, AphiaID = NULL) %>% rename(AMS.Catch=AMS_Catch, BMS.Catch=BMS_Catch)

prelData <- read.csv(file.path("boot/data/", prelData))

prelData <- prelData %>% filter(Area %in% c("27.3.a.20", "27.3.a.21", "27.3.a", "27.4.a", "27.4.b", "27.4.c", "27.7.d") &
                                     Species.Latin.Name == "Mullus surmuletus") %>%
  mutate(Area = gsubfn("27.3.a.20|27.3.a.21", 
                       list('27.3.a.20'="27.3.a",'27.3.a.21'="27.3.a"), Area),
         Species="MUR",
         Species.Latin.Name = NULL, AphiaID = NULL) %>% rename(AMS.Catch=AMS.Catch.TLW., BMS.Catch=BMS.Catch.TLW.)

officialLandings <- officialLandings[,names(HistoricalLandings)]
prelPrevYear <- prelPrevYear[,names(HistoricalLandings)]
prelData <- prelData[,names(HistoricalLandings)]
officialLandings <- rbind(HistoricalLandings, officialLandings, prelPrevYear, prelData)

#add missing FR data in 1999 from national database 
officialLandings$AMS.Catch[officialLandings$Country =="FR" & officialLandings$Year == 1999 & officialLandings$Area=="27.4.b"] <- 8.858
officialLandings$AMS.Catch[officialLandings$Country =="FR" & officialLandings$Year == 1999 & officialLandings$Area=="27.4.c"] <- 207.280
officialLandings$AMS.Catch[officialLandings$Country =="FR" & officialLandings$Year == 1999 & officialLandings$Area=="27.7.d"] <- 966.478

officialLandings <- officialLandings %>% filter(!is.na(AMS.Catch)) %>% mutate(Year = Year, offLandings = as.numeric(AMS.Catch))
minor_Country <- officialLandings %>% group_by(Country) %>% summarise(test=sum(offLandings)) %>% filter(test < 1)

officialLandings <- officialLandings %>% filter(!Country %in% minor_Country$Country) %>% group_by(Year) %>%
  mutate(poffLandings = offLandings/sum(offLandings))



write.taf(officialLandings, file =paste0("data/official_landings",assess_year,".csv"))

# Explore raw IC
taf.unzip("boot/data/IC_age2003-2024.zip", exdir = "boot/data")

StockOverviewFiles <- grep("StockOverview_\\d{4}.txt",list.files('boot/data', full.names = T), value = T)
NumbersAtAgeLengthFiles <- grep("_length", grep("NumbersAtAgeLength.txt",list.files('boot/data', recursive = T,full.names = T), value = T), invert = T, value = T)

#remove 2003 unexplained discrepancy between official and IC
  
for (y in 2004:(assess_year-1)){
  WtData <- readStockOverview(grep(y, StockOverviewFiles, value = T),grep(y, NumbersAtAgeLengthFiles, value = T)) 
  WtData$Area <- as.character(WtData$Area)
  WtData <- WtData[,names(WtData)!="UnitEffort"]
  if(y == 2004){
    WtTot <- WtData
  }else{
    WtTot <- rbind(WtTot, WtData[,names(WtTot)])
  }
}

WtTot$gear <- substr(WtTot$Fleet,1, 3)
WtTot$metier <- substr(WtTot$Fleet,1, 7)

WtTot <- WtTot %>% mutate(ID = paste0(Country,"-",Season,"-",Year,"-",Area,"-",Fleet,"-",CatchCat), 
                          gear = gsubfn("MIS|OTB|SDN|SSC|GNS|PTB|TBB|GTR|LLS|FPO|C-A|OTM|DRB|LHP", 
                                        list('MIS'="Other",'OTB'="Bottom trawl", 'SDN'="Seine",'SSC'="Seine", 'GNS'="Netters", 
                                             'GTR'="Netters", 'PTB'="Bottom trawl", 'TBB'="Beam trawl", 'LLS' = "Other",
                                             'C-A' = "Other", 'FPO' = "Other", 'OTM'="Other", 'DRB'="Other", 'LHP' = "Other"),gear),
                          Country = gsubfn("Belgium|Denmark|France|Netherlands|Norway|Germany|UK [(]England[)]|UK[(]Northern Ireland[)]|UK[(]Scotland[)]", 
                                           list('Belgium'="BE",'Denmark'="DK",'Germany'="DE",'France'="FR",'Netherlands'="NL",'Norway'="NO",
                                                'UK (England)'="GB", 'UK(Northern Ireland)'="GB", 'UK(Scotland)'="GB"),Country),
                          Season = gsub("\\d{4}", "year", Season),
                          CatchWt = CatchWt/1000)

WtTot <- WtTot %>% group_by(Year, CatchCat) %>%
  mutate(pCatchWt = CatchWt/sum(CatchWt), Cyear = sum(CatchWt))

write.csv(WtTot, file =paste0("data/IC_raw_age_all.csv"))
write.csv(WtData, file =paste0("data/IC_raw_age_",assess_year-1,".csv"))

# prepare table for model
IC_landing <- WtTot %>% filter(CatchCat=="L") %>% group_by(Year) %>% 
  summarise(IClanding = sum(CatchWt)) %>% mutate(Year = as.factor(Year))

IC_landing <- IC_landing %>% rename(year = Year, landings = IClanding) %>%
  mutate(year = as.numeric(as.character(year)), advice = 0, discards = 0, catch = landings)
IC_landing$advice[IC_landing$year== "2013"] <- 1700
IC_landing$advice[IC_landing$year%in%c("2014","2015")] <- 460
IC_landing$advice[IC_landing$year%in%c("2016","2017")] <- 552
IC_landing$advice[IC_landing$year%in%c("2018","2019")] <- 465
IC_landing$advice[IC_landing$year%in%c("2022","2023")] <- 1950
IC_landing$advice[IC_landing$year == "2024"] <- 1985
IC_landing_new <- data.frame(year = c(2025), landings = NA, advice = 1985, discards = NA, catch = NA)

IC_landing <- rbind(IC_landing, IC_landing_new)

write.taf(IC_landing, file = "data/Catch_summary.csv")

# look at discard ratio ?
Discard <- WtTot[,c("Area","Season","Fleet","Country","CatchCat","Year","gear","metier","CatchWt")] %>% filter(CatchCat == "D") %>%
  mutate(DiscardWt = CatchWt, CatchWt = NULL)
Landing <- WtTot[,c("Area","Season","Fleet","Country","CatchCat","Year","gear","metier","CatchWt","Cyear")] %>% filter(CatchCat == "L") %>%
  mutate(LandingWt = CatchWt, Lyear = Cyear, Cyear = NULL, CatchWt = NULL)

# don't know why bug in dplyr not able to drop "CatchCat" var
Discard <- Discard[,names(Discard)!="CatchCat"]
Landing <- Landing[,names(Landing)!="CatchCat"]

Discard_ratio <- merge(Discard,Landing, all.x = T) %>% filter(!is.na(LandingWt) & LandingWt>0.001) %>%
  mutate(DLratio = DiscardWt/LandingWt)

Discard_ratio <- Discard_ratio %>% group_by(Year) %>% summarise(DLratio=weighted.mean(DLratio,LandingWt), discard_cov=sum(LandingWt)/unique(Lyear), Lyear = unique(Lyear))

write.taf(Discard_ratio, file =paste0("data/Discard_ratio.csv"))

# combine IC and official landings
Landing_sum <- Landing %>% group_by(Year,Country, Area) %>% summarise(Landing=sum(LandingWt)) %>% mutate(Type = "IC")
officialLandings <- officialLandings %>% mutate(Landing = offLandings, Type = "Official" )
Landing_sum <- rbind(Landing_sum, officialLandings[,names(Landing_sum)]) %>% filter(Year > 2003)

write.taf(Landing_sum, file =paste0("data/IC_vs_official_landings.csv"))

#look at age data for a4a (running until 2021 no survey index data at age in 2022 need more work)
taf.unzip("boot/data/age_a4a.zip", exdir = "boot/data")

### ------------------------------------------------------------------------------------------------------
###  1. Settings
### ------------------------------------------------------------------------------------------------------
## Stock name
stock_Name      <- "mur-nsea"

## Assessment settings
# Year
assyear <- 2025
# Units
Units <- c("tonnes","thousands","kg")     # totals (product of the next two), numbers, weights-at-age
# mean F ages (total&landings and for discards)
meanFages <- c(1:3)
# Plusgroup age
pGrp <- 4

## Reference points
refPts <- list()
refPts[["Bpa"]] <-      NA
refPts[["Fpa"]] <-      NA
refPts[["Blim"]] <-     NA
refPts[["Flim"]] <-     NA
refPts[["BmsyTrig"]] <- NA
refPts[["Fmsy"]] <-     NA

### ------------------------------------------------------------------------------------------------------
###   3. Read and process assessment input data
### ------------------------------------------------------------------------------------------------------
## Read data

#### M Gislason
stockGislason              <- readFLStock("boot/data/indexGislason.txt")
units(stockGislason)[1:17] <- as.list(c(rep(Units,4), "NA", "NA", "f", "NA", "NA"))
years              <- as.numeric(dimnames(stockGislason@stock.n)$year)
stockGislason_ini <- stockGislason

## SOP correction
soplan <- sop(stockGislason,"landings")
#print(paste("Landings SOP range: ",range(soplan)[1]," - ",range(soplan)[2],sep=""))
stockGislason@landings.n <- sweep(stockGislason@landings.n,2,soplan,"/")
sopc  <- sop(stockGislason,"landings")
#if(round(sum(sopc-1),1)!=0) print("SOP correction failed") else print("SOP correction successful")

# Add up totals
stockGislason <- totalStk(stockGislason, Units)

stockGislason   <- setPlusGroup(stockGislason, plusgroup=pGrp)

stockGislason@catch.n[stockGislason@catch.n==0] <- 0.0001
range(stockGislason)[["minfbar"]] <- 1
range(stockGislason)[["maxfbar"]] <- 2 
#### remove 0 in weight at age (Alessandro comment)
landings.wt(stockGislason)[landings.wt(stockGislason)==0] <- NA
landings.wt(stockGislason)[5,ac(2013)] <- mean(landings.wt(stockGislason)[5,], na.rm=T)
landings.wt(stockGislason)[1,ac(2004:2008)] <- mean(landings.wt(stockGislason)[1,], na.rm=T)
landings.wt(stockGislason)[1,ac(2011)] <- mean(landings.wt(stockGislason)[1,], na.rm=T)
landings.wt(stockGislason)[5,ac(2020)] <- mean(landings.wt(stockGislason)[5,], na.rm=T) #add 2020 age 4
landings.wt(stockGislason)[5,ac(2024)] <- mean(landings.wt(stockGislason)[5,], na.rm=T) #add 2024 age 4

stock.wt(stockGislason)[stock.wt(stockGislason)==0] <- NA
stock.wt(stockGislason)[5,ac(2013)] <- mean(stock.wt(stockGislason)[5,], na.rm=T)
stock.wt(stockGislason)[1,ac(2004:2008)] <- mean(stock.wt(stockGislason)[1,], na.rm=T)
stock.wt(stockGislason)[1,ac(2011)] <- mean(stock.wt(stockGislason)[1,], na.rm=T)
stock.wt(stockGislason)[4,ac(2011)] <- mean(stock.wt(stockGislason)[4,], na.rm=T)
stock.wt(stockGislason)[5,ac(2020)] <- mean(stock.wt(stockGislason)[5,], na.rm=T) #add 2020 age 4 
stock.wt(stockGislason)[5,ac(2024)] <- mean(stock.wt(stockGislason)[5,], na.rm=T) #add 2024 age 4 

#### change catch wt (no discards)
catch.wt(stockGislason) <- landings.wt(stockGislason)

indices            <- readFLIndices("boot/data/indices.dat", na.strings="-1")
index(indices@.Data[[1]])[index(indices@.Data[[1]])==0] <- 0.0001

indices  <- FLIndices(trim(indices[[1]],age=0:pGrp, year=2004:2021))

save(stockGislason,stockGislason_ini,pGrp,indices,file= "data/a4a_input_data.Rdata")

#### explore Length data 
taf.unzip("boot/data/IC_length2021-2024.zip", exdir = "boot/data")

StockOverviewFiles <- paste0("boot/data/StockOverview", assess_year-1,"_length.txt")
NumbersAtAgeLengthFiles <- paste0("boot/data/Numbers_at_age_and_mean_weights_at_age", assess_year-1,"_length/NumbersAtAgeLength.txt")

WtData_length <- readStockOverview(grep(y, StockOverviewFiles, value = T),grep(y, NumbersAtAgeLengthFiles, value = T)) 
WtData_length$Area <- as.character(WtData_length$Area)

write.csv(WtData_length, file =paste0("data/IC_raw_length_",assess_year-1,".csv"))

### load old data

load(file="boot/data/IC_echantil_length_mur_wgnssk2024.rdata")

## read new data 2021
N2021 <- readVPAIntercatch_length('boot/data/mur.27.3a47d_all_ 2025-4-10 17_36_05_2021L_Length/canum.txt')
W2021 <- readVPAIntercatch_length('boot/data/mur.27.3a47d_all_ 2025-4-10 17_36_05_2021L_Length/weca.txt')

W2021 <- as.data.frame(W2021)[,c("len","data")]
N2021 <- as.data.frame(N2021)[,c("len","data")]

colnames(N2021) <- colnames(W2021) <- c("MeanLength","2021")

IC_echantil$Undetermined_length <- IC_echantil$Undetermined_length[,!names(IC_echantil$Undetermined_length) %in% ac(2021:2023)] #remove old 2021-2023 data
IC_echantil$Undetermined_length <- merge(IC_echantil$Undetermined_length, N2021, all = T)
IC_echantil$Undetermined_length[is.na(IC_echantil$Undetermined_length)] <- 0

IC_echantil$Undetermined_weight <- IC_echantil$Undetermined_weight[,!names(IC_echantil$Undetermined_weight) %in% ac(2021:2023)] #remove old 2021-2023 data
IC_echantil$Undetermined_weight <- merge(IC_echantil$Undetermined_weight, W2021, all = T)
IC_echantil$Undetermined_weight[IC_echantil$Undetermined_weight==0] <- NA

## read new data 2022
N2022 <- readVPAIntercatch_length('boot/data/mur.27.3a47d_all_ 2025-4-10 17_56_27_2022L_Length/canum.txt')
W2022 <- readVPAIntercatch_length('boot/data/mur.27.3a47d_all_ 2025-4-10 17_56_27_2022L_Length/weca.txt')

W2022 <- as.data.frame(W2022)[,c("len","data")]
N2022 <- as.data.frame(N2022)[,c("len","data")]

colnames(N2022) <- colnames(W2022) <- c("MeanLength","2022")

IC_echantil$Undetermined_length <- merge(IC_echantil$Undetermined_length, N2022, all = T)
IC_echantil$Undetermined_length[is.na(IC_echantil$Undetermined_length)] <- 0

IC_echantil$Undetermined_weight <- merge(IC_echantil$Undetermined_weight, W2022, all = T)
IC_echantil$Undetermined_weight[IC_echantil$Undetermined_weight==0] <- NA

## read new data 2023
N2023 <- readVPAIntercatch_length('boot/data/mur.27.3a47d_all_ 2025-4-15 11_16_33_2023L_Length/canum.txt')
W2023 <- readVPAIntercatch_length('boot/data/mur.27.3a47d_all_ 2025-4-15 11_16_33_2023L_Length/weca.txt')

W2023 <- as.data.frame(W2023)[,c("len","data")]
N2023 <- as.data.frame(N2023)[,c("len","data")]

colnames(N2023) <- colnames(W2023) <- c("MeanLength","2023")

IC_echantil$Undetermined_length <- merge(IC_echantil$Undetermined_length, N2023, all = T)
IC_echantil$Undetermined_length[is.na(IC_echantil$Undetermined_length)] <- 0

IC_echantil$Undetermined_weight <- merge(IC_echantil$Undetermined_weight, W2023, all = T)
IC_echantil$Undetermined_weight[IC_echantil$Undetermined_weight==0] <- NA

## read new data 2024
N2024 <- readVPAIntercatch_length('boot/data/mur.27.3a47d_all_ 2025-4-15 12_24_46_2024L_Length/canum.txt')
W2024 <- readVPAIntercatch_length('boot/data/mur.27.3a47d_all_ 2025-4-15 12_24_46_2024L_Length/weca.txt')

W2024 <- as.data.frame(W2024)[,c("len","data")]
N2024 <- as.data.frame(N2024)[,c("len","data")]

colnames(N2024) <- colnames(W2024) <- c("MeanLength","2024")

IC_echantil$Undetermined_length <- merge(IC_echantil$Undetermined_length, N2024, all = T)
IC_echantil$Undetermined_length[is.na(IC_echantil$Undetermined_length)] <- 0

IC_echantil$Undetermined_weight <- merge(IC_echantil$Undetermined_weight, W2024, all = T)
IC_echantil$Undetermined_weight[IC_echantil$Undetermined_weight==0] <- NA

save(IC_echantil, file="data/IC_echantil_length_mur_wgnssk2025.rdata")

# relocate survey index Q34 file
write.taf(read.taf("boot/data/GAM_MURQ34_indices1991-2024_tw.csv"), file = "data/GAM_MURQ34_indices1991-2024_tw.csv") 
