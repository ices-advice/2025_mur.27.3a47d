## Preprocess data, write TAF data tables

## Before: BTS Q3 DATRAS data in exchange format and LFD from IC
## After: BTS Q3 in a DATRAS raw object containing LFD par haul, total abundance 
## and biomass as well as exploitable abundance and biomass per Haul

#extract CGFS data from DATRAS 
# from datras directly: 
# yrs <- c(1985:2022)
# qrs <- c(1,3,4)
# cgfs <- getDatrasExchange('BTS', years=yrs, quarters=qrs, strict=FALSE)

# or Exchange files dowlaoded from DATRAS: http://datras.ices.dk/Data_products/Download/Download_Data_public.aspx 
# survey <- readExchange('boot/data/Exchange Data_2023-03-21 10_39_17_BTSQ3.zip', strict=FALSE) # don't care about sample location in CA here
load('boot/data/BTS_rawDATRAS_WGNSSK2024.Rdata')

# from datras directly: 

survey_new <- readExchange("boot/data/BTS2024_Unaggregated trawl and biological information_2025-02-05 10_16_07.zip")

# combine dataset from last year with 2023 data
survey[['HH']] <- rbind(survey[['HH']],survey_new[['HH']])
survey[['HL']] <- rbind(survey[['HL']],survey_new[['HL']])
survey[['CA']] <- rbind(survey[['CA']],survey_new[['CA']])
save(survey, file = "data/BTS_rawDATRAS_WGNSSK2025.Rdata")

# # seperate german data to check for duplicated row (duplicated function doesn't work here)
# unique(survey[['HH']]$Ship)
# unique(survey[['HH']]$Country)
# NA_ship <- subset(survey[['HH']], Ship=='NA' & Country=="DE")
# DE_ship <- subset(survey[['HH']], Ship!='NA' & Country=="DE")
# NA_ship[1,]
# DE_ship[1,]

# only haul.id and ship are different

table(duplicated(survey[['HH']][,!names(survey[['HH']]) %in% c("Ship", 'haul.id')]))
table(duplicated(survey[['HL']][,!names(survey[['HL']]) %in% c("Ship", 'haul.id')]))
table(duplicated(survey[['CA']][,!names(survey[['CA']]) %in% c("Ship", 'haul.id')]))
survey[['CA']][duplicated(survey[['CA']][,!names(survey[['CA']]) %in% c("Ship", 'haul.id')]),]

# remove duplicated data in 2023
# survey <- subset(survey, Ship != 'NA')

setDT(survey[['CA']])
setDT(survey[['HL']])
setDT(survey[['HH']])

# retain valid haul
survey <- subset(survey, HaulVal == 'V')

# subset on species of interest
# id <- findAphia('Mullus surmuletus', latin=TRUE)
# 
# survey <- subset(survey, Valid_Aphia == id)
survey <- subset(survey, Species == 'Mullus surmuletus')

# REMOVE if HaulDur < 10 min or if above 39 min 
survey <- subset(survey, HaulDur > 9)
survey <- subset(survey, HaulDur < 40)

# REMOVE night hauls?
survey <- subset(survey, DayNight == 'D')

# REMOVE Q1 data as happen in CSE
survey <- subset(survey, Quarter  == '3')

## Predict missing Depth (Based on Iago sol.27.4 data processing)
hh <- data.table(survey[['HH']])

# FIT GAM 

dmodel <- gam(log(Depth) ~ s(lon, lat, k=200), data=hh)

# PREDICT all locations

hh[, eDepth := mapply(function(x, y)
  exp(predict(dmodel, newdata=data.frame(lon=x, lat=y))), lon, lat)]

hh[is.na(Depth), Depth := round(eDepth)]

survey[['HH']]$Depth <- hh[, Depth]

# subset data more than 200m
survey <- subset(survey, Depth <= 200)

# trim Irish sea data and Western channel data
survey <- subset(survey, lon >= -3)
survey <- subset(survey, !StatRec %in% c('26E7','26E8','27E7','27E8','28E7','29E7','30E7'))

# check if no sample are missing
survey[['HL']] <- survey[['HL']][!is.na(LngtCode),]

survey <- addSpectrum(survey)
survey <- addWeightByHaul(survey, to1min = FALSE) # warning it is estimated from Length weight relation ships

# plot abundance
survey[['HH']]$nums <- apply(survey[['HH']]$N,1,sum) 
survey[['HH']]$nums[survey[['HH']]$nums==0] <- NA

# trim data only keep up to 1990 (before 7d not covered)
survey <- subset(survey, as.numeric(as.character(Year)) >= 1990)

write.taf(survey[['HH']][,c('haul.id','lat','lon','Year','nums')], file=paste0('BTSQ3_nums_1990_', wg_year-1, '.csv'), dir='data')

#trim northern part
mask <- Polygon(cbind(c(-5, -5, 15 , 15),c(54, 62, 62, 60)))
mask <- Polygons(list(mask), ID ="1")
mask <- SpatialPolygons(list(mask))
proj4string(mask) <- CRS("+init=epsg:4326")

hh <- as.data.frame(survey[['HH']])
hh <- SpatialPointsDataFrame(coords = hh[,c('lon','lat')],data= hh[,c("haul.id","HaulWgt")])
proj4string(hh) <- CRS("+init=epsg:4326")
hh$out <- over(hh,mask)

survey[['HH']]$out <- hh$out

# remove all data above (54°N,5°W)(60°N,15°E) line
survey <- subset(survey, is.na(out))

# Calculate SweepArea
# check gear technical information and haul characteristics
# Deal with missing distance
# summary(survey[['HH']]$Distance)
# summary(survey[['HH']][is.na(Distance), GroundSpeed])
# summary(survey[['HH']][is.na(Distance), HaulDur])
# summary(survey[['HH']][is.na(Distance), HaulLat])
# summary(survey[['HH']][is.na(Distance), HaulLong])
# summary(survey[['HH']][is.na(Distance), ShootLat])
# summary(survey[['HH']][is.na(Distance), ShootLong])

# negative distance ou null
survey[['HH']][!is.na(Distance) & Distance<=0, Distance:= (HaulDur/60) * GroundSpeed * 1852]

# Missing ground speed (Distance as calculated in DATRAS)
# summary(survey[['HH']][is.na(Distance),c('HaulLat', 'HaulLong', 'ShootLat', 'ShootLong', 'lat', 'lon')]) 

survey[['HH']][is.na(Distance) | Distance==0, Distance := 1.852 * 360 * 60/(2*pi) * 
                 acos(cos(radians(ShootLat))) *
                 cos(radians(HaulLat)) *
                 cos(radians(HaulLong) - radians(ShootLong)) +
                 sin(radians(ShootLat)) *
                 sin(radians(HaulLat))]

# summary(survey[['HH']]$Distance)

# SET GroundSpeed to 4 for remaining missing distance 

survey[['HH']][is.na(Distance), GroundSpeed := 4]
survey[['HH']][is.na(Distance), Distance:= (HaulDur/60) * GroundSpeed * 1852]

# summary(survey[['HH']]$Distance)
# boxplot(survey[['HH']]$Distance)

# issue with Distance too high above 10km
# test <- subset(survey,Distance>10000)
# test[['HH']]
# test[['HH']][Distance>10000, Distance1:= (HaulDur/60) * GroundSpeed * 1852]
# test[['HH']][Distance>10000, Distance2 := 1.852 * 360 * 60/(2*pi) * 
#                acos(cos(radians(ShootLat))) *
#                cos(radians(HaulLat)) *
#                cos(radians(HaulLong) - radians(ShootLong)) +
#                sin(radians(ShootLat)) *
#                sin(radians(HaulLat))]

survey[['HH']][Distance > 10000, Distance:= (HaulDur/60) * GroundSpeed * 1852]
survey[['HH']][is.na(Distance) , Distance := 1.852 * 360 * 60/(2*pi) * 
                 acos(cos(radians(ShootLat))) *
                 cos(radians(HaulLat)) *
                 cos(radians(HaulLong) - radians(ShootLong)) +
                 sin(radians(ShootLat)) *
                 sin(radians(HaulLat))]

# summary(survey[['HH']]$Distance)
# boxplot(survey[['HH']]$Distance)

# issue with Distance too low below 1km
# test <- subset(survey,Distance < 1000)
# test[['HH']]
# test[['HH']][Distance < 1000, Distance1:= (HaulDur/60) * GroundSpeed * 1852]
# test[['HH']][Distance < 1000, Distance2 := 1.852 * 360 * 60/(2*pi) *
#                acos(cos(radians(ShootLat))) *
#                cos(radians(HaulLat)) *
#                cos(radians(HaulLong) - radians(ShootLong)) +
#                sin(radians(ShootLat)) *
#                sin(radians(HaulLat))]

survey[['HH']][Distance < 1000, Distance:= (HaulDur/60) * GroundSpeed * 1852]


# ADD BeamLength: 4 for BE, 8 for NL, DE
# xtabs(~Gear+Country,survey[['HH']])

survey[['HH']][Country == 'NL', BeamLength:=8]
survey[['HH']][Country == 'BE', BeamLength:=4]
survey[['HH']][Country == 'DE', BeamLength:=7]
survey[['HH']][Country == 'GB', BeamLength:=4]

## Add Swept area
survey[['HH']][, SweptArea := Distance * BeamLength * 10^(-6)] 
survey[['HH']][GearEx=='DB', SweptArea := 2 * Distance * BeamLength * 10^(-6)] #check if NL use two 4m beam or to 8m?

# summary(survey[['HH']]$SweptArea)
# boxplot(survey[['HH']]$SweptArea)

## check selectivity of survey
sel_surv <- data.frame(size = as.numeric(gsub('.*?([0-9]+).*', '\\1', colnames(survey[['HH']]$N)))*10 ,sel_surv = apply(survey[['HH']]$N,2,sum))

# check selectivity of commercial fleets
load('boot/data/IC_length_mur_wgnssk2022.rdata')

Lan_length <- IC$Undetermined_length
MeanLength <- data.frame(MeanLength = seq(min(Lan_length$MeanLength), max(Lan_length$MeanLength), by = 10))       

Lan_length <- merge(Lan_length,MeanLength, all = T)

sel_L <- data.frame(size = Lan_length$MeanLength, sel_L = apply(Lan_length[,-1],1,sum))

# compare survey with commercial data
sel <- merge(sel_surv, sel_L, all = T)
sel$sel_surv <- sel$sel_surv/sum(sel$sel_surv, na.rm = T) 
sel$sel_L <- sel$sel_L/sum(sel$sel_L, na.rm = T) 
sel$log_ratio <- log(sel$sel_L/sel$sel_surv)
sel$log_ratio[sel$sel_surv==0] <- NA 

m <- scam( log_ratio ~ s(size,bs='mpi'), data=sel[is.finite(sel$log_ratio),] )
sel$pred <- predict(m,sel)

# correct haul numbers and weight with selectivity from survey
sel$epred <- exp(sel$pred)

sel_surv_mod <- data.frame(size = as.numeric(gsub('.*?([0-9]+).*', '\\1', colnames(survey[['HH']]$N)))*10 ,sel_surv_mod = apply(survey[['HH']]$N * t(array(sel$epred[!is.na(sel$sel_surv)], dim = dim(t(survey[['HH']]$N)))),2,sum))
sel <- merge(sel,sel_surv_mod)
sel[is.na(sel)] <- 0
sel$cumprop <- cumsum(sel$sel_L)/sum(sel$sel_L,na.rm=T)
write.taf(sel, file=paste0('BTSQ3_selratio_1990_', wg_year-1, '.csv'), dir='data')

# turn dataset back to data frame
survey[['HH']] <- as.data.frame(survey[['HH']])
survey[['HH']]$Nold <- survey[['HH']]$N 

size_threshold <- sel$size[which.min(abs(sel$cumprop-0.025))]/10 # 97.5% Landings above that size here 13cm
survey[['HH']]$N[, as.numeric(gsub('.*?([0-9]+).*', '\\1', colnames(survey[['HH']]$N)))<size_threshold] <- 0 #97.5% of Landings above 
setDT(survey[['HH']])

survey[['HH']]$HaulWgt_old <- survey[['HH']]$HaulWgt
survey <- addWeightByHaul(survey, to1min = FALSE) # warning it is estimated from Length weight relation ships

survey[['HH']]$nums_old <- survey[['HH']]$nums
survey[['HH']]$nums <- apply(survey[['HH']]$N,1,sum) 

# prepare data to trick surveyIndex package to run deltaGAM with no age structure
Nage <- as.matrix(survey[['HH']][,c('HaulWgt','nums')], rownames = rownames(survey[['HH']]$N))
colnames(Nage) <- c('0', '1') #with 0 as weight and 1 abundance
setDF(survey[['HH']])
setDF(survey[['HL']])
setDF(survey[['CA']])

# # convert all factors into characters
# survey[['CA']] <- survey[['CA']]  %>%
#   mutate_if(is.factor, as.character)
# survey[['HL']] <- survey[['HL']]  %>%
#   mutate_if(is.factor, as.character)
# survey[['HH']][!names(survey[['HH']]) %in% c("N", "Nold")] <- survey[['HH']][!names(survey[['HH']]) %in% c("N", "Nold")]  %>%
#   mutate_if(is.factor, as.character)

survey[['HH']]$NageBN <- Nage

survey[['HH']]$nums[survey[['HH']]$nums==0] <- NA

write.taf(survey[['HH']][,c('haul.id','lat','lon','Year','nums')], file=paste0('BTSQ3_nums_1990_', wg_year-1, '_final.csv'), dir='data')

survey_BTSQ3 <- survey

save(survey_BTSQ3, file=paste0('data/BTSQ3_MUR', wg_year, '.Rdata'), compress='xz')
