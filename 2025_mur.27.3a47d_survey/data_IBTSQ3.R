## Preprocess data, write TAF data tables

## Before: IBTS Q3 DATRAS data in exchange format and LFD from IC 
## After: IBTS Q3 in a DATRAS raw object containing LFD par haul, total 
## abundance and biomass as well as exploitable abundance and biomass per Haul

#extract CGFS data from DATRAS 
# from datras directly: 
# yrs <- c(1991:2022)
# qrs <- 3
# survey <- getDatrasExchange('NS-IBTS', years=yrs, quarters=qrs, strict=FALSE)

# or Exchange files dowlaoded from DATRAS: http://datras.ices.dk/Data_products/Download/Download_Data_public.aspx 
# survey <- readExchange('boot/data/Exchange Data_2023-03-28 16_18_27_IBTSQ3.zip', strict=FALSE) # don't care about sample location in CA here
load('boot/data/IBTS_rawDATRAS_WGNSSK2024.Rdata')

# from datras directly: 

survey_new <- readExchange("boot/data/IBTS2024_Unaggregated trawl and biological information_2025-02-05 10_04_55.zip")

# combine dataset from last year with 2023 data
survey[['HH']] <- rbind(survey[['HH']],survey_new[['HH']])
survey[['HL']] <- rbind(survey[['HL']],survey_new[['HL']])
survey[['CA']] <- rbind(survey[['CA']],survey_new[['CA']])

save(survey, file = "data/IBTS_rawDATRAS_WGNSSK2025.Rdata")

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

## check issue of reporting Units of catch (by 100g back in the past cf IBTSWG 2022 report p12)
survey <- addSpectrum(survey)
survey <- addWeightByHaul(survey, to1min = FALSE) # warning it is estimated from Length weight relation ships

# extract data to plot abundance distribution
survey[['HH']]$nums <- apply(survey[['HH']]$N,1,sum) 
survey[['HH']]$nums[survey[['HH']]$nums==0] <- NA

write.taf(survey[['HH']][,c('haul.id','lat','lon','Year','nums')], file=paste0('IBTSQ3_nums_1991_', wg_year-1, '.csv'), dir='data')

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
# ggplot(data = spdf_NS) + geom_sf() + geom_line(data=data_line, aes(x=x,y=y)) + coord_sf(xlim = c(-5,15), ylim = c(49,62), expand = FALSE) + geom_point(data=survey[['HH']], aes(x = lon, y = lat, shape = 4), size = 0.01, color = 'red') + scale_shape_identity() + geom_point(data=survey[['HH']], aes(x = lon, y = lat, size = nums)) + theme_bw()  #+ 

# Get swept Area from DATRAS assessment output
taf.unzip('boot/data/SweptAreaAssessmentOutput_2023-03-28 16_22_43.zip', 
          files = "SweptAreaAssessmentOutput_2023-03-28 16_22_43.csv", 
          exdir = "boot/data/")
SweptArea <- read.table('boot/data/SweptAreaAssessmentOutput_2023-03-28 16_22_43.csv', sep = ',', dec = '.', header = T)
SweptArea$haul.id <- paste(SweptArea$Year, SweptArea$Quarter, SweptArea$Country, SweptArea$Ship, SweptArea$Gear, SweptArea$StNo, SweptArea$HaulNo, sep =':')
SweptArea <- SweptArea[,c('haul.id', 'SweptAreaDSKM2', 'SweptAreaWSKM2')]

survey[['HH']] <- left_join(survey[['HH']], SweptArea, by = 'haul.id')
# survey[['HH']]$SweptAreaDSKM2 <- survey[['HH']]$SweptAreaWSKM2 <- 0

# Calculate SweptArea
# check gear technical information and haul characteristics
# Deal with missing distance
survey[['HH']][is.na(Distance) & Distance<=0, Distance:=(HaulDur/60) * 1852 * GroundSpeed]

# Missing ground speed (Distance as calculated in DATRAS)

survey[['HH']][is.na(Distance) | Distance==0, Distance := 1.852 * 360 * 60/(2*pi) * 
                 acos(cos(radians(ShootLat))) *
                 cos(radians(HaulLat)) *
                 cos(radians(HaulLong) - radians(ShootLong)) +
                 sin(radians(ShootLat)) *
                 sin(radians(HaulLat))]

# summary(survey[['HH']]$Distance)
# boxplot(survey[['HH']]$Distance)

## Add Swept area
# ADD SweptArea
survey[['HH']][is.na(survey[['HH']]$SweptAreaDSKM2), SweptAreaDSKM2 := Distance * DoorSpread * 10^(-6)] # door spread used for red mullet as considered as herding species, otherwise should use wing spread
# survey[['HH']][survey[['HH']]$SweptAreaDSKM2==0, SweptAreaDSKM2 := Distance * DoorSpread * 10^(-6)] # door spread used for red mullet as considered as herding species, otherwise should use wing spread
# survey[['HH']]$SweptAreaDSKM2[survey[['HH']]$SweptAreaDSKM2==0] <- NA
# survey[['HH']]$SweptAreaWSKM2[survey[['HH']]$SweptAreaWSKM2==0] <- NA
survey[['HH']][, SweptArea := SweptAreaDSKM2] 

# summary(survey[['HH']]$SweptArea)
# boxplot(survey[['HH']]$SweptArea)

## check selectivity of survey
sel_surv <- data.frame(size = as.numeric(gsub('.*?([0-9]+).*', '\\1', colnames(survey[['HH']]$N)))*10 ,sel_surv = apply(survey[['HH']]$N,2,sum))

# check selectivity of commercial fleets
load('boot/data/IC_length_mur_wgnssk2022.rdata')

Lan_length <- IC$Undetermined_length
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
write.taf(sel, file=paste0('IBTSQ3_selratio_1991_', wg_year-1, '.csv'), dir='data')

# turn dataset back to data frame
survey[['HH']] <- as.data.frame(survey[['HH']])
survey[['HH']]$Nold <- survey[['HH']]$N 
# survey[['HH']]$N <- survey[['HH']]$N * t(array(sel$epred[!is.na(sel$sel_surv)], dim = dim(t(survey[['HH']]$N)))) # output too far from Landings so use fish from 12cm onward

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

write.taf(survey[['HH']][,c('haul.id','lat','lon','Year','nums')], file=paste0('IBTSQ3_nums_1991_', wg_year-1, '_final.csv'), dir='data')

survey_IBTSQ3 <- survey

save(survey_IBTSQ3, file=paste0('data/IBTSQ3_MUR', wg_year, '.Rdata'), compress='xz')
