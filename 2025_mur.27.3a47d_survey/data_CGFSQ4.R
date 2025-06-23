## Preprocess data, write TAF data tables

## Before: CGFS Q4 DATRAS data in exchange format and LFD from IC
## After: CGFS Q4 in a DATRAS raw object containing LFD par haul, total 
## abundance and biomass as well as exploitable abundance and biomass per Haul

#extract CGFS data from DATRAS 
# from datras directly: 
# yrs <- c(1988:2021)
# qrs <- 4
# survey <- getDatrasExchange('FR-CGFS', years=yrs, quarters=qrs, strict=FALSE)

# or Exchange files dowlaoded from DATRAS: http://datras.ices.dk/Data_products/Download/Download_Data_public.aspx 
# survey <- readExchange('boot/data/Exchange Data_2023-03-21 10_33_45_CGFSQ4.zip', strict=FALSE) # don't care about sample location in CA here
load('boot/data/CGFS_rawDATRAS_WGNSSK2024.Rdata')

# from datras directly: 

survey_new <- readExchange("boot/data/CGFS2024_Unaggregated trawl and biological information_2025-02-05 10_27_07.zip")

# combine dataset from last year with 2023 data
survey[['HH']] <- rbind(survey[['HH']],survey_new[['HH']])
survey[['HL']] <- rbind(survey[['HL']],survey_new[['HL']])
survey[['CA']] <- rbind(survey[['CA']],survey_new[['CA']])

save(survey, file = "data/CGFS_rawDATRAS_WGNSSK2025.Rdata")

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

#transform length to cm
survey[['HL']]$LngtCode <- '1'
survey[['HL']]$LngtClas <- survey[['HL']]$LngtClas/10  # only have 1 cm class information 

# REMOVE if HaulDur < 10 min or if above 39 min 
survey <- subset(survey, HaulDur > 9)
survey <- subset(survey, HaulDur < 40)

# REMOVE night hauls?
survey <- subset(survey, DayNight == 'D')

## check issue of reporting Units of catch (by 100g back in the past cf IBTSWG 2022 report p12)
survey <- addSpectrum(survey)
survey <- addWeightByHaul(survey, to1min = FALSE) # warning it is estimated from Length weight relation ships

# GO for estimated catches at the moment see later if only use it before 2003 or if more refine

# clean up CGFS data, error with some haul location in 2012
# import Channel and North sea map GIS
spdf_NS <- ne_countries(scale = 10, continent = c('Europe'), returnclass = 'sf')

survey_HH <- as.data.frame(survey[['HH']])
survey_HH <- SpatialPointsDataFrame(survey_HH[,c('ShootLong','ShootLat')], survey_HH[, !names(survey_HH) %in% c('ShootLong','ShootLat')])

#attribute the Geographic projection (if in meters) or the Geographic coordinate system (if in Lat/Lon) of the data
proj4string(survey_HH) <- CRS('+init=epsg:4326') #Lat/Lon WGS84
test <- as(spdf_NS,'Spatial')
proj4string(test) <- CRS('+init=epsg:4326') #Lat/Lon WGS84

# get station located on land
# survey_HH[which(!is.na(over(survey_HH,test)$ne_id)),]

# only issue with longitude sign for station sampled on the 17/10/2012
survey[['HH']]$ShootLong[which(!is.na(over(survey_HH,test)$ne_id))] <- -survey[['HH']]$ShootLong[which(!is.na(over(survey_HH,test)$ne_id))] 
survey[['HH']]$HaulLong[which(!is.na(over(survey_HH,test)$ne_id))] <- -survey[['HH']]$HaulLong[which(!is.na(over(survey_HH,test)$ne_id))] 
survey[['HH']]$lon[which(!is.na(over(survey_HH,test)$ne_id))] <- -survey[['HH']]$lon[which(!is.na(over(survey_HH,test)$ne_id))] 

# plot abundance
survey[['HH']]$nums <- apply(survey[['HH']]$N,1,sum) 
survey[['HH']]$nums[survey[['HH']]$nums==0] <- NA

write.taf(survey[['HH']][,c('haul.id','lat','lon','Year','nums')], file=paste0('CGFSQ4_nums_1988_', wg_year-1, '.csv'), dir='data')

# remove data prior to 1989
survey <- subset(survey, as.numeric(as.character(Year)) >= 1989) #no catch before

save(survey,  file=paste0('data/CGFSQ4_ThalassaVSGwen', wg_year-1,'.Rdata'))
# REMOVE Seine Estuary (27F0) not sampled by the vessel 'Thalassa' after 2014

survey <- subset(survey, !StatRec == '27F0')

# Calculate SweptArea 
## missing DoorSpread for Thalassa vessel (same as IBTS Q1) use IBTSWG algorithm

#Thalassa
cond <- is.na(survey[['HH']]$DoorSpread) & survey[['HH']]$Country=='FR' & survey[['HH']]$Ship == '35HT' &
  !is.na(survey[['HH']]$Depth)

survey[['HH']][cond, DoorSpread := -1.31 + 15.58 * log(Depth)]

# # build a model for Gwen Drez based on Thalassa gear?
# model <- lm(DoorSpread ~ WingSpread + Warplngt, data = subset(as.data.frame(survey[['HH']]),Ship == '35HT'))
# summary(model) # too tricky keep the target for Gwen Drez'
# # plot(model)

# Missing WingSpread for Ship 35GD : Gwen Drez based on historical protocol
survey[['HH']][Ship=='35GD','WingSpread'] <- 10
survey[['HH']][Ship=='35GD','DoorSpread'] <- 40

# summary(survey[['HH']]$DoorSpread)
# summary(survey[['HH']]$Distance)
# boxplot(survey[['HH']]$Distance)

## Add Swept area
# ADD SweptArea
survey[['HH']][, SweptArea := Distance * DoorSpread * 10^(-6)] # door spread used for red mullet as considered as herding species, otherwise should use door spread

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
write.taf(sel, file=paste0('CGFS_selratio_1989_', wg_year-1, '.csv'), dir='data')

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
survey[['HH']]$NageBN <- Nage

survey[['HH']]$nums[survey[['HH']]$nums==0] <- NA

write.taf(survey[['HH']][,c('haul.id','lat','lon','Year','nums')], file=paste0('CGFS_nums_1989_', wg_year-1, '_final.csv'), dir='data')

# Allocate station to EEZ
taf.unzip("boot/data/World_EEZ_v6.1_20110512.zip", exdir="boot/data")
EEZ <-  sf::st_read("boot/data/World_EEZ_v6_1_20110512.shp")
EEZ <- as(EEZ,'Spatial')
proj4string(EEZ)
EEZ <- spTransform(EEZ, CRS("+init=epsg:4326")) #Lat/Lon WGS84 the same one as below

# add Country EEZ
# Transform dataframe into Spatialdataframe 
hh <- as.data.frame(survey[["HH"]])
hh <- SpatialPointsDataFrame(hh[,c("lon","lat")], hh[, !names(hh) %in% c("lon","lat")])

#attribute the Geographic projection (if in meters) or the Geographic coordinate system (if in Lat/Lon) of the data
proj4string(hh) <- CRS("+init=epsg:4326") #Lat/Lon WGS84

# associate a polygon to each sampling site. 
hh$EEZ <- over(hh, EEZ)$Sovereign
survey[['HH']]$EEZ <- hh$EEZ

# In 2022 UK 6 nautical miles were not sampled (no authorizations) test influence on model outcome

# build a buffer
p <- data.frame(lon = c(-2,-2,2.5,2.5,-2), lat = c(49,51.5,51.5,49,49))
p <- Polygons(list(Polygon(p)),1)
p <- SpatialPolygons(list(p))
proj4string(p) <- CRS("+init=epsg:4326") 

taf.unzip("boot/data/United_Kingdom_Hydrographic_Office_Maritime_Limits_and_Boundaries.zip", exdir="boot/data")
uk <- sf::st_read("boot/data/UK_Hydrographic_Office_Maritime_Limits_and_Boundaries.shp")
uk <- as(uk,'Spatial')
proj4string(uk) <- CRS("+init=epsg:4326") 

spdf_NS <- spdf_NS$geometry

p <- as(p,'sf')
# test <- as(test,'sf')
res <- st_difference(p, spdf_NS)

uk_6miles <- subset(uk,Nature.of.=="6NM")
uk_6miles <- as(uk_6miles,"sf")

# split bounding box by the UK 6NM
clip <- res %>% as("sf") %>%
  st_bbox() %>%
  st_as_sfc() %>%
  lwgeom::st_split(uk_6miles) %>% 
  st_collection_extract("POLYGON") %>% 
  st_as_sf() %>% 
  # calculate x coordinate of the centroid; east will be higher
  mutate(xpos = st_coordinates(st_centroid(.))[,"X"]) %>% 
  mutate(position = ifelse(xpos == max(xpos), "south", "north")) %>% 
  filter(position == "north") %>% as("Spatial")



# uk_6miles <- st_intersection(res, clip$x) 

# associate a polygon to each sampling site. 
hh$uk_6miles <- over(hh, clip)$position
survey[['HH']]$uk_6miles <- hh$uk_6miles

# # convert all factors into characters
# survey[['CA']] <- survey[['CA']]  %>%
#   mutate_if(is.factor, as.character)
# survey[['HL']] <- survey[['HL']]  %>%
#   mutate_if(is.factor, as.character)
# survey[['HH']][!names(survey[['HH']]) %in% c("N", "Nold", "NageBN")] <- survey[['HH']][!names(survey[['HH']]) %in% c("N", "Nold", "NageBN")]  %>%
#   mutate_if(is.factor, as.character)

survey_CGFSQ4 <- survey

save(survey_CGFSQ4, file=paste0('data/CGFSQ4_MUR', wg_year, '.Rdata'), compress='xz')
