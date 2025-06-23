## Prepare plots and tables for report

## Before:
## After:

library(TAF)
library(ggplot2)
library(rnaturalearth)
library(doParallel)
library(data.table)
# library(rgdal)
library(radiant.data) 
library(concaveman)
# library(rgeos)
library(sp)
library(sf)
taf.library(DATRAS)
taf.library(surveyIndex)
library(statmod)
library(labeling)
library(farver)

mkdir("report")
source('utilities.R')

save.plot <- T
wg_year <- 2025
#### 1. INPUT ####
# plot raw data abundance distribution by survey
spdf_NS <- ne_countries(scale = 10, continent = c('Europe'), returnclass = 'sf')

##### 1.1. IBTS Q3 #### 
dat <- read.taf(paste0('data/IBTSQ3_nums_1991_', wg_year-1,'.csv'))

if(save.plot)   taf.png(file.path(paste0('report/IBTSQ3_nums_1991_', wg_year-1)))
ggplot(data = spdf_NS) + geom_sf() + coord_sf(xlim = c(-5,15), ylim = c(49,62), expand = FALSE) + geom_point(data=dat, aes(x = lon, y = lat, shape = 4), size = 0.001, color = 'red') + scale_shape_identity() + geom_point(data=dat, aes(x = lon, y = lat, size = nums)) + facet_wrap(~Year, ncol = 8) + theme_bw()  #+ 
if(save.plot)   dev.off()

# explore distribution spread
# explore lat/lon change in distribution
# probably need to redefine area or year maybe start in 1990
dys <- split(dat, dat$Year)

stat_dys <- foreach(i=dys, .errorhandling = "pass") %dopar% {
  map_stat(i)
}

convex_map <- foreach(i=dys, .errorhandling = "pass") %dopar% {
  map_stat(i,pHull = T)$convex
}

concav_map <- foreach(i=dys, .errorhandling = "pass") %dopar% {
  map_stat(i,pHull = T)$concav
}

survey_map_stat <- rbindlist(stat_dys)
survey_map_stat$Year <- as.numeric(names(dys))

if(save.plot)   taf.png(file.path(paste0('report/IBTSQ3_lat_1991_', wg_year-1)))
plot(survey_map_stat[,c('Year','lat')], type = "l", ylim = c(min(survey_map_stat$lat_lo),max(survey_map_stat$lat_up)))
points(survey_map_stat[,c('Year','lat_up')], type = "l", lty=2)
points(survey_map_stat[,c('Year','lat_lo')], type = "l", lty=2)
dev.off()
if(save.plot)   taf.png(file.path(paste0('report/IBTSQ3_lon_1991_', wg_year-1)))
plot(survey_map_stat[,c('Year','lon')], type = "l", ylim = c(min(survey_map_stat$lon_lo),max(survey_map_stat$lon_up)))
points(survey_map_stat[,c('Year','lon_up')], type = "l", lty=2)
points(survey_map_stat[,c('Year','lon_lo')], type = "l", lty=2)
dev.off()

no_obs<- which(!is.na(concav_map))
concav_map <- do.call("rbind",
                      c(args = concav_map[no_obs], makeUniqueIDs = TRUE))
concav_map <- sp::spChFIDs(concav_map , names(dys[no_obs]))
concav_map <- st_as_sf(concav_map)
concav_map$Year <- names(dys[no_obs])

if(save.plot)   taf.png(file.path(paste0('report/IBTSQ3_mapsurf_1991_', wg_year-1)))
ggplot(data = concav_map) + geom_sf(fill='red', col="red") + 
  geom_sf(data = spdf_NS) + coord_sf(xlim = c(-5,15), ylim = c(51,60), expand = FALSE) + 
  geom_point(data = survey_map_stat, aes(x=lon,y=lat), col = "black") + 
  facet_wrap(~Year) + theme_bw()
dev.off()

if(save.plot)   taf.png(file.path(paste0('report/IBTSQ3_surf_1991_', wg_year-1)))
plot(survey_map_stat[,c('Year','concav_surf')], type = "l")
dev.off()

#trim northern part of NS
if(save.plot)   taf.png(file.path(paste0('report/IBTSQ3_nums_1991_', wg_year-1,'_trim')))
data_line <- data.frame(x=c(-5, 15),y=c(54, 60))
ggplot(data = spdf_NS) + geom_sf() + geom_line(data=data_line, aes(x=x,y=y)) + coord_sf(xlim = c(-5,15), ylim = c(49,62), expand = FALSE) + geom_point(data=dat, aes(x = lon, y = lat, shape = 4), size = 0.01, color = 'red') + scale_shape_identity() + geom_point(data=dat, aes(x = lon, y = lat, size = nums)) + theme_bw()
if(save.plot)   dev.off()

##### 1.2. BTS Q3 ##### 
dat <- read.taf(paste0('data/BTSQ3_nums_1990_', wg_year-1,'.csv'))

if(save.plot)   taf.png(file.path(paste0('report/BTS_nums_1990_', wg_year-1,'.jpeg')))
# windows()
ggplot(data = spdf_NS) + geom_sf() + coord_sf(xlim = c(-5,15), ylim = c(49,62), expand = FALSE) + geom_point(data=dat, aes(x = lon, y = lat, shape = 4), size = 0.001, color = 'red') + scale_shape_identity() + geom_point(data=dat, aes(x = lon, y = lat, size = nums)) + facet_wrap(~Year, ncol = 8) + theme_bw() 
if(save.plot)   dev.off()

# explore lat/lon change in distribution
# probably need to redefine area or year maybe start in 1990
dys <- split(dat, dat$Year)

stat_dys <- foreach(i=dys, .errorhandling = "pass") %dopar% {
  map_stat(i)
}

convex_map <- foreach(i=dys, .errorhandling = "pass") %dopar% {
  map_stat(i,pHull = T)$convex
}

concav_map <- foreach(i=dys, .errorhandling = "pass") %dopar% {
  map_stat(i,pHull = T)$concav
}

survey_map_stat <- rbindlist(stat_dys)
survey_map_stat$Year <- as.numeric(names(dys))

if(save.plot)   taf.png(file.path(paste0('report/BTSQ3_lat_1990_', wg_year-1)))
plot(survey_map_stat[,c('Year','lat')], type = "l", ylim = c(min(survey_map_stat$lat_lo),max(survey_map_stat$lat_up)))
points(survey_map_stat[,c('Year','lat_up')], type = "l", lty=2)
points(survey_map_stat[,c('Year','lat_lo')], type = "l", lty=2)
dev.off()
if(save.plot)   taf.png(file.path(paste0('report/BTSQ3_lon_1990_', wg_year-1)))
plot(survey_map_stat[,c('Year','lon')], type = "l", ylim = c(min(survey_map_stat$lon_lo),max(survey_map_stat$lon_up)))
points(survey_map_stat[,c('Year','lon_up')], type = "l", lty=2)
points(survey_map_stat[,c('Year','lon_lo')], type = "l", lty=2)
dev.off()

no_obs<- which(!is.na(concav_map))
concav_map <- do.call("rbind",
                      c(args = concav_map[no_obs], makeUniqueIDs = TRUE))
concav_map <- sp::spChFIDs(concav_map , names(dys[no_obs]))
concav_map <- st_as_sf(concav_map)
concav_map$Year <- names(dys[no_obs])

if(save.plot)   taf.png(file.path(paste0('report/BTSQ3_mapsurf_1990_', wg_year-1)))
ggplot(data = concav_map) + geom_sf(fill='red', col="red") + 
  geom_sf(data = spdf_NS) + coord_sf(xlim = c(-5,15), ylim = c(49,62), expand = FALSE) + 
  geom_point(data = survey_map_stat, aes(x=lon,y=lat), col = "black") + 
  facet_wrap(~Year) + theme_bw()
dev.off()

if(save.plot)   taf.png(file.path(paste0('report/BTSQ3_surf_1990_', wg_year-1)))
plot(survey_map_stat[,c('Year','concav_surf')], type = "l")
dev.off()

if(save.plot)   taf.png(file.path(paste0('report/BTSQ3_nums_1990_', wg_year-1,'_trim')))
data_line <- data.frame(x=c(-5, 15),y=c(54, 60))
ggplot(data = spdf_NS) + geom_sf() + geom_line(data=data_line, aes(x=x,y=y)) + coord_sf(xlim = c(-5,15), ylim = c(49,62), expand = FALSE) + geom_point(data=dat, aes(x = lon, y = lat, shape = 4), size = 0.01, color = 'red') + scale_shape_identity() + geom_point(data=dat, aes(x = lon, y = lat, size = nums)) + theme_bw()  #+ 
if(save.plot)   dev.off()

##### 1.3. CGFS Q4 #####
dat <- read.taf(paste0('data/CGFSQ4_nums_1988_', wg_year-1,'.csv'))

if(save.plot)   taf.png(file.path(paste0('report/CGFS_nums_1988_', wg_year-1)))
ggplot(data = spdf_NS) + geom_sf() + coord_sf(xlim = c(-3,3), ylim = c(49,52), expand = FALSE) + geom_point(data=dat, aes(x = lon, y = lat, shape = 4), size = 0.01, color = 'red') + scale_shape_identity() + geom_point(data=dat, aes(x = lon, y = lat, size = nums)) + facet_wrap(~Year, ncol = 7) + theme_bw()
if(save.plot)   dev.off()

# explore lat/lon change in distribution
# probably need to redefine area or year maybe start in 1990
dys <- split(dat, dat$Year)

stat_dys <- foreach(i=dys, .errorhandling = "pass") %dopar% {
  map_stat(i)
}

convex_map <- foreach(i=dys, .errorhandling = "pass") %dopar% {
  map_stat(i,pHull = T)$convex
}

concav_map <- foreach(i=dys, .errorhandling = "pass") %dopar% {
  map_stat(i,pHull = T)$concav
}

survey_map_stat <- rbindlist(stat_dys)
survey_map_stat$Year <- as.numeric(names(dys))

if(save.plot)   taf.png(file.path(paste0('report/CGFS_lat_1988_', wg_year-1)))
plot(survey_map_stat[,c('Year','lat')], type = "l", ylim = c(min(survey_map_stat$lat_lo),max(survey_map_stat$lat_up)))
points(survey_map_stat[,c('Year','lat_up')], type = "l", lty=2)
points(survey_map_stat[,c('Year','lat_lo')], type = "l", lty=2)
if(save.plot)   dev.off()
if(save.plot)   taf.png(file.path(paste0('report/CGFS_lon_1988_', wg_year-1)))
plot(survey_map_stat[,c('Year','lon')], type = "l", ylim = c(min(survey_map_stat$lon_lo),max(survey_map_stat$lon_up)))
points(survey_map_stat[,c('Year','lon_up')], type = "l", lty=2)
points(survey_map_stat[,c('Year','lon_lo')], type = "l", lty=2)
if(save.plot)   dev.off()

no_obs<- which(!is.na(concav_map))
concav_map <- do.call("rbind",
                      c(args = concav_map[no_obs], makeUniqueIDs = TRUE))
concav_map <- sp::spChFIDs(concav_map , names(dys[no_obs]))
concav_map <- st_as_sf(concav_map)
concav_map$Year <- names(dys[no_obs])

if(save.plot)   taf.png(file.path(paste0('report/CGFS_mapsurf_1988_', wg_year-1)))
ggplot(data = concav_map) + geom_sf(fill='red', col="red") + 
  geom_sf(data = spdf_NS) + coord_sf(xlim = c(-3,3), ylim = c(49,52), expand = FALSE) + 
  geom_point(data = survey_map_stat, aes(x=lon,y=lat), col = "black") + 
  facet_wrap(~Year) + theme_bw()
if(save.plot)   dev.off()

if(save.plot)   taf.png(file.path(paste0('report/CGFS_surf_1988_', wg_year-1)))
plot(survey_map_stat[,c('Year','concav_surf')], type = "l")
dev.off()

# compare Thalassa sampling with Gwen Drez
load(paste0('data/CGFSQ4_ThalassaVSGwen', wg_year-1,'.Rdata'))

if(save.plot)   taf.png(file.path(paste0('report/CGFS_1988_', wg_year-1)))
plot(survey)
plot(subset(survey, as.numeric(as.character(Year)) > 2014), col="#E69F00", plot.response =F, add=T)
plot(subset(survey, Year == "2020"), col =  "#0072B2", plot.response =F, add = T)
plot(subset(survey, Year == as.character(wg_year-1)), col =  "#D55E00", plot.response =F, add = T)
legend(x=0.94, y = 49.5, c("Gwen Drez 1988-2014",paste0("Thalassa 2015-", wg_year-1),"Thalassa 2020", paste0("Thalassa ",wg_year-1)),pch=16, col=c("black","#E69F00", "#0072B2","#D55E00"),bty="n")
if(save.plot)   dev.off()

#selectivity survey versus com. landings
# IBTS Q3
dat <- read.taf(paste0('data/IBTSQ3_selratio_1991_', wg_year-1,'.csv'))
size_threshold <- dat$size[which.min(abs(dat$cumprop-0.025))] # 97.5% Landings above that size here 13cm

# plot model
if(save.plot)   taf.png(file.path(paste0('report/IBTSQ3_modselratio_1991_', wg_year-1)))
plot(data = dat, log_ratio~size, main = 'Commercial vs IBTS Q3')
points(data = dat, pred~size, type = 'l')
if(save.plot) dev.off()

#plot selectivity 
if(save.plot)   taf.png(file.path(paste0('report/IBTSQ3_sel_1991_', wg_year-1)))
plot(data =dat, sel_surv/sum(sel_surv)~size, type ='l', ylim = c(0,max(dat$sel_surv_mod/sum(dat$sel_surv_mod))))
points(data =dat, sel_surv_mod/sum(sel_surv_mod)~size, type ='l', col = 'red')
points(data =dat, sel_L/sum(sel_L, na.rm=T)~size, type ='l', col = 'grey')
abline(v=size_threshold,lty=2)
legend(250, 0.17, legend = c("survey", "survey_mod", "commercial"), col = c('black','red','grey'), lty=1, bty = 'n')
dev.off()

if(save.plot)   taf.png(file.path(paste0('report/IBTSQ3_cumsel_1991_', wg_year-1)))
plot(data =dat, cumsum(sel_surv)/sum(sel_surv)~size, type ='l')
points(data =dat, cumsum(sel_surv_mod)/sum(sel_surv_mod)~size, type ='l', col = 'red')
points(data =dat, cumsum(sel_L)/sum(sel_L,na.rm=T)~size, type ='l', col = 'grey')
legend(250, 0.6, legend = c("survey", "survey_mod", "commercial"), col = c('black','red','grey'), lty=1, bty = 'n')
dev.off()

# BTS Q3
dat <- read.taf(paste0('data/BTSQ3_selratio_1990_', wg_year-1,'.csv'))
size_threshold <- dat$size[which.min(abs(dat$cumprop-0.025))] # 97.5% Landings above that size here 13cm

# plot model
if(save.plot)   taf.png(file.path(paste0('report/BTSQ3_modselratio_1990_', wg_year-1)))
plot(data = dat, log_ratio~size, main = 'Commercial vs BTS Q3')
points(data = dat, pred~size, type = 'l')
if(save.plot) dev.off()

#plot selectivity 
if(save.plot)   taf.png(file.path(paste0('report/BTSQ3_sel_1990_', wg_year-1)))
plot(data =dat, sel_surv/sum(sel_surv)~size, type ='l')
points(data =dat, sel_surv_mod/sum(sel_surv_mod)~size, type ='l', col = 'red')
points(data =dat, sel_L/sum(sel_L, na.rm=T)~size, type ='l', col = 'grey')
abline(v=size_threshold,lty=2)
legend(250, 0.17, legend = c("survey", "survey_mod", "commercial"), col = c('black','red','grey'), lty=1, bty = 'n')
dev.off()

if(save.plot)   taf.png(file.path(paste0('report/BTSQ3_cumsel_1990_', wg_year-1)))
plot(data =dat, cumsum(sel_surv)/sum(sel_surv)~size, type ='l')
points(data =dat, cumsum(sel_surv_mod)/sum(sel_surv_mod)~size, type ='l', col = 'red')
points(data =dat, cumsum(sel_L)/sum(sel_L,na.rm=T)~size, type ='l', col = 'grey')
legend(250, 0.6, legend = c("survey", "survey_mod", "commercial"), col = c('black','red','grey'), lty=1, bty = 'n')
dev.off()

# CGFS Q4
dat <- read.taf(paste0('data/CGFS_selratio_1989_', wg_year-1,'.csv'))
size_threshold <- dat$size[which.min(abs(dat$cumprop-0.025))] # 97.5% Landings above that size here 13cm

# plot model
if(save.plot)   taf.png(file.path(paste0('report/CGFS_modselratio_1989_', wg_year-1)))
plot(data = dat, log_ratio~size, main = 'Commercial vs CGFS Q4')
points(data = dat, pred~size, type = 'l')
if(save.plot)   dev.off()

#plot selectivity 
if(save.plot)   taf.png(file.path(paste0('report/CGFS_sel_1989_', wg_year-1)))
plot(data =dat, sel_surv/sum(sel_surv)~size, type ='l', ylim = c(0,max(dat$sel_L/sum(dat$sel_L,na.rm=T),na.rm=T)))
points(data =dat, sel_surv_mod/sum(sel_surv_mod)~size, type ='l', col = 'red')
points(data =dat, sel_L/sum(sel_L, na.rm=T)~size, type ='l', col = 'grey')
abline(v=size_threshold,lty=2)
legend(250, 0.17, legend = c("survey", "survey_mod", "commercial"), col = c('black','red','grey'), lty=1, bty = 'n')
if(save.plot) dev.off()

if(save.plot)   taf.png(file.path(paste0('report/CGFS_cumsel_1989_', wg_year-1)))
plot(data =dat, cumsum(sel_surv)/sum(sel_surv)~size, type ='l')
points(data =dat, cumsum(sel_surv_mod)/sum(sel_surv_mod)~size, type ='l', col = 'red')
points(data =dat, cumsum(sel_L)/sum(sel_L,na.rm=T)~size, type ='l', col = 'grey')
if(save.plot) dev.off()

#final survey exploitable abundance distribution
# IBTS Q3
# Figure 19.5.4.2.
dat <- read.taf(paste0('data/IBTSQ3_nums_1991_',wg_year-1,'_final.csv'))
if(save.plot)   taf.png(file.path(paste0('report/Figure.18.5.4.2.IBTSQ3_nums_1991_',wg_year-1,'selL')))
ggplot(data = spdf_NS) + geom_sf() + coord_sf(xlim = c(-5,15), ylim = c(49,62), expand = FALSE) + geom_point(data=dat, aes(x = lon, y = lat, shape = 4), size = 0.01, color = 'red') + scale_shape_identity() + geom_point(data=dat, aes(x = lon, y = lat, size = nums)) + facet_wrap(~Year, ncol = 8) + theme_bw()
if(save.plot)   dev.off()

# BTS Q3
# Figure 19.5.4.1.
dat <- read.taf(paste0('data/BTSQ3_nums_1990_',wg_year-1,'_final.csv'))
if(save.plot)   taf.png(file.path(paste0('report/Figure.18.5.4.1.BTSQ3_nums_1990_',wg_year-1,'selL')))
ggplot(data = spdf_NS) + geom_sf() + coord_sf(xlim = c(-5,15), ylim = c(49,62), expand = FALSE) + geom_point(data=dat, aes(x = lon, y = lat, shape = 4), size = 0.01, color = 'red') + scale_shape_identity() + geom_point(data=dat, aes(x = lon, y = lat, size = nums)) + facet_wrap(~Year, ncol = 8) + theme_bw()
if(save.plot)   dev.off()

# CGFS Q4
# Figure 19.5.4.3. 
dat <- read.taf(paste0('data/CGFS_nums_1989_',wg_year-1,'_final.csv'))
if(save.plot)   taf.png(file.path(paste0('report/Figure.18.5.4.3.CGFS_nums_1989_',wg_year-1,'selL')))
ggplot(data = spdf_NS) + geom_sf() + coord_sf(xlim = c(-3,3), ylim = c(49,52), expand = FALSE) + geom_point(data=dat, aes(x = lon, y = lat, shape = 4), size = 0.01, color = 'red') + scale_shape_identity() + geom_point(data=dat, aes(x = lon, y = lat, size = nums)) + facet_wrap(~Year, ncol = 8) + theme_bw() 
if(save.plot)   dev.off()

#### CHECK MODEL ####
load(file=paste0("model/surveyQ34_MUR_GAM_Cship_Gear_Tyd_D_spY",wg_year,".Rdata"))
taf.png(file.path('report/resid_qqplot_tw'))
qqnorm(surveyIndex::qres.tweedie(gam_Cship_Gear_timeYD_Depth_spy$pModels[[1]]))
qqline(surveyIndex::qres.tweedie(gam_Cship_Gear_timeYD_Depth_spy$pModels[[1]]))
dev.off()

# check temporal autocorrelation
load(file="output/residuals.Rdata")

tsres <- data.frame(year = ymd(resid_tw$Year, truncated = 2L), res = resid_tw$res, row.names = NULL)

taf.png(file.path('report/resid_acf_tw'))
acf(tsres[,2], main = paste("Autocorrelation : Tweedie model"),max.mfrow = 1, ylim =c(-0.1,0.2))
dev.off()

# check residuals against covariates
# Figure 19.5.4.4 
taf.png(file.path('report/Figure.18.5.4.4.resid_year_tw'))
boxplot(res~Year, resid_tw, main = paste("Residual vs year"), las =2)
abline(h = 0, col = "red")
dev.off()

taf.png(file.path('report/Figure.18.5.4.4.resid_CShip_tw'))
boxplot(res~CShip, resid_tw, las =2, ann = F)
abline(h = 0, col = "red")
title(main = paste("Residual vs Ship-Country"))
dev.off()

taf.png(file.path('report/Figure.18.5.4.4.resid_Gear_tw'))
boxplot(res~Gear, resid_tw, main = paste("Residual vs Gear"), las =2)
abline(h = 0, col = "red")
dev.off()

taf.png(file.path('report/Figure.18.5.4.4.resid_timeofYear_tw'))
plot(res~timeOfYear, resid_tw, main = paste("Residual vs timeOfYear"))
abline(h = 0, col = "red")
dev.off()

taf.png(file.path('report/Figure.18.5.4.4.resid_TimeShotHour_tw'))
plot(res~TimeShotHour, resid_tw, main = paste("Residual vs TimeShotHour"))
abline(h = 0, col = "red")
dev.off()

taf.png(file.path('report/Figure.18.5.4.4.resid_Depth_tw'))
plot(res~Depth, resid_tw, main = paste("Residual vs Depth"))
abline(h = 0, col = "red")
dev.off()

# check spatial distribution of High residuals
dat <- subset(resid_tw, abs(res)>3)

ggplot(data = spdf_NS) + geom_sf() + coord_sf(xlim = c(-5,15), ylim = c(49,62), expand = FALSE) + 
  geom_point(data=resid_tw, aes(x = lon, y = lat, shape = 4), size = 0.01, color = 'red') + 
  scale_shape_identity() + geom_point(data=dat, aes(x = lon, y = lat, size = abs(res))) + 
  facet_wrap(~Year, ncol = 8) + theme_bw()


#load survey input data 
load(paste0("data/surveyQ34_MUR",wg_year,".Rdata"))

for(y in unique(gam_Cship_Gear_timeYD_Depth_spy$yearNum)){
  taf.png(paste("gam_residuals_map",y,".png", sep = "_"))
  surveyIdxPlots(gam_Cship_Gear_timeYD_Depth_spy, survey, cols=1, myids=grid[[3]], par=list(mfrow=c(1,1)),
                 select=c("spatialResiduals"), plotByAge=FALSE, year=y, map.cex=0.5)
  dev.off()
  print(y)
}

taf.png("report/maps_tw.png")
surveyIdxPlots(gam_Cship_Gear_timeYD_Depth_spy, survey, cols=1, myids = grid[[3]],year = c(1991:2010), select = "absolutemap", legend =F, par = list(mfrow = c(4, 5), mar = c(0.1,0.1,2,0.1)))
dev.off()
taf.png("report/maps_tw2.png")
surveyIdxPlots(gam_Cship_Gear_timeYD_Depth_spy, survey, cols=1, myids = grid[[3]],year = c(2011:(wg_year-1)), select = "absolutemap", legend =F, par = list(mfrow = c(4, 5), mar = c(0.1,0.1,2,0.1)))
dev.off()

# plot index
idx <- read.taf(file = paste0("output/GAM_MURQ34_indices1991-",wg_year-1,"_tw.csv"))
idx$family <- "tweedie"
idx$Year <- as.numeric(idx$Year)

cbPalette <- c("#999999", NA)
cbbPalette <- c("#000000", "#009E73")

# Figure 19.5.4.5
taf.png("report/Figure.18.5.4.5.surveyQ34_index.png")
ggplot(data=idx , aes(x = Year, y = index, ymin = lo, ymax = up, color = family, fill = family)) + 
  geom_line() + geom_ribbon(alpha = 0.2, colour = NA)  + 
  geom_line(data=idx, aes(x = as.numeric(Year), y = lo), linetype = "dashed") +
  geom_line(data=idx, aes(x = as.numeric(Year), y = up), linetype = "dashed") +
  theme_bw() + 
  scale_fill_manual(values = cbPalette) + scale_color_manual(values = cbbPalette)
dev.off()

# index table
# Table 19.5.4.1. 
table_summary <- idx[,c("Year", "lo", "index", "up", "CV")] %>% 
  mutate(lo = round(lo/1000,2), index = round(index/1000,2), up = round(up/1000,2), CV = round(CV,3)) %>%
  rename('Low' = 'lo', 'Value' = 'index', 'High' = 'up')

write.taf(table_summary, "report/Table.18.5.4.1.surveyQ34_index.csv")

# plot retro
cbPalette <- c(NA, NA, NA, NA, NA, "#999999")
cbbPalette <- c("#0072B2", "#F0E442", "#009E73", "#CC79A7", "#E69F00","#000000")

idx_tw_retro <- read.taf(file = paste0("output/GAM_MURQ34_indices1991-",wg_year-1,"_retro_tw.csv"))
idx_tw_retro$retro <- as.character(idx_tw_retro$retro)

retro_sum <- read.taf(file = "output/GAM_MURQ34_retro.csv")

# Figure 19.5.4.7 
taf.png("report/Figure.18.5.4.7.surveyQ34_index_tw_retro.png")
ggplot(data=idx_tw_retro , aes(x = Year, y = index, ymin = lo, ymax = up, fill = retro, color = retro)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.2, colour = NA) + 
  annotate("text", x=1993, y=900000, label= paste("MohnRho: ", round(retro_sum$mRho_Biom[retro_sum$model == "gam_Cship_Gear_timeYD_Depth_spy"], 3))) + 
  theme_bw() + scale_fill_manual(values = cbPalette) + scale_color_manual(values = cbbPalette)
dev.off()

# plot loo
loo_tw <- read.taf(file = paste0("output/GAM_MURQ34_indices1991-",wg_year-1,"_loo_tw.csv"))

# Figure 19.5.4.6
taf.png("report/Figure.18.5.4.6.surveyQ34_index_tw_loo_std.png")
ggplot(data=loo_tw, aes(x = Year, y = index_std, ymin = lo_std, ymax = up_std, fill = survey, color = survey)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.2, linetype = 2, linewidth = 0.2) + 
  theme_bw() + scale_fill_manual(values = cbPalette) + scale_color_manual(values = c("#000000",cbbPalette[1:3]))
dev.off()

loo_tw_all <- subset(loo_tw, survey=="all")
loo_tw_all <- merge(loo_tw,loo_tw_all, by = c("Year", "age"))

loo_tw_all$index_deviation <-(loo_tw_all$index_std.x - loo_tw_all$index_std.y)/loo_tw_all$index_std.y
loo_tw_all$lo_std <-(loo_tw_all$lo_std.y - loo_tw_all$index_std.y)/loo_tw_all$index_std.y
loo_tw_all$up_std <-(loo_tw_all$up_std.y - loo_tw_all$index_std.y)/loo_tw_all$index_std.y
loo_tw_all$survey <-loo_tw_all$survey.x

taf.png("report/Figure.18.5.4.6.surveyQ34_index_tw_loo_std_deviation.png")
ggplot(data=subset(loo_tw_all, survey.x!="all") , aes(x = Year, y = index_deviation, fill = survey, color = survey)) + 
  geom_bar(stat="identity", position = "dodge") +
  geom_line(aes(y = lo_std), linetype = 2, col = "black") + geom_line(aes(y = up_std), linetype = 2, col = "black") +
  geom_hline(yintercept=0) +
  theme_bw() + scale_fill_manual(values = cbbPalette) + scale_color_manual(values = cbbPalette)
dev.off()

taf.png("report/surveyQ34_index_tw_loo.png")
ggplot(data=loo_tw , aes(x = Year, y = index, ymin = lo, ymax = up, fill = survey, color = survey)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.2, linetype = 2, linewidth = 0.2) + 
  theme_bw() + scale_fill_manual(values = rep(NA,6)) + scale_color_manual(values = cbbPalette)
dev.off()

# #plot sensitivity to lack of UK information (UK 6 miles CGFS Q4 and UKBTS in 7d)
# sensi.idx <- read.taf("output/GAM_MURQ34_indices1991-2021_UK6miles_sensi2021_tw.csv")
# cbPalette <- c(rep(NA, length(unique(sensi.idx$year_wouk))-1), "#999999")
# cbbPalette <- c(rep("#009E73", length(unique(sensi.idx$year_wouk))-1), "#000000")
# 
# # Figure 19.5.4.8 
# taf.png("gam_UK6miles_UKBTS_sensi2021.png")
# if(idx.std) ggplot(data=sensi.idx , aes(x = Year, y = std.idx, ymin = std.lo, ymax = std.up, fill = year_wouk, color = year_wouk)) + geom_line() + geom_ribbon(alpha = 0.2, colour = NA) + theme_bw() + scale_fill_manual(values = cbPalette) + scale_color_manual(values = cbbPalette)
# ggplot(data=sensi.idx , aes(x = Year, y = index, ymin = lo, ymax = up, fill = year_wouk, color = year_wouk)) + geom_line() + geom_ribbon(alpha = 0.2, colour = NA) + theme_bw() + scale_fill_manual(values = cbPalette) + scale_color_manual(values = cbbPalette)
# dev.off()
# 
# # Table 19.5.4.2.
# 
# table_summary_woUK <- sensi.idx %>% filter(year_wouk=="2021") %>% 
#   mutate(lo = round(lo/1000,2), index = round(index/1000,2), up = round(up/1000,2), CV = round(CV,3)) %>%
#   rename('Low' = 'lo', 'Value' = 'index', 'High' = 'up')
# 
# table_summary_woUK <- table_summary_woUK[,c("Year", "Low", "Value", "High", "CV")]
# names(table_summary_woUK) <- paste(names(table_summary_woUK), "woUK", sep = ".")
# table_summary <- cbind(table_summary_woUK[,-1],table_summary[-nrow(table_summary),])
# 
# write.taf(table_summary, "report/surveyQ34_index_sensitivitywoUK.csv")
# 
