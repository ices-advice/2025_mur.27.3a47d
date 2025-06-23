## Prepare plots and tables for report

## Before:
## After:

library(TAF)
library(ggplot2)
taf.library(FLEDA)
library(gsubfn)
taf.library(cat3advice)
library(dplyr)
library(reshape2)
library(FLCore)
taf.library(ggridges)
taf.library(labeling)
library(farver)

mkdir("report")
assess_year <- 2025

load(file = "model/Chr_advice_2025.Rdata")
source("utilities.R")

# Table with official landings
officialLandings <- read.taf(file =paste0("data/official_landings",assess_year,".csv"))

table_summary <- officialLandings %>% filter(Year>1974) %>% group_by(Year, Country) %>% 
  summarise(offLandings = round(sum(offLandings, na.rm =T),0)) %>% mutate(Year = as.factor(Year)) %>%
  dcast(Year ~ Country, value.var = "offLandings")  %>% 
  rename("Belgium" = "BE", "Denmark" = "DK", "France" = "FR", "Germany" = "DE", "Netherlands" = "NL", "UK" = "GB") 

table_summary[is.na(table_summary)] <- 0

table_summary$Total <- table_summary$Belgium + table_summary$Denmark +  table_summary$France + table_summary$Germany + 
  table_summary$Netherlands +  table_summary$UK   

# Table 18.5.1.1. 
write.taf(table_summary[,c("Year", "Belgium", "Denmark", "France", "Germany", "Netherlands", "UK", "Total")] , file =paste0("report/Table.18.5.1.1.official_landings_country",assess_year,".csv"))

table_summary <- officialLandings %>% filter(Year>1974) %>% 
  mutate(Area = gsubfn("27[.]3[.]a|27[.]4|27[.]4[.]a|27[.]4[.]b|27[.]4[.]c|27[.]7[.]d", 
                list("27.3.a"="3.a", "27.4.a"="4", "27.4.b"="4", "27.4.c"="4", "27.4"="4", "27.7.d"="7.d"), Area)) %>% 
  group_by(Year, Area) %>% 
  summarise(offLandings = round(sum(offLandings, na.rm =T),0)) %>% mutate(Year = as.factor(Year)) %>%
  dcast(Year ~ Area, value.var = "offLandings")  

table_summary[is.na(table_summary)] <- 0

table_summary$Total <- table_summary$`3.a` + table_summary$`4` +  table_summary$`7.d`

# Table 18.5.1.2. 
write.taf(table_summary , file =paste0("report/Table.18.5.1.2.official_landings_area",assess_year,".csv"))

#official landings
cbbPalette <- c("#000000", "#F0E442", "#009E73", "#CC79A7","#0072B2", "#E69F00")

#Figure 18.5.1.2.
taf.png("Figure.18.5.1.2.OfflandingArea.png")
p <- aggregate(offLandings~Year+Area, subset(officialLandings, Year >1974),sum)
ggplot(p, aes(x = as.numeric(Year), y = offLandings, group = Area, fill = Area)) + 
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks=seq(1974, max(officialLandings$Year), by = 2)) + scale_fill_manual(values = cbbPalette) +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust =0.5)) + xlab("Years") + ylab("Official landings (tonnes)")
dev.off()

taf.png("OfflandingAreaperc.png")
p <- aggregate(poffLandings~Year+Area, subset(officialLandings, Year >1974),sum)
ggplot(p, aes(x = as.numeric(Year), y = poffLandings, group = Area, fill = Area)) + 
  geom_bar(stat = "identity") + 
  scale_x_continuous(breaks=seq(1974, max(officialLandings$Year), by = 2)) + scale_fill_manual(values = cbbPalette) +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust =0.5)) 
dev.off()

cbbPalette <- c( "#009E73","#F0E442", "#000000", "#CC79A7","#0072B2", "#E69F00")

#Figure 18.5.1.1.
taf.png("Figure.18.5.1.1.OfflandingCountry.png")
p <- aggregate(offLandings~Year+Country,subset(officialLandings, Year >1974),sum)
ggplot(p, aes(x = as.numeric(Year), y = offLandings, group = Country, fill = Country)) + 
  geom_bar(stat = "identity") + 
  scale_x_continuous(breaks=seq(1974, max(officialLandings$Year), by = 2)) + scale_fill_manual(values = cbbPalette) +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust =0.5)) + xlab("Years") + ylab("Official landings (tonnes)")
dev.off()

taf.png("OfflandingCountryperc.png")
p <- aggregate(poffLandings~Year+Country, subset(officialLandings, Year >1974),sum)
ggplot(p, aes(x = as.numeric(Year), y = poffLandings, group = Country, fill = Country)) + 
  geom_bar(stat = "identity") + 
  scale_x_continuous(breaks=seq(1974, max(officialLandings$Year), by = 2))  + scale_fill_manual(values = cbbPalette)+
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust =0.5)) 
dev.off()

# Table 18.5.1.3
output_summary <- read.taf("output/output_summary.csv") 

table_summary <- officialLandings %>% filter(Year > 2003) %>% group_by(Year) %>% summarise(offLandings = sum(offLandings)) %>%
  rename('year'='Year')

table_summary <- output_summary %>% filter(year > 2003) %>% select(year, advice, landings) %>% full_join(table_summary) %>%
  mutate(`ICES Estimates` = round(landings,0), `Official landings` = round(offLandings,0), `Predicted catch corresp. to advice` = advice)
table_summary <- table_summary[,c(1,7,6,5)]

write.taf(table_summary , file =paste0("report/Table.18.5.1.3.official_ices_landings_advice",assess_year,".csv"))

# Table in section 18.6.1 # when don't give advice section 18.6.3

table_summary <- output_summary %>% rename('Year'='year') %>% filter(Year > 2003)

# WARNING change section names when don't give advice
write.taf(table_summary[, c('Year', 'Lc_year', 'Lc', 'Linf', 'LFeM', 'Lmean', 'indicator', "inv_indicator", 'index', 'landings', 'harvest_rate')] , file =paste0("report/Table.section.18.6.1.chr_rule",assess_year,".csv"))
# write.taf(table_summary[, c('Year', 'Lc_year', 'Lc', 'Linf', 'LFeM', 'Lmean', 'indicator', 'index', 'landings', 'harvest_rate')] , file =paste0("report/Table.section.18.6.3.chr_rule",assess_year,".csv"))

# IC data 
WtTot <- read.csv(file =paste0("data/IC_raw_age_all.csv"))
cbbPalette <- c("#009E73","#CC79A7","#F0E442","#0072B2", "#E69F00")

taf.png("IClandingGear.png")
p <- aggregate(CatchWt~Year+gear, subset(WtTot, CatchCat =="L"),sum)
ggplot(p, aes(x = as.numeric(Year), y = CatchWt, group = gear, fill = gear)) + 
  geom_bar(stat = "identity") + 
  scale_x_continuous(breaks=seq(2004, max(WtTot$Year), by = 2))  + scale_fill_manual(values = cbbPalette)+
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust =0.5)) 
dev.off()

# Figure 18.5.1.5.
taf.png("Figure.18.5.1.5.IClandingGearperc.png")
p <- aggregate(pCatchWt~Year+gear, subset(WtTot, CatchCat =="L"),sum)
ggplot(p, aes(x = as.numeric(Year), y = pCatchWt, group = gear, fill = gear)) + 
  geom_bar(stat = "identity") + 
  scale_x_continuous(breaks=seq(2004, max(WtTot$Year), by = 2))  + scale_fill_manual(values = cbbPalette)+
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust =0.5)) + ylab("Proportion of total landings") +
  xlab("Years")
dev.off()

cbbPalette <- c( "#009E73","#F0E442","#000000", "#CC79A7", "#0072B2", "#E69F00", "#828282")

taf.png("IClandingCountry.png")
p <- aggregate(CatchWt~Year+Country, subset(WtTot, CatchCat =="L"),sum)
ggplot(p, aes(x = as.numeric(Year), y = CatchWt, group = Country, fill = Country)) + 
  geom_bar(stat = "identity") + 
  scale_x_continuous(breaks=seq(2004, max(WtTot$Year), by = 2))  + scale_fill_manual(values = cbbPalette)+
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust =0.5)) 
dev.off()

# Figure 18.5.1.4.
taf.png("Figure.18.5.1.4.IClandingCountryperc.png")
p <- aggregate(pCatchWt~Year+Country, subset(WtTot, CatchCat =="L"),sum)
ggplot(p, aes(x = as.numeric(Year), y = pCatchWt, group = Country, fill = Country)) + 
  geom_bar(stat = "identity") + 
  scale_x_continuous(breaks=seq(2004, max(WtTot$Year), by = 2))  + scale_fill_manual(values = cbbPalette)+
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust =0.5)) + ylab("Proportion of total landings") +
  xlab("Years")
dev.off()

cbbPalette <- c( "#009E73","#F0E442", "#CC79A7", "#0072B2", "#E69F00", "#828282")

taf.png("IClandingSeasonperc.png")
p <- aggregate(pCatchWt~Year+Season, subset(WtTot, CatchCat =="L"),sum)
ggplot(p, aes(x = as.numeric(Year), y = pCatchWt, group = Season, fill = Season)) + 
  geom_bar(stat = "identity") + 
  scale_x_continuous(breaks=seq(2004, max(WtTot$Year), by = 2))  + scale_fill_manual(values = cbbPalette)+
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust =0.5))
dev.off()

taf.png("ICDiscardSeason.png")
p <- aggregate(CatchWt~Year+Season, subset(WtTot, CatchCat =="D"),sum)
ggplot(p, aes(x = as.numeric(Year), y = CatchWt, group = Season, fill = Season)) + 
  geom_bar(stat = "identity") + 
  scale_x_continuous(breaks=seq(2004, max(WtTot$Year), by = 2))  + scale_fill_manual(values = cbbPalette)+
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust =0.5))
dev.off()

#compare Official landings with IC Landings
#Figure 18.5.1.3
Landing_sum <- read.taf(file =paste0("data/IC_vs_official_landings.csv"))
taf.png("Figure.18.5.1.3.ICversusOffLandings.png")
p <- aggregate(Landing~Year+Type,Landing_sum,sum)
ggplot(p , aes(x = as.numeric(Year), y = Landing, group = Type, fill = Type)) + 
  geom_bar(stat = "identity", position="dodge") + 
  scale_x_continuous(breaks=seq(2004, max(Landing_sum$Year), by = 2))  + scale_fill_manual(values = cbbPalette[4:5])+
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust =0.5)) +ylab("Landings (tonnes)") + xlab("Years")
dev.off()

taf.png("ICversusOffLandingsCountry.png")
p <- aggregate(Landing~Year+Type+Country,Landing_sum,sum)
ggplot(p , aes(x = as.numeric(Year), y = Landing, group = Type, fill = Type)) + 
  geom_bar(stat = "identity", position="dodge") + 
  scale_x_continuous(breaks=seq(2004, max(Landing_sum$Year), by = 2))  + scale_fill_manual(values = cbbPalette[4:5])+
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust =0.5)) + facet_wrap(~Country, scales = "free")
dev.off()

#compare IC Landings from WGNSSK 2024 & 2025
Landing_sum2024 <- read.taf(file =paste0("boot/data/IC_vs_official_landings_wgnssk2024.csv"))
Landing_sum2024$Type <- paste0(Landing_sum2024$Type,"2024")
Landing_sum$Type <- paste0(Landing_sum$Type,"2025")
Landing_sum_comp <- rbind(Landing_sum2024[,names(Landing_sum)],Landing_sum)
p <- aggregate(Landing~Year+Type+Country,Landing_sum_comp,sum)
p <- subset(p, Year >=2021)
pIC <- p[grep("IC", p$Type),]
pIC$Year <- as.numeric(pIC$Year)
# poff <- p[grep("Official", p$Type),] #marginal changes

taf.png("Figure.18.5.1.6.IC_WGNSSK2024versusIC_WGNSSK2025.png")
ggplot(pIC , aes(x = Year, y = Landing, group = Type, fill = Type)) + 
  geom_bar(stat = "identity", position="dodge") + scale_fill_manual(values = cbbPalette[4:5])+
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust =0.5)) + facet_wrap(~Country, scales = "free")
dev.off()

# ggplot(poff , aes(x = as.numeric(Year), y = Landing, group = Type, fill = Type)) + 
#   geom_bar(stat = "identity", position="dodge") + scale_fill_manual(values = cbbPalette[4:5])+
#   theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust =0.5)) + facet_wrap(~Country, scales = "free")

# explore sampling coverage age
WtData <- read.csv(file =paste0("data/IC_raw_age_",assess_year-1,".csv"))

taf.png("landingsWtCoutryFleets.png")
plotStockOverview(WtData,plotType="LandWt",byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
                  byArea=FALSE,countryColours=TRUE,set.mar=TRUE,markSampled=TRUE,individualTotals=FALSE)
dev.off()

taf.png("landingsWtCoutryFleets7d.png")
plotStockOverview(WtData[WtData$Area=="27.7.d",],plotType="LandWt",byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
                  byArea=TRUE,countryColours=TRUE,set.mar=TRUE,markSampled=TRUE,individualTotals=FALSE)
dev.off()
taf.png("landingsWtCoutryFleets4.png")
plotStockOverview(WtData[WtData$Area=="27.4",],plotType="LandWt",byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
                  byArea=TRUE,countryColours=TRUE,set.mar=TRUE,markSampled=TRUE,individualTotals=FALSE)
dev.off()
taf.png("landingsWtCoutryFleets3a.png")
plotStockOverview(WtData[WtData$Area=="27.3.a",],plotType="LandWt",byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
                  byArea=T,countryColours=TRUE,set.mar=TRUE,markSampled=TRUE,individualTotals=FALSE)
dev.off()

taf.png("landingsWtCoutryFleetsperc.png") ### MAYBE BETTER IN THE REPORT THAN ONLY 7d
plotStockOverview(WtData,byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
                  byArea=FALSE,countryColours=TRUE,set.mar=TRUE,markSampled=TRUE,individualTotals=FALSE)
dev.off()

#Figure 18.5.2.1. 
taf.png("Figure.18.5.2.1.landingsWtCoutryFleets7dperc.png")
plotStockOverview(WtData[WtData$Area=="27.7.d",],byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
                  byArea=TRUE,countryColours=TRUE,set.mar=TRUE,markSampled=TRUE,individualTotals=FALSE)
dev.off()
taf.png("Figure.18.5.2.1.landingsWtCoutryFleets4perc.png")
plotStockOverview(WtData[WtData$Area=="27.4",],byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
                  byArea=TRUE,countryColours=TRUE,set.mar=TRUE,markSampled=TRUE,individualTotals=FALSE)
dev.off()
taf.png("landingsWtCoutryFleets3aperc.png")
plotStockOverview(WtData[WtData$Area=="27.3.a",],byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
                  byArea=TRUE,countryColours=TRUE,set.mar=TRUE,markSampled=TRUE,individualTotals=FALSE)
dev.off()

# plot stock object age distribution
load(file = "boot/data/a4a_input_data_wgnssk2024.Rdata")
stockGislason_ini2024 <- stockGislason_ini
stockGislason2024 <- stockGislason

load(file= "data/a4a_input_data.Rdata")

# Table 18.5.2.1
table_summary <- as.data.frame(round(stockGislason_ini@landings.n,0)) %>% dcast(year ~ age, value.var = "data") %>% mutate(`4+`=`4`+`5`+`6`)

write.taf(table_summary , file =paste0("report/Table.18.5.2.1.landing_num_age",assess_year,".csv"))

# Numbers at age (bubbles)
# Bubble plots
# Figure 18.5.2.2. 
taf.png(filename="Figure.18.5.2.3.Landingsbubble.png")
bubbles(age ~ year | unit, setPlusGroup(stockGislason_ini, plusgroup=pGrp)@landings.n, main="Landings N@A", col = c("blue", "white"),bub.scale = 6, strip=FALSE)
dev.off()

# Figure 18.5.2.2.
p <- as.data.frame(setPlusGroup(stockGislason_ini, plusgroup=pGrp)@landings.n)

#compare IC Landings number @ age from WGNSSK 2024 & 2025
pcomp <- as.data.frame(setPlusGroup(stockGislason_ini2024, plusgroup=pGrp)@landings.n)
p$WGNSSK <- "2025"
pcomp$WGNSSK <- "2024"
pcomp <- rbind(p,pcomp)
pcomp <- subset(pcomp, year >= 2021)
pcomp$year <- as.factor(pcomp$year)

taf.png(filename="Figure.18.5.2.2.Landingsagedistrib_comp2425.png")
ggplot(pcomp , aes(x = age, y = year, height = data, fill = WGNSSK)) + 
  geom_density_ridges(stat="identity", color = "grey",alpha = .5) + labs(x = "Age",
                                                         title= "Numbers at age") + 
  theme_ridges() + theme(axis.text.x = element_text(#angle = 45, 
    vjust =0.5)) + 
  coord_cartesian(clip = "off")
dev.off()

taf.png(filename="Landingsagedistrib.png")
p$year <- as.factor(p$year)
ggplot(p , aes(x = age, y = year, height = data)) + 
  geom_density_ridges(stat="identity",alpha = .5) + labs(x = "Age",
                                                         title= "Numbers at age") + 
  theme_ridges() + theme(axis.text.x = element_text(#angle = 45, 
    vjust =0.5)) + 
  coord_cartesian(clip = "off")

dev.off()

## Weights
# Landings
# Table 18.5.2.2
table_summary <- as.data.frame(round(stockGislason_ini@landings.wt,3)) %>% dcast(year ~ age, value.var = "data")
table_summary_p <- as.data.frame(round(setPlusGroup(stockGislason_ini, plusgroup=pGrp)@landings.wt,3)) %>% dcast(year ~ age, value.var = "data")
table_summary$`4+` <- table_summary_p$`4`
write.taf(table_summary , file =paste0("report/Table.18.5.2.2.landing_weight_age",assess_year,".csv"))

# Figure 18.5.2.3. 
toto <- setPlusGroup(stockGislason_ini, plusgroup=pGrp)
toto <- as.data.frame(landings.wt(toto))
toto$data[toto$data==0] <- NA
toto$age <- as.character(toto$age)

taf.png(filename="Figure.18.5.2.4.Landingswght.png")
ggplot(data = toto, aes(x=year,y=data, group = age, color = age)) + geom_line() + 
  geom_text(aes(label=ifelse(as.numeric(age)<4,age,paste0(age,"+")))) +
  scale_x_continuous(breaks=seq(2004, max(officialLandings$Year), by = 2)) +
  ggtitle("Landings weight at age", )+ xlab("Year") + ylab("Weight (kg)") + 
  theme_bw() + theme(legend.position="none")
dev.off()

# catch curve
ttl      <- list(label="Log catch ratios for Red Mullet in VIId/IV/IIIa", cex=1)
yttl     <- list(label="log ratio", cex=1.2)
taf.png(filename="catchcurve.png")
ccplot(data~year, data=logcc(stockGislason@catch.n), type="l",main=ttl, ylab=yttl, xlab=list(cex=1,2), scales=list(cex=0.7), col=1)
dev.off()

# explore IC length sampling coverage in 2024
WtData_length <- read.csv(file =paste0("data/IC_raw_length_",assess_year-1,".csv"))

taf.png("landingsWtCoutryFleets_length.png")
plotStockOverview(WtData_length,plotType="LandWt",byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
                  byArea=FALSE,countryColours=TRUE,set.mar=TRUE,markSampled=TRUE,individualTotals=FALSE)
dev.off()

taf.png("landingsWtCoutryFleets7d_length.png")
plotStockOverview(WtData_length[WtData_length$Area=="27.7.d",],plotType="LandWt",byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
                  byArea=T,countryColours=TRUE,set.mar=TRUE,markSampled=TRUE,individualTotals=FALSE)
dev.off()
taf.png("landingsWtCoutryFleets4_length.png")
plotStockOverview(WtData_length[WtData_length$Area=="27.4",],plotType="LandWt",byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
                  byArea=T,countryColours=TRUE,set.mar=TRUE,markSampled=TRUE,individualTotals=FALSE)
dev.off()
taf.png("landingsWtCoutryFleets3a_length.png")
plotStockOverview(WtData_length[WtData_length$Area=="27.3.a",],plotType="LandWt",byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
                  byArea=T,countryColours=TRUE,set.mar=TRUE,markSampled=TRUE,individualTotals=FALSE)
dev.off()

taf.png("landingsWtCoutryFleetsperc_length.png") # maybe better in report as well
plotStockOverview(WtData_length,byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
                  byArea=FALSE,countryColours=TRUE,set.mar=TRUE,markSampled=TRUE,individualTotals=FALSE)
dev.off()

# Figure 18.5.3.1.
taf.png("Figure.18.5.3.1.landingsWtCoutryFleets7dperc_length.png")
plotStockOverview(WtData_length[WtData_length$Area=="27.7.d",],byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
                  byArea=T,countryColours=TRUE,set.mar=TRUE,markSampled=TRUE,individualTotals=FALSE)
dev.off()
# Figure 18.5.3.1.
taf.png("Figure.18.5.3.1.landingsWtCoutryFleets4perc_length.png")
plotStockOverview(WtData_length[WtData_length$Area=="27.4",],byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
                  byArea=T,countryColours=TRUE,set.mar=TRUE,markSampled=TRUE,individualTotals=FALSE)
dev.off()
taf.png("landingsWtCoutryFleets3aperc_length.png")
plotStockOverview(WtData_length[WtData_length$Area=="27.3.a",],byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
                  byArea=T,countryColours=TRUE,set.mar=TRUE,markSampled=TRUE,individualTotals=FALSE)
dev.off()
# 
# # plot IC length distribution
# load(file="data/IC_length_mur_wgnssk2023.rdata")
# taf.png("IC_LenghtdistributionYear.png")
# p <- IC$Undetermined_length
# 
# p <- melt(data = p, id = "MeanLength", variable.name = "Year")
# p <- p[p$value!=0,]
# 
# ggplot(p , aes(x = MeanLength, y = value)) + 
#   geom_bar(stat = "identity") + 
#   scale_x_continuous(breaks=seq(min(p$MeanLength), max(p$MeanLength), by = 10))  +
#   scale_fill_manual(values = cbbPalette[4])+
#   theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust =0.5)) + facet_wrap(~Year, scales = "free_y", ncol = 1)
# dev.off()

# # plot FR sampling coverage and availability
# sample_FR <- read.taf(file =paste0("data/sample_FR_2004_",assess_year-1,".csv"))
# sample_FR <- sample_FR %>% mutate(Year = as.numeric(Year), Season = as.character(Season))
# cbbPalette <- c( "#009E73","#F0E442", "#CC79A7", "#0072B2", "#E69F00", "#828282")
# 
# # Figure 18.5.3.2.
# taf.png(paste0("sample_FR_2004_",assess_year-1,"_quarter.png"))
# ggplot(sample_FR, aes(x = Year, y = fleet_sampled, group = Season, fill = Season)) + 
#   geom_bar(stat = "identity") + 
#   scale_x_continuous(breaks=seq(2004, max(sample_FR$Year), by = 1))  + 
#   scale_fill_manual(values = cbbPalette)+
#   theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust =0.5))
# dev.off()
# 
# data_availability <- read.taf(file =paste0("data/FRsampling_coverage_2004_",assess_year-1,".csv"))
# p <- data_availability %>% filter(CatchCategory=="L" & type %in% c("FRsampled_vs_FR", "FRsampled_vs_ALL", "Area_vs_ALL")) %>%
#   mutate(Year = as.numeric(Year))
# 
# cbbPalette <- c("#828282", "#009E73", "#E69F00")
# 
# # Figure 18.5.3.3. 
# taf.png(paste0("Landind_sampling_coverageFR_2004",assess_year-1,"_area.png"))
# ggplot(p, aes(x = Year, y = value, group = type, color = type)) + 
#   geom_line(linewidth=1) + 
#   scale_x_continuous(breaks=seq(2004, max(p$Year), by = 2)) + 
#   scale_color_manual(values = cbbPalette) +
#   theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust =0.5)) + facet_wrap(~FishingArea)
# dev.off()

# plot all length distribution
# Figure 18.5.3.4. 
load(file="boot/data/IC_echantil_length_mur_wgnssk2024.rdata") # compare length distri WGNSSK 2024-2025
pcomp <- IC_echantil$Undetermined_length

pcomp <- melt(data = pcomp, id = "MeanLength", variable.name = "Year")
pcomp$MeanLength <- as.numeric(pcomp$MeanLength)/10
pcomp <- pcomp[pcomp$value!=0,]
pcomp <- subset(pcomp, !Year %in% c(2008,2013)) 

load(file="data/IC_echantil_length_mur_wgnssk2025.rdata")
taf.png("Figure.18.5.3.5.LenghtdistributionYear.png")
p <- IC_echantil$Undetermined_length

p <- melt(data = p, id = "MeanLength", variable.name = "Year")
p$MeanLength <- as.numeric(p$MeanLength)/10
p <- p[p$value!=0,]
p <- subset(p, !Year %in% c(2008,2013)) #remove year where not enough samples

ggplot(p , aes(x = MeanLength, y = Year, height = value)) + 
  geom_density_ridges(stat="identity",alpha = .5) + labs(x = "Length (cm)",
                                                         title= "Landing numbers at length") +
  scale_x_continuous(breaks=seq(min(p$MeanLength), max(p$MeanLength), by = 2)) + 
  theme_ridges() + theme(axis.text.x = element_text(#angle = 45, 
                                                    vjust =0.5)) + 
  coord_cartesian(clip = "off")

# ggplot(p , aes(x = MeanLength, y = value)) +
#   geom_bar(stat = "identity") +
#   scale_x_continuous(breaks=seq(min(p$MeanLength), max(p$MeanLength), by = 10))  +
#   scale_fill_manual(values = cbbPalette[4])+
#   theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust =0.5), axis.text.y = element_text(size = 7)) +
#   facet_wrap(~Year, scales = "free_y", ncol = 1,strip.position = "right") +
#   scale_y_continuous(n.breaks = 3)  +
#   theme(strip.text.y.right = element_text(angle = 0), strip.background = element_blank(), strip.placement = "outside")
dev.off()

p$WGNSSK <- "2025"
pcomp$WGNSSK <- "2024"
pcomp <- rbind(p,pcomp)
pcomp <- subset(pcomp, as.numeric(as.character(Year)) >= 2021)
pcomp$Year <- as.factor(pcomp$Year)

taf.png(filename="Figure.18.5.3.2.LenghtdistributionYear_comp2425.png")
ggplot(pcomp , aes(x = MeanLength, y = Year, height = value, fill = WGNSSK)) + 
  geom_density_ridges(stat="identity", color = "grey",alpha = .5) + labs(x = "Length (cm)",
                                                         title= "Landing numbers at length") +
  scale_x_continuous(breaks=seq(min(p$MeanLength), max(p$MeanLength), by = 2)) + 
  theme_ridges() + theme(axis.text.x = element_text(#angle = 45, 
    vjust =0.5)) + 
  coord_cartesian(clip = "off")

dev.off()

### ------------------------------------------------------------------------ ###
### Chr rule: cat3advice package from Simon Fisher                           ###  
### ------------------------------------------------------------------------ ###
cat3advice::advice(idx)

taf.png("report/index.png")
cat3advice::plot(idx)
dev.off()

# cat3advice::advice(bi)
cat3advice::advice(bi_ref)

### plot index with Itrigger
# Figure 18.6.1.1.
# taf.png("report/index_bpa.png")
# cat3advice::plot(bi)
# dev.off()

taf.png("report/Figure.18.6.1.1.index_bpa_ref.png") # to change when RP's change
# taf.png("report/index_bpa_ref.png")
cat3advice::plot(bi_ref)
dev.off()

### plot Lc as the mean of yearly Lc
# Figure.18.6.1.2.
lc@value <- lc_ref@value # so we get 2024 length distribution with ref Lc maybe move it in output script
taf.png("report/Figure.18.6.1.2.lfreq_lc.png") # so we get 2024
# taf.png("report/Figure.18.6.3.2.lfreq_lc.png") # need to change when change RP's
cat3advice::plot(lc)
dev.off()

taf.png("report/lfreq_lc_ref.png")
cat3advice::plot(lc_ref)
dev.off()


### plot Lmean Lc as the mean of yearly Lc
# Figure 18.6.1.4.
# taf.png("report/lfreq_lmean.png")
# cat3advice::plot(lmean)
# dev.off()
# 
# taf.png("report/lfreq_lmean_ref.png")
# cat3advice::plot(lmean_ref)
# dev.off()

taf.png("report/Figure.18.6.1.3.lfreq_lmean_ref2.png")
# taf.png("report/Figure.18.6.3.3.lfreq_lmean_ref2.png") # need to change when change RP's
lmean_ref2@value <- lmean_ref2@value[!names(lmean_ref2@value) %in% c("2008","2013","2017")]
lmean_ref2@summary <- subset(lmean_ref2@summary, !year %in% c(2008,2013,2017))
cat3advice::plot(lmean_ref2)
dev.off()

### plot Lmean Lref
# advice(fi)
# 
# taf.png("report/lfreq_lref.png")
# cat3advice::plot(fi, y_label = "Length in mm")
# dev.off()
# 
# advice(fi_ref)
# taf.png("report/lfreq_lref_ref.png")
# cat3advice::plot(fi_ref, y_label = "Length in mm")
# dev.off()

advice(fi_ref2)
taf.png("report/Figure.18.6.1.4.lfreq_lref_ref2.png")
cat3advice::plot(fi_ref2,inverse = T, y_label = "Fishing pressure proxy")
dev.off()

# The harvest rate can only be calculated for years in which both catch and index data are available. The
# harvest rate and its input data can be plotted automatically:
taf.png("report/summary_plot.png")
cat3advice::plot(hr)
dev.off()

# taf.png("report/summary_plot_ref.png")
# cat3advice::plot(hr_ref)
# dev.off()

# Harvest rate target FproxyMSY
### plot (relative) target harvest rate
# Figure 18.6.1.5.
# cat3advice::advice(Fp)
# 
# taf.png("report/FproxyMSY.png")
# cat3advice::plot(Fp)
# dev.off()
# 
# cat3advice::advice(Fp_ref2)
# 
# taf.png("report/FproxyMSY_ref.png")
# cat3advice::plot(Fp_ref2)
# dev.off()

cat3advice::advice(Fp_ref)

taf.png("report/Figure.18.6.1.4.FproxyMSY_ref.png") # change when RP's change
# taf.png("report/FproxyMSY_ref2.png") # be careful if last year of data indicator above 1 use Fp_ref  
cat3advice::plot(Fp_ref)
dev.off()

cat3advice::advice(mp)

# cat3advice::advice(advice)
cat3advice::advice(advice_ref)
# cat3advice::advice(advice_ref2)

# ### ------------------------------------------------------------------------ ###
# ### Chr rule: cat3advice package from Simon Fisher compare with WGNSSK2024   ###  
# ### ------------------------------------------------------------------------ ###
# # load assessment from last year
# load(file = "boot/data/Chr_advice_2024.Rdata")
# A_old <- A
# advice_old <- advice_ref
# bi_old <- bi_ref
# fi_old <- fi
# Fp_old <- Fp
# hr_old <- hr
# idx_old <- idx
# lc_old <- lc
# lmean_old <- lmean
# lref_old <- lref
# mp_old <- mp
# 
# load(file = "model/Chr_advice_2025.Rdata")
# 
# # compare Index and Biomass RPs (based on cat3advice package plotting function)
# object <- bi_ref
# object2 <- bi_old
# object@idx$name = "Index"
# object2@idx$name = "Index-WGNSSK2024"
#             
# object@idx <- rbind(object@idx, object2@idx)
#             
# ### get range of years and index values
# yr_min <- min(object@idx$year, na.rm = TRUE)
# yr_max <- max(object@idx$year, na.rm = TRUE)
# idx_min <- min(object@idx$index, na.rm = TRUE)
# idx_max <- max(object@idx$index, na.rm = TRUE)
# 
# y_label <- "Biomass index"
# if (!is.na(object@units)) y_label <- paste0(y_label, " in ", object@units)
#             
# ### reference lines
# b_refs <- data.frame(name = c("I[loss]", "I[trigger]"),
#                                  value = c(object@Iloss, object@Itrigger))
# b_refs$name <- factor(b_refs$name)
#             
# b_refs2 <- data.frame(name = c("I[loss]-WGNSSK2024", "I[trigger]-WGNSSK2024"),
#                                  value = c(object2@Iloss, object2@Itrigger))
# b_refs2$name <- factor(b_refs2$name)
#             
# b_refs <- rbind(b_refs, b_refs2)
# 
# ### create plot
# p <- ggplot2::ggplot() +
#   ggplot2::geom_line(data = object@idx, 
#                      ggplot2::aes(x = year, y = index, linetype = name, color = name )) +
#   ggplot2::geom_hline(data = b_refs,
#                       ggplot2::aes(yintercept = value, linetype = name, colour = name)) +
#   ggplot2::scale_linetype_manual("",
#                                  values = c("Index-WGNSSK2024" = "solid",
#                                             "Index" = "solid",
#                                             "I[loss]" = "dotted", 
#                                             "I[trigger]" = "solid",
#                                             "I[loss]-WGNSSK2024" = "dotted",
#                                             "I[trigger]-WGNSSK2024" = "solid"),
#                                  labels = scales::parse_format()) + 
#   ggplot2::scale_colour_manual("",
#                                values = c("Index" = "#077c6c",
#                                           "Index-WGNSSK2024" = "green",
#                                           "I[loss]" = "blue",
#                                           "I[trigger]" = "blue",
#                                           "I[loss]-WGNSSK2024" = "#679dfe",
#                                           "I[trigger]-WGNSSK2024" = "#679dfe"),
#                                            labels = scales::parse_format()) +
#   ggplot2::coord_cartesian(ylim = c(0, idx_max * 1.1),
#                            xlim = c(yr_min - 1, yr_max + 1),
#                            expand = FALSE) +
#   ggplot2::labs(x = "", y = y_label,
#                 title = "Biomass Index comparisons WGNSSK2024/2025") +
#   ggplot2::theme_bw(base_size = 8) +
#   ggplot2::theme(axis.title.y = ggplot2::element_text(face = "bold"),
#                  axis.title.x = ggplot2::element_blank(),
#                  legend.position = "bottom",
#                  legend.key.height = ggplot2::unit(0.5, "lines"),
#                  plot.title = ggplot2::element_text(face = "bold",
#                                                     colour = "#097e6e"))
# 
# taf.png("report/Figure.18.6.3.1.index_comparison_bpa_ref.png")
# p
# dev.off()
# 
# # compare Index and Biomass RPs (based on cat3advice package plotting function)
# object <- Fp_ref2
# object2 <- Fp_old
# 
# ### check validity
# if (isTRUE(nrow(object@HR@data) < 2))
#   stop("No harvest rate time series available.")
# 
# ### get range of years
# yr_min <- min(c(object@indicator@indicator$year, 
#                 object@HR@data$year[!is.na(object@HR@data$harvest_rate)]), na.rm = TRUE)
# yr_max <- max(c(object@indicator@indicator$year, 
#                 object@HR@data$year[!is.na(object@HR@data$harvest_rate)]), na.rm = TRUE)
# 
# ### start with HR plot
# hr_min <- min(object@HR@data$harvest_rate, na.rm = TRUE)
# hr_max <- max(object@HR@data$harvest_rate, na.rm = TRUE)
# ### hr units
# y_label <- "Harvest rate"
# if (!is.na(object@HR@units)) y_label <- paste0(y_label, " in ", object@HR@units)
# 
# ### hr target
# hr_target <- data.frame(y = c(object@value, object2@value), 
#                         type = c("F[MSYproxy]", "F[MSYproxy]-WGNSSK2023"))
# object@HR@data$type <- "HR"
# object2@HR@data$type <- "HR-WGNSSK2023"
# object@data$type <- "reference years"
# object2@data$type <- "reference years-WGNSSK2023"
# object@data$col <- "#ed6028"
# object2@data$col <- "darkred"
# object@HR@data <- rbind(object@HR@data, object2@HR@data)
# object@data <- rbind(object@data, object2@data)
# 
# ### create HR plot
# p <- ggplot2::ggplot() +
#   ggplot2::geom_line(data = object@HR@data,
#                      ggplot2::aes(x = year, y = harvest_rate, linetype = type, color = type ),
#                      # color = "#ed6028",
#                      na.rm = FALSE) +
#   ggplot2::geom_hline(data = hr_target, 
#                       ggplot2::aes(yintercept = y, linetype = type, color = type),
#                       # colour = "#679dfe",
#                       alpha = 0.8) +
#   ggplot2::scale_linetype_manual("",
#                                  values = c("HR" = "solid",
#                                             "HR-WGNSSK2023" = "solid",
#                                             "F[MSYproxy]" = "solid", 
#                                             "F[MSYproxy]-WGNSSK2023" = "solid"),
#                                  labels = scales::parse_format()) + 
#   ggplot2::scale_color_manual("",
#                               values = c("HR" = "#ed6028",
#                                          "HR-WGNSSK2023" = "darkred",
#                                          "F[MSYproxy]" = "#679dfe", 
#                                          "F[MSYproxy]-WGNSSK2023" = "blue"),
#                               labels = scales::parse_format()) +
#   ggplot2::geom_point(data = object@data,
#                       ggplot2::aes(x = year, y = harvest_rate,
#                                    shape = type, color = type),  colour = object@data$col) +
#   # ggplot2::scale_shape_manual("", values = 16, ) +
#   ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
#   ggplot2::coord_cartesian(ylim = c(0, hr_max * 1.1),
#                            xlim = c(yr_min - 1, yr_max + 1),
#                            expand = FALSE) +
#   ggplot2::labs(x = "", y = y_label, 
#                 title = "Harvest rate (catches / biomass index) comaprisons WGNSSK2023/2024") +
#   ggplot2::theme_bw(base_size = 8) +
#   ggplot2::theme(axis.title.y = ggplot2::element_text(face = "bold"),
#                  axis.title.x = ggplot2::element_blank(),
#                  legend.position = "bottom",legend.title=element_blank(),
#                  legend.key.height = ggplot2::unit(0.5, "lines"),
#                  plot.title = ggplot2::element_text(face = "bold", 
#                                                     colour = "#ed6028"))
# #p
# 
# ### length indicator
# idx_min <- min(object@indicator@indicator$indicator, na.rm = TRUE)
# idx_max <- max(object@indicator@indicator$indicator, na.rm = TRUE)
# 
# ### indicator plot
# if (isTRUE(nrow(object@indicator@indicator) > 1)) {
#   p_indicator <- ggplot2::ggplot() +
#     ggplot2::geom_line(
#       data = object@indicator@indicator,
#       ggplot2::aes(x = year, y = indicator),
#       color = "#ed6028"
#     ) +
#     ggplot2::geom_point(
#       data = object@indicator@indicator[object@indicator@indicator$indicator >= 1, ],
#       ggplot2::aes(x = year, y = indicator),
#       color = "#ed6028", shape = 16
#     ) +
#     ggplot2::geom_hline(
#       yintercept = 1,
#       colour = "#679dfe", alpha = 0.8
#     ) +
#     ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
#     ggplot2::coord_cartesian(
#       ylim = c(0, idx_max * 1.1),
#       xlim = c(yr_min - 1, yr_max + 1),
#       expand = FALSE
#     ) +
#     ggplot2::labs(
#       x = "", y = "Length-based indicator",
#       title = "Length-based indicator"
#     ) +
#     ggplot2::theme_bw(base_size = 8) +
#     ggplot2::theme(
#       axis.title.y = ggplot2::element_text(face = "bold"),
#       axis.title.x = ggplot2::element_blank(),
#       legend.position = "bottom",
#       legend.key.height = ggplot2::unit(0.5, "lines"),
#       plot.title = ggplot2::element_text(face = "bold", colour = "#ed6028")
#     )
#   p <- p_indicator/p
# }
# 
# taf.png("report/Figure.18.6.3.4.FproxyMSY_ref2_comparison.png") # be careful if last year of data indicator above 1 use Fp_ref  
# p
# dev.off()