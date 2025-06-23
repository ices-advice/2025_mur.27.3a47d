## Additional functions

## Before:
## After:
library(dplyr)
library(FLCore)

################################## readStockOverview ###############################

readStockOverview <- function(StockOverviewFile,NumbersAtAgeLengthFile){
  
  Wdata <- read.table(StockOverviewFile,header=TRUE,sep="\t")
  Wdata <- Wdata %>% rename(Fleet=Fleets, CatchWt=Catch..kg,
                            CatchCat= Catch.Cat., ReportCat=Report.cat.) %>%
    mutate(CatchCat = substr(CatchCat,1,1))
  Wdata <- Wdata[,-ncol(Wdata)]
  
  Ndata <- read.table(NumbersAtAgeLengthFile,header=TRUE,sep="\t",skip=1)
  Ndata <- Ndata %>% rename(Fleet=Fleets, CatchCat= Catch.Cat.)
  
  Wdata <- merge(Wdata,Ndata[,c("Season","Area","Country","CatchCat","Fleet","Caton","SampledCatch")],by=c("Area","Season","Fleet","Country","CatchCat"),all.x=TRUE)
  Wdata$Sampled <- ifelse(is.na(Wdata$SampledCatch),FALSE,TRUE)
  
  Wdata <- Wdata %>% mutate(Area = gsubfn("IIIaN|IIIaS|IV|VIId|IIIa|27.3.a.20|27.3.a.21|27.4.b|27.4.c", 
                       list('IIIaN'="27.3.a",'IIIaS'="27.3.a",'27.3.a.20'="27.3.a",'27.3.a.21'="27.3.a",
                            'IIIa'="27.3.a",'IV'="27.4",'27.4.b'="27.4",'27.4.c'="27.4",'VIId'="27.7.d"), Area))
  return(Wdata)
}

################################## readNumbersAtAgeLength ###############################

readNumbersAtAgeLength <- function(NumbersAtAgeLengthFile){
  
  Ndata <- read.table(NumbersAtAgeLengthFile,header=TRUE,sep="\t",skip=1)
  Ndata <- Ndata %>% rename(Fleet=Fleets, CatchCat= Catch.Cat., ReportCat=Report.cat.)
  Ndata <- Ndata[,-ncol(Ndata)]
  ageNames <- names(Ndata)[16:ncol(Ndata)]
  if(nchar(ageNames)[1]!=4){
    ages <- as.numeric(substr(ageNames,16,nchar(ageNames)))
  } else {
    ages <- as.numeric(substr(ageNames,4,nchar(ageNames)))
  }
  allAges <- min(ages):max(ages)
  missingAges <- allAges[allAges %in% ages]
  colnames(Ndata)[16:ncol(Ndata)] <- ages
  return(Ndata)
}

################################## plotStockOverview ###############################

plotStockOverview <- function(dat,plotType="LandPercent",byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,byArea=FALSE,countryColours=NULL,set.mar=TRUE,markSampled=TRUE,individualTotals=TRUE,ymax=NULL){
  
  plotTypes <- c("LandWt","LandPercent","CatchWt","DisWt","DisRatio","DiscProvided")
  if (!(plotType %in% plotTypes)) stop(paste("PlotType needs to be one of the following:",paste(plotTypes)))
  
  stock <- dat$Stock[1]
  
  impLand <- dat[dat$CatchCat=="L",]
  impDis <- dat[dat$CatchCat=="D",]
  
  nArea <- nSeason <- nCountry <- nFleet <- 1
  
  SeasonNames <- sort(unique(impLand$Season))
  AreaNames <- sort(unique(impLand$Area))
  
  countryLegend <- FALSE
  
  if (byFleet) nFleet <- length(unique(c(impLand$Fleet, impDis$Fleet))) ## YV
  if (byCountry) { 
    nCountry <- length(unique(c(impLand$Country, impDis$Country))) # YV
    countryLegend <- TRUE
  }    
  if (byArea) nArea <- length(AreaNames)
  if (bySeason) nSeason <- length(SeasonNames)
  if (!bySampled) markSampled <- FALSE
  
  
  if (length(countryColours)==1 &&countryColours){
    #countryColours <- data.frame(
    #"Country"=c("Belgium","Denmark","France","Germany","Netherlands","Norway","Poland","Sweden","UK (England)","UK(Scotland)"),
    #"Colour"=c("green", "red", "darkblue", "black", "orange","turquoise" ,"purple","yellow","magenta","blue")
    #, stringsAsFactors=FALSE)
    countryColours <- data.frame("Country"=unique(dat$Country)[order(unique(dat$Country))],
                                 "Colour"=rainbow(length(unique(dat$Country)))
                                 , stringsAsFactors=FALSE)
  }
  if (length(countryColours)==1 && countryColours==FALSE){
    countryLegend <- FALSE
    #countryColours <- data.frame(
    #"Country"=c("Belgium","Denmark","France","Germany","Norway","Netherlands","Poland","Sweden","UK (England)","UK(Scotland)")
    # , stringsAsFactors=FALSE)
    countryColours <- data.frame("Country"=unique(dat$Country)[order(unique(dat$Country))],
                                 stringsAsFactors=FALSE)
    countryColours$Colour <- rep("grey",length(countryColours$Country))
  }
  
  
  LsummaryList <- list()
  DsummaryList <- list()
  summaryNames <- NULL
  i <- 1
  if (byFleet) {
    LsummaryList[[i]] <- impLand$Fleet
    DsummaryList[[i]] <- impDis$Fleet
    summaryNames <- c(summaryNames,"Fleet")
    i <- i+1
  }
  if(byCountry) {
    LsummaryList[[i]] <- impLand$Country
    DsummaryList[[i]] <- impDis$Country
    summaryNames <- c(summaryNames,"Country")
    i <- i+1
  }
  if(bySeason) {
    LsummaryList[[i]] <- impLand$Season
    DsummaryList[[i]] <- impDis$Season
    summaryNames <- c(summaryNames,"Season")
    i <- i+1
  }
  if (byArea) {
    LsummaryList[[i]] <- impLand$Area
    DsummaryList[[i]] <- impDis$Area
    summaryNames <- c(summaryNames,"Area")
    i <- i+1
  }
  if (bySampled) {
    LsummaryList[[i]] <- impLand$Sampled
    DsummaryList[[i]] <- impDis$Sampled
    summaryNames <- c(summaryNames,"Sampled")
    i <- i+1
  }
  byNames <- summaryNames
  summaryNames <- c(summaryNames,"CatchWt")
  
  ### YV changed
  if(plotType%in%c("LandWt","LandPercent")){
    Summary <- aggregate(impLand$CatchWt,LsummaryList,sum)
  } else if (plotType=="DisWt"){
    Summary <- aggregate(impDis$CatchWt,DsummaryList,sum)
  } else if (plotType=="DisRatio"){
    SummaryD <- aggregate(impDis$CatchWt,DsummaryList,sum)
    SummaryL <- aggregate(impLand$CatchWt,LsummaryList,sum)
    if(bySampled){
      testN <- colnames(SummaryD)[grep('Group', colnames(SummaryD)) - 1]
    } else {
      testN <- colnames(SummaryD)[grep('Group', colnames(SummaryD))]	
    }
    Summary <- merge(SummaryD, SummaryL, all=T, by=testN)
    Summary$DRatio <- Summary$x.x / (Summary$x.x+Summary$x.y)
    Summary <- Summary[!is.na(Summary$DRatio),c(testN,paste('Group.',length(testN)+1,'.x', sep=''),paste('Group.',length(testN)+1,'.y', sep=''),'DRatio')]
  } else if (plotType=="DiscProvided"){
    if(bySampled){
      DsummaryList <- DsummaryList[1: (length(DsummaryList)- 1)]
      LsummaryList <- LsummaryList[1: (length(LsummaryList)- 1)]
    } else {
      DsummaryList <- DsummaryList[ length(DsummaryList)]
      LsummaryList <- LsummaryList[ length(LsummaryList)]
    }
    SummaryD <- aggregate(impDis$CatchWt,DsummaryList,sum)
    SummaryL <- aggregate(impLand$CatchWt,LsummaryList,sum)
    if(bySampled){
      testN <- colnames(SummaryD)[grep('Group', colnames(SummaryD))]
    } else {
      testN <- colnames(SummaryD)[grep('Group', colnames(SummaryD))]	
    }
    Summary <- merge(SummaryD, SummaryL, all=T, by=testN)
    Summary$DRatio <- Summary$x.x / (Summary$x.x+Summary$x.y)
    Summary <- Summary[,c(testN,'DRatio','x.y')]
    Summary$DRatio[!is.na(Summary$DRatio)] <- TRUE
    Summary$DRatio[is.na(Summary$DRatio)] <- FALSE
    Summary <- unique(Summary) 
    ProvidedDiscards <<- Summary
    
  }
  ### end YV changed
  
  
  #disSummary <- aggregate(impDis$CatchWt,DsummaryList,sum) YV
  if (plotType!="DisRatio") {
    names(Summary) <- summaryNames #YV
  } else {
    names(Summary) <- c(summaryNames[-c(grep(c('Sampled'),summaryNames), grep(c('CatchWt'),summaryNames))],
                        "SampledD", "SampledL", "DRatio")
  }
  #names(disSummary) <- summaryNames # yv
  #names(disSummary) <- c("Fleet" ,  "Country", "Area" ,   "SampledD" ,"DisWt") # yv
  #names(landSummary) <- c("Fleet" ,  "Country", "Area" ,   "SampledL" ,"LandWt") # yv
  
  
  #stratumSummary <- merge(landSummary,disSummary,by=byNames,all=TRUE) #YV
  stratumSummary <- Summary #YV
  if(plotType%in%c("LandWt","LandPercent","DiscProvided")){
    names(stratumSummary)[names(stratumSummary)=="CatchWt"] <- "LandWt" # YV
  } else if (plotType=="DisWt"){
    names(stratumSummary)[names(stratumSummary)=="CatchWt"] <- "DisWt" # YV
  } 
  
  #names(stratumSummary)[names(stratumSummary)=="CatchWt.y"] <- "DisWt"  # YV
  
  #if (bySampled ) {
  ##  stratumSummary <- stratumSummary[rev(order(stratumSummary$Sampled,stratumSummary$LandWt)),
  #	if(plotType%in%c("LandWt","LandPercent")){
  #	  stratumSummary <- stratumSummary[rev(order(stratumSummary$Sampled,stratumSummary$LandWt)),]
  #	} else if (plotType=="DisWt"){
  #	  stratumSummary <- stratumSummary[rev(order(stratumSummary$Sampled,stratumSummary$DisWt)),]
  #	}
  #} else {
  #	if(plotType%in%c("LandWt","LandPercent")){
  #	  stratumSummary <- stratumSummary[rev(order(stratumSummary$LandWt)),]
  #	} else if (plotType=="DisWt"){
  #	  stratumSummary <- stratumSummary[rev(order(stratumSummary$DisWt)),]
  #	}
  #}
  if (bySampled ) {
    if(plotType!="DisRatio"){
      stratumSummary <- stratumSummary[rev(order(stratumSummary$Sampled,stratumSummary[,dim(stratumSummary)[2]])),]
    } else {
      stratumSummary <- stratumSummary[rev(order(stratumSummary$SampledL,stratumSummary$SampledD,stratumSummary[,dim(stratumSummary)[2]])),]	
    }
  } else {
    stratumSummary <- stratumSummary[rev(order(stratumSummary[,dim(stratumSummary)[2]])),]
  }
  
  #catchData <- matrix(c(stratumSummary$LandWt,stratumSummary$DisWt),byrow=TRUE,nrow=2) YV
  catchData <- stratumSummary[,dim(stratumSummary)[2]] #YV
  
  
  if (set.mar) par(mar=c(8,4,1,1)+0.1) 
  
  for (a in 1:nArea) {
    #windows()
    if (bySeason & !(byCountry | byFleet)) nSeason <- 1
    for (s in 1:nSeason) {
      area <- AreaNames[a]
      season <- SeasonNames[s]
      
      indx <- 1:nrow(stratumSummary)
      if (bySeason & !byArea & (byCountry | byFleet)) indx <- stratumSummary$Season==season 
      if (!bySeason & byArea) indx <- stratumSummary$Area==area 
      if (bySeason & byArea & (byCountry | byFleet | bySampled)) indx <- stratumSummary$Area==area & stratumSummary$Season==season
      
      if (individualTotals) {
        sumLandWt <- sum(stratumSummary$LandWt[indx],na.rm=TRUE)
      } else {
        sumLandWt <- sum(stratumSummary$LandWt,na.rm=TRUE)
      }  
      if(byCountry) {
        colVec <- countryColours$Colour[match(stratumSummary$Country[indx],countryColours$Country)]
      } else {
        colVec <- "grey"
      }  
      
      if (plotType%in%c("LandWt","DiscProvided")) yvals <- stratumSummary$LandWt[indx]
      if (plotType=="LandPercent") yvals <- 100*stratumSummary$LandWt[indx]/sumLandWt    
      if (plotType=="DisWt") yvals <- stratumSummary$DisWt[indx] 
      if (plotType=="CatchWt") yvals <- catchData[,indx] 
      if (plotType=="DisRatio") yvals <- stratumSummary$DRatio
      
      if(plotType!="DisRatio"){
        if (!is.null(ymax)) newYmax <- ymax
        if (is.null(ymax)) newYmax <- max(yvals,na.rm=TRUE)
        if (is.null(ymax) & plotType=="LandPercent") newYmax <- max(cumsum(yvals),na.rm=TRUE)
        #if (is.null(ymax) & plotType=="CatchWt") newYmax <- max(colSums(yvals),na.rm=TRUE) #YV
        #if (plotType=="CatchWt") colVec <- c("grey","black") #YV
        #if (plotType=="CatchWt") countryLegend <- FALSE #YV
        if (markSampled) newYmax <- 1.06*newYmax
        if (byFleet) namesVec=stratumSummary$Fleet[indx]
        if (!byFleet) {
          if (byArea & bySeason) namesVec=paste(stratumSummary$Area[indx],stratumSummary$Season[indx]) 
          if (!byCountry & !byArea & bySeason) namesVec=paste(stratumSummary$Season[indx]) 
          if (byCountry) namesVec=paste(stratumSummary$Country[indx]) 
          if (bySampled & !byCountry & nArea==1 & nSeason==1) namesVec=paste(stratumSummary$Season[indx]) 
        }
        
        
        cumulativeY <- cumsum(yvals)
        yvals[yvals>newYmax] <- newYmax
        
        if ((newYmax==-Inf)) {
          plot(0,0,type="n",axes=FALSE,xlab="",ylab="")
          box()
        } else {
          b <- barplot(yvals,names=namesVec,las=2,cex.names=0.7,col=colVec,ylim=c(0,newYmax),yaxs="i")   
          
          if (bySampled & markSampled) {
            nSampled <- sum(stratumSummary$Sampled[indx])
            if(nSampled>0){
              arrows(b[1]-(b[2]-b[1])/2,newYmax*102/106,b[nSampled]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1) 
              arrows(b[nSampled]+(b[2]-b[1])/2,newYmax*102/106,b[length(b)]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1) 
              if(plotType!="DiscProvided"){
                text((b[nSampled]+b[1])/2,newYmax*104/106,"sampled",cex=0.8)
                text((b[length(b)]+b[nSampled])/2+(b[2]-b[1])/2,newYmax*104/106,"unsampled",cex=0.8)
              }else{
                text((b[nSampled]+b[1])/2,newYmax*104/106,"Landings with Discards",cex=0.8)
                text((b[length(b)]+b[nSampled])/2+(b[2]-b[1])/2,newYmax*104/106,"no Discards",cex=0.8)	
              }
            } else {
              arrows(b[1]-(b[2]-b[1])/2,newYmax*102/106,b[length(b)]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1) 
              if(plotType!="DiscProvided"){
                text(b[length(b)]+(b[2]-b[1])/4,newYmax*104/106,"unsampled",cex=0.8)
              }else{
                text(b[length(b)]+(b[2]-b[1])/4,newYmax*104/106,"no Discards",cex=0.8)
              }
            }
          }
          if (countryLegend) legend("topright",inset=0.05,legend=countryColours$Country,col=countryColours$Colour,pch=15)
          box()
          if (plotType=="LandPercent") lines(b-(b[2]-b[1])/2,cumulativeY,type="s")
          if (plotType=="LandPercent") abline(h=c(5,1),col="grey",lty=1)
          if (plotType=="LandPercent") abline(h=c(90,95,99),col="grey",lty=1)
          if (plotType=="LandPercent") abline(h=100)
        }
        if (!bySeason & !byArea) title.txt <- paste(stock)
        if (!bySeason & byArea) title.txt <- paste(stock,area)
        if (bySeason & !byArea & !(byCountry | byFleet | bySampled)) title.txt <- paste(stock)
        if (bySeason & !byArea & (byCountry | byFleet | bySampled)) title.txt <- paste(stock,season)
        if (bySeason & byArea) title.txt <- paste(stock,area,season)
        title.txt <- paste(title.txt,plotType)
        title(title.txt)
      } else {
        par(mfrow=c(2,2))
        listSample <- unique(paste(stratumSummary$SampledD,stratumSummary$SampledL))
        for(i in 1:length(listSample)){
          idx <- which(stratumSummary$SampledD[indx]==strsplit(listSample[i],' ')[[1]][1] & stratumSummary$SampledL[indx]==strsplit(listSample[i],' ')[[1]][2])
          if(length(idx)>0){
            if(byCountry) {
              colVec <- countryColours$Colour[match(stratumSummary$Country[indx][idx],countryColours$Country)]
            } else {
              colVec <- "grey"
            }  
            
            if (!is.null(ymax)) newYmax <- ymax
            if (is.null(ymax)) newYmax <- max(yvals,na.rm=TRUE)
            if (is.null(ymax) & plotType=="LandPercent") newYmax <- max(cumsum(yvals),na.rm=TRUE)
            #if (is.null(ymax) & plotType=="CatchWt") newYmax <- max(colSums(yvals),na.rm=TRUE) #YV
            #if (plotType=="CatchWt") colVec <- c("grey","black") #YV
            #if (plotType=="CatchWt") countryLegend <- FALSE #YV
            if (markSampled) newYmax <- 1.06*newYmax
            if (byFleet) namesVec=stratumSummary$Fleet[indx][idx]
            if (!byFleet) {
              if (byArea & bySeason) namesVec=paste(stratumSummary$Area[idx],stratumSummary$Season[indx][idx]) 
              if (!byCountry & !byArea & bySeason) namesVec=paste(stratumSummary$Season[indx][idx]) 
              if (byCountry) namesVec=paste(stratumSummary$Country[indx][idx]) 
              if (bySampled & !byCountry & nArea==1 & nSeason==1) namesVec=paste(stratumSummary$Season[indx][idx]) 
            }
            
            
            cumulativeY <- cumsum(yvals[indx][idx])
            yvals[yvals>newYmax] <- newYmax
            
            if ((newYmax==-Inf)) {
              plot(0,0,type="n",axes=FALSE,xlab="",ylab="")
              box()
            } else {
              b <- barplot(yvals[indx][idx],names=namesVec,las=2,cex.names=0.7,col=colVec,ylim=c(0,newYmax),yaxs="i")   
              
              #if (bySampled & markSampled) {
              #  nSampledD <- sum(stratumSummary$SampledD[idx]==T)
              #  nSampledL <- sum(stratumSummary$SampledL[idx]==T)
              #  if (nSampledD>0) arrows(b[1]-(b[2]-b[1])/2,newYmax*102/106,b[nSampledD]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1) 
              #  if (nSampledD>0) arrows(b[nSampledD]+(b[2]-b[1])/2,newYmax*102/106,b[length(b)]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1) 
              
              #  if (nSampledL>0) arrows(b[1]-(b[2]-b[1])/2,newYmax*92/106,b[nSampledL]+(b[2]-b[1])/2,newYmax*92/106,code=3,length=0.1) 
              #  if (nSampledL>0) arrows(b[nSampledL]+(b[2]-b[1])/2,newYmax*92/106,b[length(b)]+(b[2]-b[1])/2,newYmax*92/106,code=3,length=0.1) 
              
              #  } 
              if (countryLegend) legend("topright",inset=0.05,legend=countryColours$Country,col=countryColours$Colour,pch=15)
              box()
              if (plotType=="LandPercent") lines(b-(b[2]-b[1])/2,cumulativeY,type="s")
              if (plotType=="LandPercent") abline(h=c(5,1),col="grey",lty=1)
              if (plotType=="LandPercent") abline(h=c(90,95,99),col="grey",lty=1)
              if (plotType=="LandPercent") abline(h=100)
            }
            if (!bySeason & !byArea) title.txt <- paste(stock)
            if (!bySeason & byArea) title.txt <- paste(stock,area)
            if (bySeason & !byArea & !(byCountry | byFleet | bySampled)) title.txt <- paste(stock)
            if (bySeason & !byArea & (byCountry | byFleet | bySampled)) title.txt <- paste(stock,season)
            if (bySeason & byArea) title.txt <- paste(stock,area,season)
            title.txt <- paste(title.txt,plotType, "D/L", listSample[i])
            title(title.txt)
          }
        }
      }
    }
  }                                                                                                                
}

# --------------------------------------------------------------------------------------
# Functions for the diagnostics of the North Sea sole assessment
#
# Author  : Jan Jaap Poos
#           (+David Miller)
#
# Last edited: APRIL 2011
# --------------------------------------------------------------------------------------
########---------------------------+INDEX+------------------------------########
## DATA
#  data_list: Creates data frame with list of values (good for xyplot)
#  totalStk: Adds up totals for stock objects - computes landings, discards, catch, stock totals and sets units

## Assessment (XSA)
#  diagnostics: Runs XSA diagnostics
#  stock_Sum_Sole: Creates a stock summary tables for ICES WG report (no discaards!)
#  write_Stock: Outputs a .txt file of slots from an FLR stock object
#  write_Indices: Outputs a .txt file of slots from an FLR indices object

## STF
#  scanSTF: Runs STFs for a range of Fmult values
#  summTableSTF: Creates a summary from 'scanSTF' outputs
#  inputTableSTF: Creates a table of inputs to the STF
#  ageTableSTF: Creates a table by year AND AGE for the projection period 

cor.tun <- function(stk.tun){ for(i in names(stk.tun)) if(dim(stk.tun[[i]]@index)[1]>1) plot(stk.tun[[i]],type="internal",main=name(stk.tun[[i]]))}              

########--------------------------+++++++++-----------------------------########
####                                 DATA                                   ####
########--------------------------+++++++++-----------------------------########
# NAME: totalStk
# DOES: Adds up totals for stock objects - computes landings, discards, catch, stock totals and sets units
totalStk <- function(stk, Units){
  landings(stk) <- computeLandings(stk)
  discards(stk) <- computeDiscards(stk)
  catch.n(stk)  <- landings.n(stk)+discards.n(stk)
  catch.wt(stk) <-(landings.n(stk)*landings.wt(stk)+discards.n(stk)*discards.wt(stk))/catch.n(stk)
  # Make Catch=Landings if no discards
  if(sum(catch.n(stk),na.rm=T)==0) {
    catch.n(stk)  <- landings.n(stk)
    catch.wt(stk)  <- landings.wt(stk)
  }
  catch(stk)    <- computeCatch(stk)
  stock(stk)    <- computeStock(stk)
  units(stk)[1:17] <- as.list(c(rep(Units,4), "NA", "NA", "f", "NA", "NA"))
  return(stk)
}

# readVPAIntercatch_length {{{

#' Reads a single file with one year of data in VPA format as output by
#' ICES Intercatch. Modified from FLCore::readVPAIntercatch
#'
#' @param file Intercatch VPA file to load
#' @param conserv_unit Boolean variable. If TRUE stay with numbers or grams units if FALSE change units into Thousands or kilograms.
#'  Apply only to Intercatch canum and weca. \Default{TRUE}
#' @param step Apply  only to Intercatch canum and weca. Length resolution wanted for outputs either mm or cm. \Default{cm}
#' @return An object of class FLQuant.

readVPAIntercatch_length <- function(file, conserv_unit=T, res="cm") {
  
  contents <- scan(file=file, what="", sep="\n", quiet=TRUE)
  
  # 1. Catch category
  # 2. stock
  # 3. 1 2
  # 4. year
  year <- as.numeric(strsplit(contents[4], " ")[[1]])[1]
  # 5. ages
  len <- do.call(seq, as.list(as.numeric(strsplit(contents[5], " ")[[1]])))
  # 6. 1
  # 7. units
  units <- switch(contents[7],
                  "in numbers" = "1",
                  "in grams" = "gr",
                  "in tonnes" = "tonnes",
                  "NA")
  # 8. data: 300,999.876
  data <- as.numeric(strsplit(gsub(",", "",
                                   gsub("\\s+", "-", contents[8])), "-")[[1]])
  
  # CORRECT age for caton
  if(length(data) == 1)
    len <- "all"
  
  if(units == "1") {
    if(!conserv_unit){
      data <- data / 1000
      units <- "thousands" 
    }
    if(res=="cm") {
      data <- aggregate(num~len, data=data.frame(len=trunc(len/10)*10, num=data), sum)
      len <- data$len
      data <- data$num
    }
  } else if (units == "gr") {
    if(!conserv_unit){
      data <- data / 1000
      units <- "kg" 
    }
    if(res=="cm") {
      data <- aggregate(num~len, data=data.frame(len=trunc(len/10)*10, num=data), sum)
      len <- data$len
      data <- data$num
    }
  }
  return(FLQuant(data, dimnames=list(len=len, year=year), units=units))
} # }}}


##########################################################################################################################
#"Mean weight weighted by numbers at age or length” weighting
ICallocation  <- function(numbers_group, weights_group, NoSample){
  #Numbers at age
  numbers_group2<-numbers_group
  NofR<-nrow(numbers_group)
  
  head(numbers_group)
  
  for (i in 14:(ncol(numbers_group)-2)){
    numbers_group[NofR+1,i]<-sum(numbers_group[,i])
    numbers_group[NofR+1,i+1][is.na(numbers_group)[NofR+1,i+1]] <- 0
  }
  
  numbers_group["caton"][is.na(numbers_group["caton"])] <- 0
  numbers_group[NofR+1,"caton"]<-sum(numbers_group$caton)
  
  numbers_group3<-numbers_group[NofR+1,]
  numbers_group4<-numbers_group3
  
  NofC<-ncol(NoSample)
  
  for (i in c(1:nrow(NoSample))){
    for (j in 14:(ncol(numbers_group3)-2)){
      
      numbers_group4[i,j]<-NoSample[i,"caton"]*(numbers_group3[1,j]/numbers_group3[1,"caton"])
    }
  }
  
  numbers_group4<-numbers_group4[,c(14:(ncol(numbers_group4)-2))]
  
  #Mean weights at age
  for (i in 14:(ncol(numbers_group)-2)){
    for (j in 1:(nrow(numbers_group)-1)){
      numbers_group2[j,i]<-numbers_group[j,i]/numbers_group[NofR+1,i]
    }
  }
  
  numbers_group2[is.na(numbers_group2)] <- 0
  numbers_group2<-numbers_group2[order(numbers_group2$caton),]
  weights_group<-weights_group[order(weights_group$caton),]
  
  weights_group2<-numbers_group2
  
  for (i in 14:(ncol(numbers_group2)-2)){
    for (j in 1:nrow(numbers_group2)){
      weights_group2[j,i]<-numbers_group2[j,i]*weights_group[j,i]
    }
  }
  
  for (i in 14:(ncol(weights_group2)-2)){
    weights_group2[NofR+1,i]<-sum(weights_group2[,i],na.rm=T)
    weights_group2[NofR+1,i+1][is.na(weights_group2)[NofR+1,i+1]] <- 0
  }
  
  weights_group2<-weights_group2[NofR+1,c(14:(ncol(weights_group2)-2))]
  
  weights_group3 <- weights_group2
  for(i in 2:dim(numbers_group4)[1]){
    weights_group3 <- rbind(weights_group3,weights_group2)
  }
  
  return(list(num = cbind(NoSample, numbers_group4), wgt = cbind(NoSample, weights_group3)))
}
