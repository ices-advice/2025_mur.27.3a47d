## Additional functions

## Before:
## After:

# from degree to radians
radians <- function(x) {y <- x*pi/180}

# Extract spatial distribution statistics
# x <- dys$'1991'
map_stat <- function(x, weight="nums", pHull = F) {
  x <- data.frame(x)
  if(all(is.na(x$nums))){
    x <- data.frame(convex_surf=NA,
                    concav_surf=NA,       
                    lon=NA,      
                    lat=NA,    
                    lon_up=NA,   
                    lat_up=NA,     
                    lon_lo=NA,   
                    lat_lo=NA)
    if(pHull) return(list(summary=x,convex=NA,concav=NA)) else return(x)
  } 
  x <- subset(x,!is.na(x[,weight]))
  x <- SpatialPointsDataFrame(x[,c('lon','lat')], data= x[,names(x)%in%c(weight,"haul.id")])
  proj4string(x) <- CRS("+init=epsg:4326")
  x <- spTransform(x, CRS("+init=epsg:3035"))
  x <- as.data.frame(x)
  x <- x %>% rename(lon = coords.x1, lat = coords.x2)
  
  #convex hull
  s <- x[chull(as.matrix(x[,c('lon','lat')])),c('lon','lat')]
  s <- as.matrix(rbind(s,s[1,]))
  s <- st_sfc(st_polygon(list(s))) %>%
    st_set_crs(3035)
  
  # plot(s)
  #concave hull
  s1 <- concaveman(as.matrix(x[,c('lon','lat')]))
  s1 <- rbind(s1,s1[1,])
  s1 <- st_sfc(st_polygon(list(s1))) %>%
    st_set_crs(3035)
  
  # Calculate area of the polygon

  x <- data.frame(lon=weighted.mean(x=x$lon,w=x[,names(x)==weight],na.rm=T),
                  lat=weighted.mean(x=x$lat,w=x[,names(x)==weight],na.rm=T),
                  sdlon=weighted.sd(x=x$lon,w=x[,names(x)==weight],na.rm=T),
                  sdlat=weighted.sd(x=x$lat,w=x[,names(x)==weight],na.rm=T), 
                  convex_surf=as.numeric(st_area(s))/10^6,concav_surf=as.numeric(st_area(s1))/10^6)
  s <- as(s,'Spatial')
  s1 <- as(s1,'Spatial')
  s <- spTransform(s, CRS("+init=epsg:4326"))
  s1 <- spTransform(s1, CRS("+init=epsg:4326"))
  s$ID <-1
  s1$ID <-1
  
  x$lon_up <- x$lon + x$sdlon
  x$lon_lo <- x$lon - x$sdlon
  x$lat_up <- x$lat + x$sdlat
  x$lat_lo <- x$lat - x$sdlat
  
  x <- SpatialPointsDataFrame(x[,c('lon','lat')], data= x[,!names(x)%in%c('lon','lat')])
  proj4string(x) <- CRS("+init=epsg:3035")
  x <- spTransform(x, CRS("+init=epsg:4326"))
  x <- as.data.frame(x)
  x <- x %>% rename(lon = coords.x1, lat = coords.x2)
  x <- SpatialPointsDataFrame(x[,c('lon_up','lat_up')], data= x[,!names(x)%in%c('lon_up','lat_up')])
  proj4string(x) <- CRS("+init=epsg:3035")
  x <- spTransform(x, CRS("+init=epsg:4326"))
  x <- as.data.frame(x)
  x <- x %>% rename(lon_up = coords.x1, lat_up = coords.x2)
  x <- SpatialPointsDataFrame(x[,c('lon_lo','lat_lo')], data= x[,!names(x)%in%c('lon_lo','lat_lo')])
  proj4string(x) <- CRS("+init=epsg:3035")
  x <- spTransform(x, CRS("+init=epsg:4326"))
  x <- as.data.frame(x)
  x <- x %>% rename(lon_lo = coords.x1, lat_lo = coords.x2)
  x <- x[,!names(x)%in%c('sdlon','sdlat')]
  
  if(pHull) return(list(summary=x,convex=s,concav=s1)) else return(x)
}

# extract index from surveyIdx object
gather.idx <- function(m, bound=T){
  if(is(m)!="surveyIdx") stop("Should provided a surveyIdx object")
  tmp <- as.data.frame(m$idx)
  tmp$Year <- rownames(tmp)
  idx <- tmp %>% gather(age, index, -Year)
  
  if(bound){
    tmp <- as.data.frame(m$up)
    names(tmp) <- names(as.data.frame(m$idx))
    tmp$Year <- rownames(m$idx)
    tmp <- tmp %>% gather(age, up, -Year)
    idx <- merge(idx, tmp)
    tmp <- as.data.frame(m$lo)
    names(tmp) <- names(as.data.frame(m$idx))
    tmp$Year <- rownames(m$idx)
    tmp <- tmp %>% gather(age, lo, -Year)
    idx <- merge(idx, tmp)
    idx$CV <- (log(idx$up)-log(idx$lo))/4
  }
  return(idx)
}

#extract index of retrospective analysis from surveyIdx object
gather.retro <- function(retro, bound = F){
  retro <- lapply(retro, function(x) {x <- gather.idx(x, bound)
  x$retro <- max(x$Year)
  return(x)})
  retro <- bind_rows(retro)
}

# fixed an issue in surveyIdxPlots function with spatial prediction
surveyIdxPlots <- function (x, dat, alt.idx = NULL, myids, cols = 1:length(x$pModels), 
          select = c("index", "map", "residuals", "fitVsRes"), par = list(mfrow = c(3, 
                                                                                    3)), colors = rev(heat.colors(6)), map.cex = 1, plotByAge = TRUE, 
          legend = TRUE, predD = NULL, year = NULL, main = NULL, legend.signif = 3, 
          legend.pos = "topright", restoreOldPar = FALSE, mapBubbles = FALSE, 
          cutp = NULL, map.pch = 16, ...) 
{
  if (!plotByAge & !is.null(par)) {
    op <- par(par)
    if (restoreOldPar) 
      on.exit(par(op))
  }
  mainwasnull <- is.null(main)
  for (a in cols) {
    if (mainwasnull) 
      main <- paste("Age group", colnames(dat$Nage)[a])
    if (plotByAge & !is.null(par)) {
      op <- par(par)
      if (restoreOldPar) 
        on.exit(par(op))
    }
    if (any(select == "index")) {
      ys = range(as.numeric(levels(dat$Year)))
      ys = ys[1]:ys[2]
      yl = range(c(x$idx[, a], 0, x$lo[, a], x$up[, a])/mean(x$idx[, 
                                                                   a]), na.rm = TRUE)
      if (!is.null(alt.idx) && a <= ncol(alt.idx)) {
        yl = range(c(alt.idx[, a]/mean(alt.idx[, a]), 
                     yl)) * 1.1
        plot(ys, alt.idx[, a]/mean(alt.idx[, a], na.rm = TRUE), 
             ylim = yl, col = 2, ylab = "Index", xlab = "Year", 
             main = main)
      }
      else {
        plot(ys, rep(NA, length(ys)), ylim = yl, col = 2, 
             ylab = "Index", xlab = "Year", main = main)
      }
      idx = x$idx
      lo = x$lo
      up = x$up
      idx[x$idx <= 0] = NA
      lo[x$idx <= 0] = NA
      up[x$idx <= 0] = NA
      lines(ys, idx[, a]/mean(idx[, a], na.rm = TRUE), 
            lwd = 2)
      lines(ys, lo[, a]/mean(idx[, a], na.rm = TRUE), lwd = 2, 
            lty = 2)
      lines(ys, up[, a]/mean(idx[, a], na.rm = TRUE), lwd = 2, 
            lty = 2)
      if (legend && !is.null(alt.idx)) 
        legend(legend.pos, pch = c(1, NA), lty = c(NA, 
                                                   1), col = c(2, 1), legend = c("alt.idx", "GAM"))
    }
    if (any(select == "map")) {
      xlims = range(dat$lon, na.rm = TRUE)
      ylims = range(dat$lat, na.rm = TRUE)
      mapvals = NULL
      if (is.null(predD)) {
        tmp = subset(dat, haul.id %in% myids)
      }
      else {
        tmp = predD
      }
      if (is.null(year)) {
        concT = surveyIndex:::concTransform(log(x$gPreds[[a]]))
        mapvals = x$gPreds[[a]]
      }
      else {
        y = which(as.numeric(as.character(names(x$gPreds2[[a]]))) == 
                    year)
        if (length(y) == 0) 
          stop(paste("Year", year, "age group", a, "not found."))
        concT = surveyIndex:::concTransform(log(x$gPreds2[[a]][[y]]))
        mapvals = x$gPreds2[[a]][[y]]
      }
      if (length(colors) > 1) 
        zFac = cut(concT, 0:length(colors)/length(colors))
      else zFac = 1
      if (length(map.cex) > 1) 
        sFac = cut(log(x$gPreds[[a]]), length(map.cex))
      else sFac = 1
      myCols = colors
      plot(tmp$lon, y = tmp$lat, col = 1, pch = 1, cex = map.cex[sFac], 
           xlim = xlims, ylim = ylims, xlab = "Longitude", 
           ylab = "Latitude", main = main, ...)
      points(tmp$lon, y = tmp$lat, col = myCols[zFac], 
             pch = 16, cex = map.cex[sFac])
      maps::map("worldHires", xlim = xlims, ylim = ylims, 
                fill = TRUE, plot = TRUE, add = TRUE, col = grey(0.5))
      if (legend) {
        maxcuts = aggregate(mapvals ~ zFac, FUN = max)
        mincuts = aggregate(mapvals ~ zFac, FUN = min)
        mm = mean(mapvals)
        ml = signif(mincuts[, 2]/mm, legend.signif)
        ml[1] = 0
        leg = paste0("[", ml, ",", signif(maxcuts[, 2]/mm, 
                                          legend.signif), "]")
        legend(legend.pos, legend = leg, pch = 16, col = colors, 
               bg = "white")
      }
    }
    if (any(select == "absolutemap") || any(select == "CVmap")) {
      if (is.null(year) || length(year) == 0) 
        stop("argument 'year' must be vector of length>=1 for type 'absolutemap'")
      if (!all(year %in% levels(dat$Year))) 
        stop("invalid years selected")
      xlims = range(dat$lon, na.rm = TRUE)
      ylims = range(dat$lat, na.rm = TRUE)
      if (any(select == "absolutemap")) 
        colsel = "gPreds2"
      else colsel = "gPreds2.CV"
      goodyears = which(names(x[[colsel]][[a]])%in%as.character(year))
      ally = data.frame(val = numeric(0), year = character(0))
      # cc = 0
      for (y in goodyears) {
        # cc = cc + 1
        ally = rbind(ally, data.frame(val = x[[colsel]][[a]][[y]], 
                                      year = names(x[[colsel]][[a]])[y]))
      }
      ally$conc = surveyIndex:::concTransform(log(ally$val))
      if (is.null(cutp)) {
        ally$zFac = cut(ally$conc, 0:length(colors)/length(colors))
      } else {
        if (length(cutp) != length(colors) + 1) 
          stop("incompatible number of colors and length of cutp")
        ally$zFac = cut(ally$val, cutp)
      }
      bubbleScale = 0.005 * max(dat$Nage[, a])
      for (yy in year) {
        sel = which(ally$year == yy)
        if (is.null(predD)) {
          tmp = subset(dat, haul.id %in% myids)
        }
        else {
          tmp = predD
          if (is.list(tmp) && !class(tmp) %in% c("data.frame", 
                                                 "DATRASraw")) 
            tmp = predD[[as.character(yy)]]
        }
        plot(tmp$lon, y = tmp$lat, col = 1, pch = 1, 
             cex = map.cex, xlab = "Longitude", ylab = "Latitude", 
             axes = FALSE, type = ifelse(length(sel) > 0, 
                                         "p", "n"))
        box()
        title(yy, line = 1)
        if (length(sel) == 0) 
          next
        points(tmp$lon, y = tmp$lat, col = colors[as.numeric(ally$zFac[sel])], 
               pch = map.pch, cex = map.cex)
        maps::map("worldHires", xlim = xlims, ylim = ylims, 
                  fill = TRUE, plot = TRUE, add = TRUE, col = grey(0.5))
        if (mapBubbles) {
          dy = subset(dat, Year == yy)
          points(dy$lon, dy$lat, cex = sqrt(dy$Nage[, 
                                                    a]/bubbleScale))
        }
        if (legend && yy == year[1]) {
          if (is.null(cutp)) {
            maxcuts = aggregate(val ~ zFac, data = ally, 
                                FUN = max)
            mincuts = aggregate(val ~ zFac, data = ally, 
                                FUN = min)
            mm = mean(ally$val)
            ml = signif(mincuts[, 2]/mm, legend.signif)
            ml[1] = 0
            leg = paste0("[", ml, ",", signif(maxcuts[, 
                                                      2]/mm, legend.signif), "]")
            legend(legend.pos, legend = leg, pch = 16, 
                   col = colors, bg = "white")
          }
          else {
            legend(legend.pos, legend = levels(ally$zFac), 
                   pch = 16, col = colors, bg = "white")
          }
        }
      }
    }
    for (k in 1:length(select)) {
      ss = suppressWarnings(as.numeric(select[k]))
      if (!is.na(ss)) {
        plot.gam(x$pModels[[a]], select = ss, main = main, 
                 ...)
      }
    }
    if (any(select == "residuals") || any(select == "fitVsRes") || 
        any(select == "resVsYear") || any(select == "resVsShip") || 
        any(select == "spatialResiduals")) {
      resi <- x$residuals[[a]]
    }
    if (any(select == "residuals")) {
      hist(resi, nclass = 30, main = main, xlab = "Residuals", 
           ...)
    }
    if (any(select == "fitVsRes")) {
      plot(fitted(x$pModels[[a]]), residuals(x$pModels[[a]]), 
           xlab = "Fitted", ylab = "Residuals", main = main, 
           ...)
    }
    if (any(select == "resVsYear")) {
      plot(dat$Year, resi, main = main, xlab = "Year", 
           ylab = "Residuals", ...)
    }
    if (any(select == "resVsShip")) {
      plot(dat$Ship, resi, main = main, xlab = "Year", 
           ylab = "Residuals", ...)
    }
    if (any(select == "spatialResiduals")) {
      scale <- 3 * map.cex
      if (is.null(year) || length(year) > 1) 
        stop("a single year must be supplied")
      sel <- which(dat[[2]]$Year == as.character(year))
      plot(dat$lon, dat$lat, type = "n", xlab = "Longitude", 
           ylab = "Latitude", main = main, ...)
      maps::map("worldHires", fill = TRUE, plot = TRUE, 
                add = TRUE, col = grey(0.5))
      positive = resi[sel] > 0
      points(dat$lon[sel][positive], dat$lat[sel][positive], 
             pch = 1, cex = scale * sqrt(resi[sel][positive]), 
             col = "blue")
      points(dat$lon[sel][!positive], dat$lat[sel][!positive], 
             pch = 1, cex = scale * sqrt(-resi[sel][!positive]), 
             col = "red")
    }
  }
}
