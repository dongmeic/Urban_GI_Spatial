# Objectives: point pattern analysis of green infrastructure (GI) and water quality (WQ) sampling sites
# Input: spatial data frame of GI and WQ
# Output: 1. A map to compare the mean center and standard distance;
#         2. Results of Quadrat Analysis and Nearest Neighbor Analysis;
#         3. Maps for Kernel density estimation;
#         4. Results of second-order analysis of point patterns (K, L and G functions)

# libraries
library(rgdal)
library(spatstat)
library(sp)
library(raster)
library(GISTools)

# functions
# to draw northarrow
northarrow <- function(loc,size,bearing=0,cols,cex=1,...) {
  # checking arguments
  if(missing(loc)) stop("loc is missing")
  if(missing(size)) stop("size is missing")
  # default colors are white and black
  if(missing(cols)) cols <- rep(c("white","black"),8)
  # calculating coordinates of polygons
  radii <- rep(size/c(1,4,2,4),4)
  x <- radii[(0:15)+1]*cos((0:15)*pi/8+bearing)+loc[1]
  y <- radii[(0:15)+1]*sin((0:15)*pi/8+bearing)+loc[2]
  # drawing polygons
  for (i in 1:15) {
    x1 <- c(x[i],x[i+1],loc[1])
    y1 <- c(y[i],y[i+1],loc[2])
    polygon(x1,y1,col=cols[i])
  }
  # drawing the last polygon
  polygon(c(x[16],x[1],loc[1]),c(y[16],y[1],loc[2]),col=cols[16])
  # drawing letters
  b <- c("E","N","W","S")
  for (i in 0:3) text((size+par("cxy")[1])*cos(bearing+i*pi/2)+loc[1],
                      (size+par("cxy")[2])*sin(bearing+i*pi/2)+loc[2],b[i+1],
                      cex=cex)
}

add.northarrow <- function(){
  northarrow(c(980000,260000),7000)
}

add.scale <- function(){
  GISTools::map.scale(1040000,140000,19685.083,"km",3,2,sfcol='brown')
} #19685 feet 0.5 inches equals to 6 km

mc_sd <- function(spdf){
  xy <- coordinates(spdf)
  xy <- unique(xy)
  mc <- apply(xy, 2, mean)
  sd <- sqrt(sum((xy[,1] - mc[1])^2 + (xy[,2] - mc[2])^2) / nrow(xy))
  bearing <- 1:360 * pi/180
  cx <- mc[1] + sd * cos(bearing)
  cy <- mc[2] + sd * sin(bearing)
  return(list(mc, cx, cy))
}

quadrat_analysis <- function(bound, spdf, res){
  # need to read boundary data (e.g., "nyadwi_dis") first
  city <- readOGR(dsn = paste0(infolder, "BD"), layer =bound, stringsAsFactors = FALSE)
  r <- raster(city)
  res(r) <- res
  r <- rasterize(city, r)
  quads <- as(r, 'SpatialPolygons')
  npts <- rasterize(coordinates(spdf), r, fun='count', background=0)
  ncpts <- mask(npts, r)
  f <- freq(ncpts, useNA='no')
  # number of quadrats
  quadrats <- sum(f[,2])
  # number of cases
  cases <- sum(f[,1] * f[,2])
  mu <- cases / quadrats
  ff <- data.frame(f)
  colnames(ff) <- c('K', 'X')
  ff$Kmu <- ff$K - mu
  ff$Kmu2 <- ff$Kmu^2
  ff$XKmu2 <- ff$Kmu2 * ff$X
  s2 <- sum(ff$XKmu2) / (sum(ff$X)-1)
  VMR <- s2 / mu
  m <- quadrats
  chi = VMR*(m-1)
  p = 1-pchisq(chi,m-1)
  return(list(VMR, p))
}

convert.z.score<-function(z, one.sided=NULL) {
  if(is.null(one.sided)) {
    pval = pnorm(-abs(z));
    pval = 2 * pval
  } else if(one.sided=="-") {
    pval = pnorm(z);
  } else {
    pval = pnorm(-z);                                                                                 
  }
  return(pval);
}

nearest_neighbor_analysis <- function(ppp,TotPoints){
  nearestNeighbor <- nndist(ppp)
  nearestNeighbor=as.data.frame(as.numeric(nearestNeighbor))
  colnames(nearestNeighbor) = "Distance"
  # basic mean distance
  NND_bar <- mean(nearestNeighbor$Distance)
  # perfectly random, distance
  density <- intensity.ppp(ppp)
  NND_R <- 1/(2*sqrt(density)) 
  NND_D <- 1.07453/sqrt(density)
  R <- NND_bar/NND_R
  sigma_NND <- 0.26136/sqrt(TotPoints*density)
  Z <- (NND_bar-NND_R)/sigma_NND
  # run the function first
  p <- convert.z.score(Z)
  return(list(R, p))
}
# global settings
setwd("/nfs/urbangi-data/spatial_data/output")
infolder <- "/nfs/urbangi-data/spatial_data/"
crs <- CRS("+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333 
           +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83 
           +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")

# input data
# boundary - census tract
tract <- readOGR(dsn = paste0(infolder, "BD"), layer ="cb_2015_36_tract_500k_clip_proj", stringsAsFactors = FALSE)
# boundary - dissovled administrative boundary with water 
nyadwi <- readOGR(dsn = paste0(infolder, "BD"), layer ="nyadwi_dis", stringsAsFactors = FALSE)
# boundary - dissovled administrative boundary without water
city <- readOGR(dsn = paste0(infolder, "BD"), layer ="nyad_dis", stringsAsFactors = FALSE)
# green infrastructure
gi.spdf <- readOGR(dsn = ".", layer ="gi_pts_in", stringsAsFactors = FALSE)
gi.spdf <- remove.duplicates(gi.spdf)
gi.df <- read.csv("gi_pts.csv", stringsAsFactors = FALSE)
coordinates(gi.df) =~X_Coordina+Y_Coordina
gi.df <- remove.duplicates(gi.df)
# water quality data
wq.spdf <- readOGR(".", layer="wq_pts_in", stringsAsFactors = FALSE)

# mean center and standard distance
png("mean_center.png", width=9, height=8, units="in", res=300)
par(xpd=FALSE,mfrow=c(1,1),mar=c(1.5,0.5,2.5,0.5))
plot(wq.spdf, pch=19, main="Stormwater green infrastructure in New York City")
plot(tract, bord="dimgrey", add=T)
plot(nyadwi, lwd=1.5, bord="dimgrey", add=T)
plot(gi.spdf, col='green', cex=.6, pch=16, add=T)
points(cbind(mc_sd(gi.spdf)[[1]][1], mc_sd(gi.spdf)[[1]][2]), pch='*', col='darkgreen', cex=4)
lines(cbind(mc_sd(gi.spdf)[[2]], mc_sd(gi.spdf)[[3]]), col='darkgreen', lwd=2, lty=2)
plot(wq.spdf, col='blue', pch=19, add=T)
points(cbind(mc_sd(wq.spdf)[[1]][1], mc_sd(wq.spdf)[[1]][2]), pch='*', col='red', cex=4)
lines(cbind(mc_sd(wq.spdf)[[2]], mc_sd(wq.spdf)[[3]]), col='red', lwd=2, lty=2)
add.northarrow()
add.scale()
text(cbind(912000, 250000), family="Arial Black", "Legend", cex=1.2)
points(910000, 230000, pch=19, col="blue")
text(cbind(933000, 230000), "WQ sampling sites")
points(910000, 240000, pch=16, cex=.6, col="green")
text(cbind(928000, 240000), "SGI locations")
points(910000, 220000, pch='*', col='darkgreen', cex=4)
text(cbind(932000, 220000), "SGI mean center")
points(910000, 210000, pch='*', col='red', cex=4)
text(cbind(932000, 210000), "WQ mean center")
segments(907000, 200000, 914000, 200000, col="darkgreen", lwd=2, lty=2)
text(cbind(925000, 200000), "SGI SD")
segments(907000, 190000, 914000, 190000, col="red", lwd=2, lty=2)
text(cbind(925000, 190000), "WQ SD")
dev.off()

# Quadrat Analysis
gi_qa <- quadrat_analysis("nyadwi_dis", gi.spdf, 1000)
c(gi_qa[[1]], gi_qa[[2]]) # 10.49334  0.00000
wq_qa <- quadrat_analysis("nyadwi_dis", wq.spdf, 1000)
c(wq_qa[[1]], wq_qa[[2]]) # 1.534345 0.000000

# Nearest Neighbor Analysis
window <- as.owin(nyadwi)
gi.ppp <- ppp(x=gi.spdf@coords[,1],y=gi.spdf@coords[,2],window=window)
proj4string(gi.spdf) <- proj4string(nyadwi)
gi.nna <- nearest_neighbor_analysis(gi.ppp, poly.counts(gi.spdf, nyadwi))
c(gi.nna[[1]], gi.nna[[2]]) # 0.221568 0.000000
wq.ppp <- ppp(x=wq.spdf@coords[,1],y=wq.spdf@coords[,2],window=window)
proj4string(wq.spdf) <- proj4string(nyadwi)
wq.nna <- nearest_neighbor_analysis(wq.ppp, poly.counts(wq.spdf, nyadwi))
c(wq.nna[[1]], wq.nna[[2]]) # 4.874665e-01 7.233301e-60 

# Kernel density estimation
gi.dens <- kde.points(gi.spdf, lims=nyadwi)
wq.dens <- kde.points(wq.spdf, lims=nyadwi)
png("Kernel_density.png", width=10, height=6, units="in", res=300)
par(mfrow=c(1,2),mar=c(1,0.5,2,0.5))
level.plot(gi.dens)
masker <- poly.outer(gi.dens, nyadwi, extend=100)
add.masking(masker)
plot(city, add=T, bord="dimgrey")
plot(nyadwi, add=T, lwd=1.5, bord="dimgrey")
title("SGI density")
level.plot(wq.dens)
masker <- poly.outer(wq.dens, nyadwi, extend=100)
add.masking(masker)
plot(city, add=T, bord="dimgrey")
plot(nyadwi, add=T, lwd=1.5, bord="dimgrey")
title("WQ sampling density")
dev.off()

# K, L, G functions
kf <- Kest(gi.ppp, correction = 'border')
par(mfrow=c(1,1),mar=c(4,4,2,1))
plot(kf)
gi.kf.env <- envelope(gi.ppp, Kest, correction = 'border')
gi.lf.env <- envelope(gi.ppp, Lest, correction = 'border')
gi.gf.env <- envelope(gi.ppp, Gest, correction = 'border')

wq.kf.env <- envelope(wq.ppp, Kest, correction = 'border')
wq.lf.env <- envelope(wq.ppp, Lest, correction = 'border')
wq.gf.env <- envelope(wq.ppp, Gest, correction = 'border')

png("K_L_G_functions.png", width=13, height=8, units="in", res=300)
par(mfrow=c(2,3),mar=c(4,5,2,1))
plot(gi.kf.env, main="K function by SGI")
plot(gi.lf.env, main="L function by SGI")
plot(gi.gf.env, main="G function by SGI")
plot(wq.kf.env, main="K function by WQ")
plot(wq.lf.env, main="L function by WQ")
plot(wq.gf.env, main="G function by WQ")
dev.off()