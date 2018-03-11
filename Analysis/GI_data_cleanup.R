# By Dongmei Chen
# This script is designed to identify the point pattern of green infrastructure
# Results: Point pattern of GI is much more clustered than random
# libraries
library(rgdal)
library(spatstat)
library(maptools)
library(sp)
library(raster)
library(plotrix)
library(plyr)
library(GISTools)
library(maps)

# global settings
setwd("/nfs/urbangi-data/spatial_data/output")
infolder <- "/nfs/urbangi-data/spatial_data/"

gi.1 <- readOGR(dsn = paste0(infolder, "GI"), layer ="GI_assets_public", stringsAsFactors = FALSE)
head(gi.1)
gi.2 <- readOGR(dsn = paste0(infolder, "GI"), layer ="geo_export_6091f18f-fa96-40b8-ba60-320ff66d9d33", stringsAsFactors = FALSE)
head(gi.2) # gi.1 and gi.2 are the same dataset

# point pattern analysis
######################### A. Data Preparation #####################
# projection and roi
crs <- CRS("+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333 
           +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83 
           +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")

# data
gi <- readOGR(dsn="./shapefile", layer="GIsites_all", stringsAsFactors=FALSE)
gi <- spTransform(gi, crs)
head(gi)
gi.df <- as.data.frame(gi)
write.csv(gi.df, "csv/gi_pts.csv", row.names=FALSE)
nyadwi <- readOGR(dsn = paste0(infolder, "BD"), layer ="nyadwi_dis", stringsAsFactors = FALSE)
inside <- !is.na(over(gi, as(nyadwi, "SpatialPolygons")))
gi_pts_in <- gi[inside, ]
writeOGR(gi_pts_in, dsn="./shapefile", layer="gi_pts_in", overwrite_layer = TRUE,driver = "ESRI Shapefile")
attach(gi.df)
##coordinates: set spatial coordinates to create a Spatial object, or retrieve spatial coordinates from a Spatial object
coordinates(gi.df) =~X+Y
##zerodist: find point pairs with equal spatial coordinates
zero=zerodist(gi.df)
length(unique(zero[,1]))
##remove.duplicates(obj, zero = 0.0, remove.second = TRUE, memcmp = TRUE) in zerodist {sp}
gi.df=remove.duplicates(gi.df)
zero=zerodist(gi.df)
length(unique(zero[,1]))

######################### B. Perform Quadrat Analysis ##################

#Quadrat analysis requires several steps. The most important is defining the number of quadrats 

##Create a window(boundary) object to define the study site.
tract <- readOGR(dsn = paste0(infolder, "BD"), layer ="cb_2015_36_tract_500k_clip_proj", stringsAsFactors = FALSE)
##as.owin: Convert Data To Class owin
window = as.owin(tract)

##Prepare gi data for point pattern analysis. This requires defining the dataset as a "ppp" object.
##ppp:Create a Point Pattern
##PPP object: "window"     "n"          "x"          "y"          "markformat"
nyc.ppp = ppp(x=gi.df$X_Coordina,y=gi.df$Y_Coordina,window=window)
# Warning message:
#   1 point was rejected as lying outside the specified window 
exterior_points <- attr(nyc.ppp, "rejects")
names(nyc.ppp)
nyc.ppp <- as.ppp(nyc.ppp)
##slot: The Slots in an Object from a Formal Class
##sapply: Apply a Function over a List or Vector
# density: points/square meters
density <- nyc.ppp$n/sum(sapply(slot(tract,"polygons"),slot,"area")) # 4.827238e-07

##Define number of quadrats that you want to use in your analysis.
##Here, quads is the number that you square to get your total number of quadrats.
##For example, if quads = 4, then the total number of quadrats = 16.

quads = 20

##Use the function "quadratcount" to calculate the number of points per quadrat.
##quadratcount:Quadrat counting for a point pattern
qcount = quadratcount(nyc.ppp, nx = quads, ny = quads)

##Create a map showing the quadrats overlayed on the window file with the 
##number of points displayed per quadrat.
jpeg("QuadratCounting_20.jpeg",2500,2000,res=300)
plot(nyc.ppp,pch="+",cex=0.5,main="GI in NYC")
plot(qcount,add=T,col="red")
dev.off()

##Now, prepare the data to be able to calculate the variables for a quadrat analysis.
##First, define the quadrat count dataset as a data frame.
qcount.df = as.data.frame((qcount))
qcount.df
# total points
sum(qcount.df$Freq) # 4052
# mean 
sum(qcount.df$Freq)/16 #253.25

##Second, count the number of quadrats with a distinct number of points.
##count {plyr}: Count the number of occurences.
# qcount.df = count(qcount.df,'Freq') # count function doesn't work
qcount.df <- as.data.frame(table(qcount.df$Freq))
colnames(qcount.df) = c("x","f")

##Third, create new columns for total number of points and for fx^2.
qcount.df = cbind(qcount.df, TotPoints = as.numeric(qcount.df$x) * as.numeric(qcount.df$f))
qcount.df = cbind(qcount.df, fx2 = (as.numeric(qcount.df$x)^2)*as.numeric(qcount.df$f))

##Fourth, calculate the sum of each column, which you will use as inputs into the 
##formula for VMR.
f.sum = sum(qcount.df$f)
f.sum # 215
TotPoints.sum = sum(qcount.df$TotPoints)
TotPoints.sum # 1066
fx2.sum = sum(qcount.df$fx2)
fx2.sum # 20788
##Fifth, calculate VAR, MEAN, and VMR.
m <- quads^2
VAR = (fx2.sum-TotPoints.sum^2/m)/(m-1)
VAR
MEAN = TotPoints.sum/m
MEAN # 2.665
VMR = VAR/MEAN
VMR # 16.87813
##Finally, perform the test statistic to test for the existence of a random spatial pattern.

# chi-square 
chi = VMR*(m-1)
chi
1-pchisq(chi,m-1)

######################### C. Perform Nearest Neighbor Analysis ##################
#This requires much less coding thanks to the nndist function in the spatstat package. 
#This function calculates the distance between each point and its nearest neighbor.

##Calculate the nearest neigbor distance using the nndist function.
##nndist {spatstat}: Nearest neighbour distances
nearestNeighbor = nndist(nyc.ppp)
png("hist.png", width=5, height=6, units="in", res=300)
hist(nearestNeighbor,xlim=c(0,10000),ylim=c(0,200),breaks=400,main = "Frequency of NND", xlab = "Nearest neighbour distance (NND)", ylab = "Frequency", col = "lightgreen", cex.lab=1.2)
box()
dev.off()

##Convert the nearestNeighbor object into a dataframe.
nearestNeighbor=as.data.frame(as.numeric(nearestNeighbor))
head(nearestNeighbor)
##Change the column name to "Distance"
colnames(nearestNeighbor) = "Distance"

##Calculate the nearest neighbor statistic to test for a random spatial distribution. 
# density = total points/area
# basic mean distance
NND_bar <- mean(nearestNeighbor$Distance)
# perfectly random, distance
density <- intensity.ppp(nyc.ppp)
density
NND_R <- 1/(2*sqrt(density)) 
NND_R
NND_D <- 1.07453/sqrt(density)
NND_D
R <- NND_bar/NND_R
R # 0.2588226
sigma_NND <- 0.26136/sqrt(TotPoints.sum*density)
sigma_NND
Z <- (NND_bar-NND_R)/sigma_NND
Z # -90.25843

## loop
vmr <- c(rep(0,20))
chi <- c(rep(0,20))
for (i in 1:21){
  quads <- i
  qcount <- quadratcount(nyc.ppp, nx = quads, ny = quads)
  qcount.df <- as.data.frame((qcount))
  qcount.df = count(qcount.df,'Freq')
  colnames(qcount.df) = c("x","f")
  qcount.df = cbind(qcount.df, TotPoints = as.numeric(qcount.df$x) * as.numeric(qcount.df$f))
  qcount.df = cbind(qcount.df, fx2 = (as.numeric(qcount.df$x)^2)*as.numeric(qcount.df$f))
  f.sum = sum(qcount.df$f)
  TotPoints.sum = sum(qcount.df$TotPoints)
  fx2.sum = sum(qcount.df$fx2)
  m <- quads^2
  VAR = (fx2.sum-TotPoints.sum^2/m)/(m-1)
  MEAN = TotPoints.sum/m
  VMR = VAR/MEAN
  vmr[i] <- VMR
  chi[i] <- VMR*(m-1)
  print(i)
}

##################################### Mapping Point Patterns ################################################
# reference: http://rspatial.org/analysis/rst/8-pointpat.html
city <- readOGR(dsn = paste0(infolder, "BD"), layer ="nyad_dis", stringsAsFactors = FALSE)
plot(city, col='light blue')
plot(gi, col='red', cex=.5, pch='+', add=T)
xy <- coordinates(gi)
dim(xy)
xy <- unique(xy)
dim(xy)
head(xy)
# mean center
mc <- apply(xy, 2, mean)
# standard distance
sd <- sqrt(sum((xy[,1] - mc[1])^2 + (xy[,2] - mc[2])^2) / nrow(xy))
points(cbind(mc[1], mc[2]), pch='*', col='green', cex=5)
# make a circle
bearing <- 1:360 * pi/180
cx <- mc[1] + sd * cos(bearing)
cy <- mc[2] + sd * sin(bearing)
circle <- cbind(cx, cy)
lines(circle, col='green', lwd=2)
cityArea <- area(city)
dens <- nrow(xy) / cityArea # 4.829621e-07

r <- raster(city)
res(r) <- 1000

r <- rasterize(city, r)
plot(r)
quads <- as(r, 'SpatialPolygons')
plot(quads, add=TRUE)
plot(gi, col='red', cex=.5, pch='+', add=T)

ngi <- rasterize(coordinates(gi), r, fun='count', background=0)
plot(ngi)
plot(city, add=TRUE)

ncgi <- mask(ngi, r)
plot(ncgi)
plot(city, add=TRUE)

f <- freq(ncgi, useNA='no')
head(f)
plot(f, pch=20)

# number of quadrats
quadrats <- sum(f[,2])
# number of cases
cases <- sum(f[,1] * f[,2])
mu <- cases / quadrats
mu

ff <- data.frame(f)
colnames(ff) <- c('K', 'X')
ff$Kmu <- ff$K - mu
ff$Kmu2 <- ff$Kmu^2
ff$XKmu2 <- ff$Kmu2 * ff$X
head(ff)

s2 <- sum(ff$XKmu2) / (sum(ff$X)-1)
s2

VMR <- s2 / mu
VMR

d <- dist(xy)
class(d)

dm <- as.matrix(d)
dm[1:5, 1:5]

diag(dm) <- NA
dm[1:5, 1:5]

dmin <- apply(dm, 1, min, na.rm=TRUE)
head(dmin)

mdmin <- mean(dmin)

wdmin <- apply(dm, 1, which.min)

plot(city)
plot(gi, col='red', cex=.1, pch=16, add=T)
ord <- rev(order(dmin))

far25 <- ord[1:25]
neighbors <- wdmin[far25]

points(xy[far25, ], col='blue', pch=20)
points(xy[neighbors, ], col='red')

# drawing the lines, easiest via a loop
for (i in far25) {
  lines(rbind(xy[i, ], xy[wdmin[i], ]), col='red')
}

max(dmin)
## [1] 21860.85
# get the unique distances (for the x-axis)
distance <- sort(unique(round(dmin)))
# compute how many cases there with distances smaller that each x
Gd <- sapply(distance, function(x) sum(dmin < x))
# normalize to get values between 0 and 1
Gd <- Gd / length(dmin)
plot(distance, Gd)

# using xlim to exclude the extremes
plot(distance, Gd, xlim=c(0,5000))

stepplot <- function(x, y, type='l', add=FALSE, ...) {
  x <- as.vector(t(cbind(x, c(x[-1], x[length(x)]))))
  y <- as.vector(t(cbind(y, y)))
  if (add) {
    lines(x,y, ...)
  } else {
    plot(x,y, type=type, ...)
  }
}

stepplot(distance, Gd, type='l', lwd=2, xlim=c(0,5000))

# get the centers of the 'quadrats' (raster cells)
p <- rasterToPoints(r)
# compute distance from all gi sites to these cell centers
d2 <- pointDistance(p[,1:2], xy, longlat=FALSE)

# the remainder is similar to the G function
Fdistance <- sort(unique(round(d2)))
mind <- apply(d2, 1, min)
Fd <- sapply(Fdistance, function(x) sum(mind < x)) # this takes a while
Fd <- Fd / length(mind)
plot(Fdistance, Fd, type='l', lwd=2)
#Error: Unable to establish connection with R session

ef <- function(d, lambda) {
  E <- 1 - exp(-1 * lambda * pi * d^2)
}
expected <- ef(0:2000, dens)

plot(distance, Gd, type='l', lwd=2, col='red', las=1,
     ylab='F(d) or G(d)', xlab='Distance', yaxs="i", xaxs="i")
lines(Fdistance, Fd, lwd=2, col='blue')
lines(0:2000, expected, lwd=2)
legend(15000, .3,
       c(expression(italic("G")["d"]), expression(italic("F")["d"]), 'expected'),
       lty=1, col=c('red', 'blue', 'black'), lwd=2, bty="n")

distance <- seq(1, 30000, 100)
Kd <- sapply(distance, function(x) sum(d < x)) # takes a while
Kd <- Kd / (length(Kd) * dens)
plot(distance, Kd, type='l', lwd=2)

library(maptools)
cityOwin <- as.owin(city)
class(cityOwin)
cityOwin

pts <- coordinates(gi)
head(pts)

p <- ppp(pts[,1], pts[,2], window=cityOwin)
class(p)
p
plot(p)
ds <- density(p)
class(ds)
plot(ds, main='gi density')

nrow(pts)
r <- raster(ds)
s <- sum(values(r), na.rm=TRUE)
s * prod(res(r))

str(ds)
sum(ds$v, na.rm=TRUE) * ds$xstep * ds$ystep
p$n

win <- aggregate(tract)
plot(tract)
points(p, col='red', pch=20, cex=.25)
plot(win, add=TRUE, border='blue', lwd=3)


