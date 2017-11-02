# Background:
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
gi <- readOGR(dsn = paste0(infolder, "GI"), layer ="GI_assets_public", stringsAsFactors = FALSE)
head(gi)
gi.df <- as.data.frame(gi)
attach(gi.df)
##coordinates: set spatial coordinates to create a Spatial object, or retrieve spatial coordinates from a Spatial object
coordinates(gi.df) =~X_Coordina+Y_Coordina
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
density <- nyc.ppp$n/sum(sapply(slot(tract,"polygons"),slot,"area"))

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
sum(qcount.df$Freq)
# mean 
sum(qcount.df$Freq)/16

##Second, count the number of quadrats with a distinct number of points.
##count {plyr}: Count the number of occurences.
qcount.df = count(qcount.df,'Freq')
##Change the column names to be consistent with the textbook.
colnames(qcount.df) = c("x","f")

##Third, create new columns for total number of points and for fx^2.
qcount.df = cbind(qcount.df, TotPoints = as.numeric(qcount.df$x) * as.numeric(qcount.df$f))
qcount.df = cbind(qcount.df, fx2 = (as.numeric(qcount.df$x)^2)*as.numeric(qcount.df$f))

##Fourth, calculate the sum of each column, which you will use as inputs into the 
##formula for VMR.
f.sum = sum(qcount.df$f)
f.sum
TotPoints.sum = sum(qcount.df$TotPoints)
TotPoints.sum
fx2.sum = sum(qcount.df$fx2)
fx2.sum
##Fifth, calculate VAR, MEAN, and VMR.
m <- quads^2
VAR = (fx2.sum-TotPoints.sum^2/m)/(m-1)
VAR
MEAN = TotPoints.sum/m
MEAN
VMR = VAR/MEAN
VMR # 262.1493
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
hist(nearestNeighbor,xlim=c(0,10000),ylim=c(0,200),breaks=4000,main = "Frequency of NND", xlab = "Nearest neighbour distance (NND)", ylab = "Frequency", col = "lightgreen", cex.lab=1.2)
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
for (i in 20:39){
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


