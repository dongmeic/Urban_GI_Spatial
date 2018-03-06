# By Dongmei Chen
# This script is designed to visualize green infrastructure data
# libraries
library(rgdal)
library(sp)
library(maptools)

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
  GISTools::map.scale(1040000,125000,19685.083,"km",3,2,sfcol='brown')
} #19685 feet 0.5 inches equals to 6 km

# global settings
setwd("/nfs/urbangi-data/spatial_data/output")
infolder <- "/nfs/urbangi-data/spatial_data/"
bound <- readOGR(dsn = paste0(infolder, "BD"), layer = "nyad_dis")
nyadwi <- readOGR(dsn = paste0(infolder, "BD"), layer ="nyadwi_dis")
lonlat <- CRS("+proj=longlat +datum=NAD83")
crs <- CRS("+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333
           +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83
           +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")
bound <- spTransform(bound, crs)
# green infrastructure points
gi_pts <- readOGR(dsn = paste0(infolder, "GI"), layer ="GI_2018_02_22", stringsAsFactors = FALSE)
gi_pts <- spTransform(gi_pts, crs)
pilots <- readOGR(dsn = paste0(infolder, "GI"), layer ="GI_pilots", stringsAsFactors = FALSE)
pilots <- spTransform(pilots, crs)

# sewershed and watershed
wbdhu12 <- readOGR(dsn = paste0(infolder, "WBDHU"), layer = "wbdhu_12", stringsAsFactors = FALSE)
wbdhu12 <- spTransform(wbdhu12, crs)
sewershed <- readOGR(dsn = paste0(infolder, "watershed/Sewershed"), layer = "Sewershed", stringsAsFactors = FALSE)

# water quality sampling points
wq_pts <- readOGR(dsn = "./shapefile", layer = "dep_wq_sampling_sites", stringsAsFactors = FALSE)
# GI and pilot locations with hydrolograogic unit labels
SP <- SpatialPoints(coords = coordinates(wbdhu12))
png("figure/GI_pilot_sites.png", width=8, height=8, units="in", res=300)
plot(bound, bord="white")
plot(wbdhu12, add=T)
plot(gi_pts, pch=16, cex=1.2, col=rgb(0,0.5,0,0.5), add=T)
plot(wq_pts, pch=20, col=rgb(0,0,0.8), add=T)
plot(pilots, pch=1, cex=1.5, col='red', add=T)
plot(bound, bord="grey58", add=T)
pointLabel(coordinates(SP),labels=wbdhu12$NAME, cex = 0.8, col="dark gray")
northarrow(c(925050,195000),3500)
add.scale()
legend(920000, 260000, bty="n",
       pch=c(20,16,1), 
       col=c(rgb(0,0,0.8),rgb(0,0.5,0,0.4),rgb(1,0,0)), 
       pt.cex=c(1,1.2,1.5),
       cex = 1.2,
       legend=c("WQ sites", "GI sites", "GI pilot sites"))
legend(915000, 235000, bty="n",lty = 1, col=c("black", "grey"),
       legend = c("WBD HU12", "NYC"))
dev.off()


