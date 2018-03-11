# Objectives: area pattern analysis of water quality using Enterococci as an indicator
# Input: spatial data frame of WQ
# Output: 1. Lagged mean plot (need to replace NAs with zeros); 
#         2. Moran's I;
#         3. LISAs (local indicators of spatial association) with maps;

# libraries
library(spdep)
library(maptools)
library(rgdal)
library(raster)
library(rgeos)
library(spatialEco)
library(sp)
library(classInt)

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

mapping <- function(spdf, color, var, nclr, legend.title, main.title){
  plotvar <- as.numeric(spdf@data[,var])
  plotclr <- brewer.pal(nclr,color)
  class <- classIntervals(plotvar, nclr, style="jenks", dataPrecision=1)
  colcode <- findColours(class, plotclr)
  plot(nyccwi, col=colcode)
  title(main.title,cex.main=1.5)
  legend(925000,238000, legend=names(attr(colcode, "table")),
         fill=attr(colcode, "palette"), cex=1.2, title=legend.title, bty="n")
}

# global settings
setwd("/nfs/urbangi-data/spatial_data/output")
infolder <- "/nfs/urbangi-data/spatial_data/"
crs <- CRS("+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333 
           +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83 
           +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")
lonlat <- CRS("+proj=longlat +datum=NAD83")

# input data
#hwq_pts <- readOGR(dsn="./shapefile", layer="hwq_pts_in", stringsAsFactors = FALSE)
wq.df <- read.csv("csv/wq_pts_all.csv", stringsAsFactors = FALSE)
# remove the less symbol
gsub("<", "", wq.df)
wq.df$ent <- as.numeric(wq.df$ent)
coordinates(wq.df) <- ~lon+lat
proj4string(wq.df) <- lonlat
wq.spdf <- spTransform(wq.df, crs)
wq.spdf.u <- remove.duplicates(wq.spdf)

sewershed
sewershed <- readOGR(dsn = paste0(infolder, "CSO"), layer ="Sewershed", stringsAsFactors = FALSE)
#reference: https://gis.stackexchange.com/questions/163445/r-solution-for-topologyexception-input-geom-1-is-invalid-self-intersection-er
#sewershed <- gSimplify(sewershed, tol = 0.00001)
sewershed <- gBuffer(sewershed, byid=TRUE, width=0)
gIsValid(sewershed, reason = T)
combined <- readOGR(dsn = paste0(infolder, "CSO"), layer ="combinedsewer_drainage_area_proj", stringsAsFactors = FALSE)
#combined <- gSimplify(combined, tol = 0.00001)
combined <- gBuffer(combined, byid=TRUE, width=0)
gIsValid(combined, reason = T)
nyccwi <- readOGR(dsn = paste0(infolder, "BD"), layer ="nyccwi", stringsAsFactors = FALSE)
gIsValid(nyccwi, reason = T)

# perform point in polygon 
# reference: https://gis.stackexchange.com/questions/137621/join-spatial-point-data-to-polygons-in-r
head(nyccwi@data)  # polygons
head(wq.spdf@data) # points
plot(nyccwi)
points(wq.spdf, pch=20, col='blue')
## point in polygon, something is wrong here
# pts.poly <- point.in.poly(wq.spdf, nyccwi)
# pts.poly@data$pids <- 1:nrow(pts.poly) 
# head(pts.poly@data)
## use over function instead
pts.poly <- over(wq.spdf, nyccwi[,"CounDist"])
gi.spdf <- readOGR(dsn = "./shapefile", layer ="gi_pts_in", stringsAsFactors = FALSE)
gi.pts.poly <- over(gi.spdf, nyccwi[,"CounDist"])
wq.spdf$CounDist <- pts.poly$CounDist
gi.spdf$CounDist <- gi.pts.poly$CounDist
wq.spdf$pids <- 1:nrow(wq.spdf)
gi.spdf$pids <- 1:nrow(gi.spdf)
# aggregate data by polygon
cpts <- aggregate(pids ~ CounDist, data = wq.spdf@data, FUN = function(x){y <- length(x); return(y)})
colnames(cpts)[2] <- "cpts"
gicpts <- aggregate(pids ~ CounDist, data = gi.spdf@data, FUN = function(x){y <- length(x); return(y)})
colnames(gicpts)[2] <- "gicpts"
ment <- aggregate(ent ~ CounDist, data = wq.spdf@data, FUN = mean)
nyccwi@data <- merge(nyccwi@data, cpts, by="CounDist", all=T)
nyccwi@data <- merge(nyccwi@data, ment, by="CounDist", all=T)
nyccwi@data <- merge(nyccwi@data, gicpts, by="CounDist", all=T)
#write.csv(nyccwi@data, "wq_data_cc.csv", row.names=FALSE)
head(nyccwi@data)
nyccwi@data$gidens <- nyccwi@data$gicpts/sum(nyccwi@data$gicpts)*1000

nyccwi.nb <- poly2nb(nyccwi)
nyccwi.nb

nyccwi.nb2 <- poly2nb(nyccwi, queen=FALSE)
nyccwi.nb2

plot(nyccwi, border='lightgrey')
plot(nyccwi.nb, coordinates(nyccwi), add=TRUE, col='blue', lwd=2)
plot(nyccwi.nb2, coordinates(nyccwi), add=TRUE, col='yellow')

nyccwi.lw <- nb2listw(nyccwi.nb2)

gidens.lagged.means <- lag.listw(nyccwi.lw, nyccwi$gidens, NAOK=TRUE)
# replace NAs with zeros?
#nyccwi$ent[is.na(nyccwi$ent)] <- 0
#nyccwi$ent[nyccwi$ent==0] <- NA
shades <- auto.shading(nyccwi$ent, n=6, cols = brewer.pal(5, 'Blues'))
choropleth(nyccwi, nyccwi$ent, shades)

shades <- auto.shading(nyccwi$gidens, n=6, cols = brewer.pal(5, 'Blues'))
choropleth(nyccwi, nyccwi$gidens, shades)

plot(nyccwi$ent, ent.lagged.means, asp=1, xlim=range(na.omit(nyccwi$ent)), ylim=range(na.omit(nyccwi$ent)))
abline(a=0,b=1)
abline(v=mean(na.omit(nyccwi$ent)),lty=2)
abline(h=mean(na.omit(ent.lagged.means)),lty=2)

plot(nyccwi$gidens, gidens.lagged.means, asp=1, xlim=range(na.omit(nyccwi$gidens)), ylim=range(na.omit(nyccwi$gidens)))
abline(a=0,b=1)
abline(v=mean(na.omit(nyccwi$gidens)),lty=2)
abline(h=mean(na.omit(gidens.lagged.means)),lty=2)

moran.test(nyccwi$ent, nyccwi.lw, na.action=na.exclude, zero.policy = TRUE)
# Moran I test under randomisation
# 
# data:  nyccwi$ent  
# weights: nyccwi.lw 
# omitted: 2, 3, 4, 5, 11, 12, 13, 15, 18, 20, 21, 22, 24, 34, 35, 39, 41, 44 
# 
# Moran I statistic standard deviate = -0.30781, p-value = 0.6209
# alternative hypothesis: greater
# sample estimates:
#   Moran I statistic       Expectation          Variance 
# -0.06538677       -0.03125000        0.01229916 

moran.test(nyccwi$gidens, nyccwi.lw, na.action=na.exclude, zero.policy = TRUE)

# LISA
nyccwi.lI <- localmoran(nyccwi$ent, nyccwi.lw, na.action=na.exclude, zero.policy = TRUE)
nyccwi.lI <- localmoran(nyccwi$gidens, nyccwi.lw, na.action=na.exclude, zero.policy = TRUE)

# mapping water quality
png(paste("figure/water_quality.png", sep=""), width=9, height=8, units="in", res=300)
par(xpd=FALSE,mfrow=c(1,1),mar=c(0.5,0.5,2.5,0.5))
mapping(nyccwi, 'Blues', "ent", 8, "Enterococci", "Water quality in New York City")
add.northarrow()
add.scale()
dev.off()

# mapping GI density
png(paste("figure/GI_density.png", sep=""), width=9, height=8, units="in", res=300)
par(xpd=FALSE,mfrow=c(1,1),mar=c(0.5,0.5,2.5,0.5))
mapping(nyccwi, 'Blues', "gidens", 8, expression("GI density ("*10^-3*")"), "Green infrastructure density in New York City")
add.northarrow()
add.scale()
dev.off()

# mapping LISA
nyccwi_lI <- nyccwi
nyccwi_lI$lisa <- nyccwi.lI[,1]
nyccwi_lI$pvalue <- nyccwi.lI[,5]
png(paste("figure/local_moran_I_pvalue_gi.png", sep=""), width=10, height=6, units="in", res=300)
par(xpd=FALSE,mfrow=c(1,2),mar=c(0.5,0.5,2.5,0.5))
mapping(nyccwi_lI, 'PRGn', "lisa", 5, "Local Moran's I", "Local Moran's I")
mapping(nyccwi_lI, 'PuRd', "pvalue", 5, "Local p-value", "Local p-value")
dev.off()