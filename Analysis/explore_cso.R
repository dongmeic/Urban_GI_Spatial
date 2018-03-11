# By Dongmei Chen
# This script is designed to visualize CSO data

# global settings
setwd("/nfs/urbangi-data/spatial_data/output")
infolder <- "/nfs/urbangi-data/spatial_data/"

# libraries
library(rgdal)
library(RColorBrewer)
library(classInt)
library(ggplot2)

# projection and roi(region of interest)
crs <- CRS("+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333 
           +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83 
           +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")
lonlat <- CRS("+proj=longlat +datum=NAD83")

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

quick.map <- function(spdf,pspdf,var,color,legend.title,outname) {
  nclr <- 5
  plotclr <- brewer.pal(nclr,color)
  plotvar <- as.numeric(spdf@data[,var])
  class <- classIntervals(plotvar, nclr, style="jenks", dataPrecision=1)
  colcode <- findColours(class, plotclr)
  png(paste0("figure/",outname,".png"), width=9, height=8, units="in", res=300)
  par(xpd=FALSE,mfrow=c(1,1),mar=c(0.5,0.5,2.5,0.5))
  plot(bound, border="white")
  if (class(spdf)[1] == "SpatialPolygonsDataFrame"){
    plot(spdf, col=colcode, add=T)
    plot(wbdhu12, bord="grey", add=T)
    plot(greinfr, pch=20, col=rgb(0,0.8,0), cex=0.2, add=T)
    plot(wq_pts, pch=1, col=rgb(0,0,0.8), cex=0.8, add=T)
    plot(pspdf, pch=20, col=rgb(0.8,0,0), cex=0.4, add=T)
  }else if (class(spdf)[1] == "SpatialPointsDataFrame"){
    plotvar <- as.numeric(spdf@data[,var])
    class <- classIntervals(plotvar, nclr, style="quantile", dataPrecision=1)
    colcode <- findColours(class, plotclr)
    #colcode[1:100] <- paste0(colcode[1:100], "33")
    #attr(colcode,"palette") <- paste0(attr(colcode,"palette"), "33")
    #colcode[colcode=="NA33"] <- "NA"
    plot(wbdhu12, bord="grey", add=T)
    plot(sewershed, add=T)
    plot(spdf, col=colcode, pch=16, cex=3, add=T)
    plot(greinfr, pch=20, col=rgb(0,0.8,0), cex=0.2, add=T)
  }else{
    cat("The spatial dataframe is not supported.")
  }
  title("Combined sewer overflows in NYC",cex.main=1.5)
  legend(910000,240000, legend=names(attr(colcode, "table")),
         fill=attr(colcode, "palette"), cex=1.2, title=legend.title, bty="n")
  add.northarrow()
  add.scale()
  dev.off()
}

bound <- readOGR(dsn = paste0(infolder, "BD"), layer = "nyad_dis")
bound <- spTransform(bound, crs)
csoloc <- readOGR(dsn=paste0(infolder, "CSO"), layer ="dec_cso_2016", stringsAsFactors=FALSE) 
csoloc <- spTransform(csoloc, crs)
greinfr <- readOGR(dsn="./shapefile", layer="GIsites_all", stringsAsFactors=FALSE)
greinfr <- spTransform(greinfr,crs)
wbdhu12 <- readOGR(dsn = paste0(infolder, "WBDHU"), layer = "wbdhu_12", stringsAsFactors = FALSE)
wbdhu12 <- spTransform(wbdhu12, crs)
wq_pts <- readOGR(dsn = "./shapefile", layer = "dep_wq_sampling_sites", stringsAsFactors = FALSE)
pilots <- readOGR(dsn = paste0(infolder, "GI"), layer ="GI_pilots", stringsAsFactors = FALSE)
pilots <- spTransform(pilots, crs)
sewershed <- readOGR(dsn = paste0(infolder, "watershed/Sewershed"), layer = "Sewershed")

png("figure/CSO_outfalls.png", width=8, height=8, units="in", res=300)
plot(bound, bord="white", title="Combined sewer overflow outfalls in NYC")
plot(wbdhu12, add=T)
plot(wq_pts, pch=16, cex=1.2, col=rgb(0,0,0.8,0.8), add=T)
plot(csoloc, pch=16, cex=0.8, col=rgb(0.8,0,0,0.8), add=T)
plot(greinfr, pch=20, cex=0.3, col=rgb(0,0.8,0,0.8), add=T)
plot(pilots, pch=1, cex=1.5, col=rgb(0,1,0), add=T)
plot(bound, bord="grey58", add=T)
northarrow(c(925050,195000),3500)
add.scale()
legend(920000, 260000, bty="n",
       pch=c(1,16,16,20), 
       col=c(rgb(0,1,0),rgb(0,0,0.8,0.8),rgb(0.8,0,0,0.8),rgb(0,0.8,0,0.8)), 
       pt.cex=c(1.5,1.2,0.8,0.3),
       cex = 1.2,
       legend=c("GI pilot sites", "WQ sites", "CSO outfalls", "GI sites"))
legend(915000, 230000, bty="n",lty = 1, col=c("black", "grey58"),
       legend = c("WBD HU12", "NYC"))
dev.off()

# CSO volume and events
csoloc$Sewershed <- over(csoloc, sewershed)$Sewershed=
cso_shed <- aggregate(volume_13 ~ Sewershed, data=csoloc, mean, na.rm=TRUE)
Sewershed <- merge(sewershed, cso_shed, by="Sewershed")
cso_shed <- aggregate(events_13 ~ Sewershed, data=csoloc, mean, na.rm=TRUE)
Sewershed <- merge(Sewershed, cso_shed, by="Sewershed")
cso_shed <- aggregate(volume_14 ~ Sewershed, data=csoloc, mean, na.rm=TRUE)
Sewershed <- merge(Sewershed, cso_shed, by="Sewershed")
cso_shed <- aggregate(events_14 ~ Sewershed, data=csoloc, mean, na.rm=TRUE)
Sewershed <- merge(Sewershed, cso_shed, by="Sewershed")
cso_shed <- aggregate(volume_15 ~ Sewershed, data=csoloc, mean, na.rm=TRUE)
Sewershed <- merge(Sewershed, cso_shed, by="Sewershed")
cso_shed <- aggregate(events_15 ~ Sewershed, data=csoloc, mean, na.rm=TRUE)
Sewershed <- merge(Sewershed, cso_shed, by="Sewershed")
cso_shed <- aggregate(volume_16 ~ Sewershed, data=csoloc, mean, na.rm=TRUE)
Sewershed <- merge(Sewershed, cso_shed, by="Sewershed")
cso_shed <- aggregate(events_16 ~ Sewershed, data=csoloc, mean, na.rm=TRUE)
Sewershed <- merge(Sewershed, cso_shed, by="Sewershed")

# quick maps
# quick.map(spdf,pspdf,var,color,legend.title,outname)
yrs <- 13:16
# points
for (i in yrs){
  quick.map(csoloc,csoloc,paste0("volume_",i),"Blues",paste0("Volume 20",i),paste0("cso_volume_pts",i))
  print(i)
}

for (i in yrs){
  quick.map(csoloc,csoloc,paste0("events_",i),"Blues",paste0("Events 20",i),paste0("cso_events_pts",i))
  print(i)
}

# area
for (i in yrs){
  quick.map(Sewershed,csoloc,paste0("volume_",i),"Blues",paste0("Volume 20",i),paste0("cso_volume",i))
  print(i)
}

for (i in yrs){
  quick.map(Sewershed,csoloc,paste0("events_",i),"Blues",paste0("Events 20",i),paste0("cso_events",i))
  print(i)
}

# reorganize data frame
data <- csoloc@data[,7:14]
data_1 <- data[,1:2]; data_2 <- data[,3:4]; data_3 <- data[,5:6]; data_4 <- data[,7:8]
names(data_1) <- c("volume", "events")
names(data_2) <- c("volume", "events")
names(data_3) <- c("volume", "events")
names(data_4) <- c("volume", "events")
data_5 <- rbind(data_1, data_2, data_3, data_4)
data_6 <- cbind(data_5, c(rep(2013, dim(data_1)[1]), 
                          rep(2014, dim(data_1)[1]), 
                          rep(2015, dim(data_1)[1]),
                          rep(2016, dim(data_1)[1])))
colnames(data_6)[3] <- "year"
data_7 <- na.omit(data_6)
data_7$year <- as.character(data_7$year)

ggplot(data=data_7) + geom_boxplot(aes(year, log(volume))) + 
  labs(x="Year", y="CSO volume (log)", title="Combined sewer overflows in NYC")
ggsave(paste0("figure/cso_volume.png"), width=4, height=3, units="in")

ggplot(data=data_7) + geom_boxplot(aes(year, log(events))) + 
  labs(x="Year", y="CSO events (log)", title="Combined sewer overflows in NYC")
ggsave(paste0("figure/cso_events.png"), width=4, height=3, units="in")
