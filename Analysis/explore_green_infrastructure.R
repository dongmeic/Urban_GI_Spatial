# By Dongmei Chen
# This script is designed to visualize green infrastructure data
# libraries
library(rgdal)
library(sp)
library(maptools)
library(ggplot2)

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
gi_web <- readOGR(dsn = paste0(infolder, "GI"), layer ="DEP_GREEN_INFRASTRUCTURE", stringsAsFactors = FALSE)
gi_web <- spTransform(gi_web, crs)
# sewershed and watershed
wbdhu12 <- readOGR(dsn = paste0(infolder, "WBDHU"), layer = "wbdhu_12", stringsAsFactors = FALSE)
wbdhu12 <- spTransform(wbdhu12, crs)
sewershed <- readOGR(dsn = paste0(infolder, "watershed/Sewershed"), layer = "Sewershed", stringsAsFactors = FALSE)

# water quality sampling points
wq_pts <- readOGR(dsn = "./shapefile", layer = "dep_wq_sampling_sites", stringsAsFactors = FALSE)
# GI and pilot locations with hydrolograogic unit labels
SP <- SpatialPoints(coords = coordinates(wbdhu12))
wbdhu12$NAME;coordinates(SP)
label_pts <- c(6,8,11,20,21,34)
png("figure/GI_pilot_sites.png", width=8, height=8, units="in", res=300)
plot(bound, bord="white")
plot(wbdhu12, add=T)
plot(gi_pts, pch=16, cex=1.2, col=rgb(0,0.5,0,0.5), add=T)
plot(gi_web, pch=16, cex=1.2, col=rgb(0.5,0.8,0,0.5), add=T)
plot(wq_pts, pch=20, col=rgb(0,0,0.8), add=T)
plot(pilots, pch=1, cex=1.5, col='red', add=T)
plot(bound, bord="grey58", add=T)
pointLabel(coordinates(SP)[label_pts,],labels=wbdhu12$NAME[label_pts], cex = 0.8, col="dark gray")
northarrow(c(925050,195000),3500)
add.scale()
legend(920000, 260000, bty="n",
       pch=c(20,16,16,1), 
       col=c(rgb(0,0,0.8),rgb(0,0.5,0,0.4),rgb(0.5,0.8,0,0.5),rgb(1,0,0)), 
       pt.cex=c(1,1.2,1.2,1.5),
       cex = 1.2,
       legend=c("WQ sites", "GI sites 1", "GI sites 2", "GI pilot sites"))
legend(915000, 230000, bty="n",lty = 1, col=c("black", "grey58"),
       legend = c("WBD HU12", "NYC"))
dev.off()

names(gi_pts);head(gi_pts)
names(gi_web);head(gi_web)
table(gi_pts$GI_Type)
table(gi_web$GI_TECHNOL)
gi_pts_1 <- gi_pts[gi_pts$GI_Type != "ROWB",]
gi_pts_2 <- gi_web[gi_web$GI_TECHNOL != "Rain Barrel",]
tGI_1 <- table(gi_pts_1$GI_Type)
tGI_2 <- table(gi_pts_2$GI_TECHNOL)
par(mfrow=c(2,1),xpd=FALSE,mar=c(2,3,3,0))
barplot(tGI_1, col = rainbow(20), main = "GI types", cex.names = 0.6, cex.axis=1.5, cex.main=3)
barplot(tGI_2, col = rainbow(20), cex.names = 0.6, cex.axis=1.5, cex.main=3)
length(names(tGI_1));length(names(tGI_2))

# reorganize green infrastructure data
bluebelt <- c("Bluebelt","Constructed Wetland")
raingarden <- c("Rain Garden","ROW Rain Garden", "ROWRG")
bioswale <- c("ROWB", "ROWEB","Bioswale")
greenroof <- c("Combined Blue/Green Roof", "Green Roof","Rooftop Farm")
permeable <- c("Permeable Pavers", "Pervious Concrete","Porous Asphalt","Porous Concrete",
               "ROW Permeable Pavers", "ROW Porous Concrete","ROW Structural Soil","Gravel Bed",
               "Pervious Pavement")
detention <- c("Blue Roof","Detention System (Connected to Sewer)", "Dual System", "Dual System (See Notes)",
               "Engineered Soil Tree Pit","ROW Infiltration Basin with Concrete Top",
               "ROW Infiltration Basin with Grass Top", "ROW Stormwater Seepage Basin",
               "ROW Subsurface Pipe/Broken Stone","Subsurface Detention System","Subsurface Pipe",
               "Synthetic Turf Field Storage Layer","Tree Pit")
greenstreet <- c("ROWGS", "ROWSGS","SGS", "SGS (old)")
rainbarrel <- c("Rainwater Harvesting","Cistern","Rain Barrel","Rainwater Reuse System")

gi_pts$GItypes <- ifelse(gi_pts$GI_Type %in% bluebelt, "Bluebelts", 
                         ifelse(gi_pts$GI_Type %in% raingarden, "Rain gardens",
                                ifelse(gi_pts$GI_Type %in% bioswale, "Bioswales",
                                      ifelse(gi_pts$GI_Type %in% greenroof, "Green roofs",
                                             ifelse(gi_pts$GI_Type %in% permeable, "Permeable",
                                                    ifelse(gi_pts$GI_Type %in% detention, "Detention",
                                                           ifelse(gi_pts$GI_Type %in% greenstreet, "Green streets", 
                                                                  ifelse(gi_pts$GI_Type %in% rainbarrel, "Rain barrels","Others"))))))))
gi_web$GItypes <- ifelse(gi_web$GI_TECHNOL %in% bluebelt, "Bluebelts", 
                         ifelse(gi_web$GI_TECHNOL %in% raingarden, "Rain gardens",
                                ifelse(gi_web$GI_TECHNOL %in% bioswale, "Bioswales",
                                       ifelse(gi_web$GI_TECHNOL %in% greenroof, "Green roofs",
                                              ifelse(gi_web$GI_TECHNOL %in% permeable, "Permeable",
                                                     ifelse(gi_web$GI_TECHNOL %in% detention, "Detention",
                                                            ifelse(gi_web$GI_TECHNOL %in% greenstreet, "Green streets", 
                                                                   ifelse(gi_web$GI_TECHNOL %in% rainbarrel, "Rain barrels","Others"))))))))

# merge both GI sites data
# add hydrographic unit and sewershed id in both data
wbdhu12$huid <- paste0("hu", seq(1, length(wbdhu12$TNMID)))
gi_pts$hu12 <- over(gi_pts, wbdhu12)$huid
gi_web$hu12 <- over(gi_web, wbdhu12)$huid
gi_pts$sewershed <- over(gi_pts, sewershed)$Sewershed
gi_web$sewershed <- over(gi_web, sewershed)$Sewershed
gi_web$Borough <- ifelse(gi_web$BOROUGH == "1", "Manhattan", 
                           ifelse(gi_web$BOROUGH == "2", "Brooklyn",
                                  ifelse(gi_web$BOROUGH == "3", "Queens",
                                         ifelse(gi_web$BOROUGH == "4", "Bronx",
                                                ifelse(gi_web$BOROUGH == "5", "Staten Island", gi_web$BOROUGH)))))
table(gi_web$Borough)
gi_web$Borough <- ifelse(gi_web$Borough == "brooklyn", "Brooklyn", gi_web$Borough)
table(gi_web$Borough)
table(gi_pts$Borough)
gi_pts$X <- coordinates(gi_pts)[,1]
gi_pts$Y <- coordinates(gi_pts)[,2]
gi_web$X <- coordinates(gi_web)[,1]
gi_web$Y <- coordinates(gi_web)[,2]
# add unique ID
names(gi_web)[which(names(gi_web)=="UNIQUEID")] <- "Asset_ID"
gi_pts_3 <- gi_pts[gi_pts$Status_Gro == "Constructed",]
colnames <- c("Asset_ID", "X", "Y", "Borough", "GItypes", "hu12", "sewershed")
GIsites1 <- gi_pts_3[,colnames]
GIsites2 <- gi_web[,colnames]
GIsites <- rbind(GIsites1@data, GIsites2@data)
head(GIsites)
GIsites <- GIsites[!duplicated(GIsites[,1:7]),]
# remove duplicated points
gi_web_1 <- gi_web@data[!duplicated(gi_web@data[,c(1,79:84)]),]
gi_web$year <- ifelse(is.na(gi_web$INSTALLATI) & !is.na(gi_web$DATE_ADDED), substring(gi_web$DATE_ADDED,0,4), 
                      ifelse(is.na(gi_web$DATE_ADDED), substring(gi_web$LAST_MOD_1,0,4),substring(gi_web$INSTALLATI,0,4)))
table(gi_web$year)
# covert data frame to spatial data frame
xy <- data.frame(GIsites[,c(2,3)])
coordinates(xy) <- c("X", "Y")
proj4string(xy) <- crs
GIsites_all <- SpatialPointsDataFrame(coords = xy, data = GIsites, proj4string = crs)
par(mfrow=c(1,1),xpd=FALSE,mar=c(0.5,0.5,0.5,0.5))
plot(GIsites_all, pch=16, col="green")
plot(bound, add=T)
GIsites_all$Borough <- ifelse(is.na(GIsites_all$Borough) & GIsites_all$X < 1000000, "Staten Island", GIsites_all$Borough)
GIsites_all$Borough <- ifelse(is.na(GIsites_all$Borough), "Queens", GIsites_all$Borough)
writeOGR(GIsites_all, dsn="./shapefile", layer="GIsites_all", 
         overwrite_layer = TRUE,driver = "ESRI Shapefile")
# check GI types by borough
type.colors <- c('#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854','#ffd92f','#e5c494','#b3b3b3')
ggplot(GIsites_all@data[GIsites_all@data$GItypes !="Others",], aes(x=Borough, fill=GItypes)) +
  geom_histogram(position="stack", stat="count")+ scale_fill_manual(values=type.colors)+
  labs(y="GI quantity", fill="Types", 
       title="Green infrastructure types in NYC by borough",
       subtitle="using only constructed sites")
ggsave(paste0("figure/GI_types_borough.png"), width=6, height=5, units="in")

statu.colors <- c('#66c2a5','#fc8d62','#8da0cb','#e78ac3')
ggplot(gi_pts@data[gi_pts@data$GItypes !="Others",], aes(x=Borough, fill=Status_Gro)) +
  geom_histogram(position="stack", stat="count")+ scale_fill_manual(values=statu.colors)+
  labs(y="GI quantity", fill="Status", 
       title="Green infrastructure construction status in NYC by borough",
       subtitle="using only GI sites 1")
ggsave(paste0("figure/GI_status_borough.png"), width=6, height=5, units="in")

ggplot(gi_web@data, aes(x=year, fill=GItypes)) +
  geom_histogram(position="stack", stat="count")+ scale_fill_manual(values=type.colors)+
  labs(x="Year", y="GI quantity", fill="Types", 
       title="Green infrastructure installation in NYC",
       subtitle="using only GI sites 2")
ggsave(paste0("figure/GI_year_borough.png"), width=6, height=5, units="in")

# map all GI types
type.names <- c("Bioswales", "Bluebelts","Detention","Green roofs",
                "Green streets","Permeable","Rain barrels", "Rain gardens")
n <- length(type.names)
for (i in 1:n){
  png(paste0("figure/map_",type.names[i],".png"), width=8, height=8, units="in", res=300)
  plot(bound, bord="white")
  plot(GIsites_all[GIsites_all$GItypes != type.names[i],], pch=16, cex=1.2, col="grey90", add=T)
  plot(GIsites_all[GIsites_all$GItypes == type.names[i],], pch=16, cex=2, col=rgb(0.5,0.8,0,0.5), add=T)
  plot(wq_pts, pch=20, col=rgb(0,0,0.8,0.6), add=T)
  plot(pilots, pch=1, cex=2, col='red', add=T)
  plot(bound, bord="grey58", add=T)
  plot(wbdhu12, add=T)
  northarrow(c(925050,195000),3500)
  add.scale()
  legend(920000, 260000, bty="n",
         pch=c(16,1,16,20), 
         col=c(rgb(0.5,0.8,0,0.5),"red","grey90",rgb(0,0,0.8,0.6)), 
         pt.cex=c(2,2,1.2,1),
         cex = 1.2,
         legend=c(type.names[i], "Pilot sites", "Others",  "WQ sites"))
  legend(915000, 230000, bty="n",lty = 1, col=c("black", "grey58"),
         legend = c("WBD HU12", "NYC"))
  dev.off()
  print(i)
}


