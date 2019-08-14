# By Dongmei Chen
# This script is designed to review data and organize a datatable for further data analysis

library(rgdal)
library(ggplot2)
library(dplyr)
library(geosphere)
library(rgeos)


# import functions
source("/research-home/dchen/Urban_GI_Spatial/Analysis/functions.R")

# global settings
setwd("/nfs/urbangi-data/spatial_data/output")
infolder <- "/nfs/urbangi-data/spatial_data/"

# projection and roi(region of interest)
crs <- CRS("+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333 
           +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83 
           +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")
lonlat <- CRS("+proj=longlat +datum=NAD83")
bound <- readOGR(dsn = paste0(infolder, "BD"), layer = "nyad_dis")
bound <- spTransform(bound, crs)

# watershed
wbdhu12 <- readOGR(dsn = paste0(infolder, "WBDHU"), layer = "wbdhu_12", stringsAsFactors =FALSE)
wbdhu12 <- spTransform(wbdhu12, crs)

# sewershed
sewershed <- readOGR(dsn = paste0(infolder, "watershed/Sewershed"), layer = "Sewershed")
head(sewershed@data)
plot(sewershed)

# CSO shed
cso.shed <- readOGR(dsn=paste0(infolder, "watershed"), layer = "CSO_drainage_area", stringsAsFactors=FALSE)
cso.shed <- spTransform(cso.shed, crs)
head(cso.shed@data)
plot(cso.shed)

# priority CSO shed
priority.cso.watersheds <- readOGR(dsn = paste0(infolder, "watershed"), 
                                   layer = "priority_cso_watersheds", stringsAsFactors=FALSE)
priority.cso.watersheds <- spTransform(priority.cso.watersheds, crs)
head(priority.cso.watersheds@data)
plot(priority.cso.watersheds)

# key regulators
keyreg <- readOGR(dsn="./shapefile", layer = "key_regulators", stringsAsFactors=FALSE)
head(keyreg@data)

# CSO data
csoloc <- readOGR(dsn="./shapefile", layer="csoloc", stringsAsFactors=FALSE)
head(csoloc@data)
plot(csoloc)

# monthly CSO
monthly_cso <- readOGR(dsn="./shapefile", layer="monthly_cso", stringsAsFactors=FALSE)
head(monthly_cso@data)

# GI data
greinfr <- readOGR(dsn="./shapefile", layer="GIsites_all", stringsAsFactors=FALSE)
greinfr <- spTransform(greinfr,crs)
greinfr@data$LON <- greinfr@coords[,1]
greinfr@data$LAT <- greinfr@coords[,2]
head(greinfr@data)

mitigated_area <- readOGR(dsn="./shapefile", layer ="DEP_GI_withDA_040618", stringsAsFactors=FALSE)
mitigated_area <- spTransform(mitigated_area, lonlat)
mitigated_area@data$LON <- mitigated_area@coords[,1]
mitigated_area@data$LAT <- mitigated_area@coords[,2]
head(mitigated_area@data)

# pilot programs
pilots <- readOGR(dsn = paste0(infolder, "GI"), layer ="GI_pilots", stringsAsFactors=FALSE)
pilots <- spTransform(pilots, crs)
head(pilots@data)

# climate stations
climstations <- readOGR(dsn = "./shapefile",layer = "climate_stations", stringsAsFactors = FALSE)
head(climstations@data)

# climate data
storms <- read.csv("CSV/stormdata_nyc.csv", stringsAsFactors=FALSE)
head(storms)
clim <- read.csv("csv/climatedata_nyc.csv", stringsAsFactors=FALSE)
head(clim)

# harbor water quality
dep_hwq <- read.csv("csv/harbor_water_quality.csv",stringsAsFactors=FALSE)
head(dep_hwq)
wq_pts <- readOGR(dsn = "./shapefile", layer = "dep_wq_sampling_sites", stringsAsFactors=FALSE)
wq_pts <- spTransform(wq_pts, crs)
head(wq_pts@data)

hwq_shp <- readOGR(dsn = "./shapefile", layer = "harbor_water_quality", stringsAsFactors=FALSE)
head(hwq_shp@data)

# spatial join between water quality sampling sites and SGI points
# to get mitigated area and distance to different types of SGI in the same watershed

# 1. update harbor water sampling sites with watershed code
wbdhu12$nid <- paste0("hu", seq(1, length(wbdhu12$TNMID)))
wq_pts$hu12 <- over(wq_pts, wbdhu12)$nid

wq_df <- wq_pts@data
wq_data <- hwq_shp@data

ptm <- proc.time() 
wq_df$MitigatedArea <- rep(NA, dim(wq_df)[1])
for(site in wq_df$Station){
  wds <- subset(wq_df, Station==site)
  for(hu in wds$hu12){
    ma_pts <- subset(mitigated_area@data, hu12==hu)
    ma_pts <- ma_pts[!is.na(ma_pts$mtgtn_2),]
    if(nrow(ma_pts)==0){
      wq_df$MitigatedArea[i] <- NA
    }else{
      i <- which(wq_df$Station==site & wq_df$hu12==hu)
      wq_df$MitigatedArea[i] <- 
      idw(wq_df$Long[i], wq_df$Lat[i], ma_pts$LON, ma_pts$LAT, ma_pts$mtgtn_2)
    }
  }
}
proc.time() - ptm

colnames(wq_df)[which(colnames(wq_df)=='Station')] <- 'site'
wq_df <- wq_df[,-1]
dep_hwq <- merge(dep_hwq, wq_df, by='site')

par(mfrow=c(2, 2), mar=c(4,4,2,1))
for(var in c('DO_top', 'FC_top', 'Ent_top', 'Tra')){
  plot(dep_hwq$MitigatedArea, dep_hwq[,var], main=var, ylab=var, xlab='Mitigated area', pch=16, cex=0.8)
}

# key options: "Ent_top", "DO_top", "FC_top", "Tra"
vars <- c("Ent_top", "DO_top", "FC_top", "Tra")
ylabnms <-c('Enterococci', 'Dissolved Oxygen', 'Fecal Coliform', 'Transparency')
par(mfrow=c(2, 2), mar=c(4,4,2,1))
for(var in vars){
  df.s <- wq_data %>% filter(pre2 > 0 & Key == var & !is.na(Value))
  df.val <- group_by(df.s, site)  %>% summarise(group=mean(Value))
  df.val <- merge(df.val, wq_df[,c('site', 'MitigatedArea')], by='site')
  df.val <- df.val[df.val$MitigatedArea < 25000,]
  df.val <- df.val[complete.cases(df.val), ]
  plot(df.val$MitigatedArea, df.val$group, xlab='SGI mitigated area', 
       ylab=ylabnms[which(vars==var)], pch=19, cex=1.2)
  lines(lowess(df.val$group~df.val$MitigatedArea), col=2, cex=1.5)
}

par(mfrow=c(1, 1), mar=c(4,4,2,1))
par(mfrow=c(1, 1), mar=c(0,0,0,0))

# check watershed boundaries
plot(wq_pts, cex=1.5, pch=19, col='blue')
plot(wbdhu12, add=T, bord='lightblue')
plot(priority.cso.watersheds, add=T, bord='red')
plot(greinfr[greinfr$GItypes != 'Rain barrels',], pch=1, cex=0.8, col='darkgreen', add=T)
text(centroid(wbdhu12)[,1], centroid(wbdhu12)[,2], wbdhu12$NAME, pos = 1, cex=0.8)

# try intersection with priority watershed
intersect1 <- gIntersects(wbdhu12, priority.cso.watersheds, byid = TRUE)
wbdhu12@data$PRIORITY <- rep(NA, 34)
for(i in 1:length(wbdhu12@plotOrder)){
  if(any(intersect1[,i])){
    wbdhu12@data$PRIORITY[i] <- 1
  }else{
    wbdhu12@data$PRIORITY[i] <- 0
  }
}

plot(wbdhu12[wbdhu12$PRIORITY==1,])
plot(priority.cso.watersheds, add=T, bord='red')
