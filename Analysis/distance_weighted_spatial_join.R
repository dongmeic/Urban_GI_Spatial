# By Dongmei Chen
# This script is designed to review data and organize a datatable for further data analysis

library(rgdal)

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
head(greinfr@data)

mitigated_area <- readOGR(dsn="./shapefile", layer ="DEP_GI_withDA_040618", stringsAsFactors=FALSE)
mitigated_area <- spTransform(mitigated_area, crs)
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
head(wq_pts@data)

hwq_shp <- readOGR(dsn = "./shapefile", layer = "harbor_water_quality", stringsAsFactors=FALSE)
head(hwq_shp@data)
















