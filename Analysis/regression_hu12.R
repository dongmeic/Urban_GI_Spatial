# By Dongmei Chen
# This script is designed to explore the correlation and regression between GI and WQ

# libraries
library(rgal)

# global settings
setwd("/nfs/urbangi-data/spatial_data/output")
infolder <- "/nfs/urbangi-data/spatial_data/"

bound <- readOGR(dsn = paste0(infolder, "BD"), layer = "nyad_dis")
bound <- spTransform(bound, crs)
nyadwi <- readOGR(dsn = paste0(infolder, "BD"), layer ="nyadwi_dis")
lonlat <- CRS("+proj=longlat +datum=NAD83")
crs <- CRS("+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333
           +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83
           +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")
gi_pts <- readOGR(dsn="./shapefile", layer="GIsites_all", stringsAsFactors=FALSE)
gi_pts <- spTransform(gi_pts, crs)
wbdhu12 <- readOGR(dsn = paste0(infolder, "WBDHU"), layer = "wbdhu_12", stringsAsFactors = FALSE)
wbdhu12 <- spTransform(wbdhu12, crs)
dep_hwq_pts <- readOGR(dsn = "./shapefile", layer = "harbor_water_quality", stringsAsFactors = FALSE)
csoloc <- readOGR(dsn=paste0(infolder, "CSO"), layer ="dec_cso_2016", stringsAsFactors=FALSE) 
csoloc <- spTransform(csoloc, crs)
sewershed <- readOGR(dsn = paste0(infolder, "watershed/Sewershed"), layer = "Sewershed")



