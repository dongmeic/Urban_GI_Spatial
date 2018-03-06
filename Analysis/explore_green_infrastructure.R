# By Dongmei Chen
# This script is designed to visualize green infrastructure data
# libraries
library(rgdal)

# global settings
setwd("/nfs/urbangi-data/spatial_data/output")
infolder <- "/nfs/urbangi-data/spatial_data/"
bound <- readOGR(dsn = paste0(infolder, "BD"), layer = "nyad_dis")
nyadwi <- readOGR(dsn = paste0(infolder, "BD"), layer ="nyadwi_dis")
lonlat <- CRS("+proj=longlat +datum=NAD83")
crs <- CRS("+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333
           +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83
           +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")
# green infrastructure points
gi_pts <- readOGR(dsn = paste0(infolder, "GI"), layer ="GI_2018_02_22")
gi_pts <- spTransform(gi_pts, crs)
proj4string(bound) <- crs

# GI and pilot locations with hydrolograogic unit labels
