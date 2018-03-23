# By Dongmei Chen
# This script is designed to explore the CSO events and volume from the key regulators.

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

# libraries
library(rgdal)

regulators <- readOGR(dsn = paste0(infolder, "CSO/regulators"), layer = "regulators", stringsAsFactors = FALSE)
sewershed <- readOGR(dsn = paste0(infolder, "CSO"), layer = "Sewershed", stringsAsFactors = FALSE)
#listreg <- unique(regulators$ITEM_ID)
keyreg <- c( "PR-R6W", "PR-R35W", "PR-R13E", "OH-R1", "OH-R6", "RH-R2", "RH-R20", "NC-B4", 
             "NC-B1", "BB-L4", "NC-M47", "NR-N16", "NR-N23","NR-N33", "BB-L22", "WI-R23", 
             "WI-R53", "BBH-02", "BBH-06", "TI-R9", "TI-R10A", "HP-R10", "HP-R13", 
             "HP-R5",  "WI-R67", "26W-R1", "26W-R2", "JAM-R3")
key.reg <- regulators[regulators$ITEM_ID %in% keyreg,]
key.reg <- spTransform(key.reg, crs)
plot(sewershed)
plot(key.reg, pch=16, cex=0.8, col="red", add=T)
pointLabel(coordinates(key.reg)[,1], coordinates(key.reg)[,2], key.reg$ITEM_ID, cex=0.8, col="blue")
key.reg$ITEM_ID
# 26W-01, 26W-02, BBH-02, BBH-06, BBL-04, BBL-22, HP-05, HP-10, HP-13, 
# JA-03, NCB-01, NCB-04, NCM-47, NR-16, NR-23, NR-33, OH-01, 0H-06, 
# PR-06W, PR-13E, RH-02, RH-20, TI-09, TI-10A, WIB-53, WIB-67, WIM-23
# 
# [1] "WI-R67"  "PR-R6W"  "PR-R13E" "PR-R35W" "BB-L4"   "BB-L22"  "WI-R23"  "BBH-02"  "BBH-06" 
# [10] "TI-R10A" "TI-R9"   "NR-N33"  "JAM-R3"  "NC-B4"   "NC-B1"   "26W-R1"  "26W-R2"  "RH-R2"  
# [19] "NC-M47"  "RH-R20"  "OH-R1"   "HP-R5"   "OH-R6"   "HP-R13"  "HP-R10"  "WI-R53" 
key.reg$OUTFALL_ID <- c("WIB-67", "PR-06W", "PR-13E", "PR-35W", "BBL-04", "BBL-22", "WIM-23",
                        "BBH-02", "BBH-06", "TI-10A", "TI-09", "NR-16", "NR-23", "NR-33", 
                        "JA-03", "NCB-04", "NCB-01", "26W-01", "26W-02", "RH-02", "NCM-47", 
                        "RH-20", "OH-01", "HP-05", "0H-06", "HP-13", "HP-10", "WIB-53")
writeOGR(key.reg, dsn="./shapefile", layer="key_regulators", overwrite_layer = TRUE,driver = "ESRI Shapefile")
