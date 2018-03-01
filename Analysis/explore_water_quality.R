# By Dongmei Chen
# This script is designed to view the cleaned up water quality data and 
# for correlation and regression between water quality and SGI density
# Some data were created from water_quality_data_cleanup.R
# libraries
library(ggplot2)
# global settings
setwd("/nfs/urbangi-data/spatial_data/output")
infolder <- "/nfs/urbangi-data/spatial_data/"

# check the temporal dynamics of water quality
wq <- read.csv("wq_pts_all.csv",stringsAsFactors = FALSE)
wq$ent <- as.numeric(wq$ent)
wq.1 <- wq[!is.na(wq$ent),]
wq.1$month <- as.character(wq.1$month)
head(wq)
ggplot(aes(day, ent), data=wq.1) + geom_jitter(color=month)
ggplot(aes(year, ent), data=wq.1) + geom_bar(stat = "identity", aes(fill=month))
wq.1$date <- as.Date(day, origin = paste0(as.character(year), "-01-01"))
write.csv(wq.1, "water_quality_date.csv", row.names=FALSE)

# check only DEP harbor water quality data
dep_hwq <- read.csv("harbor_WQ_pts.csv",stringsAsFactors = FALSE)
head(dep_hwq)

# CSO outfalls

