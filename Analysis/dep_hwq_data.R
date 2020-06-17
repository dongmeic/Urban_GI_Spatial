# By Dongmei Chen
# This script is designed to reorganize harbor water quality data
# Created on June 2020

library(lubridate)
inpath <- "/Users/dongmeichen/Documents/All/UrbanGI/waterquality/dep_harbor_water/"
file <- "HarborWaterQuality.csv"

data <- read.csv(paste0(inpath, file), stringsAsFactors = FALSE)
names(data)
head(data)

class(data$Sample.Date)
data$Sample.Date <- as.Date(data$Sample.Date, format = "%m/%d/%Y")
range(year(data$Sample.Date), na.rm = TRUE)
