library(lubridate)
inpath <- "/Users/dongmeichen/Documents/All/UrbanGI/waterquality/dep_harbor_water/"
file <- "HarborWaterQuality.csv"

data <- read.csv(paste0(inpath, file), stringsAsFactors = FALSE)
head(data)
names(data)
class(data$Sample.Date)
data$Sample.Date <- as.Date(data$Sample.Date, format = "%m/%d/%Y")
range(year(data$Sample.Date), na.rm = TRUE)
