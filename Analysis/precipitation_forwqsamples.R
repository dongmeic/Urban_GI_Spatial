# By Rachel Scarlett
# Modified by Dongmei Chen
#this script is to calculate precipitation depth 1-2 days before a 
#sample is collected
#with this script, we can isolate wq samples that are collected near storms


# Libraries ---------------------------------------------------------------
library(lubridate)

# Setwd -------------------------------------------------------------------
setwd("/nfs/urbangi-data/spatial_data/output")
infolder <- "/nfs/urbangi-data/spatial_data/"

# Load Data ---------------------------------------------------------------
NY_precip <- read.csv(file="csv/climate_date.csv",stringsAsFactors = FALSE)
NY_precip <- NY_precip[,c("DATE","PRCP")]
NY_precip$DATE <- as.Date(NY_precip$DATE)
head(NY_precip)
#load WQ data
NY_wq <- read.csv(file="csv/harbor_water_quality.csv", header=T)
NY_wq$date <- as.Date(NY_wq$date)
head(NY_wq)

# Find depth of precipitation before sampling event -----------------------
#calculate precipitation 2 days before sample
#start.PE is 2 days before sample, end PE is the date of the sample

precipfunc = function(start.PE,end.PE){
  pseries[which(pseries$DATE >= start.PE & pseries$DATE <= end.PE),]
}

pseries <- NY_precip
head(pseries)

#hold results
NY_wq$pre10<- rep(NA, length(start.test))
NY_wq$pre7 <- rep(NA, length(start.test))
NY_wq$pre2 <- rep(NA, length(start.test))

#WQ dates
end.test <- NY_wq$date
start.test <- end.test - 10 # change the number here to set the days before sampling date

for(i in 1:length(end.test)){
  # change the number here too
  NY_wq$pre10[i] <- sum(precipfunc(start.test[i], end.test[i])$PRCP)
  print(NY_wq$pre10[i])
}

#histograms
hist(NY_wq$pre10)
hist(NY_wq$pre7)
hist(NY_wq$pre2)

head(NY_wq)

#save results

write.csv(NY_wq, file = "csv/harbor_water_quality_pre.csv", row.names=FALSE)

# write to shapefile
df2spdf <- function(col1, col2, colname1, colname2, df){
  xy <- data.frame(df[,c(col1,col2)])
  coordinates(xy) <- c(colname1, colname2)
  proj4string(xy) <- lonlat
  spdf <- SpatialPointsDataFrame(coords = xy, data = df, proj4string = lonlat)
  spdf <- spTransform(spdf, crs)
  return(spdf)
}
harbor_wq <- read.csv('csv/harbor_water_quality_pre.csv', stringsAsFactors = FALSE)
harbor_wq <- harbor_wq[!is.na(harbor_wq$year),]
harbor_wq.td <- gather(harbor_wq, Key, Value, -site, -date, -year, -month, -day, -pre10, -pre7, -pre2)
harbor_wq.td$year <- as.character(harbor_wq.td$year)
coords <- read.csv(paste0(infolder, "WQ/DEP/harbor_sampling_coordinates.csv"), 
                   stringsAsFactors = FALSE)
coords <- coords[,-1]
colnames(coords)[1] <- "site"
harbor_wq.df <- merge(coords, harbor_wq.td, by="site")
harborwq.df <- merge(coords, harbor_wq, by="site")
colnames(harborwq.df)[2:3] <- c("lat", "lon")
write.csv(harborwq.df, "csv/harbor_WQ_pts.csv", row.names=FALSE)
harbor_wq.shp <- df2spdf(3,2,"Long","Lat",harbor_wq.df)
writeOGR(harbor_wq.shp, dsn="./shapefile", layer="harbor_water_quality", 
         overwrite_layer = TRUE,driver = "ESRI Shapefile")



