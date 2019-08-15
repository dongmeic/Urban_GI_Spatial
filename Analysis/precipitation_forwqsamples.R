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
NY_precip <- read.csv(file="csv/climatedata_nyc.csv",stringsAsFactors = FALSE)
NY_precip <- NY_precip[NY_precip$STATION == "US1NJBG0003",]
NY_precip <- NY_precip[,c("DATE","PRCP")]
NY_precip$DATE <- as.Date(NY_precip$DATE)
head(NY_precip)
#load WQ data
NY_wq <- read.csv(file="csv/harbor_water_quality.csv", header=T,stringsAsFactors = FALSE)
NY_wq$date <- as.Date(NY_wq$date)
head(NY_wq)

# Use an inverse distance weighting method to adjust precipitation
wq_pts <- readOGR(dsn = "./shapefile", layer = "dep_wq_sampling_sites", stringsAsFactors=FALSE)
wq_pre_df <- wq_pts@data
NY_precip <- NY_precip[!is.na(NY_precip$PRCP) | !is.na(NY_precip$DATE),]

# this will take more than two hours
ptm <- proc.time() 
pre_df <- data.frame(SITE=character(), DATE=as.Date(character()), PRCP=numeric(), stringsAsFactors=FALSE)
for(date in unique(NY_precip$DATE)){
  clm <- subset(NY_precip, DATE==date)
  print(date)
  for(site in wq_pre_df$Station){
    wds <- subset(wq_pre_df, Station==site)
    i <- which(wq_pre_df$Station == site)
    pre <- idw(wq_pre_df$Long[i], wq_pre_df$Lat[i], clm$LONGITUDE, clm$LATITUDE, clm$PRCP)
    ndf <- data.frame(SITE=site, DATE=date, PRCP=pre)
    pre_df <- rbind(pre_df, ndf)
    print(site)
  }
}
proc.time() - ptm
write.csv(pre_df, "csv/precipitation_IDW.csv", row.names=FALSE)

# Find depth of precipitation before sampling event -----------------------
#calculate precipitation 2 days before sample
#start.PE is 2 days before sample, end PE is the date of the sample

# without considering precipitation difference within the city
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
start.test <- end.test - 2
for(i in 1:length(end.test)){
  # change the number here too
  NY_wq$pre2[i] <- sum(precipfunc(start.test[i], end.test[i])$PRCP)
  print(NY_wq$pre2[i])
}

start.test <- end.test - 7
for(i in 1:length(end.test)){
  # change the number here too
  NY_wq$pre7[i] <- sum(precipfunc(start.test[i], end.test[i])$PRCP)
  print(NY_wq$pre7[i])
}

start.test <- end.test - 10
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

# considering precipitation difference among sampling locations
cumprecip <- function(df, date, n){
  start <- date - n
  sum(df$PRCP[which(df$DATE >= start & df$DATE <= date)])
}

pre_df$DATE <- as.Date(pre_df$DATE)

NY_wq$pre10<- rep(NA, dim(NY_wq)[1])
NY_wq$pre7 <- rep(NA, dim(NY_wq)[1])
NY_wq$pre2 <- rep(NA, dim(NY_wq)[1])

# this will take more than two hours
ptm <- proc.time() 
SITES <- unique(NY_wq$site[!is.na(NY_wq$site)])
for(site in SITES){
  NY_wq.s <- subset(NY_wq, site==site)
  clm.s <- subset(pre_df, SITE==site)
  print(site)
  DATES <- unique(NY_wq.s$date[!is.na(NY_wq.s$date)])
  for(date in DATES){
    i <- NY_wq$site == site & NY_wq$date == date
    NY_wq$pre2[i] <- cumprecip(clm.s, date, 2)
    NY_wq$pre7[i] <- cumprecip(clm.s, date, 7)
    NY_wq$pre10[i] <- cumprecip(clm.s, date, 10)
    print(DATES[DATES==date])
  }
}
proc.time() - ptm

#save results

#write.csv(NY_wq, file = "csv/harbor_water_quality_pre.csv", row.names=FALSE)
write.csv(NY_wq, file = "csv/harbor_water_quality_pre_updated.csv", row.names=FALSE)

# write to shapefile
df2spdf <- function(col1, col2, colname1, colname2, df){
  xy <- data.frame(df[,c(col1,col2)])
  coordinates(xy) <- c(colname1, colname2)
  proj4string(xy) <- lonlat
  spdf <- SpatialPointsDataFrame(coords = xy, data = df, proj4string = lonlat)
  spdf <- spTransform(spdf, crs)
  return(spdf)
}
harbor_wq <- read.csv('csv/harbor_water_quality_pre_updated.csv', stringsAsFactors = FALSE)
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
writeOGR(harbor_wq.shp, dsn="./shapefile", layer="harbor_water_quality_updated", 
         overwrite_layer = TRUE,driver = "ESRI Shapefile")



