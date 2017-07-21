# This script is created to visualize the climatic conditions (e.g., temperature, precipitation and storm events) 
# and water quality conditions in the New York City
# Data sources: storm events (https://www.ncdc.noaa.gov/stormevents/); 
# temperature and precipitation (https://www.ncdc.noaa.gov/data-access/land-based-station-data);
# water quality (http://www.nyc.gov/html/dep/html/harborwater/harbor_water_sampling_results.shtml)

# created by Dongmei Chen
# date: 07-20-17

# libraries
library(sp)
library(rgdal)
library(maps)
library(maptools)
library(animation)

bound <- readOGR(dsn = paste0(infolder, "BD"), layer = "nyad_dis")
lonlat <- CRS("+proj=longlat +datum=NAD83")
crs <- CRS("+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333
           +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83
           +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")
proj4string(bound) <- crs
# functions
# convert dataframe to spatial dataframe
df2spdf <- function(col1, col2, colname1, colname2, df){
  xy <- data.frame(df[,c(col1,col2)])
  coordinates(xy) <- c(colname1, colname2)
  proj4string(xy) <- lonlat
  xy.n <- spTransform(xy, crs)
  spdf <- SpatialPointsDataFrame(coords = xy.n, data = df, proj4string = crs)
  return(spdf)
}
# get year, month, yday, hour information from date
get.int <- function(x,y){
  dt <- as.POSIXlt(x)
  unlist(unclass(dt))
  if (y=="y"){
    return(dt$year+1900)
  }else if(y=="m"){
    return(dt$mon+1)
  }else if(y=="d"){
    return(dt$yday+1)
  }else if(y=="h"){
    if (dt$min >= 30){
      return(dt$hour+1)
    }else{
      return(dt$hour)
    }
  }else{
    cat("oops, you need to select one of \"y\", \"m\", \"d\", and \"h\" with quotes!")
  }
}

setwd("/nfs/urbangi-data/spatial_data/output")
infolder <- "/nfs/urbangi-data/spatial_data/"

# # get storm events data in the NYC
# ndf <- data.frame(matrix(ncol = 58, nrow = 0))
# storm.events <- read.csv(paste0(infolder,"CLM/Stormdata_1996.csv"))
# colnames(ndf) <- names(storm.events)
# pct <- proc.time() # this may take a while
# for (year in 1996:2013){
#   if (year > 2009){
#     storm.events <- read.csv(paste0(infolder,"CLM/stormdata_",year,".csv"))
#   }else{
#     storm.events <- read.csv(paste0(infolder,"CLM/Stormdata_",year,".csv"))
#   }
#   ny.storm <- storm.events[storm.events$STATE %in% c("NEW YORK"),]
#   ny.storm.c <- ny.storm[!is.na(ny.storm$BEGIN_LAT),]
#   if (year < 2000){
#     ny.storm.c$BEGIN_LON <- -ny.storm.c$BEGIN_LON
#   }
#   xy <- ny.storm.c[,c("BEGIN_LON", "BEGIN_LAT")]
#   spdf <- SpatialPointsDataFrame(coords = xy, data = ny.storm.c, 
#                                  proj4string = CRS(proj4string(bound)))
#   inside.nyc <- !is.na(sp::over(spdf, as(bound, "SpatialPolygons")))
#   points <- spdf[inside.nyc,]
#   df <- as.data.frame(points)
#   ndf <- rbind(ndf,df)
#   print(year)
# }
# proc.time() - pct
# # write output
# write.csv(ndf,"stormdata_nyc.csv")

# read storm events data in the NYC
storms <- read.csv("stormdata_nyc.csv", stringsAsFactors = FALSE)
str(storms)
storms$MONTH <- as.numeric(t(sapply(storms$BEGIN_YEARMONTH, function(x) substring(x, first=c(1,5), last=c(4,6))))[,2])
# seasonal variation in storm events
ss.stms <- aggregate(EVENT_TYPE~MONTH, data = storms, function(x) length(x))
plot(ss.stms$MONTH, ss.stms$EVENT_TYPE, xlab="Month", ylab="No. storms", type="l", lwd=1.5)
points(ss.stms$MONTH, ss.stms$EVENT_TYPE, col="red", pch=16, cex=1.5)
title("Seasonal variation in storm events over years (1996 - 2013, NYC)")
table(storms$EVENT_TYPE) # check the storm event types

stms.spdf <- df2spdf(47,46,"BEGIN_LON","BEGIN_LAT",storms)
plot(stms.spdf, pch=19, col="blue", cex=0.8)
plot(bound, bord="dark gray", add=T)
title("Spatial distribution of storm events over years (1996 - 2013, NYC)")

# # check the locations and types of the storm events over years
# for (year in 1996:2013){
#   stms <- stms.spdf[stms.spdf$YEAR==year,]
#   #stms <- remove.duplicates(stms)
#   png(paste0("storms_",year,".png"), width=9, height=8, units="in", res=300)
#   par(xpd=FALSE,mfrow=c(1,1),mar=c(0.5,0.5,2.5,0.5))
#   plot(bound,bord="dark grey")
#   plot(stms,pch=19,col="blue", cex=0.8, add=T)
#   #textxy(stms$BEGIN_LON, stms$BEGIN_LAT, stms$EVENT_TYPE, cex=0.8, col="red", offset = 0.6)
#   if (year == 2008 | year == 2010){
#     pointLabel(stms$BEGIN_LON, stms$BEGIN_LAT, stms$EVENT_TYPE,cex=0.6, col="red", offset = 0.6)
#   }else{
#     pointLabel(stms$BEGIN_LON, stms$BEGIN_LAT, stms$EVENT_TYPE,cex=0.8, col="red", offset = 0.6)
#   }
#   title(paste0("Storm events in ", year))
#   dev.off()
#   print(year)
# }
#im.convert("storms_*.png", output = "storms_events_nyc.gif")

# read climate data in the NYC
clm.df <- read.csv("https://www.ncei.noaa.gov/orders/cdo/1025271.csv", stringsAsFactors = FALSE)
clm.df$YEAR <- unlist(lapply(clm.df$DATE, function(x) get.int(x,'y')))
clm.df$MONTH <- unlist(lapply(clm.df$DATE, function(x) get.int(x,'m')))
clm.df$YDAY <- unlist(lapply(clm.df$DATE, function(x) get.int(x,'d')))
clm.df$HOUR <- unlist(lapply(clm.df$DATE, function(x) get.int(x,'h')))
tmp1 <- aggregate(DAILYAverageDryBulbTemp~YDAY, data=clm.df, mean)
tmp2 <- aggregate(DAILYAverageWetBulbTemp~YDAY, data=clm.df, mean)
tmp.d <- merge(tmp1, tmp2, by="YDAY")
tmp3 <- aggregate(MonthlyMaximumTemp~MONTH, data=clm.df, mean)
tmp4 <- aggregate(MonthlyMinimumTemp~MONTH, data=clm.df, mean)
tmp5 <- aggregate(MonthlyMeanTemp~MONTH, data=clm.df, mean)
tmp.m <- merge(tmp3, tmp4, by="MONTH")
tmp.m <- merge(tmp.m,tmp5, by="MONTH")
mybar <- barplot(tmp.m$MonthlyMeanTemp, names.arg = tmp.m$MONTH)
points(mybar, tmp.m$MonthlyMaximumTemp, pch=19, col="red")
points(mybar, tmp.m$MonthlyMinimumTemp, pch=19, col="blue")
legend("topleft",c("Max","Min"),pch=c(19,19),col =c("red","blue"))
title("Monthly mean temperature (°F)")
legend(300,80, c("Dry","Wet"),lty=c(1,1), col=c("red","blue"))

plot(tmp.d$YDAY, tmp.d$DAILYAverageDryBulbTemp,type='l', xlab="Day of year", ylab="Temperature", col="red")
lines(tmp.d$YDAY, tmp.d$DAILYAverageWetBulbTemp, col="blue")
title("Daily average dry/wet bulb temperature (°F)")
legend(300,80, c("Dry","Wet"),lty=c(1,1), col=c("red","blue"))

clm.df$HOURLYDRYBULBTEMPC <- as.numeric(clm.df$HOURLYDRYBULBTEMPC)
clm.df$HOURLYWETBULBTEMPC <- as.numeric(clm.df$HOURLYWETBULBTEMPC)
clm.df$HOURLYPrecip <- as.numeric(clm.df$HOURLYPrecip)
library(dplyr)
tmp.m.h <- data.frame(h=numeric(),t1=numeric(),t2=numeric(),m=character())
pre.m.h <- data.frame(h=numeric(),p=numeric(),m=character())
for (i in 1:12){
  tmp6 <- aggregate(HOURLYDRYBULBTEMPC~HOUR, data=filter(clm.df,MONTH==i), mean, na.action = na.omit)
  tmp7 <- aggregate(HOURLYWETBULBTEMPC~HOUR, data=filter(clm.df,MONTH==i), mean)
  pre.h <- aggregate(HOURLYPrecip~HOUR, data=filter(clm.df,MONTH==i), mean, na.action = na.omit)
  tmp.h <- merge(tmp6, tmp7, by="HOUR")
  tmp.h$Month <- rep(sprintf("%02d", i),length(tmp.h$HOUR))
  pre.h$Month <- rep(sprintf("%02d", i),length(tmp.h$HOUR))
  tmp.m.h <- rbind(tmp.m.h,tmp.h)
  pre.m.h <- rbind(pre.m.h,pre.h)
  print(i)
}
library(ggplot2)
library(tidyr)
library(plotly)
tmp.m.h.n <- gather(tmp.m.h, Key, Value, -Month, -HOUR)
names(tmp.m.h.n)[1] <- "Hour"
tmp.m.h.n$Value <- round(tmp.m.h.n$Value,1)
tmp.m.h.n$Key <- factor(tmp.m.h.n$Key, labels = c("Hourly dry bulb temperature (°C)","Hourly wet bulb temperature (°C)"))
tmp.p <- ggplot(data = tmp.m.h.n,
       aes(x = Hour, y = Value, color = Month)) +
  facet_wrap(~Key, dir = "v")+
  geom_line(stat = 'summary',
            fun.y = 'mean')+
  geom_point()+
  xlab("Hour of day")+
  ylab("Temperature (°C)")
ggplotly(tmp.p)

# monthly precipitation
clm.df$DAILYPrecip <- as.numeric(clm.df$DAILYPrecip)
pre.m.d <- aggregate(DAILYPrecip~MONTH, data=clm.df, sum, na.action = na.omit)
pre.y.m.d <- data.frame(m=numeric(),p=numeric(),y=character())
for (y in 2013:2017){
  pre.y <- aggregate(DAILYPrecip~MONTH, data=filter(clm.df,YEAR==y), sum, na.action = na.omit)
  pre.y$Year <- rep(toString(y),length(pre.y$MONTH))
  pre.y.m.d <- rbind(pre.y.m.d,pre.y)
  print(y)
}
pre.y.m.d$MONTH <- as.factor(pre.y.m.d$MONTH)
clm.df$Year <- as.factor(clm.df$YEAR)
clm.df$Yday <- as.factor(clm.df$YDAY)
pre.df <- clm.df[!is.na(clm.df$DAILYPrecip),]
p <- ggplot(pre.y.m.d, aes(MONTH, DAILYPrecip,fill=Year))
p+geom_bar(position = "fill", stat = "identity")+
  xlab("Month")+ylab("Daily precipitation")

# daily precipitation
p <- ggplot(pre.df, aes(YDAY,DAILYPrecip, color=Year)) +geom_point()+ylim(0,2)+xlab("Day of year")+ylab("Daily precipitation")
p
ggplotly(p + facet_wrap(~MONTH))

names(pre.m.h)[1:2] <- c("Hour", "Precipitation")
pre.m.h$Precipitation <- round(pre.m.h$Precipitation * 1000, 1)
pre.m.h.n <- filter(pre.m.h, Hour > 0)
ggplot(data = pre.m.h,
                aes(x = Hour, y = Precipitation, color = Month)) +
  geom_line(stat = 'summary',
            fun.y = 'mean')+
  geom_point()+
  xlab("Hour of day")+
  ylab(expression('Hourly precipitation ('~{10}^{-3}~'mm)'))+
  geom_vline(xintercept = 6, color = "black", linetype=4)+
  geom_vline(xintercept = 20, color = "black", linetype=4)

pre.p <- ggplot(data = pre.m.h.n,
                 aes(x = Hour, y = Precipitation, color = Month)) +
  geom_line(stat = 'summary',
            fun.y = 'mean')+
  geom_point()+
  xlab("Hour of day")+
  ylab('Hourly precipitation (µm)')
ggplotly(pre.p)

# read water quality data 
read.csv(paste0(infolder, "WQ/water_quality.csv"))


