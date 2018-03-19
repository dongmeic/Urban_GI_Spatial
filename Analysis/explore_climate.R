# By Dongmei Chen
# This script is designed to visualize climate data
# based on visualize_climate_water.R
# Documentation: https://www1.ncdc.noaa.gov/pub/data/cdo/documentation/GHCND_documentation.pdf
# libraries
library(sp)
library(rgdal)
library(maps)
library(maptools)
library(dplyr)

# functions
# convert dataframe to spatial dataframe
df2spdf <- function(col1, col2, colname1, colname2, df){
  xy <- data.frame(df[,c(col1,col2)])
  coordinates(xy) <- c(colname1, colname2)
  proj4string(xy) <- lonlat
  spdf <- SpatialPointsDataFrame(coords = xy, data = df, proj4string = lonlat)
  spdf <- spTransform(spdf, crs)
  return(spdf)
}

lonlat <- CRS("+proj=longlat +datum=NAD83")
crs <- CRS("+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333
           +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83
           +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")
setwd("/nfs/urbangi-data/spatial_data/output")
infolder <- "/nfs/urbangi-data/spatial_data/"

wbdhu12 <- readOGR(dsn = paste0(infolder, "WBDHU"), layer = "wbdhu_12", stringsAsFactors = FALSE)
crs <- CRS("+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333
           +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83
           +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")
wbdhu12 <- spTransform(wbdhu12, crs)
bound <- readOGR(dsn = paste0(infolder, "BD"), layer = "nyad_dis")
lonlat <- CRS("+proj=longlat +datum=NAD83")
proj4string(bound) <- crs
month.colors <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c',
                  '#fb9a99','#e31a1c','#fdbf6f','#ff7f00',
                  '#cab2d6','#6a3d9a','#ffff99','#b15928')
year.colors <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99',
                 '#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a')

# read storm data
# file.loc <- "https://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/"
filenm <- "StormEvents_details-ftp_v1.0_d"
dates <- c("1950_c20170120","1951_c20160223","1952_c20170619","1953_c20160223","1954_c20160223","1955_c20160223",
           "1956_c20170717","1957_c20160223","1958_c20160223","1959_c20160223","1960_c20160223","1961_c20160223",
           "1962_c20160223","1963_c20160223","1964_c20160223","1965_c20170619","1966_c20160223","1967_c20160223",
           "1968_c20160223","1969_c20170717","1970_c20160223","1971_c20160223","1972_c20170717","1973_c20160223",
           "1974_c20160223","1975_c20160223","1976_c20160223","1977_c20160223","1978_c20160223","1979_c20160223",
           "1980_c20170717","1981_c20170717","1982_c20160223","1983_c20160223","1984_c20170717","1985_c20160223",
           "1986_c20160223","1987_c20160223","1988_c20170717","1989_c20170717","1990_c20170717","1991_c20170717",
           "1992_c20170717","1993_c20170717","1994_c20170717","1995_c20170522","1996_c20170717","1997_c20170717",
           "1998_c20170717","1999_c20170717","2000_c20170717","2001_c20170717","2002_c20170717","2003_c20170717",
           "2004_c20170717","2005_c20170717","2006_c20170717","2007_c20170717","2008_c20170718","2009_c20170816",
           "2010_c20170726","2011_c20170519","2012_c20170519","2013_c20170519","2014_c20170718","2015_c20170918",
           "2016_c20180117","2017_c20180216")
years <- 1950:2017
df <- read.table(paste0(infolder, "CLM/details/", filenm, dates[1],".csv.gz"), sep=",", header = TRUE)
for (i in 2:length(years)){
  input <- read.table(paste0(infolder, "CLM/details/", filenm, dates[i],".csv.gz"), sep=",", header = TRUE)
  df <- rbind(df, input)
  print(years[i])
}
table(df$STATE)
df_NY <- df[df$STATE == "NEW YORK",]
table(df_NY$CZ_NAME)
counties_NYC <- c("NEW YORK", "BRONX", "KINGS", "QUEENS", "RICHMOND")
df_NYC <- filter(df_NY, CZ_NAME %in% counties_NYC)
# year1 <- as.numeric(substring(df_NYC$BEGIN_YEARMONTH,1,4))
# month1 <- as.numeric(substring(df_NYC$BEGIN_YEARMONTH,5,6))
# df_NYC$BEGIN_TIME <- ifelse(nchar(df_NYC$BEGIN_TIME)==3, 
#                             paste0(0,df_NYC$BEGIN_TIME), 
#                             df_NYC$BEGIN_TIME)
# df_NYC$END_TIME <- ifelse(nchar(df_NYC$END_TIME)==3, 
#                           paste0(0,df_NYC$BEGIN_TIME), 
#                             df_NYC$END_TIME)
# hour1 <- as.numeric(substring(df_NYC$BEGIN_TIME,1,2))
# minute1 <- as.numeric(substring(df_NYC$BEGIN_TIME,3,4))
# year2 <- as.numeric(substring(df_NYC$END_YEARMONTH,1,4))
# month2 <- as.numeric(substring(df_NYC$END_YEARMONTH,5,6))
# hour2 <- as.numeric(substring(df_NYC$END_TIME,1,2))
# minute2 <- as.numeric(substring(df_NYC$END_TIME,3,4))
# df_NYC$Start <- ISOdate(year1,month1,df_NYC$BEGIN_DAY,hour1,minute1,0,tz="EST")
# df_NYC$End <- ISOdate(year2,month2,df_NYC$END_DAY,hour2,minute2,0,tz="EST")
df_NYC$Start <- as.POSIXlt(df_NYC$BEGIN_DATE_TIME, format = "%d-%b-%Y %H:%M:%S") 
df_NYC$End <- as.POSIXlt(df_NYC$END_DATE_TIME, format = "%d-%b-%Y %H:%M:%S")
df_NYC$Duration <- abs(as.numeric(difftime(df_NYC$End, df_NYC$Start, units="hours")))
write.csv(df_NYC, "csv/stormdata_nyc.csv", row.names=FALSE)
# as.numeric(t(sapply(storms$BEGIN_YEARMONTH, function(x) substring(x, first=c(1,5), last=c(4,6))))[,2])
storms <- read.csv("CSV/stormdata_nyc.csv", stringsAsFactors = FALSE)
storms$MONTH <- as.numeric(substring(df_NYC$BEGIN_YEARMONTH,5,6))
storms$DOY <- as.numeric(strftime(storms$Start, format = "%j"))
df_NYC_df  <- df_NYC[!is.na(df_NYC$BEGIN_LAT) & !is.na(df_NYC$BEGIN_LON),]
df_NYC_df$BEGIN_LON <- ifelse(df_NYC_df$BEGIN_LON > -750 & df_NYC_df$BEGIN_LON < -735, 
                              df_NYC_df$BEGIN_LON/10, df_NYC_df$BEGIN_LON)
df_NYC_spdf  <- df2spdf(46,45,"BEGIN_LON", "BEGIN_LAT", df_NYC_df)
writeOGR(df_NYC_spdf, dsn="./shapefile", layer="storm_events", 
         overwrite_layer = TRUE,driver = "ESRI Shapefile")
storms <- read.csv("CSV/stormdata_nyc.csv", stringsAsFactors = FALSE)
str(storms)
head(storms)

# check the locations and types of the storm events over years
for (year in 2008:2017){
  stms <- df_NYC_spdf[df_NYC_spdf$YEAR==year,]
  #stms <- remove.duplicates(stms)
  png(paste0("figure/storms_",year,".png"), width=9, height=8, units="in", res=300)
  par(xpd=FALSE,mfrow=c(1,1),mar=c(0.5,0.5,2.5,0.5))
  plot(bound,bord="white")
  plot(wbdhu12, add=T)
  plot(bound,bord="dark grey", add=T)
  plot(stms,pch=19,col="blue", cex=0.8, add=T)
  #textxy(stms$BEGIN_LON, stms$BEGIN_LAT, stms$EVENT_TYPE, cex=0.8, col="red", offset = 0.6)
  if (year == 2008 | year == 2010){
    pointLabel(stms$BEGIN_LON, stms$BEGIN_LAT, stms$EVENT_TYPE,cex=0.6, col="red", offset = 0.6)
  }else{
    pointLabel(stms$BEGIN_LON, stms$BEGIN_LAT, stms$EVENT_TYPE,cex=0.8, col="red", offset = 0.6)
  }
  title(paste0("Storm events in ", year))
  dev.off()
  print(year)
}

df_NYC_spdf$Month <- as.numeric(substring(df_NYC_spdf$BEGIN_YEARMONTH,5,6))
months <- c("January", "February", "March", "April", "May", "June", "July", 
            "August", "September", "October", "November", "December")

for (Month in 1:12){
  stms <- df_NYC_spdf[df_NYC_spdf$Month==Month,]
  #stms <- remove.duplicates(stms)
  png(paste0("figure/storms_",Month,".png"), width=9, height=8, units="in", res=300)
  par(xpd=FALSE,mfrow=c(1,1),mar=c(0.5,0.5,2.5,0.5))
  plot(bound,bord="white")
  plot(wbdhu12, add=T)
  plot(bound,bord="dark grey", add=T)
  plot(stms,pch=19,col="blue", cex=0.8, add=T)
  #textxy(stms$BEGIN_LON, stms$BEGIN_LAT, stms$EVENT_TYPE, cex=0.8, col="red", offset = 0.6)
  if (Month == 6 | Month == 7 | Month == 8 | Month == 9){
    pointLabel(stms$BEGIN_LON, stms$BEGIN_LAT, stms$EVENT_TYPE,cex=0.6, col="red", offset = 0.6)
  }else{
    pointLabel(stms$BEGIN_LON, stms$BEGIN_LAT, stms$EVENT_TYPE,cex=0.8, col="red", offset = 0.6)
  }
  title(paste0("Storm events in ", months[Month]))
  dev.off()
  print(months[Month])
}

s2010 <- storms[storms$YEAR==2010,]

ggplot(storms[storms$YEAR > 2007 & storms$YEAR < 2018,]) + 
  geom_bar(aes(as.character(YEAR), EVENT_ID, fill = as.factor(MONTH)), 
           position = "stack", stat = "summary", fun.y = function(x) length(x)) + 
  scale_fill_manual(values=month.colors) +
  labs(x="Year", y="Number of Storms", fill="Month", 
       title="Storm events in NYC over time",
       subtitle="(2008 - 2017)")
ggsave(paste0("figure/storms_year_2008.png"), width=8, height=5, units="in")

ggplot(storms) + 
  geom_bar(aes(YEAR, EVENT_ID, fill = as.factor(MONTH)), 
           position = "stack", stat = "summary", fun.y = function(x) length(x)) + 
  scale_fill_manual(values=month.colors) +
  labs(x="Year", y="Number of Storms", fill="Month", 
       title="Storm events in NYC over time",
       subtitle="since 1958")
ggsave(paste0("figure/storms_year.png"), width=8, height=5, units="in")

ggplot(storms[storms$YEAR > 2007 & storms$YEAR < 2018, ]) + 
  geom_bar(aes(formatC(MONTH, width=2, flag="0"), EVENT_ID, fill=as.character(MONTH)), 
           stat = "summary", fun.y = function(x) length(x))+
  scale_fill_manual(values=month.colors)+
  geom_point(aes(formatC(MONTH, width=2, flag="0"), EVENT_ID, size=2, colour=as.character(YEAR)),
             stat = "summary", fun.y = function(x) length(x))+
  scale_colour_manual(values=year.colors)+
  scale_size(guide = "none")+
  labs(x="Month", y="Number of storms", fill="Month", colour="Year",
       title = "Storm events in NYC over time",
       subtitle = "(2008 - 2017)")
ggsave(paste0("figure/storms_month_2008.png"), width=8, height=7, units="in")

storms$Event <- rep(1, length(storms$EVENT_ID))

ggplot(storms[storms$YEAR > 2007 & storms$YEAR < 2018,]) + 
  geom_bar(aes(DOY, Event, fill = as.factor(YEAR)), 
           position = "stack", stat = "summary", fun.y = sum) + 
  scale_fill_manual(values=month.colors) +
  stat_summary(aes(x=DOY, y = Event, group=1), fun.y=sum,  geom="line", group=1)+
  labs(x="Day of year", y="Number of Storms", fill="Year", 
       title="Storm events in NYC over time",
       subtitle="(2008 - 2017)")
ggsave(paste0("figure/storms_day.png"), width=10, height=6, units="in")

# read temperature and precipitation
clim <- read.csv(paste0(infolder, "CLM/1227099.csv"),stringsAsFactors = FALSE)
ymdt <- as.POSIXlt(as.Date(clim$DATE, format="%Y-%m-%d"))
clim$Year <- ymdt$year + 1900
clim$Month <- ymdt$mon + 1
clim$DOY <- ymdt$yday + 1
# clim_df <- clim[clim$STATION=="USC00308721" & clim$Year > 2007,]
# write.csv(clim_df, "csv/climate_date.csv", row.names=FALSE)
names(clim)

pts <- as.data.frame(rbind(c(40.6386, -73.7622),c(40.8656, -72.8643)))
pts$Station <- c("USW00094789", "USC00308721")
pts$Name <- c("JFK INTERNATIONAL AIRPORT", "NY US UPTON COOP NWSFO NEW YORK")
names(pts) <- c("Latitude","Longitude")
locs <- df2spdf(2,1,"Longitude", "Latitude", pts)
GIsites <- readOGR(dsn = "./shapefile", layer = "GIsites_all", stringsAsFactors = FALSE)
wq_pts <- readOGR(dsn = "./shapefile", layer = "dep_wq_sampling_sites", stringsAsFactors = FALSE)

png(paste0("figure/clim_monitoring_pt.png"), width=8, height=6, units="in", res=300)
plot(bound, bord="white")
plot(wbdhu12, add=T)
plot(bound, bord="grey58", add=T)
plot(locs, pch=16, cex=1.5, col="red", add=T)
plot(GIsites, pch=16, cex=0.5, col=rgb(0,0.8,0,0.8), add=T)
plot(wq_pts, pch=20, col=rgb(0,0,0.8,0.6), add=T)
pointLabel(locs$Longitude, locs$Latitude, locs$Name, cex=1.2, col="red", offset = 2)
text(940000, 260000, cex=1.4, "Climate monitoring location")
legend(920000, 240000, bty="n", pch=c(16,20,16), 
       col=c(rgb(0,0.8,0,0.8),rgb(0,0,0.8,0.6),"red"), 
       pt.cex=c(0.5,1,1.5),
       cex = 1.2,
       legend=c("GI sites","WQ sites","CLM site"))
legend(915000, 215000, bty="n",lty = 1, col=c("black", "grey58"),
       legend = c("WBD HU12", "NYC"))
dev.off()


clim <- clim[clim$STATION == "USC00308721",]

# yearly precipitation
ggplot(clim[clim$Year < 2018 & clim$Year > 2007,]) + 
  geom_bar(aes(as.character(Year), PRCP, fill = as.factor(Month)), 
           position = "stack", stat = "summary", fun.y = mean) + 
  scale_fill_manual(values=month.colors) +
  labs(x="Year", y="Precipitation (mm)", fill="Month", 
       title="Annual precipitation in NYC",
       subtitle="(2012 - 2017)")
ggsave(paste0("figure/precp_year.png"), width=8, height=5, units="in")

# monthly precipitation
ggplot(clim[clim$Year > 2007 & clim$Year < 2018, ]) + 
  geom_bar(aes(formatC(Month, width=2, flag="0"), PRCP, fill=as.character(Month)), 
           stat = "summary", fun.y = mean)+
  scale_fill_manual(values=month.colors)+
  geom_point(aes(formatC(Month, width=2, flag="0"), PRCP, size=2, colour=as.character(Year)),
             stat = "summary", fun.y = mean)+
  scale_colour_manual(values=year.colors)+
  scale_size(guide = "none")+
  labs(x="Month", y="Precipitation (mm)", fill="Month", colour="Year",
       title = "Seasonal precipitation in NYC",
       subtitle = "(2012 - 2017)")
ggsave(paste0("figure/precp_month.png"), width=8, height=7, units="in")

# yearly temperature
ggplot(clim[clim$Year < 2018 & clim$Year > 2007,]) + 
  geom_bar(aes(as.character(Year), (TMAX+TMIN)/2, fill = as.factor(Month)), 
           position = "stack", stat = "summary", fun.y = mean) + 
  scale_fill_manual(values=month.colors) +
  labs(x="Year", y="(Tmax + Tmin)/2 (°C)", fill="Month", 
       title="Annual temperature in NYC",
       subtitle="(2012 - 2017)")
ggsave(paste0("figure/temp_year.png"), width=8, height=5, units="in")

# monthly temperature
ggplot(clim[clim$Year > 2007 & clim$Year < 2018, ]) + 
  geom_bar(aes(formatC(Month, width=2, flag="0"), (TMAX+TMIN)/2, fill=as.character(Month)), 
           stat = "summary", fun.y = mean)+
  scale_fill_manual(values=month.colors)+
  geom_point(aes(formatC(Month, width=2, flag="0"), (TMAX+TMIN)/2, size=2, colour=as.character(Year)),
             stat = "summary", fun.y = mean)+
  scale_colour_manual(values=year.colors)+
  scale_size(guide = "none")+ 
  labs(x="Month", y="(Tmax + Tmin)/2 (°C)", fill="Month", colour="Year",
       title = "Seasonal temperature in NYC",
       subtitle = "(2012 - 2017)")
ggsave(paste0("figure/temp_month.png"), width=8, height=7, units="in")

plot(clim$DOY, clim$TAVG)
ggplot(clim, aes(x = formatC(Month, width=2, flag="0"), y = (TMAX+TMIN)/2)) + geom_boxplot()
ggplot(clim, aes(x = DOY, y = log(PRCP+1))) + geom_point()+
  stat_summary(aes(y = log(PRCP), col="red", group=1), fun.y=median,  geom="line", group=1)

ggplot(clim[clim$Year > 2007 & clim$Year < 2018, ]) + 
  geom_bar(aes(DOY, PRCP, fill = as.factor(Year)), 
           position = "stack", stat = "summary", fun.y = sum) + 
  scale_fill_manual(values=month.colors) +
  stat_summary(aes(x=DOY, y = PRCP, group=1), fun.y=sum,  geom="line", group=1)+
  labs(x="Day of year", y="Precipitation (mm)", fill="Year", 
       title="Precipitation in NYC",
       subtitle="(2012 - 2017)")
ggsave(paste0("figure/precp_day.png"), width=10, height=6, units="in")

# daily temperature
png(paste0("figure/temp_day.png"), width=9, height=5, units="in", res=300)
par(xpd=FALSE,mfrow=c(1,1),mar=c(4.5,4.5,0.5,0.5))
plot(clim$DOY, clim$TMIN, pch=16, cex=0.5, col=rgb(0,0,1,0.8),
     xlab="Day of year", ylab="Temperature (°C)",
     xlim=c(1, 366), ylim=c(-22, 40))
points(clim$DOY, clim$TMAX, pch=16, cex=0.5, col=rgb(1,0,0,0.8))
points(clim$DOY, clim$TAVG, pch=16, cex=0.5, col=rgb(0,1,0,0.8))
legend(200, 0, bty="n", pch=c(16,16,16), 
       col=c(rgb(1,0,0,0.8),rgb(0,1,0,0.8),rgb(0,0,1,0.8)),
       pt.cex=c(0.5,0.5,0.5),
       legend=c("Tmax", "Tmean", "Tmin"))
dev.off()

ggplot(clim[clim$Year > 2007 & clim$Year < 2018, ]) + 
  geom_bar(aes(DOY, PRCP, fill = as.factor(Year)), 
           position = "stack", stat = "summary", fun.y = sum) + 
  scale_fill_manual(values=month.colors) +
  stat_summary(aes(x=DOY, y = PRCP, group=1), fun.y=sum,  geom="line", group=1)+
  labs(x="Day of year", y="Precipitation (mm)", fill="Year", 
       title="Precipitation in NYC",
       subtitle="(2012 - 2017)")
ggsave(paste0("figure/precp_day.png"), width=10, height=6, units="in")

greyscales <- colorRampPalette(c('black','white'))(10)

years <- 2012:2017
colors <- c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')
png(paste0("figure/cumprep_day.png"), width=9, height=5, units="in", res=300)
par(xpd=FALSE,mfrow=c(1,1),mar=c(4.5,4.5,0.5,0.5))
plot(c(0, clim[clim$Year == 2012, ]$DOY), cumsum(c(0, clim[clim$Year == 2012, ]$PRCP)), 
     type="l", col=colors[1], xlim=c(1,366), ylim=c(0, 1500), lwd=3,
     xlab="Day of year", ylab="Cumulative precipitation")
for (i in 2:6){
  lines(c(0, clim[clim$Year == years[i], ]$DOY), 
        cumsum(c(0, clim[clim$Year == years[i], ]$PRCP)), lty=1, lwd=3, col=colors[i])
}
legend(0,1500,lty = rep(1,6), col=colors[1:6], 
       bty="n", lwd=rep(2,6), ncol=3,legend=years)
dev.off()

# update climate data
clim_1 <- read.csv(paste0(infolder, "CLM/1250709.csv"),stringsAsFactors = FALSE)
clim_2 <- read.csv(paste0(infolder, "CLM/1250713.csv"),stringsAsFactors = FALSE)
clim_3 <- read.csv(paste0(infolder, "CLM/1250717.csv"),stringsAsFactors = FALSE)
colnames <- c("STATION", "NAME", "LATITUDE", "LONGITUDE", "ELEVATION", "DATE", "PRCP",
              "TAVG", "TMAX", "TMIN", "TMIN")
clim_1 <- clim_1[,colnames];clim_2 <- clim_2[,colnames];clim_3 <- clim_3[,colnames]
clim_n <- rbind(clim_1, clim_2, clim_3)

par(xpd=FALSE,mfrow=c(1,1),mar=c(0.5,0.5,0.5,0.5))
clm.pts <- df2spdf(4,3,"LONGITUDE","LATITUDE",clim_n)
clm.pts <- remove.duplicates(clm.pts)
clm.pts$NO <- 1:length(clm.pts$NAME)
plot(bound)
plot(clm.pts, pch=16, col="red", add=T)
pointLabel(clm.pts$LONGITUDE, clm.pts$LATITUDE, as.character(clm.pts$NO), 
           cex=1.2, col="blue", offset = 2)

# pick the stations
stations <- c(15,169,157,12,166,158,143,160,73,6,102,7,105,99,117,65,66,98,5,125,146,9,96,79)
clm.pts <- clm.pts[clm.pts$NO %in% stations,]
png(paste0("figure/clim_monitoring_pts.png"), width=8, height=6, units="in", res=300)
par(xpd=FALSE,mfrow=c(1,1),mar=c(0.5,0.5,0.5,0.5))
plot(bound, bord="white")
plot(wbdhu12, add=T)
plot(bound, bord="grey58", add=T)
plot(GIsites, pch=16, cex=0.5, col=rgb(0,0.8,0,0.8), add=T)
plot(wq_pts, pch=20, col=rgb(0,0,0.8,0.6), add=T)
plot(clm.pts, pch=16, cex=1.5, col="red", add=T)
#pointLabel(clm.pts$LONGITUDE, clm.pts$LATITUDE, clm.pts$NAME,cex=0.5, col="blue", offset = 2)
text(940000, 260000, cex=1.4, "Climate monitoring locations")
legend(920000, 240000, bty="n", pch=c(16,20,16), 
       col=c(rgb(0,0.8,0,0.8),rgb(0,0,0.8,0.6),"red"), 
       pt.cex=c(0.5,1,1.5),
       cex = 1.2,
       legend=c("GI sites","WQ sites","CLM sites"))
legend(915000, 215000, bty="n",lty = 1, col=c("black", "grey58"),
       legend = c("WBD HU12", "NYC"))
dev.off()

# get climate data from the selected stations
clim <- clim_n[clim_n$NAME %in% clm.pts$NAME,]
ymdt <- as.POSIXlt(as.Date(clim$DATE, format="%Y-%m-%d"))
clim$Year <- ymdt$year + 1900
clim$Month <- ymdt$mon + 1
clim$DOY <- ymdt$yday + 1
write.csv(clim, "csv/climatedata_nyc.csv", row.names=FALSE)

# repeated codes here
# yearly precipitation
ggplot(clim) + 
  geom_bar(aes(as.character(Year), PRCP, fill = as.factor(Month)), 
           position = "stack", stat = "summary", fun.y = mean) + 
  scale_fill_manual(values=month.colors) +
  labs(x="Year", y="Precipitation (mm)", fill="Month", 
       title="Annual precipitation in NYC",
       subtitle="(2008 - 2017)")
ggsave(paste0("figure/precp_year.png"), width=8, height=5, units="in")

# monthly precipitation
ggplot(clim) + 
  geom_bar(aes(formatC(Month, width=2, flag="0"), PRCP, fill=as.character(Month)), 
           stat = "summary", fun.y = mean)+
  scale_fill_manual(values=month.colors)+
  geom_point(aes(formatC(Month, width=2, flag="0"), PRCP, size=2, colour=as.character(Year)),
             stat = "summary", fun.y = mean)+
  scale_colour_manual(values=year.colors)+
  scale_size(guide = "none")+
  labs(x="Month", y="Precipitation (mm)", fill="Month", colour="Year",
       title = "Seasonal precipitation in NYC",
       subtitle = "(2008 - 2017)")
ggsave(paste0("figure/precp_month.png"), width=8, height=7, units="in")

# yearly temperature
ggplot(clim) + 
  geom_bar(aes(as.character(Year), (TMAX+TMIN)/2, fill = as.factor(Month)), 
           position = "stack", stat = "summary", fun.y = mean) + 
  scale_fill_manual(values=month.colors) +
  labs(x="Year", y="(Tmax + Tmin)/2 (°C)", fill="Month", 
       title="Annual temperature in NYC",
       subtitle="(2008 - 2017)")
ggsave(paste0("figure/temp_year.png"), width=8, height=5, units="in")

# monthly temperature
ggplot(clim) + 
  geom_bar(aes(formatC(Month, width=2, flag="0"), (TMAX+TMIN)/2, fill=as.character(Month)), 
           stat = "summary", fun.y = mean)+
  scale_fill_manual(values=month.colors)+
  geom_point(aes(formatC(Month, width=2, flag="0"), (TMAX+TMIN)/2, size=2, colour=as.character(Year)),
             stat = "summary", fun.y = mean)+
  scale_colour_manual(values=year.colors)+
  scale_size(guide = "none")+ 
  labs(x="Month", y="(Tmax + Tmin)/2 (°C)", fill="Month", colour="Year",
       title = "Seasonal temperature in NYC",
       subtitle = "(2008 - 2017)")
ggsave(paste0("figure/temp_month.png"), width=8, height=7, units="in")

ggplot(clim) + 
  geom_bar(aes(DOY, PRCP, fill = as.factor(Year)), 
           position = "stack", stat = "summary", fun.y = sum) + 
  scale_fill_manual(values=month.colors) +
  stat_summary(aes(x=DOY, y = PRCP, group=1), fun.y=sum,  geom="line", group=1)+
  labs(x="Day of year", y="Precipitation (mm)", fill="Year", 
       title="Precipitation in NYC",
       subtitle="(2008 - 2017)")
ggsave(paste0("figure/precp_day.png"), width=10, height=6, units="in")

# daily temperature
png(paste0("figure/temp_day.png"), width=9, height=5, units="in", res=300)
par(xpd=FALSE,mfrow=c(1,1),mar=c(4.5,4.5,0.5,0.5))
plot(clim$DOY, clim$TMIN, pch=16, cex=0.5, col=rgb(0,0,1,0.8),
     xlab="Day of year", ylab="Temperature (°C)",
     xlim=c(1, 366), ylim=c(-22, 40))
points(clim$DOY, clim$TMAX, pch=16, cex=0.5, col=rgb(1,0,0,0.8))
points(clim$DOY, clim$TAVG, pch=16, cex=0.5, col=rgb(0,1,0,0.8))
legend(200, 0, bty="n", pch=c(16,16,16), 
       col=c(rgb(1,0,0,0.8),rgb(0,1,0,0.8),rgb(0,0,1,0.8)),
       pt.cex=c(0.5,0.5,0.5),
       legend=c("Tmax", "Tmean", "Tmin"))
dev.off()

years <- 2008:2017
png(paste0("figure/cumprep_day.png"), width=9, height=5, units="in", res=300)
par(xpd=FALSE,mfrow=c(1,1),mar=c(4.5,4.5,0.5,0.5))
plot(c(0, clim[clim$Year == 2008 & clim$STATION == "US1NJBG0003", ]$DOY), 
     cumsum(c(0, clim[clim$Year == 2008 & clim$STATION == "US1NJBG0003", ]$PRCP)), 
     type="l", col=year.colors[1], xlim=c(1,366), ylim=c(0, 1800), lwd=3,
     xlab="Day of year", ylab="Cumulative precipitation (mm)")
for (i in 2:10){
  lines(c(0, clim[clim$Year == years[i] & clim$STATION == "US1NJBG0003", ]$DOY), 
        cumsum(c(0, clim[clim$Year == years[i] & clim$STATION == "US1NJBG0003", ]$PRCP)), 
        lty=1, lwd=3, col=year.colors[i])
}
legend(0,1800,lty = rep(1,6), col=year.colors[1:12], 
       bty="n", lwd=rep(2,6), ncol=5,legend=years)
dev.off()
