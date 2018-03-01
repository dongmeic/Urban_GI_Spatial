# By Dongmei Chen
# This script is designed to clean up water quality data and 
# identify the point pattern of the sampling sites
# libraries
library(dplyr)
library(tidyr)
library(rgdal)
library(maptools)
library(gdata)
library(readxl)
library(reshape2)
library(spatstat)
library(sp)
library(raster)

# global settings
lonlat <- CRS("+proj=longlat +datum=NAD83")
nyc.crs <- CRS("+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333
           +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83
           +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")
setwd("/nfs/urbangi-data/spatial_data/output")
infolder <- "/nfs/urbangi-data/spatial_data/"

# functions
# convert dataframe to spatial dataframe
df2spdf <- function(col1, col2, colname1, colname2, df){
  xy <- data.frame(df[,c(col1,col2)])
  coordinates(xy) <- c(colname1, colname2)
  proj4string(xy) <- lonlat
  spdf <- SpatialPointsDataFrame(coords = xy, data = df, proj4string = lonlat)
  return(spdf)
}

# get locations
getloc <- function(path,file.name, year, layer.name){
  kmlfile <- paste0(infolder, "WQ/", path, "/", file.name,".kml")
  pts.spdf <- readOGR(kmlfile, layer.name)
  if (path == "WTA"){
    # tkml <- getKMLcoordinates(kmlfile, ignoreAltitude=T)
    # pts.df <- data.frame(matrix(unlist(tkml), nrow=length(tkml), byrow=T),stringsAsFactors=FALSE)
    # pts.df <- cbind(pts.df, pts.spdf$Name)
    pts.df <- as.data.frame(pts.spdf[,c("Name")]); pts.df <- pts.df[,-4]
    colnames(pts.df) <- c("loc", "lon", "lat")
    pts.df$year <- rep(year, length(pts.df$loc))
  }else if(path == "CF"){
    if (year < 2015){
      pts.df <- as.data.frame(pts.spdf[,c("Site_ID")]); pts.df <- pts.df[,-4]
      colnames(pts.df) <- c("SiteID", "lon", "lat")
      pts.df$year <- rep(year, length(pts.df$SiteID))
    }else if(year == 2015){
      pts.df <- as.data.frame(pts.spdf[,c("Name")]); pts.df <- pts.df[,-4]
      colnames(pts.df) <- c("SiteName", "lon", "lat")
      pts.df$year <- rep(year, length(pts.df$SiteName))
    }else if(year >= 2016){
      pts.df <- as.data.frame(pts.spdf[,c("Name", "Site_Name")]); pts.df <- pts.df[,-5]
      colnames(pts.df) <- c("SiteID", "SiteName", "lon", "lat")
      pts.df$year <- rep(year, length(pts.df$SiteID))
    }
  }
  return(pts.df)
}

# clean water trail association data
clean.wta <- function(df){
  # get dates and high tide time
  df.1 <- df[1:2,]
  df.2 <- df[-1:-2,];colnames(df.2) <- as.character(unlist(df.2[1,]));df.2 = df.2[-1, ] # make first row name as the column names in the second half
  df.1 <- df.1[!sapply(df.1, function(x) all(x == ""))]
  M <- t(df.1);rownames(M) <- NULL;colnames(M) <- NULL # transpose matrix and remove row and colnames
  DF <- as.data.frame(M);colnames(DF) <- as.character(unlist(DF[1,]));DF = DF[-1, ] # make first row name as the column names
  n <- length(df.2$`Sample Site`) # number to repeat dates
  DF <- DF[rep(seq_len(nrow(DF)), each=n),] # repeat dates
  colnames(df.2)[which(colnames(df.2) =="Most Probable Number (MPN) of Enterococcus colonies per 100 ml")] = "MPN"
  #colnames(df.2)[which(colnames(df.2) =="Most Probable Number (MPN) of Enterococcus colonies per 100ml")] = "MPN"
  df.2 <- df.2[,-which(colnames(df.2) %in% c("Duplicate sample MPN",
                                             "Duplicate sample",
                                             "Duplicate Sample",
                                             "Duplicate sample/notes",
                                             "Duplicate sample/Notes",
                                             "Duplicate Sample/Notes",
                                             "Duplicate Sample/notes","Notes"))]
  days <- length(unique(DF$`Sample Date`))
  # reorganize the table
  df.2.1 <- rep(df.2[,1],times=days)
  df.2.2 <- rbind(df.2[,2], df.2[,4])
  df.2.3 <- rbind(df.2[,3], df.2[,5])
  for (i in 1:(days-2)){
    df.2.2 <- rbind(df.2.2, df.2[,4+i*2])
    df.2.3 <- rbind(df.2.3, df.2[,5+i*2])
  }
  DF2 <- cbind(df.2.1, DF, as.vector(t(df.2.2)), as.vector(t(df.2.3)))
  rownames(DF2) <- NULL
  colnames(DF2) <- c("loc","date","tide","time","MPN")
  DF2$year <- as.numeric(format(as.Date(DF2$date), format = "%Y"))
  DF2$month <- as.numeric(format(as.Date(DF2$date), format = "%m"))
  DF2$day <- as.POSIXlt(as.Date(DF2$date))[["yday"]]+1
  return(DF2)
}

##################################### Clean-up datasets #############################################
# harbor water
# collect common information over years
wq.df <- data.frame(site=character(),
                    date=character(),
                    DO_top=numeric(),
                    DO_bot=numeric(),
                    FC_top=numeric(),
                    FC_bot=numeric(),
                    Ent_top=numeric(),
                    Ent_bot=numeric(),
                    Tra=numeric())
for (year in 2008:2016){
  if (year == 2008){
    wq.tb <- read.xls(paste0(infolder, "WQ/DEP/dep_hs", year,".xls"), 
                      sheet = 1, skip=9, blank.lines.skip=TRUE, 
                      header = TRUE, stringsAsFactors=FALSE)
    wq.tb <- wq.tb[c("X...","X","Top.30","Bot.28","Top.15","Bot.14","Top.16","Bot.15","X.8")]
  }else if (year == 2009){
    wq.tb <- read.xls(paste0(infolder, "WQ/DEP/dep_hs", year,".xls"), 
                      sheet = 1, skip=7, blank.lines.skip=TRUE, 
                      header = TRUE, stringsAsFactors=FALSE)
    wq.tb <- wq.tb[c("X...","X","Top.30","Bot.28","Top.15","Bot.14","Top.16","Bot.15","X.8")]
  }else if (year == 2010 | year == 2011){
    wq.tb <- read.xls(paste0(infolder, "WQ/DEP/dep_hs", year,".xls"),
                      sheet = 1, skip=8, blank.lines.skip=TRUE, 
                      header = TRUE, stringsAsFactors=FALSE)
    wq.tb <- wq.tb[c("X...","X","Top.30","Bot.28","Top.15","Bot.14","Top.16","Bot.15","X.8")]
  }else{
    wq.tb <- read.xls(paste0(infolder, "WQ/DEP/harbor_sampling_ytd_", year,".xls"), 
                      sheet = 1, skip=5, blank.lines.skip=TRUE, 
                      header = TRUE, stringsAsFactors=FALSE)
    wq.tb <- wq.tb[,1:9]
  }
  colnames(wq.tb) <- c("site", "date", "DO_top", "DO_bot", "FC_top", "FC_bot", "Ent_top","Ent_bot","Tra")
  wq.df <- rbind(wq.df, wq.tb)
  print(year)
}
# split the date information
wq.df$year <- as.numeric(format(as.Date(wq.df$date), format = "%Y"))
wq.df$month <- as.numeric(format(as.Date(wq.df$date), format = "%m"))
wq.df$day <- as.POSIXlt(as.Date(wq.df$date))[["yday"]]+1
sapply(wq.df, class)
wq.df[,3:9] <- sapply(wq.df[,3:9],as.numeric)
write.csv(wq.df, "harbor_water_quality.csv", row.names=FALSE)
harbor_wq <- read.csv('harbor_water_quality.csv', stringsAsFactors = FALSE)
harbor_wq <- harbor_wq[!is.na(harbor_wq$year),]
harbor_wq.td <- gather(harbor_wq, Key, Value, -site, -date, -year, -month, -day)
harbor_wq.td$year <- as.character(harbor_wq.td$year)
coords <- read.csv(paste0(infolder, "WQ/DEP/harbor_sampling_coordinates.csv"), 
                   stringsAsFactors = FALSE)
coords <- coords[,-1]
colnames(coords)[1] <- "site"
harbor_wq.df <- merge(coords, harbor_wq.td, by="site")
harborwq.df <- merge(coords, harbor_wq, by="site")
colnames(harborwq.df)[2:3] <- c("lat", "lon")
write.csv(harborwq.df, "harbor_WQ_pts.csv", row.names=FALSE)
harbor_wq.shp <- df2spdf(3,2,"Long","Lat",harbor_wq.df)
writeOGR(harbor_wq.shp, dsn=".", layer="harbor_water_quality", 
         overwrite_layer = TRUE,driver = "ESRI Shapefile")

# water trail association
# collect location info
loc.11 <- getloc("WTA", "2011 NYCWTA Citizens' Water Quality Testing Pilot Program", 2011, "Untitled layer")
loc.12 <- getloc("WTA", "NYCWTA-TRP 2012 Citizens' Water Quality Testing Sites", 2012, "Untitled layer")
loc.13 <- getloc("WTA", "2013 Citizens' Water Quality Testing program--sampling sites and results", 2013, "Untitled layer")
loc.15 <- getloc("WTA", "2015 Citizens Water Quality Testing Program", 2015, "Untitled layer")
loc.16 <- getloc("WTA", "2016 Citizens Water Quality Testing Program", 2016, "Untitled layer")
loc.all <- rbind(loc.11, loc.12, loc.13, loc.15, loc.16)
write.csv(loc.all, "NYCWTA_sites_all.csv")
# remove duplicated points
loc.pts <- loc.all[!duplicated(loc.all[,1:3]),]
write.csv(loc.pts, "NYCWTA_sites.csv")
# read yearly data
# 2011
file <- "EnteroPilotData2011"
dt.11 <- read.xls(paste0(infolder, "WQ/WTA/", file, ".xlsx"), sheet=1, skip=10, blank.lines.skip=TRUE, header = FALSE, stringsAsFactors=FALSE)
head(dt.11); # check data
dt.11.1 <- dt.11[1:2,] # get dates and high tide time as first half
dt.11.1 <- dt.11.1[,-seq(3,13, by=2)] # remove empty columns
M <- t(dt.11.1);rownames(M) <- NULL;colnames(M) <- NULL # transpose matrix and remove row and colnames
DF <- as.data.frame(M);colnames(DF) <- as.character(unlist(DF[1,]));DF = DF[-1, ] # make first row name as the column names
dt.11.2 <- dt.11[3:15,];colnames(dt.11.2) <- as.character(unlist(dt.11.2[1,]));dt.11.2 = dt.11.2[-1, ] # make first row name as the column names in the second half
names(dt.11.2) = gsub(pattern = ":", replacement = "", x = names(dt.11.2)) # remove ":" in the data frame
n <- length(dt.11.2$`Sample Site`) # number to repeat dates
DF <- DF[rep(seq_len(nrow(DF)), each=n),] # repeat dates
head(dt.11.2) # check the second half
days <- length(unique(DF$`Sample Date`)) # number of dates
# reorganize the table
dt.11.2.1 <- rep(dt.11.2[,1],times=days)
dt.11.2.2 <- rbind(dt.11.2[,2], dt.11.2[,4])
dt.11.2.3 <- rbind(dt.11.2[,3], dt.11.2[,5])
for (i in 1:(days-2)){
  dt.11.2.2 <- rbind(dt.11.2.2, dt.11.2[,4+i*2])
  dt.11.2.3 <- rbind(dt.11.2.3, dt.11.2[,5+i*2])
}

df.11 <- cbind(dt.11.2.1, DF, as.vector(t(dt.11.2.2)), as.vector(t(dt.11.2.3)))
rownames(df.11) <- NULL
colnames(df.11) <- c("loc","date","tide","time","MPN")
df.11$year <- as.numeric(format(as.Date(df.11$date), format = "%Y"))
df.11$month <- as.numeric(format(as.Date(df.11$date), format = "%m"))
df.11$day <- as.POSIXlt(as.Date(df.11$date))[["yday"]]+1
# 2012 - 2016
file <- "CWQT All Sites from 2012-2016"
dt.12 <- read.xls(paste0(infolder, "WQ/WTA/", file, ".xlsx"), 
                  sheet=1, skip=11, blank.lines.skip=TRUE, 
                  header = FALSE, stringsAsFactors=FALSE)
df.12 <- clean.wta(dt.12)
dt.13 <- read.xls(paste0(infolder, "WQ/WTA/", file, ".xlsx"), 
                  sheet=2, skip=13, blank.lines.skip=TRUE, 
                  header = FALSE, stringsAsFactors=FALSE)
df.13 <- clean.wta(dt.13)
dt.14 <- read.xls(paste0(infolder, "WQ/WTA/", file, ".xlsx"), 
                  sheet=3, skip=17, blank.lines.skip=TRUE, 
                  header = FALSE, stringsAsFactors=FALSE)
df.14 <- clean.wta(dt.14)
dt.15 <- read.xls(paste0(infolder, "WQ/WTA/", file, ".xlsx"), 
                  sheet=4, skip=12, blank.lines.skip=TRUE, 
                  header = FALSE, stringsAsFactors=FALSE)
df.15 <- clean.wta(dt.15)
dt.16 <- read.xls(paste0(infolder, "WQ/WTA/", file, ".xlsx"), 
                  sheet=5, skip=11, blank.lines.skip=TRUE, 
                  header = FALSE, stringsAsFactors=FALSE)
df.16 <- clean.wta(dt.16)
df.wta <- rbind(df.11,df.12,df.13,df.14,df.15,df.16)
df.wta.wq <- merge(df.wta, loc.pts, by="loc")
df.wta.wq <- df.wta.wq[,-11]; colnames(df.wta.wq)[which(colnames(df.wta.wq)=="year.x")] <- "year"
write.csv(df.wta, "NYCWTA_WQ.csv", row.names=FALSE)
write.csv(df.wta.wq, "NYCWTA_WQ_pts.csv", row.names=FALSE) # with coordinate information
spdf.wta <- df2spdf(9,10,"lon","lat",df.wta.wq)
writeOGR(spdf.wta, dsn=".", layer="NYCWTA_WQ", overwrite_layer = TRUE,driver = "ESRI Shapefile")

# CFE/Save the sound
# 2013
filename <- "Save the Sound Water Quality Data (c) 2013"
file <- paste0(infolder, "WQ/CF/", filename, ".xlsx")
n <- length(excel_sheets(file))
df.all <- data.frame(matrix(ncol = 12, nrow = 0))
colnames(df.all) <- c("SiteID", "SiteName", "Date", "FC", "Pre0", "Pre1", "Pre2", "Pre3", "CumPre", "MFC", "Pass", "Fail")
for (i in 1:n){
  df <- read.xls(file, sheet=i, skip=1, header = FALSE, stringsAsFactors=FALSE)
  df <- df[,-13]
  df[1,] <- colnames(df.all)
  colnames(df) <- as.character(unlist(df[1,]));df = df[-1, ]
  df.all <- rbind(df.all, df)
  print(i)
}
write.csv(df.all, "CF_WQ_2013.csv", row.names=FALSE)

# 2014-1
filename <- "Save the Sound Water Quality Data (c) 2014_1"
file <- paste0(infolder, "WQ/CF/", filename, ".xlsx")
n <- length(excel_sheets(file))
df.all <- data.frame(matrix(ncol = 14, nrow = 0))
colnames(df.all) <- c("SiteID", "SiteName", "Date", "Ent", "FC", 
                      "Pre0", "Pre1", "Pre2", "Pre3", "CumPre", 
                      "MEnt", "MFC", "Pass", "Fail")
for (i in 1:n){
  if (i ==1){
    df <- read.xls(file, sheet=i, skip=1, header = FALSE, stringsAsFactors=FALSE)
  }else{
    df <- read.xls(file, sheet=i, skip=0, header = TRUE, stringsAsFactors=FALSE)
  }
  colnames(df) <- colnames(df.all)
  df.all <- rbind(df.all, df)
  print(i)
}
write.csv(df.all, "CF_WQ_2014_1.csv", row.names=FALSE)
# 2014-2
filename <- "Save the Sound Water Quality Data (c) 2014_2"
file <- paste0(infolder, "WQ/CF/", filename, ".xlsx")
df.1 <- read.xls(file, sheet=1, skip=1, header = FALSE, stringsAsFactors=FALSE)
df.1[1,] <- c("SiteID", "SiteName", "Date", "Ent", "FC", 
              "Pre0", "Pre1", "Pre2", "Pre3", "CumPre", "MCFU", "Pass", "Fail")
colnames(df.1) <- as.character(unlist(df.1[1,]));df.1 = df.1[-1, ]
df.2 <- read.xls(file, sheet=2, skip=0, header = FALSE, stringsAsFactors=FALSE)
colnames(df.2) <- c("SiteID", "SiteName", "Date", "Ent", "FC", 
                    "Pre0", "Pre1", "Pre2", "Pre3", "CumPre", "MCFU", "Pass", "Fail")
df <- rbind(df.1, df.2)
write.csv(df, "CF_WQ_2014_2.csv", row.names=FALSE)

# 2015, 2016, 2016 -2
year <- "2015"  # change manually
filename <- paste0("Save the Sound Water Quality Data (c) ", year)
file <- paste0(infolder, "WQ/CF/", filename, ".xlsx")
n <- length(excel_sheets(file))
df.all <- data.frame(matrix(ncol = 12, nrow = 0)) # skip if year = "2016 -2"
colnames <- c("SiteID", "SiteName", "Date", "Ent", "MEnt",
              "Pre0", "Pre1", "Pre2", "Pre3", "CumPre")
colnames(df.all) <- colnames
for (i in 1:n){
  df <- read.xls(file, sheet=i, skip=0, header = TRUE, stringsAsFactors=FALSE)
  colnames(df) <- colnames(df.all)
  df.all <- rbind(df.all, df)
  print(i)
}
write.csv(df.all, "CF_WQ_2015.csv", row.names=FALSE) # change manually
 
# 2017
df.17 <- read.csv(paste0(infolder, "WQ/CF/2017 STS Entero Data EXCEL Release.csv"))
df.17 <- df.17[, -which(names(df.17) %in% c("State","X"))]
colnames(df.17) <- colnames
write.csv(df.17, "CF_WQ_2017.csv", row.names=FALSE)

# combine years 2014 - 2017
cf.14.1 <- read.csv("CF_WQ_2014_1.csv", stringsAsFactors=FALSE)
cf.14.1 <- cf.14.1[,1:11];cf.14.1 <- cf.14.1[,-5]
cf.14.1 <- cf.14.1[colnames]
cf.14.2 <- read.csv("CF_WQ_2014_2.csv", stringsAsFactors=FALSE)
cf.14.2 <- cf.14.2[,1:11];cf.14.2 <- cf.14.2[,-5]
colnames(cf.14.2)[which(colnames(cf.14.2)=="MCFU")] <- "MEnt"
cf.14.2 <- cf.14.2[colnames]
cf.15 <- read.csv("CF_WQ_2015.csv", stringsAsFactors=FALSE)
cf.16 <- read.csv("CF_WQ_2016.csv", stringsAsFactors=FALSE)
cf.17 <- read.csv("CF_WQ_2017.csv", stringsAsFactors=FALSE)
cf <- rbind(cf.15, cf.16, cf.17)
cf <- cf[,-3]; cf <- cf[,-4]
cf <- rbind(cf.14.1, cf.14.2, cf)
write.csv(cf, "CF_WQ.csv", row.names=FALSE)

# get locations for CF dataset
site.13 <- getloc("CF","Summer 2013 Sampling Sites",2013,"Sites")
write.csv(site.13, "CF_WQ_Sites_2013.csv", row.names=FALSE)
site.14 <- getloc("CF","Summer 2014 Sampling Sites",2014,"Sites")
write.csv(site.14, "CF_WQ_Sites_2014.csv", row.names=FALSE)
site.15 <- getloc("CF","Season Summary 2015",2015,"STS 2015 Dynamic Map.xlsx")
write.csv(site.15, "CF_WQ_Sites_2015.csv", row.names=FALSE)
site.16 <- getloc("CF","Season Summary 2016",2016,"STS 2016 Google Map.xlsx")
write.csv(site.16, "CF_WQ_Sites_2016.csv", row.names=FALSE)
site.17 <- getloc("CF","Season Summary 2017",2017,"STS 2017 Google Map.xlsx")
write.csv(site.17, "CF_WQ_Sites_2017.csv", row.names=FALSE)

# reorganize sites in 2015
site.15.1 <- merge(cf.15, site.15, by="SiteName")
site.15.2 <- site.15.1[,c("SiteID","lon","lat","year")]
site.16.1 <- site.16[,c("SiteID","lon","lat","year")]
site.17.1 <- site.17[,c("SiteID","lon","lat","year")]
cf.sites <- rbind(site.13, site.14, site.15.2, site.16.1, site.17.1)
# remove duplicated points
cf.sites <- cf.sites[!duplicated(cf.sites[,1:3]),]
write.csv(cf.sites, "CF_WQ_Sites_All.csv", row.names=FALSE)

# match up coordinates
cf.pts <- merge(cf, cf.sites, by="SiteID")
cf.pts$month <- as.numeric(format(as.Date(as.character(cf.pts$Date),"%m/%d/%Y"), format = "%m"))
cf.pts$day <- as.POSIXlt(as.Date(as.character(cf.pts$Date),"%m/%d/%Y"))[["yday"]]+1
write.csv(cf.pts, "CF_WQ_pts.csv", row.names=FALSE) # with coordinates
spdf.cf <- df2spdf(11,12,"lon","lat",cf.pts)
writeOGR(spdf.cf, dsn=".", layer="CF_WQ", overwrite_layer = TRUE,driver = "ESRI Shapefile")

# WQ Portal
wqp.sites <- read.csv(paste0(infolder, "WQ/Portal/station.csv"), stringsAsFactors=FALSE)
wqp.res <- read.csv(paste0(infolder, "WQ/Portal/result.csv"), stringsAsFactors=FALSE)
# subset data
wqp.df <- wqp.res %>%
  select("MonitoringLocationIdentifier", "ActivityStartDate", "ActivityStartTime.Time", 
         "CharacteristicName", "ResultMeasureValue", "ResultMeasure.MeasureUnitCode") %>%
  subset(CharacteristicName %in% c("Dissolved oxygen (DO)",
                                   "Enterococcus",
                                   "Fecal Coliform",
                                   "Temperature, water")) %>%
  mutate(
    year = as.numeric(format(as.Date(ActivityStartDate), format = "%Y")),
    month = as.numeric(format(as.Date(ActivityStartDate), format = "%m")),
    day = as.POSIXlt(as.Date(ActivityStartDate))[["yday"]]+1,
    ID = c(1:length(ActivityStartDate)))
  wqp.df.1 <- wqp.df %>%
    select("ID", "CharacteristicName", "ResultMeasureValue") %>% dcast(ID~CharacteristicName,fill=0) %>%
    merge(wqp.df[,-which(names(wqp.df) %in% c("CharacteristicName", "ResultMeasureValue"))],by="ID") %>%
    merge(wqp.sites[,which(names(wqp.sites) %in% c("MonitoringLocationIdentifier",
                                                   "LatitudeMeasure",
                                                   "LongitudeMeasure"))], 
          by="MonitoringLocationIdentifier") %>%
    select(-one_of(c("ID")))
  colnames(wqp.df.1)[1:8] <- c("Id","DO","Ent","FC","T","Date","Time","Unit")
  colnames(wqp.df.1)[12:13] <- c("lat","lon")
  head(wqp.df.1) # final check 
  write.csv(wqp.df.1, "Portal_WQ_pts.csv", row.names=FALSE)
  spdf.wqp <- df2spdf(13,12,"lon","lat",wqp.df.1)
  writeOGR(spdf.wqp, dsn=".", layer="Portal_WQ", overwrite_layer = TRUE,driver = "ESRI Shapefile")
  
##################################### Point Pattern Analysis #############################################
# get an uniform data frame for water quality dta from different sources
# (harbor water, water trail association, CFE/Save the sound, WQ Portal)
colnames.com <- c("site", "lon", "lat", "ent", "year", "month", "day")
harbor_wq <- read.csv('harbor_WQ_pts.csv', stringsAsFactors = FALSE)
harbor_wq <- harbor_wq[c("site", "lon", "lat", "Ent_top", "year", "month", "day")]
colnames(harbor_wq) <- colnames.com
NYCWTA_wq <- read.csv('NYCWTA_WQ_pts.csv', stringsAsFactors = FALSE)
NYCWTA_wq <- NYCWTA_wq[c("loc", "lon", "lat", "MPN", "year", "month", "day")]
colnames(NYCWTA_wq) <- colnames.com
cf_wq <- read.csv('CF_WQ_pts.csv', stringsAsFactors = FALSE) 
cf_wq <- cf_wq[c("SiteName", "lon", "lat", "Ent", "year", "month", "day")]
colnames(cf_wq) <- colnames.com
Portal_wq <- read.csv('Portal_WQ_pts.csv', stringsAsFactors = FALSE)
Portal_wq <- Portal_wq[c("Id", "lon", "lat", "Ent", "year", "month", "day")]
colnames(Portal_wq) <- colnames.com
all_wq_pts <- rbind(harbor_wq, NYCWTA_wq, cf_wq, Portal_wq)
write.csv(all_wq_pts, "wq_pts_all.csv", row.names=FALSE)
all_wq_pts <- read.csv("wq_pts_all.csv", stringsAsFactors = FALSE)
wq_pts <- all_wq_pts[!duplicated(all_wq_pts[,2:3]),]

# how many sampling sites?
habor <- readOGR(".", "harbor_water_quality", stringsAsFactors = FALSE)
# check duplicates
coordinates(wq_pts) =~lon+lat
proj4string(wq_pts) <- lonlat
wq_pts_proj <- spTransform(wq_pts, nyc.crs)
zero=zerodist(wq_pts)
length(unique(zero[,1]))
##Create a window(boundary) object to define the study site.
tract <- readOGR(dsn = paste0(infolder, "BD"), layer ="cb_2015_36_tract_500k_clip_proj", stringsAsFactors = FALSE)
##as.owin: Convert Data To Class owin
nyadwi <- readOGR(dsn = paste0(infolder, "BD"), layer ="nyadwi_dis", stringsAsFactors = FALSE)
plot(nyadwi)
plot(tract, bord="red", add=T)
inside <- !is.na(over(wq_pts_proj, as(nyadwi, "SpatialPolygons")))
wq_pts_in <- wq_pts_proj[inside, ]
writeOGR(wq_pts_in, dsn=".", layer="wq_pts_in", overwrite_layer = TRUE,driver = "ESRI Shapefile")
plot(wq_pts_in, col="blue", add=T)
wq.df <- as.data.frame(wq_pts_in)
window = as.owin(nyadwi)
nyc.ppp = ppp(x=wq.df$lon,y=wq.df$lat,window=window)
nyc.ppp <- as.ppp(nyc.ppp)
density <- nyc.ppp$n/sum(sapply(slot(tract,"polygons"),slot,"area")) # 3.299963e-08

##Define number of quadrats that you want to use in your analysis.
##Here, quads is the number that you square to get your total number of quadrats.
##For example, if quads = 4, then the total number of quadrats = 16.

quads = 20

##Use the function "quadratcount" to calculate the number of points per quadrat.
##quadratcount:Quadrat counting for a point pattern
qcount = quadratcount(nyc.ppp, nx = quads, ny = quads)

##Create a map showing the quadrats overlayed on the window file with the 
##number of points displayed per quadrat.
jpeg("QuadratCounting_wq_20.jpeg",2500,2000,res=300)
plot(nyc.ppp,pch="+",cex=0.5,main="WQ sampling sites in NYC")
plot(qcount,add=T,col="red")
dev.off()

##Now, prepare the data to be able to calculate the variables for a quadrat analysis.
##First, define the quadrat count dataset as a data frame.
qcount.df = as.data.frame((qcount))
qcount.df
# total points
sum(qcount.df$Freq) # 277
# mean 
sum(qcount.df$Freq)/16 # 17.3125

##Second, count the number of quadrats with a distinct number of points.
qcount.ndf = as.data.frame(table(qcount.df$Freq))
colnames(qcount.df) = c("x","f")

##Third, create new columns for total number of points and for fx^2.
qcount.df = cbind(qcount.df, TotPoints = as.numeric(qcount.df$x) * as.numeric(qcount.df$f))
qcount.df = cbind(qcount.df, fx2 = (as.numeric(qcount.df$x)^2)*as.numeric(qcount.df$f))

##Fourth, calculate the sum of each column, which you will use as inputs into the 
##formula for VMR.
f.sum = sum(qcount.df$f)
f.sum # 277
TotPoints.sum = sum(qcount.df$TotPoints)
TotPoints.sum # 28803
fx2.sum = sum(qcount.df$fx2)
fx2.sum # 4110179
##Fifth, calculate VAR, MEAN, and VMR.
m <- quads^2
VAR = (fx2.sum-TotPoints.sum^2/m)/(m-1)
VAR # 5103.125
MEAN = TotPoints.sum/m
MEAN # 72.0075
VMR = VAR/MEAN
VMR # 70.86936
##Finally, perform the test statistic to test for the existence of a random spatial pattern.

# chi-square 
chi = VMR*(m-1)
chi
1-pchisq(chi,m-1)

nearestNeighbor = nndist(nyc.ppp)
png("hist.png", width=5, height=6, units="in", res=300)
hist(nearestNeighbor,xlim=c(0,10000),breaks=400,main = "Frequency of NND", xlab = "Nearest neighbour distance (NND)", ylab = "Frequency", col = "lightgreen", cex.lab=1.2)
box()
dev.off()

##Convert the nearestNeighbor object into a dataframe.
nearestNeighbor=as.data.frame(as.numeric(nearestNeighbor))
head(nearestNeighbor)
##Change the column name to "Distance"
colnames(nearestNeighbor) = "Distance"

##Calculate the nearest neighbor statistic to test for a random spatial distribution. 
# density = total points/area
# basic mean distance
NND_bar <- mean(nearestNeighbor$Distance)
# perfectly random, distance
density <- intensity.ppp(nyc.ppp)
density # 2.384448e-08
NND_R <- 1/(2*sqrt(density)) 
NND_R # 3237.994
NND_D <- 1.07453/sqrt(density)
NND_D # 6958.644
R <- NND_bar/NND_R
R # 0.4874665
sigma_NND <- 0.26136/sqrt(TotPoints.sum*density)
sigma_NND # 9.973012
Z <- (NND_bar-NND_R)/sigma_NND
Z # -166.4072

city <- readOGR(dsn = paste0(infolder, "BD"), layer ="nyad_dis", stringsAsFactors = FALSE)
plot(city, col='light blue')
plot(wq_pts_in, col='red', pch='+', add=T)
xy <- coordinates(wq_pts_in)
dim(xy)
xy <- unique(xy)
dim(xy)
head(xy)
# mean center
mc <- apply(xy, 2, mean)
# standard distance
sd <- sqrt(sum((xy[,1] - mc[1])^2 + (xy[,2] - mc[2])^2) / nrow(xy))
points(cbind(mc[1], mc[2]), pch='*', col='green', cex=5)
# make a circle
bearing <- 1:360 * pi/180
cx <- mc[1] + sd * cos(bearing)
cy <- mc[2] + sd * sin(bearing)
circle <- cbind(cx, cy)
lines(circle, col='green', lwd=2)
cityArea <- area(city)
dens <- nrow(xy) / cityArea # 4.829621e-07

r <- raster(city)
res(r) <- 1000

r <- rasterize(city, r)
plot(r)
quads <- as(r, 'SpatialPolygons')
plot(quads, add=TRUE)
plot(wq_pts_in, col='red',pch='+', add=T)

nwq <- rasterize(coordinates(wq_pts_in), r, fun='count', background=0)
plot(nwq)
plot(city, add=TRUE)

ncwq <- mask(nwq, r)
plot(ncwq)
plot(city, add=TRUE)

f <- freq(ncwq, useNA='no')
head(f)
plot(f, pch=20)

# number of quadrats
quadrats <- sum(f[,2])
# number of cases
cases <- sum(f[,1] * f[,2])
mu <- cases / quadrats
mu # 0.01069646

ff <- data.frame(f)
colnames(ff) <- c('K', 'X')
ff$Kmu <- ff$K - mu
ff$Kmu2 <- ff$Kmu^2
ff$XKmu2 <- ff$Kmu2 * ff$X
head(ff)

s2 <- sum(ff$XKmu2) / (sum(ff$X)-1)
s2 # 0.01486239

VMR <- s2 / mu
VMR # 1.389469

d <- dist(xy)
class(d)

dm <- as.matrix(d)
dm[1:5, 1:5]

diag(dm) <- NA
dm[1:5, 1:5]

dmin <- apply(dm, 1, min, na.rm=TRUE)
head(dmin)

mdmin <- mean(dmin)

wdmin <- apply(dm, 1, which.min)

plot(city)
plot(wq_pts_in, col='red', pch=16, add=T)
ord <- rev(order(dmin))

far25 <- ord[1:25]
neighbors <- wdmin[far25]

points(xy[far25, ], col='blue', pch=20)
points(xy[neighbors, ], col='red')

# drawing the lines, easiest via a loop
for (i in far25) {
  lines(rbind(xy[i, ], xy[wdmin[i], ]), col='red')
}

max(dmin)
## [1] 12261.89
# get the unique distances (for the x-axis)
distance <- sort(unique(round(dmin)))
# compute how many cases there with distances smaller that each x
Gd <- sapply(distance, function(x) sum(dmin < x))
# normalize to get values between 0 and 1
Gd <- Gd / length(dmin)
plot(distance, Gd)

stepplot <- function(x, y, type='l', add=FALSE, ...) {
  x <- as.vector(t(cbind(x, c(x[-1], x[length(x)]))))
  y <- as.vector(t(cbind(y, y)))
  if (add) {
    lines(x,y, ...)
  } else {
    plot(x,y, type=type, ...)
  }
}

stepplot(distance, Gd, type='l', lwd=2)

# get the centers of the 'quadrats' (raster cells)
p <- rasterToPoints(r)
# compute distance from all sampling sites to these cell centers
d2 <- pointDistance(p[,1:2], xy, longlat=FALSE)

# the remainder is similar to the G function
Fdistance <- sort(unique(round(d2)))
mind <- apply(d2, 1, min)
Fd <- sapply(Fdistance, function(x) sum(mind < x)) # this takes a while
Fd <- Fd / length(mind)
plot(Fdistance, Fd, type='l', lwd=2)


ef <- function(d, lambda) {
  E <- 1 - exp(-1 * lambda * pi * d^2)
}
expected <- ef(0:12000, dens)

plot(distance, Gd, type='l', lwd=2, col='red', las=1,
     ylab='F(d) or G(d)', xlab='Distance', yaxs="i", xaxs="i")
lines(Fdistance, Fd, lwd=2, col='blue')
lines(0:12000, expected, lwd=2)
legend(8000, .3,
       c(expression(italic("G")["d"]), expression(italic("F")["d"]), 'expected'),
       lty=1, col=c('red', 'blue', 'black'), lwd=2, bty="n")

distance <- seq(1, 30000, 100)
Kd <- sapply(distance, function(x) sum(d < x)) # takes a while
Kd <- Kd / (length(Kd) * dens)
plot(distance, Kd, type='l', lwd=2)

library(maptools)
cityOwin <- as.owin(city)
class(cityOwin)
cityOwin

pts <- coordinates(wq_pts_in)
head(pts)

p <- ppp(pts[,1], pts[,2], window=cityOwin)
class(p)
p
plot(p)
ds <- density(p)
class(ds)
plot(ds, main='wq sampling density')

nrow(pts) #277
r <- raster(ds)
s <- sum(values(r), na.rm=TRUE)
s * prod(res(r)) # 49.74827

str(ds)
sum(ds$v, na.rm=TRUE) * ds$xstep * ds$ystep
p$n #55

win <- aggregate(tract)
plot(tract)
points(p, col='red', pch=20)
plot(win, add=TRUE, border='blue', lwd=3)
