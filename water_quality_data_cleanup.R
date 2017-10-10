# libraries
library(dplyr)
library(tidyr)
library(rgdal)
library(maptools)
library(gdata)
library(readxl)
library(reshape2)

# global settings
lonlat <- CRS("+proj=longlat +datum=NAD83")
nyc.crs <- CRS("+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333
           +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83
           +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")
setwd("/nfs/urbangi-data/spatial_data/output")
infolder <- "/nfs/urbangi-data/spatial_data/"

# functions
df2spdf <- function(col1, col2, colname1, colname2, df){
  xy <- data.frame(df[,c(col1,col2)])
  coordinates(xy) <- c(colname1, colname2)
  proj4string(xy) <- lonlat
  spdf <- SpatialPointsDataFrame(coords = xy, data = df, proj4string = lonlat)
  return(spdf)
}

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
    }else if(year == 2016){
      pts.df <- as.data.frame(pts.spdf[,c("Name", "Site_Name")]); pts.df <- pts.df[,-5]
      colnames(pts.df) <- c("SiteID", "SiteName", "lon", "lat")
      pts.df$year <- rep(year, length(pts.df$SiteID))
    }
  }
  return(pts.df)
}

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
  df.2 <- df.2[,-which(colnames(df.2) %in% c("Duplicate sample MPN","Duplicate sample","Duplicate Sample","Duplicate sample/notes","Duplicate sample/Notes","Duplicate Sample/Notes","Duplicate Sample/notes","Notes"))]
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

# harbor water
# collect common information over years
wq.df <- data.frame(site=character(),date=character(),DO_top=numeric(),DO_bot=numeric(),FC_top=numeric(),FC_bot=numeric(),Ent_top=numeric(),Ent_bot=numeric(),Tra=numeric())
for (year in 2008:2016){
  if (year == 2008){
    wq.tb <- read.xls(paste0(infolder, "WQ/DEP/dep_hs", year,".xls"), sheet = 1, skip=9, blank.lines.skip=TRUE, header = TRUE, stringsAsFactors=FALSE)
    wq.tb <- wq.tb[c("X...","X","Top.30","Bot.28","Top.15","Bot.14","Top.16","Bot.15","X.8")]
  }else if (year == 2009){
    wq.tb <- read.xls(paste0(infolder, "WQ/DEP/dep_hs", year,".xls"), sheet = 1, skip=7, blank.lines.skip=TRUE, header = TRUE, stringsAsFactors=FALSE)
    wq.tb <- wq.tb[c("X...","X","Top.30","Bot.28","Top.15","Bot.14","Top.16","Bot.15","X.8")]
  }else if (year == 2010 | year == 2011){
    wq.tb <- read.xls(paste0(infolder, "WQ/DEP/dep_hs", year,".xls"), sheet = 1, skip=8, blank.lines.skip=TRUE, header = TRUE, stringsAsFactors=FALSE)
    wq.tb <- wq.tb[c("X...","X","Top.30","Bot.28","Top.15","Bot.14","Top.16","Bot.15","X.8")]
  }else{
    wq.tb <- read.xls(paste0(infolder, "WQ/DEP/harbor_sampling_ytd_", year,".xls"), sheet = 1, skip=5, blank.lines.skip=TRUE, header = TRUE, stringsAsFactors=FALSE)
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
write.csv(wq.df, "harbor_water_quality.csv")
harbor_wq <- read.csv('harbor_water_quality.csv', stringsAsFactors = FALSE)
harbor_wq <- harbor_wq[!is.na(harbor_wq$year),]
harbor_wq.td <- gather(harbor_wq, Key, Value, -site, -date, -year, -month, -day)
harbor_wq.td <- harbor_wq.td[harbor_wq.td$Key!="X",]
harbor_wq.td$year <- as.character(harbor_wq.td$year)
coords <- read.csv(paste0(infolder, "WQ/DEP/harbor_sampling_coordinates.csv"), stringsAsFactors = FALSE)
coords <- coords[,-1]
colnames(coords)[1] <- "site"
harbor_wq.df <- merge(coords, harbor_wq.td, by="site")
harbor_wq.shp <- df2spdf(3,2,"Long","Lat",harbor_wq.df)
writeOGR(harbor_wq.shp, dsn=".", layer="harbor_water_quality", overwrite_layer = TRUE,driver = "ESRI Shapefile")

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
dt.12 <- read.xls(paste0(infolder, "WQ/WTA/", file, ".xlsx"), sheet=1, skip=11, blank.lines.skip=TRUE, header = FALSE, stringsAsFactors=FALSE)
df.12 <- clean.wta(dt.12)
dt.13 <- read.xls(paste0(infolder, "WQ/WTA/", file, ".xlsx"), sheet=2, skip=13, blank.lines.skip=TRUE, header = FALSE, stringsAsFactors=FALSE)
df.13 <- clean.wta(dt.13)
dt.14 <- read.xls(paste0(infolder, "WQ/WTA/", file, ".xlsx"), sheet=3, skip=17, blank.lines.skip=TRUE, header = FALSE, stringsAsFactors=FALSE)
df.14 <- clean.wta(dt.14)
dt.15 <- read.xls(paste0(infolder, "WQ/WTA/", file, ".xlsx"), sheet=4, skip=12, blank.lines.skip=TRUE, header = FALSE, stringsAsFactors=FALSE)
df.15 <- clean.wta(dt.15)
dt.16 <- read.xls(paste0(infolder, "WQ/WTA/", file, ".xlsx"), sheet=5, skip=11, blank.lines.skip=TRUE, header = FALSE, stringsAsFactors=FALSE)
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
colnames(df.all) <- c("SiteID", "SiteName", "Date", "Ent", "FC", "Pre0", "Pre1", "Pre2", "Pre3", "CumPre", "MEnt", "MFC", "Pass", "Fail")
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
df.1[1,] <- c("SiteID", "SiteName", "Date", "Ent", "FC", "Pre0", "Pre1", "Pre2", "Pre3", "CumPre", "MCFU", "Pass", "Fail")
colnames(df.1) <- as.character(unlist(df.1[1,]));df.1 = df.1[-1, ]
df.2 <- read.xls(file, sheet=2, skip=0, header = FALSE, stringsAsFactors=FALSE)
colnames(df.2) <- c("SiteID", "SiteName", "Date", "Ent", "FC", "Pre0", "Pre1", "Pre2", "Pre3", "CumPre", "MCFU", "Pass", "Fail")
df <- rbind(df.1, df.2)
write.csv(df, "CF_WQ_2014_2.csv", row.names=FALSE)

# 2015, 2016, 2016 -2
year <- "2015"  # change manually
filename <- paste0("Save the Sound Water Quality Data (c) ", year)
file <- paste0(infolder, "WQ/CF/", filename, ".xlsx")
n <- length(excel_sheets(file))
df.all <- data.frame(matrix(ncol = 12, nrow = 0)) # skip if year = "2016 -2"
colnames(df.all) <- c("SiteID", "SiteName", "Town", "Date", "Time", "Ent", "MEnt","Pre0", "Pre1", "Pre2", "Pre3", "CumPre")
for (i in 1:n){
  df <- read.xls(file, sheet=i, skip=0, header = TRUE, stringsAsFactors=FALSE)
  colnames(df) <- colnames(df.all)
  df.all <- rbind(df.all, df)
  print(i)
}
write.csv(df.all, "CF_WQ_2015.csv", row.names=FALSE) # change manually

# combine years 2014 - 2016
cf.14.1 <- read.csv("CF_WQ_2014_1.csv")
colnames <- c("SiteID", "SiteName", "Date", "Ent", "MEnt","Pre0", "Pre1", "Pre2", "Pre3", "CumPre")
cf.14.1 <- cf.14.1[,1:11];cf.14.1 <- cf.14.1[,-5]
cf.14.1 <- cf.14.1[colnames]
cf.14.2 <- read.csv("CF_WQ_2014_2.csv")
cf.14.2 <- cf.14.2[,1:11];cf.14.2 <- cf.14.2[,-5]
colnames(cf.14.2)[which(colnames(cf.14.2)=="MCFU")] <- "MEnt"
cf.14.2 <- cf.14.2[colnames]
cf.15 <- read.csv("CF_WQ_2015.csv")
cf.16 <- read.csv("CF_WQ_2016.csv")
df.all <- data.frame(matrix(ncol = 12, nrow = 0))
cf <- rbind(cf.15, cf.16)
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
# reorganize sites in 2015
site.15.1 <- merge(cf.15, site.15, by="SiteName")
site.15.2 <- site.15.1[,c("SiteID","lon","lat","year")]
site.16.1 <- site.16[,c("SiteID","lon","lat","year")]
cf.sites <- rbind(site.13, site.14, site.15.2, site.16.1)
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
  subset(CharacteristicName %in% c("Dissolved oxygen (DO)","Enterococcus","Fecal Coliform","Temperature, water")) %>%
  mutate(
    year = as.numeric(format(as.Date(ActivityStartDate), format = "%Y")),
    month = as.numeric(format(as.Date(ActivityStartDate), format = "%m")),
    day = as.POSIXlt(as.Date(ActivityStartDate))[["yday"]]+1,
    ID = c(1:length(ActivityStartDate)))
  wqp.df.1 <- wqp.df %>%
    select("ID", "CharacteristicName", "ResultMeasureValue") %>% dcast(ID~CharacteristicName,fill=0) %>%
    merge(wqp.df[,-which(names(wqp.df) %in% c("CharacteristicName", "ResultMeasureValue"))],by="ID") %>%
    merge(wqp.sites[,which(names(wqp.sites) %in% c("MonitoringLocationIdentifier","LatitudeMeasure","LongitudeMeasure"))], by="MonitoringLocationIdentifier") %>%
    select(-one_of(c("ID")))
  colnames(wqp.df.1)[1:8] <- c("Id","DO","Ent","FC","T","Date","Time","Unit")
  colnames(wqp.df.1)[12:13] <- c("lat","lon")
  head(wqp.df.1) # final check 
  write.csv(wqp.df.1, "Portal_WQ_pts.csv", row.names=FALSE)
  spdf.wqp <- df2spdf(13,12,"lon","lat",wqp.df.1)
  writeOGR(spdf.wqp, dsn=".", layer="Portal_WQ", overwrite_layer = TRUE,driver = "ESRI Shapefile") 

  
