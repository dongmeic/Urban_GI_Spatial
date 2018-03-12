# By Dongmei Chen
# This script is designed to explore the correlation and regression between GI and WQ

# libraries
library(rgdal)
library(swfscMisc)

# global settings
setwd("/nfs/urbangi-data/spatial_data/output")
infolder <- "/nfs/urbangi-data/spatial_data/"

bound <- readOGR(dsn = paste0(infolder, "BD"), layer = "nyad_dis")
bound <- spTransform(bound, crs)
nyadwi <- readOGR(dsn = paste0(infolder, "BD"), layer ="nyadwi_dis")
lonlat <- CRS("+proj=longlat +datum=NAD83")
crs <- CRS("+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333
           +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83
           +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")
gi_pts <- readOGR(dsn="./shapefile", layer="GIsites_all", stringsAsFactors=FALSE)
gi_pts <- spTransform(gi_pts, crs)
wbdhu12 <- readOGR(dsn = paste0(infolder, "WBDHU"), layer = "wbdhu_12", stringsAsFactors = FALSE)
wbdhu12 <- spTransform(wbdhu12, crs)
wbdhu12$nid <- paste0("hu", seq(1, length(wbdhu12$TNMID)))
wq_pts <- readOGR(dsn = "./shapefile", layer = "dep_wq_sampling_sites", stringsAsFactors = FALSE)
wq_pts <- spTransform(wq_pts, crs)
dep_hwq_pts <- readOGR(dsn = "./shapefile", layer = "harbor_water_quality", stringsAsFactors = FALSE)
dep_hwq_pts <- spTransform(dep_hwq_pts, crs)
csoloc <- readOGR(dsn=paste0(infolder, "CSO"), layer ="dec_cso_2016", stringsAsFactors=FALSE) 
csoloc <- spTransform(csoloc, crs)
sewershed <- readOGR(dsn = paste0(infolder, "watershed/Sewershed"), layer = "Sewershed", stringsAsFactors=FALSE)

# measure the distance between water quality sampling points with cso outfalls

earth.dist<-function(lat1,long1,lat2,long2){
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlat <- b1-a1
  dlon<- b2-a2
  a <- (sin(dlat/2))^2 +cos(a1)*cos(b1)*(sin(dlon/2))^2
  c <- 2*atan2(sqrt(a),sqrt(1-a))
  R <- 6378.145
  dist <- R *c
  return(dist)
}

# find the closest cso outfall
find.nearest.point <- function(lat, long){
  v <- vector()
  n <- nrow(csoloc)
  for (i in 1:n){
    v[i] <- earth.dist(lat, long, csoloc$latitude[i], csoloc$longtitude[i])
  }
  x <- sample(which(v == min(v)), 1)
  return(csoloc$outfall_id[x])
}

n <- nrow(wq_pts)
for (i in 1:n){
  #if (i %% 10 == 0)  { cat(100 * i / n, '% complete\r', sep='') }
  wq_pts$outfall[i] <- find.nearest.point(wq_pts$Lat[i], wq_pts$Long[i])
  cat("\nProcessing water quality sampling point ", i, 
      "\nFound outfall ", wq_pts$outfall[i], "\n")
}

spdf <- wq_pts[,c("Station", "outfall")]
names(spdf)[1] <- "site"
dep_hwq_pts@data <- merge(dep_hwq_pts@data, spdf@data, by="site")
# find the right sewershed for the water quality sampling points
csoloc$sewershed <- over(csoloc, sewershed)$Sewershed
spdf <- csoloc[,c("outfall_id", "sewershed")]
names(spdf)[1] <- "outfall"
dep_hwq_pts@data <- merge(dep_hwq_pts@data, spdf@data, by="outfall")

# aggregate SGI density with sewershed
df_gi <- aggregate(Asset_ID~sewershed, data=gi_pts, function(x) length(x))
colnames(df_gi)[2] <- "SGI_density"
reg.test <- function(var, fun, unit){
  df <- dep_hwq_pts@data %>% filter(pre2>0) %>% filter(Key==var)
  names(df)[which(names(df)==unit)] <- "unit"
  df <- aggregate(Value~unit, data=df, fun, na.rm=TRUE)
  names(df)[which(names(df)=="unit")] <- unit
  df <- merge(df_gi, df, by=unit)
  print(summary(lm(df$Value~df$SGI_density)))
  plot(df$SGI_density, df$Value)
}
reg.test("DO_top", median, "sewershed")

# # merge SGI density with water quality sampling points
# dep_hwq_pts@data <- merge(dep_hwq_pts@data, df_gi, by="sewershed")
# reg.wq <- function(var,fun){
#   df <- dep_hwq_pts@data %>% filter(pre2>0) %>% filter(Key==var)
#   df1 <- aggregate(Value~site, data=df, fun, na.rm=TRUE)
#   df2 <- aggregate(SGI_density~site, data=df, mean, na.rm=TRUE)
#   df3 <- merge(df1, df2, by="site")
#   print(summary(lm(df3$Value~df3$SGI_density)))
#   plot(df3$SGI_density, df3$Value)
# }

reg.wq("DO_top", median)

csoloc <- merge(csoloc, df_gi, by="sewershed")
reg.cso <- function(var,fun){
  df <- csoloc@data
  names(df)[which(names(df)==var)] <- "Value"
  df1 <- aggregate(Value~sewershed, data=df, fun, na.rm=TRUE)
  df2 <- aggregate(SGI_density~sewershed, data=df, mean, na.rm=TRUE)
  df3 <- merge(df1, df2, by="sewershed")
  print(summary(lm(df3$Value~df3$SGI_density)))
  plot(df3$SGI_density, df3$Value)
}

reg.cso("events_13", median)

# by HU12
df_gi <- aggregate(Asset_ID~hu12, data=gi_pts, function(x) length(x))
colnames(df_gi)[2] <- "SGI_density"
dep_hwq_pts$hu12 <- over(dep_hwq_pts, wbdhu12)$nid
reg.test("Tra", median, "hu12")

# aggregate SGI density within certain radius from a water quality sampling point
n <- nrow(wq_pts)
for (i in 1:n){
  circle.loc <- as.data.frame(circle.polygon(wq_pts$Long[i], wq_pts$Lat[i], radius=5, units = "km"))
  ch <- chull(circle.loc)
  coords <- circle.loc[c(ch, ch[1]),]
  sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID=1)))
  proj4string(sp_poly) <- lonlat
  sp_poly <- spTransform(sp_poly, crs)
  gi_in <- gi_pts[!is.na(over(gi_pts, as(sp_poly, "SpatialPolygons"))),]
  wq_pts$sgi[i] <- length(gi_in$Asset_ID)
  if (wq_pts$sgi[i]>1){
    df <- as.data.frame(table(gi_in$GItypes))
    freq.max <- max(df$Freq)
    wq_pts$most[i] <- as.character(df$Var1[which(df$Freq == freq.max)])
  }else if(wq_pts$sgi[i]==1){
    wq_pts$most[i] <- gi_in$GItypes
  }else{
    wq_pts$most[i] <- NA
  }
  cat("\nProcessing water quality sampling point ", i, 
      "\nFound number of SGI ", wq_pts$sgi[i], 
      "\nThe most common SGI type is", wq_pts$most[i], "\n")
}

spdf <- wq_pts[,c("Station", "sgi")]
names(spdf)[1] <- "site"
dep_hwq_pts@data <- merge(dep_hwq_pts@data, spdf@data, by="site")

# closest distance to one SGI and the SGI type
nearest.gi <- function(lat, long){
  v <- vector()
  n <- nrow(gi_pts)
  for (i in 1:n){
    v[i] <- earth.dist(lat, long, gi_pts$X[i], gi_pts$Y[i])
  }
  x <- v[which(v == min(v))]
  y <- gi_pts$GItypes[sample(which(v == min(v)),1)]
  return(list(dist=x, type=y))
}
coords <- coordinates(wq_pts)
for (i in 1:n){
  nearest_gi <- nearest.gi(coords[,1][i], coords[,2][i])
  wq_pts$dist[i] <- nearest_gi$dist
  wq_pts$type[i] <- nearest_gi$type
  cat("\nProcessing water quality sampling point ", i, 
      "\nThe nearest distance is ", wq_pts$dist[i],
      "\nThe nearest GI type is ", wq_pts$type[i], "\n")
}

head(wq_pts)
spdf <- wq_pts[,c("Station", "sgi", "dist", "type", "most")]
names(spdf)[1] <- "site"
dep_hwq_pts@data <- merge(dep_hwq_pts@data, spdf@data, by="site")

df <- dep_hwq_pts@data %>% filter(pre2 > 0 & Key == "DO_top")
hist(log(df$Value))
model <- lm(log(Value+1)~Lat*Long+as.factor(year)+as.factor(month)+pre7+sgi+dist+type+most, 
             data=df)
model <- step(model)
summary(model)
par(mfrow=c(2, 2))
plot(model)
