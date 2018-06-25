# By Dongmei Chen
# This script is designed to explore the correlation and regression between GI and WQ

# libraries
library(rgdal)
library(swfscMisc)
library(mgcv)

# global settings
setwd("/nfs/urbangi-data/spatial_data/output")
infolder <- "/nfs/urbangi-data/spatial_data/"

crs <- CRS("+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333
           +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83
           +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")

bound <- readOGR(dsn = paste0(infolder, "BD"), layer = "nyad_dis")
bound <- spTransform(bound, crs)
nyadwi <- readOGR(dsn = paste0(infolder, "BD"), layer ="nyadwi_dis")
lonlat <- CRS("+proj=longlat +datum=NAD83")
gi_pts <- readOGR(dsn="./shapefile", layer="GIsites_all", stringsAsFactors=FALSE)
gi_pts <- spTransform(gi_pts, lonlat)
# check duplicated asset ID
gi_pts[duplicated(gi_pts$Asset_ID),]@data
# add unique ID for each GI
gi_pts$GIid <- paste0("gi", seq(1, length(gi_pts$Asset_ID)))
gi_pts$longtitude <- gi_pts@coords[,1]
gi_pts$latitude <- gi_pts@coords[,2]
gi_pts <- spTransform(gi_pts, crs)
wbdhu12 <- readOGR(dsn = paste0(infolder, "WBDHU"), layer = "wbdhu_12", stringsAsFactors = FALSE)
wbdhu12 <- spTransform(wbdhu12, crs)
wbdhu12$nid <- paste0("hu", seq(1, length(wbdhu12$TNMID)))
wq_pts <- readOGR(dsn = paste0(infolder, "WQ/DEP"), layer = "harbor_sampling_sites_priority", stringsAsFactors = FALSE)
wq_pts <- spTransform(wq_pts, crs)
wq_pts$hu12 <- over(wq_pts, wbdhu12)$nid
dep_hwq_pts <- readOGR(dsn = "./shapefile", layer = "harbor_water_quality", stringsAsFactors = FALSE)
dep_hwq_pts <- spTransform(dep_hwq_pts, crs)
csoloc <- readOGR(dsn=paste0(infolder, "CSO"), layer ="dec_cso_2016", stringsAsFactors=FALSE) 
csoloc <- spTransform(csoloc, crs)
#sewershed <- readOGR(dsn = paste0(infolder, "watershed/Sewershed"), layer = "Sewershed", stringsAsFactors=FALSE)
csoshed <- readOGR(dsn = paste0(infolder, "watershed"), layer = "CSO_drainage_area", stringsAsFactors=FALSE)
csoshed <- spTransform(csoshed, crs)
csoshed$ID <- paste0("csoshed", seq(1, length(csoshed$OutfallAre)))
gi_pts$csoshed <- over(gi_pts, csoshed)$ID
# measure the distance between water quality sampling points with cso outfalls

PriorityCSOWatershed <- readOGR(dsn=paste0(infolder,"watershed"),
                                layer="priority_cso_watersheds", stringsAsFactors = FALSE)
PriorityCSOWatershed <- spTransform(PriorityCSOWatershed, crs)

# using the GI data with mitigated area instead
# the borough information was not correct in the data "DEP_GI_withDA_040618"
mitigated_area <- readOGR(dsn="./shapefile", layer ="DEP_GI_withDA_040618", stringsAsFactors=FALSE)
mitigated_area <- spTransform(mitigated_area, lonlat)
mitigated_area$GIid <- paste0("gi", seq(1, length(mitigated_area$Asst_ID)))
mitigated_area$longtitude <- mitigated_area@coords[,1]
mitigated_area$latitude <- mitigated_area@coords[,2]
mitigated_area <- spTransform(mitigated_area, crs)

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

# find the closest green infrastructure
find.nearest.point <- function(lat, long){
  v <- vector()
  n <- nrow(gi_pts)
  for (i in 1:n){
    v[i] <- earth.dist(lat, long, gi_pts$latitude[i], gi_pts$longtitude[i])
  }
  x <- sample(which(v == min(v)), 1)
  return(gi_pts$GIid[x])
}

n <- nrow(wq_pts)
for (i in 1:n){
  #if (i %% 10 == 0)  { cat(100 * i / n, '% complete\r', sep='') }
  wq_pts$gi[i] <- find.nearest.point(wq_pts$Lat[i], wq_pts$Long[i])
  cat("\nProcessing water quality sampling point ", i, 
      "\nFound green infrastructure ", wq_pts$gi[i], "\n")
}

mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

spdf <- wq_pts[,c("Station", "hu12")]
names(spdf)[1] <- "site"
dep_hwq_pts@data <- merge(dep_hwq_pts@data, spdf@data, by="site")
# find the right sewershed for the water quality sampling points
# csoloc$sewershed <- over(csoloc, sewershed)$Sewershed
# csoloc$csoshed <- over(csoloc, csoshed)$ID
spdf <- gi_pts[,c("GIid", "csoshed")]
names(spdf)[1] <- "gi"
dep_hwq_pts@data <- merge(dep_hwq_pts@data, spdf@data, by="gi")

# aggregate SGI density with csoshed
df_gi <- aggregate(Asset_ID~hu12, data=gi_pts, function(x) length(x))
colnames(df_gi)[2] <- "SGI_density"
# test
var <- "Tra"; fun <- "mean"; unit <- "csoshed"
reg.test <- function(var, fun, unit){
  df <- dep_hwq_pts@data %>% filter(pre2>0) %>% filter(Key==var)
  names(df)[which(names(df)==unit)] <- "unit"
  df <- aggregate(Value~unit, data=df, fun, na.rm=TRUE)
  names(df)[which(names(df)=="unit")] <- unit
  df <- merge(df_gi, df, by=unit)
  print(summary(lm(df$Value~df$SGI_density)))
  print(cor.test(df$Value, df$SGI_density))
  par(mfrow=c(1,1),xpd=FALSE,mar=c(4,4,2,1))
  plot(df$SGI_density, df$Value)
}
# value options: "Ent_top", "DO_top", "FC_top", "Tra"
reg.test("Tra", mean, "hu12")

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
# git_pts or mitigated_area
n <- nrow(wq_pts)

r <- 4
for (i in 1:n){
  # try radius 2, 4, 8, 16
  circle.loc <- as.data.frame(circle.polygon(wq_pts$Long[i], wq_pts$Lat[i], radius=r, units = "km"))
  ch <- chull(circle.loc)
  coords <- circle.loc[c(ch, ch[1]),]
  sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID=1)))
  proj4string(sp_poly) <- lonlat
  sp_poly <- spTransform(sp_poly, crs)
  gi_in <- mitigated_area[!is.na(over(mitigated_area, as(sp_poly, "SpatialPolygons"))),]
  wq_pts$sgi[i] <- length(gi_in$Asst_ID)
  wq_pts$mta[i] <- sum(gi_in$mtgtn_2, na.rm = TRUE)
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
      "\nThe mitigated area is ", wq_pts$mta[i], 
      "\nThe most common SGI type is", wq_pts$most[i], "\n")
}

for (i in 1:n){
  # try radius 2, 4, 8, 16
  circle.loc <- as.data.frame(circle.polygon(wq_pts$Long[i], wq_pts$Lat[i], radius=r, units = "km"))
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

spdf <- wq_pts[,c("Station", "Priority", "hu12", "sgi", "most", "mta")]
names(spdf)[1] <- "site"
df <- dep_hwq_pts@data
df <- merge(df, spdf@data, by="site")

# # add another two variables (optional)
# # closest distance to one SGI and the SGI type
# nearest.gi <- function(lat, long){
#   v <- vector()
#   n <- nrow(gi_pts)
#   for (i in 1:n){
#     v[i] <- earth.dist(lat, long, gi_pts$X[i], gi_pts$Y[i])
#   }
#   x <- v[which(v == min(v))]
#   y <- gi_pts$GItypes[sample(which(v == min(v)),1)]
#   return(list(dist=x, type=y))
# }
# coords <- coordinates(wq_pts)
# for (i in 1:n){
#   nearest_gi <- nearest.gi(coords[,1][i], coords[,2][i])
#   wq_pts$dist[i] <- nearest_gi$dist
#   wq_pts$type[i] <- nearest_gi$type
#   cat("\nProcessing water quality sampling point ", i, 
#       "\nThe nearest distance is ", wq_pts$dist[i],
#       "\nThe nearest GI type is ", wq_pts$type[i], "\n")
# }
# 
# head(wq_pts)
# spdf <- wq_pts[,c("Station", "dist", "type")]
# names(spdf)[1] <- "site"
# hwq.df <- merge(dep_hwq_pts@data, spdf@data, by="site")

# value options: "Ent_top", "DO_top", "FC_top", "Tra"
df.s <- df %>% filter(pre2 > 0 & Key == "DO_top" & !is.na(Value))
par(mfrow=c(1, 1))
hist(df.s$Value)
summary(df.s$Value)
dim(df.s)
# model with latitude, longitude, year, month, precipitation, SGI density, 
# distance to the closest density, the closest SGI type, the most frequent SGI type
# trials in modeling
# model <- lm(log(Value+1)~Lat*Long+as.factor(year)+as.factor(month)+pre7+sgi+dist+type+most, 
#              data=df.s)
# model <- lm(Value~Lat*Long+pre2+sgi+dist+type+most,data=df.s)
# model with SGI density, mitigated area, binary variable with priority watershed, pre2, year as factor
model <- lm(Value~Lat*Long+as.factor(year)+pre2+Priority+sgi+most+mta,data=df.s)
model <- step(model)
summary(model)
par(mfrow=c(2, 2))
plot(model)
par(mfrow=c(1, 1))
plot(df.s$sgi, df.s$Value)
plot(df.s$mta, df.s$Value)

df.s.s <- subset(df.s, Priority==1)
df.s.s0 <- subset(df.s, Priority==0)
plot(df.s.s$year, df.s.s$Value)
plot(df.s.s0$year, df.s.s0$Value)
plot(df.s.s$mta, df.s.s$Value)
plot(df.s.s0$mta, df.s.s0$Value)
#unique(df.s$site)

# to test normality of the response variable
samp <- df.s$Value[sample(1:5000)]
shapiro.test(samp)
shapiro.test(log(samp))
shapiro.test(log(samp + 1))
shapiro.test(sqrt(samp))
hist(log(samp + 1))
hist(samp^0.6)
shapiro.test(samp^0.5)

mod <- lm(log(Value+ 1) ~ (Lat + Long + sgi + mta)^2 + as.factor(year) + pre2 + Priority + most, df.s)
#mod <- glm((Value+1) ~ (Lat + Long + sgi + mta)^2 + as.factor(year) + pre2 + Priority + most, df.s, family="Gamma")
mod<- step(mod)
# deviance: 1-(Residual deviance/Residual deviance) 
summary(mod)

par(mfrow=c(2, 2))
plot(mod)
anova(mod)
# to check the relationships between predictors and the response variable to suggest data transformation
add.mod <- gam(Value ~ s(Lat) + s(Long) + s(sgi) + s(mta) + as.factor(year) + s(pre2) + Priority + most, 
               data=df.s)
par(mfrow=c(2,3))
plot(add.mod)
# to look at the interactions between variables and nonlinear relationships
predictors <- which(names(df.s) %in% c('Lat','Long', 'pre2', 'sgi', 'mta'))
# pairs(df.s[,predictors])
pairs(df.s[,predictors], pch=16, col=rgb(0,0,0, 0.1))
pairs(df.s[,predictors], pch=16, col=rgb(0,0,0, 0.1), panel=panel.smooth)
length(predictors)
preds <- names(df.s[predictors])


cc <- df.s[complete.cases(df.s), ]
par(mfrow=c(2, 3))
for (p in preds) {
  plot(df.s$Value~df.s[, p], pch=16, col=rgb(0,0,0,0.1))
  lines(lowess(cc$Value~cc[, p]), col=2)
}