# By Dongmei Chen
# This script is designed to visualize the cleaned up water quality data
# Input data were created from water_quality_data_cleanup.R
# libraries
library(ggplot2)
library(rgdal)
library(spatstat)
library(sp)
library(raster)
library(GISTools)
library(dplyr)
library(classInt)
library(RColorBrewer)

# global settings
setwd("/nfs/urbangi-data/spatial_data/output")
infolder <- "/nfs/urbangi-data/spatial_data/"
month.colors <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c',
                  '#fb9a99','#e31a1c','#fdbf6f','#ff7f00',
                  '#cab2d6','#6a3d9a','#ffff99','#b15928')
year.colors <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99',
                 '#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a')
bound <- readOGR(dsn = paste0(infolder, "BD"), layer = "nyad_dis")
bound <- spTransform(bound, crs)
nyadwi <- readOGR(dsn = paste0(infolder, "BD"), layer ="nyadwi_dis")
lonlat <- CRS("+proj=longlat +datum=NAD83")
crs <- CRS("+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333
           +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83
           +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")
# green infrastructure points
gi_pts <- readOGR(dsn = paste0(infolder, "GI"), layer ="GI_2018_02_22")
gi_pts <- spTransform(gi_pts, crs)
# water sampling points
dep_pts <- read.csv(paste0(infolder, "WQ/DEP/harbor_sampling_coordinates.csv"),stringsAsFactors = FALSE)
# run function df2spdf first
dep_sampling_sites <- df2spdf(4,3,"Long","Lat",dep_pts)
writeOGR(dep_sampling_sites, dsn="./shapefile", layer="dep_wq_sampling_sites", 
         overwrite_layer = TRUE,driver = "ESRI Shapefile")
names(dep_sampling_sites)[2] <- "site"
# functions
df2spdf <- function(col1, col2, colname1, colname2, df){
  xy <- data.frame(df[,c(col1,col2)])
  coordinates(xy) <- c(colname1, colname2)
  proj4string(xy) <- lonlat
  spdf <- SpatialPointsDataFrame(coords = xy, data = df, proj4string = lonlat)
  spdf <- spTransform(spdf, crs)
  return(spdf)
}
# to draw northarrow
northarrow <- function(loc,size,bearing=0,cols,cex=1,...) {
  # checking arguments
  if(missing(loc)) stop("loc is missing")
  if(missing(size)) stop("size is missing")
  # default colors are white and black
  if(missing(cols)) cols <- rep(c("white","black"),8)
  # calculating coordinates of polygons
  radii <- rep(size/c(1,4,2,4),4)
  x <- radii[(0:15)+1]*cos((0:15)*pi/8+bearing)+loc[1]
  y <- radii[(0:15)+1]*sin((0:15)*pi/8+bearing)+loc[2]
  # drawing polygons
  for (i in 1:15) {
    x1 <- c(x[i],x[i+1],loc[1])
    y1 <- c(y[i],y[i+1],loc[2])
    polygon(x1,y1,col=cols[i])
  }
  # drawing the last polygon
  polygon(c(x[16],x[1],loc[1]),c(y[16],y[1],loc[2]),col=cols[16])
  # drawing letters
  b <- c("E","N","W","S")
  for (i in 0:3) text((size+par("cxy")[1])*cos(bearing+i*pi/2)+loc[1],
                      (size+par("cxy")[2])*sin(bearing+i*pi/2)+loc[2],b[i+1],
                      cex=cex)
}

add.northarrow <- function(){
  northarrow(c(980000,260000),7000)
}

add.scale <- function(){
  GISTools::map.scale(1040000,125000,19685.083,"km",3,2,sfcol='brown')
} #19685 feet 0.5 inches equals to 6 km

# make year plot
plot.year <- function(df, var, ylabel, title, subtitle, filename){
  ggplot(df) + 
    geom_bar(aes(year, var, fill = as.factor(month)), 
             position = "stack", stat = "summary", fun.y = "mean") + 
    scale_fill_manual(values=month.colors) +
    labs(x="Year", y=ylabel, fill="Month", 
         title=title,
         subtitle = subtitle)
  print(ggsave(paste0("figure/",filename,".png"), width=8, height=5, units="in"))
}

plot.month <- function(df, var, yfun1, yfun2, ylabel, title, subtitle, filename){
  ggplot(df) + 
    geom_bar(aes(month, var, fill=month), 
             stat = "summary", fun.y = yfun1)+
    scale_fill_manual(values=month.colors)+
    geom_point(aes(month, var, size=2, colour=year),
               stat = "summary", fun.y = yfun2)+
    scale_colour_manual(values=year.colors)+
    scale_size(guide = "none")+
    labs(x="Month", y=ylabel, fill="Month", colour="Year",
         title = title,
         subtitle = subtitle)
  print(ggsave(paste0("figure/",filename,".png"), width=8, height=7, units="in"))
}

plot.day <- function(df, var, yfun, ylablel, title, subtitle, filename){
  ggplot(df, aes(x=day, y=var)) + geom_point(aes(colour=year)) +
    stat_summary(aes(y = var, group=1), fun.y=yfun,  geom="line", group=1)+
    scale_colour_manual(values=year.colors)+
    labs(x="Day of year", y=ylablel, colour="Year",
         title=title,
         subtitle = subtitle)
  print(ggsave(paste0("figure/",filename,".png"), width=10, height=6, units="in"))
}

# check time series data
plot(sewershed, col="grey58")
plot(cso_watersheds, bord="red", add=T)
time <- 1:12
for (i in time){
  plot(wqp[wqp$month==i,], pch=16, cex=0.5, col=month.colors[i],add=T)
}

# get time series data from wqp
get.timeseries <- function(spdf){
  spdf$nid <- paste0(seq(1, dim(spdf)[1]))
  wqp$code <- over(wqp, spdf)$nid
  wqp@data[,2:4] <- sapply(wqp@data[,2:4], as.numeric)
  wqp_do <- aggregate(DO ~ code, data=wqp, mean, na.rm=TRUE)
  colnames(wqp_do)[1] <- "nid"
  wqp_ent <- aggregate(Ent ~ code, data=wqp, mean, na.rm=TRUE)
  colnames(wqp_ent)[1] <- "nid"
  wqp_fc <- aggregate(FC ~ code, data=wqp, mean, na.rm=TRUE)
  colnames(wqp_fc)[1] <- "nid"
  spdf_wqp <- merge(spdf, wqp_do, by="nid")
  spdf_wqp <- merge(spdf_wqp, wqp_ent, by="nid")
  spdf_wqp <- merge(spdf_wqp, wqp_fc, by="nid")
  for (i in sort(unique(wqp$year))){
    wqp_do <- aggregate(DO ~ code, data=wqp[wqp$year==i,], mean, na.rm=TRUE)
    colnames(wqp_do) <- c("nid", paste0("DO",i))
    wqp_ent <- aggregate(Ent ~ code, data=wqp[wqp$year==i,], mean, na.rm=TRUE)
    colnames(wqp_ent) <- c("nid", paste0("Ent",i))
    wqp_fc <- aggregate(FC ~ code, data=wqp[wqp$year==i,], mean, na.rm=TRUE)
    colnames(wqp_fc) <- c("nid", paste0("FC",i))
    spdf_wqp <- merge(spdf_wqp, wqp_do, by="nid")
    spdf_wqp <- merge(spdf_wqp, wqp_ent, by="nid")
    spdf_wqp <- merge(spdf_wqp, wqp_fc, by="nid")
    print(i)
  }
  for (i in sort(unique(wqp$month))){
    wqp_do <- aggregate(DO ~ code, data=wqp[wqp$month==i,], mean, na.rm=TRUE)
    colnames(wqp_do) <- c("nid", paste0("DO",i))
    wqp_ent <- aggregate(Ent ~ code, data=wqp[wqp$month==i,], mean, na.rm=TRUE)
    colnames(wqp_ent) <- c("nid", paste0("Ent",i))
    wqp_fc <- aggregate(FC ~ code, data=wqp[wqp$month==i,], mean, na.rm=TRUE)
    colnames(wqp_fc) <- c("nid", paste0("FC",i))
    spdf_wqp <- merge(spdf_wqp, wqp_do, by="nid")
    spdf_wqp <- merge(spdf_wqp, wqp_ent, by="nid")
    spdf_wqp <- merge(spdf_wqp, wqp_fc, by="nid")
    print(i)
  }
  return(spdf_wqp)
}

# require city boundary, green infrastructure points and sampling points
# require libraries classInt, RColorBrewer, GISTools 
quick.map <- function(spdf,pspdf,shed,var,legend.title,main.title,color,outname) {
  nclr <- 5
  plotclr <- brewer.pal(nclr,color)
  class <- classIntervals(plotvar, nclr, style="jenks", dataPrecision=1)
  colcode <- findColours(class, plotclr)
  if (length(grep("dep",outname))){
    if (length(grep(var,c("DO", "Ent", "FC")))){
      pspdf <- pspdf[pspdf$Key == paste0(var,"_top"),]
    }else{
      pspdf <- pspdf[pspdf$Key == var,]
    }
    wqsp <- pspdf[!is.na(pspdf$Value),]
  }else{
    wqsp <- pspdf[!is.na(pspdf@data[,var]),]
  }
  png(paste0("figure/",outname,".png"), width=9, height=8, units="in", res=300)
  par(xpd=FALSE,mfrow=c(1,1),mar=c(0.5,0.5,2.5,0.5))
  plot(bound, border="white")
  if (class(spdf)[1] == "SpatialPolygonsDataFrame"){
    plotvar <- as.numeric(spdf@data[,var])
    if(length(grep("shed",shed))){
      plot(spdf, col=colcode, add=T)
      plot(wbdhu12, bord="grey", add=T)
    }else{
      plot(spdf, col=colcode, add=T)
      plot(bound, bord="grey58", add=T)
    }
    plot(gi_pts, pch=20, col=rgb(0,0.8,0), cex=0.8, add=T)
    plot(wqsp, pch=1, col=rgb(0,0,0.8), cex=0.8, add=T)
  }else if (class(spdf)[1] == "SpatialPointsDataFrame"){
    if (length(grep("dep",outname))){
      df <- aggregate(Value~site, data=wqsp, mean, na.rm=TRUE)
      wqsp <- merge(dep_sampling_sites, df, by="site")
      plotvar <- wqsp$Value
    }else{
      plotvar <- as.numeric(wqsp@data[,var])
    }
    class <- classIntervals(plotvar, nclr, style="quantile", dataPrecision=1)
    colcode <- findColours(class, plotclr)
    plot(wbdhu12, add=T)
    plot(bound, bord="grey", add=T)
    plot(wqsp, col=colcode, pch=16, cex=3, add=T)
  }else{
    cat("The spatial dataframe is not supported.")
  }
  title(main.title,cex.main=1.5)
  legend(910000,240000, legend=names(attr(colcode, "table")),
         fill=attr(colcode, "palette"), cex=1.2, title=legend.title, bty="n")
  add.northarrow()
  add.scale()
  dev.off()
}

# time -2008:2017; 1:12
map.time <- function(time,spdf,pspdf,var,legend.title,color,outname){
  if (length(time) == 10){
    png(paste0("figure/",outname,".png"), width=9, height=15, units="in", res=600)
    par(xpd=FALSE,mfrow=c(5,2),mar=c(0.5,0.5,2.5,0.5))
    
  }else if(length(time) == 12){
    png(paste0("figure/",outname,".png"), width=8, height=12, units="in", res=600)
    par(xpd=FALSE,mfrow=c(4,3),mar=c(0.5,0.5,2.5,0.5))
  }
  for (i in time){
    if (length(time) == 10){
      pspdf.i <- pspdf[pspdf$year == i,]
    }else{
      pspdf.i <- pspdf[pspdf$month == i,]
    }
    nclr <- 5
    plotclr <- brewer.pal(nclr,color)
    plotvar <- as.numeric(spdf@data[,paste0(var,i)])
    if (length(grep("dep",outname))){
      if (length(grep(var,c("DO", "Ent", "FC")))){
        pspdf.i <- pspdf.i[pspdf.i$Key == paste0(var,"_top"),]
      }else{
        pspdf.i <- pspdf.i[pspdf.i$Key == var,]
      }
      wqsp <- pspdf.i[!is.na(pspdf.i$Value),]
    }else{
      wqsp <- pspdf.i[!is.na(pspdf.i@data[,var]),]
    }
    plot(bound, border="white")
    if (length(na.omit(plotvar)) < 4){
      n <- length(na.omit(plotvar))
      cat('\nplotvar:', plotvar, '\nnclr:', n, "\n")
      if (n == 1){
        cat("\nSingle unique value in", i, "\n")
      }
      if(length(grep("dep",outname))==0){
        plot(wbdhu12, add=T)
        plot(bound, bord="grey58", add=T)
      }else{
        plot(wbdhu12, bord="grey58", add=T)
        plot(spdf, add=T)
      }
      plot(wqsp, col="red", pch=1, cex=3, add=T)
      title(i,cex.main=1.5)
    }else{
      class <- classIntervals(plotvar, nclr, style="pretty", dataPrecision=1)
      colcode <- findColours(class, plotclr)
      plot(spdf, col=colcode, add=T)
      plot(bound, bord="grey58", add=T)
      plot(wqsp, pch=1, col=rgb(0,0,0.8), cex=0.8, add=T)
      title(i,cex.main=1.5)
      legend(905000,280000, legend=names(attr(colcode, "table")),
             fill=attr(colcode, "palette"), cex=0.8, title=legend.title, bty="n")
    }
  }
  dev.off()
}

# check the temporal dynamics of water quality
wq <- read.csv("csv/wq_pts_all.csv",stringsAsFactors = FALSE)
wq <- data.frame(lapply(wq, function(x) {gsub("<", "", x)}), stringsAsFactors = FALSE)
wq <- data.frame(lapply(wq, function(x) {gsub(">", "", x)}), stringsAsFactors = FALSE)
wq <- data.frame(lapply(wq, function(x) {gsub("*", "", x)}), stringsAsFactors = FALSE)
sapply(wq, class)
wq[,2:7] <- sapply(wq[,2:7],as.numeric)
summary(wq$ent)
wq <- wq[!is.na(wq$ent) & !is.na(wq$year) & wq$year >= 2008,]
wq$year <- as.character(wq$year)
wq$month <- formatC(wq$month, width=2, flag="0")
wq_update <- update.df(wq, c("ent"))
head(wq)
ggplot(aes(day, ent), data=wq) + 
  geom_jitter(aes(color=month)) + 
  scale_color_manual(values=month.colors)

# annual
wq$ent_1 <- wq$ent/1000
plot.year(wq, wq$ent_1, "Enterococci (10 MPN/mL)", 
                      "Annual changes of water quality in NYC", 
                      "using level of enterococci as an indicator", 
                      "enterococci")

#wq$date <- as.Date(wq$day, origin = paste0(as.character(wq$year), "-01-01"))
#write.csv(wq, "csv/water_quality_date.csv", row.names=FALSE)

# check DEP harbor water quality and water quality portal
dep_hwq <- read.csv("csv/harbor_water_quality.csv",stringsAsFactors = FALSE)
head(dep_hwq)
sapply(dep_hwq, class)
dep_hwq <- dep_hwq[!is.na(dep_hwq$month) & !is.na(dep_hwq$year) & !is.na(dep_hwq$day),]
dep_hwq$month <- formatC(dep_hwq$month, width=2, flag="0")
dep_hwq$year <- as.character(dep_hwq$year)
 
wq_portal <- read.csv("csv/Portal_WQ_pts.csv", stringsAsFactors = FALSE)
sapply(wq_portal, class)
wq_portal$month <- formatC(wq_portal$month, width=2, flag="0")
wq_portal$year <- as.character(wq_portal$year)
wq_portal$DO <- as.numeric(wq_portal$DO)
wq_portal$Ent <- as.numeric(wq_portal$Ent)
#wq_portal <- update.df(wq_portal, c("DO", "Ent", "FC"))

# harbor water
plot.year(dep_hwq, dep_hwq$DO_top, "Dissolved oxygen (mg/L)", 
          "Annual changes of harbor water quality in NYC", 
          "using level of dissolved oxygen as an indicator", 
          "hwq_dissolved_oxygen_top")

# portal
wq_portal_1 <- wq_portal[!is.na(wq_portal$DO),]
plot.year(wq_portal_1, wq_portal_1$DO, "Dissolved oxygen (mg/L)", 
          "Annual changes of water quality in NYC from WQP", 
          "using level of dissolved oxygen as an indicator", 
          "portal_dissolved_oxygen")

# harbor water
dep_hwq_1 <- dep_hwq
dep_hwq_1$FC_top <- dep_hwq_1$FC_top/10000
plot.year(dep_hwq_1, dep_hwq_1$FC_top, expression("Fecal coliform ("*10^2*"/mL)"), 
          "Annual changes of harbor water quality in NYC", 
          "using level of fecal coliform as an indicator", 
          "hwq_fecal_coliform_top")

# portal
wq_portal_2 <- wq_portal[!is.na(wq_portal$FC),]
wq_portal_2$FC_1 <- wq_portal_2$FC/10000
plot.year(wq_portal_2, wq_portal_2$FC_1, expression("Fecal coliform ("*10^2*"/mL)"), 
          "Annual changes of water quality in NYC from WQP", 
          "using level of fecal coliform as an indicator", 
          "portal_fecal_coliform")

# harbor water
dep_hwq_2 <- dep_hwq
dep_hwq_2$Ent_top <- dep_hwq_2$Ent_top/100
plot.year(dep_hwq_2, dep_hwq_2$Ent_top, "Enterococci (#/mL)", 
          "Annual changes of harbor water quality in NYC", 
          "using level of enterococci as an indicator", 
          "hwq_enterococci_top")

# portal
wq_portal_3 <- wq_portal[!is.na(wq_portal$Ent),]
wq_portal_3$Ent_1 <- wq_portal_3$Ent/100
plot.year(wq_portal_3, wq_portal_3$Ent_1, "Enterococci (MPN/mL)", 
          "Annual changes of water quality in NYC from WQP", 
          "using level of enterococci as an indicator", 
          "portal_enterococci")

# harbor water
plot.year(dep_hwq, dep_hwq$Tra, "Transparency (ft.)", 
          "Annual changes of harbor water quality in NYC", 
          "using level of transparency as an indicator", 
          "hwq_transparency")

# monthly
wq$ent_2 <- log(wq$ent+1)
plot.month(wq, wq$ent_2, median, median, "Enterococci (log)", 
           "Seasonal changes of water quality in NYC", 
           "using level of enterococci as an indicator", 
           "enterococci_monthly")

# harbor water
plot.month(dep_hwq, dep_hwq$DO_top, mean, mean, "Dissolved oxygen (mg/L)", 
           "Seasonal changes of harbor water quality in NYC", 
           "using level of dissolved oxygen as an indicator", 
           "hwq_dissolved_oxygen_monthly")

# portal
wq_portal_1_1 <- wq_portal_1[wq_portal_1$DO > 0, ]
plot.month(wq_portal_1, wq_portal_1$DO, mean, median, "Dissolved oxygen (mg/L)", 
           "Seasonal changes of water quality in NYC from WQP", 
           "using level of dissolved oxygen as an indicator", 
           "portal_dissolved_oxygen_monthly")

# harbor water
dep_hwq_3 <- dep_hwq[!is.na(dep_hwq$Ent_top),]
dep_hwq_3$Ent_top <- log(dep_hwq_3$Ent_top+1)
plot.month(dep_hwq_3, dep_hwq_3$Ent_top, median, median, "Enterococci (log)", 
           "Seasonal changes of water quality in NYC from WQP", 
           "using level of enterococci as an indicator", 
           "hwq_enterococci_monthly")

# portal
wq_portal_3$Ent_2 <- log(wq_portal_3$Ent)
wq_portal_3$Ent_3 <- log(wq_portal_3$Ent + 1)
plot.month(wq_portal_3, wq_portal_3$Ent_2, median, median, "Enterococci (log)", 
           "Seasonal changes of water quality in NYC from WQP", 
           "using level of enterococci as an indicator", 
           "portal_enterococci_monthly")

# harbor water
dep_hwq_4 <- dep_hwq[!is.na(dep_hwq$FC_top),]
dep_hwq_4$FC_top <- log(dep_hwq_4$FC_top+1)
plot.month(dep_hwq_4, dep_hwq_4$FC_top, median, median, "Fecal coliform (log)", 
           "Seasonal changes of harbor water quality in NYC", 
           "using level of fecal coliform as an indicator", 
           "hwq_fecal_coliform_monthly")

# portal
wq_portal_2$FC_2 <- log(wq_portal_2$FC)
wq_portal_2$FC_3 <- log(wq_portal_2$FC + 1)
plot.month(wq_portal_2, wq_portal_2$FC_2, median, median, "Fecal coliform (log)", 
           "Seasonal changes of water quality in NYC from WQP", 
           "using level of fecal coliform as an indicator", 
           "portal_fecal_coliform_monthly")

plot.month(dep_hwq, dep_hwq$Tra, mean, mean, "Transparency (ft.)", 
           "Seasonal changes of harbor water quality in NYC", 
           "using level of transparency as an indicator", 
           "hwq_transparency_monthly")

# daily
plot.day(wq, wq$ent_2, median, "Enterococci (log)", 
         "Diurnal changes of water quality in NYC", 
         "using level of enterococci as an indicator",
         "enterococci_daily")

wq_2 <- wq[wq$ent <= 104,]
plot.day(wq_2, wq_2$ent, mean, "Enterococci (MPN/100 mL)", 
         "Diurnal changes of water quality in NYC", 
         "using level of enterococci as an indicator",
         "enterococci_daily_2")

# harbor water
# remove one outlier
dep_hwq_5 <- dep_hwq[dep_hwq$DO_top < 60 & !is.na(dep_hwq$DO_top),]
plot.day(dep_hwq_5, dep_hwq_5$DO_top, mean, "Dissolved oxygen (mg/L)", 
         "Diurnal changes of harbor water quality in NYC", 
         "using level of dissolved oxygen as an indicator",
         "hwq_dissolved_oxygen_daily")

# remove one outlier
wq_portal_1_2 <- wq_portal_1[wq_portal_1$DO < 100, ]
plot.day(wq_portal_1_2, wq_portal_1_2$DO, mean, "Dissolved oxygen (mg/L)", 
         "Diurnal changes of water quality in NYC from WQP", 
         "using level of dissolved oxygen as an indicator",
         "portal_dissolved_oxygen_daily")

# harbor water
plot.day(dep_hwq_3, dep_hwq_3$Ent_top, median, "Enterococci (log)", 
         "Diurnal changes of harbor water quality in NYC", 
         "using level of enterococci as an indicator",
         "hwq_enterococci_daily_1")

dep_hwq_6 <- dep_hwq[dep_hwq$Ent_top <= 104,] 
plot.day(dep_hwq_6, dep_hwq_6$Ent_top, mean, "Enterococci (#/100 mL)", 
         "Diurnal changes of harbor water quality in NYC", 
         "using level of enterococci as an indicator",
         "hwq_enterococci_daily_2")

# portal
plot.day(wq_portal_3, wq_portal_3$Ent_3, median, "Enterococci (log)", 
         "Diurnal changes of water quality in NYC from WQP", 
         "using level of enterococci as an indicator",
         "portal_enterococci_daily")

# harbor water
dep_hwq_7 <- dep_hwq[dep_hwq$FC_top <= 2000,]
plot.day(dep_hwq_7, dep_hwq_7$FC_top, mean, "Fecal coliform (#/mL)", 
         "Diurnal changes of harbor water quality in NYC", 
         "using level of fecal coliform as an indicator",
         "hwq_fecal_coliform_daily")

plot.day(dep_hwq_4, dep_hwq_4$FC_top, median, "Fecal coliform (log)", 
         "Diurnal changes of harbor water quality in NYC", 
         "using level of fecal coliform as an indicator",
         "hwq_fecal_coliform_daily_1")

# portal
plot.day(wq_portal_2, wq_portal_2$FC_2, median, "Fecal coliform (log)", 
         "Diurnal changes of water quality in NYC from WQP", 
         "using level of fecal coliform as an indicator",
         "portal_fecal_coliform_daily")

dep_hwq_8 <- dep_hwq[!is.na(dep_hwq$Tra),]
plot.day(dep_hwq_8, dep_hwq_8$Tra, mean, "Transparency (ft.)", 
         "Diurnal changes of harbor water quality in NYC", 
         "using level of transparency as an indicator",
         "hwq_transparency_daily")

# check the spatial dynamics of water quality
# watershed boundary dataset hydrographic unit
wbdhu12 <- readOGR(dsn = paste0(infolder, "WBDHU"), layer = "wbdhu_12", stringsAsFactors = FALSE)
wbdhu12 <- spTransform(wbdhu12, crs)
# sewershed
sewershed <- readOGR(dsn = paste0(infolder, "watershed/Sewershed"), layer = "Sewershed")
# subsewershed
subsewersheds <- readOGR(dsn = paste0(infolder, "watershed/Subsewersheds"), layer = "Subsewersheds")
# cso watersheds
cso_watersheds <- readOGR(dsn = paste0(infolder, "watershed"), layer = "CSO_watersheds")
cso_watersheds <- spTransform(cso_watersheds, crs)
cso_all <- readOGR(dsn = paste0(infolder, "watershed"), layer = "CSO_all")
cso_all <- spTransform(cso_all, crs)
cso_outfall <- readOGR(dsn = paste0(infolder, "CSO"), layer = "dec_cso_2016")
cso_outfall <- spTransform(cso_outfall, crs)
# separate water quality data: harbor water quality and citizen science 
dep_hwq_pts <- readOGR(dsn = "./shapefile", layer = "harbor_water_quality", stringsAsFactors = FALSE)
# check variable and year range
head(dep_hwq_pts@data);unique(dep_hwq_pts@data$year)
dep_hwq_pts <- spTransform(dep_hwq_pts, crs)
# read citizen science data
wta <- readOGR(dsn="./shapefile", layer="NYCWTA_WQ", stringsAsFactors = FALSE)
cf <- readOGR(dsn="./shapefile", layer="CF_WQ", stringsAsFactors = FALSE)
# read WQ portal
wqp <- readOGR(dsn="./shapefile", layer="Portal_WQ", stringsAsFactors = FALSE)
wqp <- spTransform(wqp, crs)
# show data
par(mar=c(1,1,3,2))
# create a boundary box
e <- as(raster::extent(900000, 1100050, 100000, 332550), "SpatialPolygons")
proj4string(e) <- crs
png("figure/WQ_sampling_sites.png", width=8, height=8, units="in", res=300)
par(xpd=FALSE,mfrow=c(1,1),mar=c(1.5,0.5,2.5,0.5))
plot(e, bord="white")
plot(wbdhu12, add=T, bord="grey")
plot(bound, add=T)
plot(wta, add=T, col="red", pch=16, cex=0.8)
plot(cf, add=T, col="red", pch=17, cex=0.8)
plot(wqp, add=T, col=rgb(0,0.5,0,0.5), pch=20)
plot(dep_hwq_pts, add=T, col="blue", pch=16)
northarrow(c(1095050,240000),7000)
GISTools::map.scale(1095050,200000,19685.083,"km",3,2,sfcol='brown')
legend(905000, 300000, bty="n",
       pch=c(16, 16, 17, 20), 
       col=c(rgb(0,0,1),rgb(1,0,0), rgb(1,0,0), rgb(0,0.5,0,0.5)), 
       pt.cex=c(1,0.8, 0.8, 1),
       legend=c("DEP sites", "WTA sties", "CF sites", "WQP sites"))
legend(900000, 270000, bty="n",lty = 1, col=c("grey", "black"),
       legend = c("WBD HU12", "NYC"))
#text(cbind(905000, 325000), family="Arial Black", "Legend", cex=1.12)
dev.off()

# mapping
# WBD hu12
# portal
wqp$pid <- paste0("wqp", seq(1, length(wqp$Id)))
#quick.map(spdf,pspdf,shed,var,legend.title,main.title,color,outname)
wbdhu12_wqp <- get.timeseries(wbdhu12)
quick.map(wbdhu12_wqp,wqp,"hu12","DO","DO (mg/L)", "Dissolved oxygen level in NYC from WQP","PuBu","hu12_wqp_do")
quick.map(wbdhu12_wqp,wqp,"hu12","Ent","Ent (MPN/100 mL)", "Enterococci level in NYC from WQP","Greens","hu12_wqp_ent")
quick.map(wbdhu12_wqp,wqp,"hu12","FC","FC (MPN/100 mL)", "Fecal coliform level in NYC from WQP","Purples","hu12_wqp_fc")
# yearly and monthly average
#map.time(time,spdf,pspdf,var,legend.title,color,outname)
map.time(2008:2017, wbdhu12_wqp,wqp,"DO","DO (mg/L)","BuPu","hu12_wqp_do_yearly")
map.time(1:12, wbdhu12_wqp,wqp,"DO","DO (mg/L)","BuPu","hu12_wqp_do_monthly")
map.time(2008:2017, wbdhu12_wqp,wqp,"Ent","Ent (MPN/100 mL)","Greens","hu12_wqp_ent_yearly")
map.time(1:12, wbdhu12_wqp,wqp,"Ent","Ent (MPN/100 mL)","Greens","hu12_wqp_ent_monthly")
map.time(2008:2017, wbdhu12_wqp,wqp,"FC","FC (MPN/100 mL)","Purples","hu12_wqp_fc_yearly")
map.time(1:12, wbdhu12_wqp,wqp,"FC","FC (MPN/100 mL)","Purples","hu12_wqp_fc_monthly")

# all water quality points
wq_pts_all <- df2spdf(2,3,'lon','lat',wq)
wq_pts_all$hu12 <- over(wq_pts_all, wbdhu12)$nid
wqpts_ent <- aggregate(ent ~ hu12, data=wq_pts_all, mean, na.rm=TRUE)
colnames(wqpts_ent)[1] <- "nid"
wqpts_ent_hu12 <- merge(wbdhu12, wqpts_ent, by="nid")
quick.map(wqpts_ent_hu12,wq_pts_all,"hu12","ent","Ent (MPN/100 mL)", "Enterococci level in NYC","PuBuGn","hu12_wqpts_ent")
  
hu12_wqpts_all
# harbor water
dep_hwq_pts$pid <- paste0("hwq", seq(1, length(dep_hwq_pts$site)))
dep_hwq_pts$hu12 <- over(dep_hwq_pts, wbdhu12)$nid
dep_hwq_pts_do_hu <- aggregate(Value ~ hu12, data=filter(dep_hwq_pts@data, Key=="DO_top"), mean, na.rm=TRUE)
colnames(dep_hwq_pts_do_hu) <- c("nid", "DO")
dep_hwq_pts_ent_hu <- aggregate(Value ~ hu12, data=filter(dep_hwq_pts@data, Key=="Ent_top"), mean, na.rm=TRUE)
colnames(dep_hwq_pts_ent_hu) <- c("nid", "Ent")
dep_hwq_pts_fc_hu <- aggregate(Value ~ hu12, data=filter(dep_hwq_pts@data, Key=="FC_top"), mean, na.rm=TRUE)
colnames(dep_hwq_pts_fc_hu) <- c("nid", "FC")
dep_hwq_pts_tra_hu <- aggregate(Value ~ hu12, data=filter(dep_hwq_pts@data, Key=="Tra"), mean, na.rm=TRUE)
colnames(dep_hwq_pts_tra_hu) <- c("nid", "Tra")
wbdhu12_dep_hwq_pts <- merge(wbdhu12, dep_hwq_pts_do_hu, by="nid")
wbdhu12_dep_hwq_pts <- merge(wbdhu12_dep_hwq_pts, dep_hwq_pts_ent_hu, by="nid")
wbdhu12_dep_hwq_pts <- merge(wbdhu12_dep_hwq_pts, dep_hwq_pts_fc_hu, by="nid")
wbdhu12_dep_hwq_pts <- merge(wbdhu12_dep_hwq_pts, dep_hwq_pts_tra_hu, by="nid")
#quick.map(spdf,pspdf,shed,var,legend.title,main.title,color,outname)
quick.map(wbdhu12_dep_hwq_pts,dep_hwq_pts,"hu12","DO","DO (mg/L)", "Dissolved oxygen level in NYC harbor water","PuBu","hu12_dep_hwq_pts_do")
quick.map(wbdhu12_dep_hwq_pts,dep_hwq_pts,"hu12","Ent","Ent (MPN/100 mL)", "Enterococci level in NYC harbor water","Greens","hu12_dep_hwq_pts_ent")
quick.map(wbdhu12_dep_hwq_pts,dep_hwq_pts,"hu12","FC","FC (MPN/100 mL)", "Fecal coliform level in NYC harbor water","Purples","hu12_dep_hwq_pts_fc")
quick.map(wbdhu12_dep_hwq_pts,dep_hwq_pts,"hu12","Tra","Tra (ft.)", "Transparency level in NYC harbor water","Blues","hu12_dep_hwq_pts_tra")

# aggregate to points
quick.map(dep_hwq_pts,dep_hwq_pts,"hu12","DO","DO (mg/L)", "Dissolved oxygen level in NYC harbor water","PuBu","hu12_dep_hwq_pts_do_1")
quick.map(dep_hwq_pts,dep_hwq_pts,"hu12","Ent","Ent (MPN/100 mL)", "Enterococci level in NYC harbor water","Greens","hu12_dep_hwq_pts_ent_1")
quick.map(dep_hwq_pts,dep_hwq_pts,"hu12","FC","FC (MPN/100 mL)", "Fecal coliform level in NYC harbor water","Purples","hu12_dep_hwq_pts_fc_1")
quick.map(dep_hwq_pts,dep_hwq_pts,"hu12","Tra","Tra (ft.)", "Transparency level in NYC harbor water","Blues","hu12_dep_hwq_pts_tra_1")

# yearly and monthly average
for (i in sort(unique(dep_hwq_pts$year))){
  dep_hwq_pts_do_hu <- aggregate(Value ~ hu12, data=dep_hwq_pts[dep_hwq_pts$year==i & dep_hwq_pts$Key == "DO_top",], mean, na.rm=TRUE)
  colnames(dep_hwq_pts_do_hu) <- c("nid", paste0("DO",i))
  dep_hwq_pts_ent_hu <- aggregate(Value ~ hu12, data=dep_hwq_pts[dep_hwq_pts$year==i & dep_hwq_pts$Key == "Ent_top",], mean, na.rm=TRUE)
  colnames(dep_hwq_pts_ent_hu) <- c("nid", paste0("Ent",i))
  dep_hwq_pts_fc_hu <- aggregate(Value ~ hu12, data=dep_hwq_pts[dep_hwq_pts$year==i & dep_hwq_pts$Key == "FC_top",], mean, na.rm=TRUE)
  colnames(dep_hwq_pts_fc_hu) <- c("nid", paste0("FC",i))
  dep_hwq_pts_tra_hu <- aggregate(Value ~ hu12, data=dep_hwq_pts[dep_hwq_pts$year==i & dep_hwq_pts$Key == "Tra",], mean, na.rm=TRUE)
  colnames(dep_hwq_pts_tra_hu) <- c("nid", paste0("Tra",i))
  wbdhu12_dep_hwq_pts <- merge(wbdhu12_dep_hwq_pts, dep_hwq_pts_do_hu, by="nid")
  wbdhu12_dep_hwq_pts <- merge(wbdhu12_dep_hwq_pts, dep_hwq_pts_ent_hu, by="nid")
  wbdhu12_dep_hwq_pts <- merge(wbdhu12_dep_hwq_pts, dep_hwq_pts_fc_hu, by="nid")
  wbdhu12_dep_hwq_pts <- merge(wbdhu12_dep_hwq_pts, dep_hwq_pts_tra_hu, by="nid")
  print(i)
}
for (i in sort(unique(dep_hwq_pts$month))){
  dep_hwq_pts_do_hu <- aggregate(Value ~ hu12, data=dep_hwq_pts[dep_hwq_pts$month==i & dep_hwq_pts$Key == "DO_top",], mean, na.rm=TRUE)
  colnames(dep_hwq_pts_do_hu) <- c("nid", paste0("DO",i))
  dep_hwq_pts_ent_hu <- aggregate(Value ~ hu12, data=dep_hwq_pts[dep_hwq_pts$month==i & dep_hwq_pts$Key == "Ent_top",], mean, na.rm=TRUE)
  colnames(dep_hwq_pts_ent_hu) <- c("nid", paste0("Ent",i))
  dep_hwq_pts_fc_hu <- aggregate(Value ~ hu12, data=dep_hwq_pts[dep_hwq_pts$month==i & dep_hwq_pts$Key == "FC_top",], mean, na.rm=TRUE)
  colnames(dep_hwq_pts_fc_hu) <- c("nid", paste0("FC",i))
  dep_hwq_pts_tra_hu <- aggregate(Value ~ hu12, data=dep_hwq_pts[dep_hwq_pts$month==i & dep_hwq_pts$Key == "Tra",], mean, na.rm=TRUE)
  colnames(dep_hwq_pts_tra_hu) <- c("nid", paste0("Tra",i)) 
  wbdhu12_dep_hwq_pts <- merge(wbdhu12_dep_hwq_pts, dep_hwq_pts_do_hu, by="nid")
  wbdhu12_dep_hwq_pts <- merge(wbdhu12_dep_hwq_pts, dep_hwq_pts_ent_hu, by="nid")
  wbdhu12_dep_hwq_pts <- merge(wbdhu12_dep_hwq_pts, dep_hwq_pts_fc_hu, by="nid")
  wbdhu12_dep_hwq_pts <- merge(wbdhu12_dep_hwq_pts, dep_hwq_pts_tra_hu, by="nid")
  print(i)
}
dep_hwq_pts$year <- as.numeric(dep_hwq_pts$year)
map.time(2008:2017, wbdhu12_dep_hwq_pts,dep_hwq_pts,"DO","DO (mg/L)","BuPu","hu12_dep_hwq_pts_do_yearly")
map.time(1:12, wbdhu12_dep_hwq_pts,dep_hwq_pts,"DO","DO (mg/L)","BuPu","hu12_dep_hwq_pts_do_monthly")
map.time(2008:2017, wbdhu12_dep_hwq_pts,dep_hwq_pts,"Ent","Ent (MPN/100 mL)","Greens","hu12_dep_hwq_pts_ent_yearly")
map.time(1:12, wbdhu12_dep_hwq_pts,dep_hwq_pts,"Ent","Ent (MPN/100 mL)","Greens","hu12_dep_hwq_pts_ent_monthly")
map.time(2008:2017, wbdhu12_dep_hwq_pts,dep_hwq_pts,"FC","FC (MPN/100 mL)","Purples","hu12_dep_hwq_pts_fc_yearly")
map.time(1:12, wbdhu12_dep_hwq_pts,dep_hwq_pts,"FC","FC (MPN/100 mL)","Purples","hu12_dep_hwq_pts_fc_monthly")
map.time(2008:2017, wbdhu12_dep_hwq_pts,dep_hwq_pts,"Tra","Transparency (ft.)","Blues","hu12_dep_hwq_pts_tra_yearly")
map.time(1:12, wbdhu12_dep_hwq_pts,dep_hwq_pts,"Tra","Transparency (ft.)","Blues","hu12_dep_hwq_pts_tra_monthly")

# sewershed
sewershed_wqp <- get.timeseries(sewershed)
quick.map(sewershed_wqp,wqp,"sewershed","DO","DO (mg/L)", "Dissolved oxygen level in NYC from WQP","PuBu","sewer_wqp_do")
quick.map(sewershed_wqp,wqp,"sewershed","Ent","Ent (MPN/100 mL)", "Enterococci level in NYC from WQP","Greens","sewer_wqp_ent")
quick.map(sewershed_wqp,wqp,"sewershed","FC","FC (MPN/100 mL)", "Fecal coliform level in NYC from WQP","Purples","sewer_wqp_fc")


