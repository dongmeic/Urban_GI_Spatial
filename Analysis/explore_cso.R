# By Dongmei Chen
# This script is designed to visualize CSO data

# global settings
setwd("/nfs/urbangi-data/spatial_data/output")
infolder <- "/nfs/urbangi-data/spatial_data/"

# libraries
library(rgdal)
library(RColorBrewer)
library(classInt)
library(ggplot2)
library(reshape)
library(grid)

# projection and roi(region of interest)
crs <- CRS("+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333 
           +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83 
           +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")
lonlat <- CRS("+proj=longlat +datum=NAD83")
bound <- readOGR(dsn = paste0(infolder, "BD"), layer = "nyad_dis")
bound <- spTransform(bound, crs)

# functions
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

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

quick.map <- function(spdf,pspdf,var,color,legend.title,outname) {
  nclr <- 5
  plotclr <- brewer.pal(nclr,color)
  plotvar <- as.numeric(spdf@data[,var])
  class <- classIntervals(plotvar, nclr, style="jenks", dataPrecision=1)
  colcode <- findColours(class, plotclr)
  png(paste0("figure/",outname,".png"), width=9, height=8, units="in", res=300)
  par(xpd=FALSE,mfrow=c(1,1),mar=c(0.5,0.5,2.5,0.5))
  plot(bound, border="white")
  if (class(spdf)[1] == "SpatialPolygonsDataFrame"){
    plot(spdf, col=colcode, add=T)
    plot(wbdhu12, bord="grey", add=T)
    plot(greinfr, pch=20, col=rgb(0,0.8,0), cex=0.2, add=T)
    plot(wq_pts, pch=1, col=rgb(0,0,0.8), cex=0.8, add=T)
    plot(pspdf, pch=20, col=rgb(0.8,0,0), cex=0.4, add=T)
  }else if (class(spdf)[1] == "SpatialPointsDataFrame"){
    plotvar <- as.numeric(spdf@data[,var])
    class <- classIntervals(plotvar, nclr, style="quantile", dataPrecision=1)
    colcode <- findColours(class, plotclr)
    #colcode[1:100] <- paste0(colcode[1:100], "33")
    #attr(colcode,"palette") <- paste0(attr(colcode,"palette"), "33")
    #colcode[colcode=="NA33"] <- "NA"
    plot(wbdhu12, bord="grey", add=T)
    plot(sewershed, add=T)
    plot(spdf, col=colcode, pch=16, cex=3, add=T)
    plot(greinfr, pch=20, col=rgb(0,0.8,0), cex=0.2, add=T)
  }else{
    cat("The spatial dataframe is not supported.")
  }
  title("Combined sewer overflows in NYC",cex.main=1.5)
  legend(910000,240000, legend=names(attr(colcode, "table")),
         fill=attr(colcode, "palette"), cex=1.2, title=legend.title, bty="n")
  add.northarrow()
  add.scale()
  dev.off()
}

reshape.df <- function(df,col1,col2){
  data <- df[,col1:col2]
  data_1 <- data[,1:2]; data_2 <- data[,3:4]; data_3 <- data[,5:6]; data_4 <- data[,7:8]
  names(data_1) <- c("Volume", "Events")
  names(data_2) <- c("Volume", "Events")
  names(data_3) <- c("Events", "Volume")
  names(data_4) <- c("Events", "Volume")
  data_5 <- rbind(data_1, data_2, data_3, data_4)
  data_6 <- cbind(data_5, c(rep("2013", dim(data_1)[1]), 
                            rep("2014", dim(data_2)[1]), 
                            rep("2015", dim(data_3)[1]),
                            rep("2016", dim(data_4)[1])))
  colnames(data_6)[3] <- "Year"
  return(data_6)
}

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

summary.list = function(x)list(
  N.with.NA.removed= length(x[!is.na(x)]),
  Count.of.NA= length(x[is.na(x)]),
  Mean=mean(x, na.rm=TRUE),
  Median=median(x, na.rm=TRUE),
  Max.Min=range(x, na.rm=TRUE),
  Range=max(x, na.rm=TRUE) - min(x, na.rm=TRUE),
  Variance=var(x, na.rm=TRUE),
  Std.Dev=sd(x, na.rm=TRUE),
  Coeff.Var=sd(x, na.rm=TRUE)/mean(x, na.rm=TRUE),
  Std.Error=sd(x, na.rm=TRUE)/sqrt(length(x[!is.na(x)])),
  Quantile=quantile(x, na.rm=TRUE)
)

Stats <- function(x){
  Mean <- mean(x, na.rm=TRUE)
  SD <- sd(x, na.rm=TRUE)
  Range <- max(x, na.rm=TRUE) - min(x, na.rm=TRUE)
  Variance <- var(x, na.rm=TRUE)
  Coeff.Var <- sd(x, na.rm=TRUE)/mean(x, na.rm=TRUE)
  return(c(Mean=Mean, SD=SD, Range=Range, Var=Variance, Coeff_Var = Coeff.Var))
}

csoloc <- readOGR(dsn=paste0(infolder, "CSO"), layer ="dec_cso_2016", stringsAsFactors=FALSE) 
csoloc <- spTransform(csoloc, crs)
greinfr <- readOGR(dsn="./shapefile", layer="GIsites_all", stringsAsFactors=FALSE)
greinfr <- spTransform(greinfr,crs)
wbdhu12 <- readOGR(dsn = paste0(infolder, "WBDHU"), layer = "wbdhu_12", stringsAsFactors = FALSE)
wbdhu12 <- spTransform(wbdhu12, crs)
wq_pts <- readOGR(dsn = "./shapefile", layer = "dep_wq_sampling_sites", stringsAsFactors = FALSE)
pilots <- readOGR(dsn = paste0(infolder, "GI"), layer ="GI_pilots", stringsAsFactors = FALSE)
pilots <- spTransform(pilots, crs)
sewershed <- readOGR(dsn = paste0(infolder, "watershed/Sewershed"), layer = "Sewershed")
cso.shed <- readOGR(dsn=paste0(infolder, "CSO"), layer = "combinedsewer_drainage_area", stringsAsFactors = FALSE)
cso.shed <- spTransform(cso.shed, crs)
keyreg <- readOGR(dsn="./shapefile", layer = "key_regulators", stringsAsFactors = FALSE)
wwtp <- readOGR(dsn=paste0(infolder, "CSO/wastewater_treatment_plants"), 
                layer = "wastewater_treatment_plants", stringsAsFactors = FALSE)
wwtp <- spTransform(wwtp, crs)

# priority CSO watershed
# priority.cso.watersheds <- readOGR(dsn = paste0(infolder, "watershed"), 
#                                    layer = "priority_cso_watersheds", stringsAsFactors = FALSE)
# priority.outfalls <- priority.cso.watersheds$outfall
png("figure/CSO_outfalls.png", width=8, height=8, units="in", res=300)
par(xpd=TRUE,mfrow=c(1,1),mar=c(0.5,0.5,2.5,0.5))
plot(bound, main="Stormwater green infrastructure and water quality monitoring in NYC")
#plot(wbdhu12, add=T)
plot(cso.shed, bord="grey58", add=T)
plot(wq_pts, pch=16, cex=1.2, col=rgb(0,0,0.8,0.6), add=T)
plot(csoloc, pch=16, cex=0.8, col=rgb(0.8,0,0,0.8), add=T)
plot(wwtp, pch=2, cex=1.5, add=T)
plot(keyreg, pch=7, cex=1.5, add=T)
plot(greinfr, pch=20, cex=0.5, col=rgb(0,0.8,0,0.8), add=T)
plot(pilots, pch=1, cex=1.2, col=rgb(0,0.3,0), add=T)
northarrow(c(925050,195000),3500)
add.scale()
legend(920000, 270000, bty="n",
       pch=c(7,2,1,16,16,20), 
       col=c(rgb(0,0,0),rgb(0,0,0),rgb(0,0.3,0),rgb(0,0,0.8,0.6),rgb(0.8,0,0,0.8),rgb(0,0.8,0,0.8)), 
       pt.cex=c(1.5,1.5,1.2,1.2,0.8,0.5),
       cex = 1.2,
       legend=c("CSO key regulators","WWTP","SGI pilot sites", "WQ sampling sites", "CSO outfalls", "SGI locations"))
legend(915000, 230000, bty="n",lty = 1, col=c("black", "grey58"),
       legend = c("NYC", "CSO-shed"))
dev.off()

# CSO volume and events by sewershed
csoloc$Sewershed <- over(csoloc, sewershed)$Sewershed
cso_sewershed <- aggregate(volume_13 ~ Sewershed, data=csoloc, mean, na.rm=TRUE)
Sewershed <- merge(sewershed, cso_sewershed, by="Sewershed")
cso_sewershed <- aggregate(events_13 ~ Sewershed, data=csoloc, mean, na.rm=TRUE)
Sewershed <- merge(Sewershed, cso_sewershed, by="Sewershed")
cso_sewershed <- aggregate(volume_14 ~ Sewershed, data=csoloc, mean, na.rm=TRUE)
Sewershed <- merge(Sewershed, cso_sewershed, by="Sewershed")
cso_sewershed <- aggregate(events_14 ~ Sewershed, data=csoloc, mean, na.rm=TRUE)
Sewershed <- merge(Sewershed, cso_sewershed, by="Sewershed")
cso_sewershed <- aggregate(volume_15 ~ Sewershed, data=csoloc, mean, na.rm=TRUE)
Sewershed <- merge(Sewershed, cso_sewershed, by="Sewershed")
cso_sewershed <- aggregate(events_15 ~ Sewershed, data=csoloc, mean, na.rm=TRUE)
Sewershed <- merge(Sewershed, cso_sewershed, by="Sewershed")
cso_sewershed <- aggregate(volume_16 ~ Sewershed, data=csoloc, mean, na.rm=TRUE)
Sewershed <- merge(Sewershed, cso_sewershed, by="Sewershed")
cso_sewershed <- aggregate(events_16 ~ Sewershed, data=csoloc, mean, na.rm=TRUE)
Sewershed <- merge(Sewershed, cso_sewershed, by="Sewershed")

# CSO volume and events by CSOshed
outfalls <- c("26-001", "26-002", "26-003", "26-004", "26-005")
csoloc$outfall <- ifelse(csoloc$outfall_id %in% outfalls, paste0("26W",substrRight(csoloc$outfall_id,4)), 
                         csoloc$outfall_id)
# writeOGR(csoloc, dsn="./shapefile", layer="csoloc", overwrite_layer = TRUE, driver = "ESRI Shapefile")
csoloc <- readOGR(dsn="./shapefile", layer="csoloc", stringsAsFactors=FALSE)
cso.shed <- merge(cso.shed, csoloc, by="outfall")


# quick maps
# quick.map(spdf,pspdf,var,color,legend.title,outname)
yrs <- 13:16
# points
for (i in yrs){
  quick.map(csoloc,csoloc,paste0("volume_",i),"Blues",paste0("Volume 20",i),paste0("cso_volume_pts",i))
  print(i)
}

for (i in yrs){
  quick.map(csoloc,csoloc,paste0("events_",i),"Blues",paste0("Events 20",i),paste0("cso_events_pts",i))
  print(i)
}

# area
# quick.map(spdf,pspdf,var,color,legend.title,outname)
for (i in yrs){
  quick.map(cso.shed,csoloc,paste0("volume_",i),"Blues",paste0("Volume 20",i),paste0("cso_volume",i))
  print(i)
}

for (i in yrs){
  quick.map(cso.shed,csoloc,paste0("events_",i),"Blues",paste0("Events 20",i),paste0("cso_events",i))
  print(i)
}

for (i in yrs){
  quick.map(Sewershed,csoloc,paste0("volume_",i),"Blues",paste0("Volume 20",i),paste0("cso_volume_sewershed",i))
  print(i)
}

for (i in yrs){
  quick.map(Sewershed,csoloc,paste0("events_",i),"Blues",paste0("Events 20",i),paste0("cso_events_sewershed",i))
  print(i)
}

# reorganize data frame
data_7 <- reshape.df(csoloc@data, 7, 14)
ggplot(data=data_7,aes(Year, log(Volume))) + geom_boxplot() + 
  labs(y="CSO volume (log)", title="Combined sewer overflows in NYC")
ggsave(paste0("figure/cso_volume.png"), width=4, height=3, units="in")

ggplot(data=data_7,aes(Year, Events)) + geom_boxplot() + 
  labs(y="CSO events", title="Combined sewer overflows in NYC")+
  geom_point(stat = 'summary',
             fun.y = 'mean',
             color = "red")
ggsave(paste0("figure/cso_events.png"), width=4, height=3, units="in")

# SGI density by CSO watershed
greinfr$outfall <- over(greinfr, cso.shed)$outfall
# greinfr$GI_ID <- 1:length(greinfr$Asset_ID)
# greinfr <- greinfr[ ,!names(greinfr) %in% "GI_ID"]
SGIqntty <- aggregate(Asset_ID ~ outfall, data=greinfr, function(x) length(x))
names(SGIqntty)[2] <- "SGIqntty"
cso.shed <- merge(cso.shed, SGIqntty, by="outfall")
cso.shed.spdf <- cso.shed[!is.na(cso.shed$SGIqntty) && cso.shed$SGIqntty > 0, ]
# check in ArcGIS and select the CSO watersheds with high SGI density visually
SGIdens.h.cso.shed <- c("26W-003", "BB-006", "BB-008", "HP-008", "NCB-015", "OH-007", "RH-034")
cso.shed.spdf$SGIlevel <- ifelse(cso.shed.spdf$outfall %in% SGIdens.h.cso.shed, "High", "Low")
cso.shed.df <- as.data.frame(cso.shed.spdf)
cso.shed.df$SGIdens <- cso.shed.df$SGIqntty/cso.shed.df$acreage
par(xpd=FALSE,mfrow=c(1,1),mar=c(4.5,4.5,0.5,0.5))
# check CSO volumes or events vs SGI density by CSO watersheds
plot(cso.shed.df$SGIdens, cso.shed.df$volume_13)
plot(cso.shed.df$SGIdens, cso.shed.df$events_16)
lm <- lm(cso.shed.df$events_13 ~ cso.shed.df$SGIdens)
summary(lm) 
# not significant

# check the summary statistics of CSO volumes or events
volume.df <- cso.shed.df[,c(13,15,18,20)]
volume.ndf <- cbind(cso.shed.df, t(apply(volume.df,1,Stats)))
events.df <- cso.shed.df[,c(14,16,17,19)]
events.ndf <- cbind(cso.shed.df, t(apply(events.df,1,Stats)))
# cso.shed.df$volume_mean <- rowMeans(volume.df,na.rm = TRUE)
# cso.shed.df$events_mean <- rowMeans(events.df,na.rm = TRUE)
summary.list(cso.shed.df$events_13)
plot(volume.ndf$SGIdens, volume.ndf$Mean)
ggplot(data=volume.ndf)+ geom_boxplot(aes(SGIlevel, Range))

# reshape dataframe volume.ndf & events.ndf
cso.vol.df <- reshape.df(volume.ndf, 13, 20)
cso.vol.df$SGIlevel <- rep(volume.ndf$SGIlevel,4)
cso.vol.df$SGIdens <- rep(volume.ndf$SGIdens,4)

ggplot(data=cso.vol.df)+ geom_boxplot(aes(SGIlevel, Volume, color=Year))+
  labs(x="SGI density level", title="Combined sewer overflows in NYC")
ggsave(paste0("figure/cso_volume_SGIlevel.png"), width=4, height=3, units="in")

ggplot(data=cso.vol.df)+ geom_boxplot(aes(SGIlevel, Events, color=Year))+
  labs(x="SGI density level", title="Combined sewer overflows in NYC")
ggsave(paste0("figure/cso_events_SGIlevel.png"), width=4, height=3, units="in")

ggplot(data=cso.vol.df)+ geom_point(aes(SGIdens, Volume, color=Year))+
  labs(x="SGI density",title="Combined sewer overflows in NYC")
ggsave(paste0("figure/cso_volume_SGIdens.png"), width=4, height=3, units="in")

ggplot(data=cso.vol.df)+ geom_point(aes(SGIdens, Events, color=Year))+
  labs(x="SGI density",title="Combined sewer overflows in NYC")
ggsave(paste0("figure/cso_events_SGIdens.png"), width=4, height=3, units="in")

write.csv(cso.vol.df, "CSV/cso_vol_events_sgi.csv", row.names = F)

# use managed imperviousness instead of SGI density
mitigated_area <- readOGR(dsn="./shapefile", layer ="DEP_GI_withDA_040618", stringsAsFactors=FALSE)
mitigated_area <- spTransform(mitigated_area, crs)
mitigated_area$outfall <- over(mitigated_area, cso.shed)$outfall
mitigatedArea <- aggregate(mtgtn_2 ~ outfall, data=mitigated_area, function(x) sum(x))
cso.shed <- merge(cso.shed, mitigatedArea, by="outfall")
cso.shed$mtgtn_2 <- round(cso.shed$mtgtn_2, 1)
writeOGR(cso.shed, dsn="./shapefile", layer="cso_watershed", 
         overwrite_layer = TRUE,driver = "ESRI Shapefile")
cso.shed.spdf <- cso.shed[!is.na(cso.shed$mtgtn_2) && cso.shed$mtgtn_2 > 0, ]
cso.shed.df <- as.data.frame(cso.shed.spdf)
cso.mitigated.df <- reshape.df(cso.shed.df, 13, 20)
cso.mitigated.df$MitigatedArea <- rep(cso.shed.df$mtgtn_2,4)

ggplot(data=cso.mitigated.df)+ geom_point(aes(MitigatedArea, Volume, color=Year))+
  labs(x="SGI mitigated area", title="Combined sewer overflows in NYC")
ggsave(paste0("figure/cso_volume_mitigated.png"), width=4, height=3, units="in")

ggplot(data=cso.mitigated.df)+ geom_point(aes(MitigatedArea, Events, color=Year))+
  labs(x="SGI mitigated area", title="Combined sewer overflows in NYC")
ggsave(paste0("figure/cso_events_mitigated.png"), width=4, height=3, units="in")

# combine above both plots (using density instead of levels)
cols <- c('#ffffd4','#fed98e','#fe9929','#cc4c02')
test <- cor.test(~SGIdens+Volume, cso.vol.df)
p1 <- ggplot(data=cso.vol.df)+ geom_point(aes(SGIdens, Volume, color=Year))+
  scale_color_manual(values=cols)+
  labs(x="SGI density",title=paste0("Correlation: r = ",
                                    format(as.numeric(test$estimate), digits = 2),", p-value = ",format(as.numeric(test$p.value), digits = 2)))
test <- cor.test(~SGIdens+Events, cso.vol.df)
p2 <- ggplot(data=cso.vol.df)+ geom_point(aes(SGIdens, Events, color=Year))+
  scale_color_manual(values=cols)+
  labs(x="SGI density",title=paste0("Correlation: r = ",
                                    format(as.numeric(test$estimate), digits = 2),", p-value = ",format(as.numeric(test$p.value), digits = 2)))
test <- cor.test(~MitigatedArea+Volume, data=cso.mitigated.df)
p3 <- ggplot(data=cso.mitigated.df)+ geom_point(aes(MitigatedArea, Volume, color=Year))+scale_color_manual(values=cols)+
  labs(x="SGI mitigated area", title=paste0("Correlation: r = ", format(as.numeric(test$estimate), digits = 2),", p-value = ",format(as.numeric(test$p.value), digits = 2)))
test <- cor.test(~MitigatedArea+Events, data=cso.mitigated.df)
p4 <- ggplot(data=cso.mitigated.df)+ geom_point(aes(MitigatedArea, Events, color=Year))+scale_color_manual(values=cols)+
  labs(x="SGI mitigated area", title=paste0("Correlation: r = ", format(as.numeric(test$estimate), digits = 2),", p-value = ",format(as.numeric(test$p.value), digits = 2)))

png(paste0("figure/sgi_cso.png"), width=12, height=8, units="in", res=300)
par(mar=c(2,2,2,2))
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
print(p1, vp = vplayout(1, 1))
print(p2, vp = vplayout(1, 2))
print(p3, vp = vplayout(2, 1))
print(p4, vp = vplayout(2, 2))
dev.off()

# select a particular year
cor.test(~MitigatedArea+Volume, data=subset(cso.mitigated.df, Year==2015))
cor.test(~MitigatedArea+Events, data=subset(cso.mitigated.df, Year==2013))

# monthly CSO data
monthly_cso_2014 <- read.csv(paste0(infolder, "CSO/2014_page17.csv"))
monthly_cso_2014 <- monthly_cso_2014[,!(colnames(monthly_cso_2014) %in% c("All", "Duration", "Category"))]
cso_2014 <- melt(monthly_cso_2014, id.vars = "Regulator", measure.vars = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
names(cso_2014)[2:3] <- c("Month", "Events")
cso_2014$Year <- rep(2014, dim(cso_2014)[1])
monthly_cso_2015 <- read.csv(paste0(infolder, "CSO/2015_page65.csv"))
monthly_cso_2015 <- monthly_cso_2015[,!(colnames(monthly_cso_2015) %in% c("All", "Duration", "Category"))]
cso_2015 <- melt(monthly_cso_2015, id.vars = "Regulator", measure.vars = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
names(cso_2015)[2:3] <- c("Month", "Events")
cso_2015$Year <- rep(2015, dim(cso_2015)[1])
monthly_cso_2016 <- read.csv(paste0(infolder, "CSO/2016_page57.csv"))
monthly_cso_2016 <- monthly_cso_2016[,!(colnames(monthly_cso_2016) %in% c("All", "Duration", "Category"))]
cso_2016 <- melt(monthly_cso_2016, id.vars = "Regulator", measure.vars = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
names(cso_2016)[2:3] <- c("Month", "Events")
cso_2016$Year <- rep(2016, dim(cso_2016)[1])
cso_monthly <- rbind(cso_2014, cso_2015, cso_2016)
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
cso_monthly$Month <- sapply(cso_monthly$Month, function(x) which(months == x))
cso_monthly$Events <- ifelse(cso_monthly$Events == "*", "", cso_monthly$Events)
cso_monthly$Events <- as.numeric(cso_monthly$Events)
cso_monthly_2015 <- subset(cso_monthly, Year==2015)
cso_monthly_2016 <- subset(cso_monthly, Year==2016)
clim <- read.csv("csv/climatedata_nyc.csv", stringsAsFactors = FALSE)
clim_2015 <- subset(clim, Year==2015 & STATION=="USW00014732")
clim_2016 <- subset(clim, Year==2016 & STATION=="USW00014732")
plot(clim_2015$Month, clim_2015$PRCP, xlim=c(1,12.6))
points(cso_monthly_2015$Month+0.5, cso_monthly_2015$Events, col="red")
head(cso_monthly)

# get coordinates for key regulators
coords.df <- cbind(keyreg$OUTFALL_ID,data.frame(keyreg@coords))
head(coords.df)
colnames(coords.df) <- c("Regulator", "x", "y")
cso_monthly <- merge(cso_monthly, coords.df, by="Regulator")
head(cso_monthly)

xy <- data.frame(cso_monthly[,c(5,6)])
coordinates(xy) <- c("x", "y")
proj4string(xy) <- proj4string(keyreg)
spdf <- SpatialPointsDataFrame(coords = xy, data = cso_monthly, proj4string = proj4string(keyreg))
writeOGR(spdf, dsn="./shapefile", layer="monthly_cso", 
         overwrite_layer = TRUE,driver = "ESRI Shapefile")

# updated on August 19th, 2018
# monthly CSO
monthly_cso <- readOGR(dsn="./shapefile", layer="monthly_cso", stringsAsFactors=FALSE)
head(monthly_cso)
plot(monthly_cso$Month, monthly_cso$Events)
cso.m.df <- as.data.frame(monthly_cso)
cso.m.df$Year <- as.character(cso.m.df$Year)
cso.m.df$Month <- formatC(cso.m.df$Month, flag = "0", digits = 1)
png(paste0("figure/cso_events_monthly.png"), width=8, height=7, units="in", res=300)
ggplot(data=cso.m.df)+ geom_boxplot(aes(Month, Events, color=Year))+
  labs(title="Combined sewer overflows in NYC")
dev.off()

# check monthly precipitation and CSO events
cso.events.2015 <- subset(cso.m.df, Year==2015)
cso.events.2016 <- subset(cso.m.df, Year==2016)
events.2015 <- aggregate(Events~Month, cso.events.2015, sum)
events.2016 <- aggregate(Events~Month, cso.events.2016, sum)
clim <- read.csv("csv/climate_date.csv")
clim <- clim[clim$STATION == "USC00308721",]
clim <- clim[clim$Year == 2015 | clim$Year == 2016,]
clim.prcp.2015 <- subset(clim, Year==2015)
clim.prcp.2016 <- subset(clim, Year==2016)
prcp.2015 <- aggregate(PRCP ~ Month, clim.prcp.2015, sum)
prcp.2016 <- aggregate(PRCP ~ Month, clim.prcp.2016, sum)
plot(events.2015$Month, events.2015$Events)
plot(events.2016$Month, events.2016$Events)
plot(prcp.2015$Month, prcp.2015$PRCP)
plot(prcp.2016$Month, prcp.2016$PRCP)
