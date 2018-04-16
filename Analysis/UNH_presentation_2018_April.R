library(rgdal)
library(ggplot2)
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

Mode = function(x){
  ta = table(x)
  tam = max(ta)
  if (all(ta == tam)){
      mod = unique(x)
  }else{
    if(is.numeric(x)){
      mod = as.numeric(names(ta)[ta == tam])
    }else{
      mod = names(ta)[ta == tam]
    }
  }
  return(mod)
}

# global settings
setwd("/nfs/urbangi-data/spatial_data/output")
infolder <- "/nfs/urbangi-data/spatial_data/"
bound <- readOGR(dsn = paste0(infolder, "BD"), layer = "nyad_dis")
nyadwi <- readOGR(dsn = paste0(infolder, "BD"), layer ="nyadwi_dis")
lonlat <- CRS("+proj=longlat +datum=NAD83")
crs <- CRS("+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333
           +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83
           +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")
bound <- spTransform(bound, crs)
wbdhu12 <- readOGR(dsn = paste0(infolder, "WBDHU"), layer = "wbdhu_12", stringsAsFactors = FALSE)
wbdhu12 <- spTransform(wbdhu12, crs)

GIsites_all <- readOGR(dsn="./shapefile", layer="GIsites_all", stringsAsFactors = FALSE)
gitypes.colors <- c('#e6ab02','#999999','#ff7f00', '#4daf4a','#e41a1c','#984ea3','#f781bf','#377eb8')
gitypes <- c("Rain barrels", "Detention", "Permeable", "Green streets", 
             "Green roofs", "Bioswales", "Rain gardens", "Bluebelts")
png(paste0("figure/GI_types.png"), width=8, height=8, units="in", res=300)
par(mfrow=c(1,1),xpd=FALSE,mar=c(0.5,0.5,0.5,0.5))
plot(bound, bord="grey58")
plot(wbdhu12, add=T)
for (i in 1:length(gitypes)){
  points(GIsites_all[GIsites_all$GItypes==gitypes[i],], pch=16, cex=1.2, col=gitypes.colors[i])
  # if(gitypes[i] %in% c("Detention", "Permeable")){
  #   points(GIsites_all[GIsites_all$GItypes==gitypes[i],], pch=1, cex=1.2, col="grey")
  # }
}
legend(920000, 270000, bty="n", pch=rep(16, 8), col=gitypes.colors, pt.cex=rep(1,8), 
       legend=gitypes, title="SGI types", cex=1.2)
legend(915000, 220000, bty="n",lty = 1, col=c("black", "grey58"),
       legend = c("WBD HU12", "NYC"))
northarrow(c(925050,195000),3500)
add.scale()
dev.off()

type.colors <- gitypes.colors[order(gitypes)]
ggplot(GIsites_all@data[GIsites_all@data$GItypes !="Others",], aes(x=Borough, fill=GItypes)) +
  geom_histogram(position="stack", stat="count")+ scale_fill_manual(values=type.colors)+
  labs(y="SGI quantity", fill="Types", 
       title="SGI types in NYC by borough",
       subtitle="using only constructed sites")
ggsave(paste0("figure/GI_types_borough.png"), width=6, height=5, units="in")

# cso
cso.sgi.df <- read.csv("CSV/cso_vol_events_sgi.csv")
cso.sgi.df<- cso.sgi.df[complete.cases(cso.sgi.df), ]
gam.mod <- glm((Volume + 1) ~ SGIdens, data=cso.sgi.df, family=Gamma())

par(mfrow=c(2,2),xpd=FALSE,mar=c(3.5,2.5,0.5,0.5))
iterations <- 10000
coef.matrix <- matrix(nrow=2, ncol=iterations)
n <- nrow(cso.sgi.df)
for (i in 1:iterations) {
  idx <- sample(n, size=n, replace=T)
  samp.df <- cso.sgi.df[idx, ]
  mod <- lm(log(Volume + 1) ~ SGIdens, data=samp.df)
  coef.matrix[, i] <- coef(mod)
}

(coeff.summary <- apply(coef.matrix, 1, quantile, probs=c(0.025, 0.05, 0.1, 0.9, 0.95, 0.975), nar.rm=T))
hist(coef.matrix[2,])

# main GI types in CSO watershed
cso.shed <- readOGR(dsn=paste0(infolder, "CSO"), layer = "combinedsewer_drainage_area", stringsAsFactors = FALSE)
cso.shed <- spTransform(cso.shed, crs)
plot(cso.shed)
plot(GIsites_all, pch=16, cex=0.5, col="red", add=T)
greinfr <- spTransform(GIsites_all, crs)
greinfr$outfall <- over(greinfr, cso.shed)$outfall
SGItype <- aggregate(GItypes ~ outfall, data=greinfr, function(x) Mode(x))
names(SGItype)[2] <- "SGItype"
cso.shed <- merge(cso.shed, SGItype, by="outfall")
csoloc <- readOGR(dsn="./shapefile", layer="csoloc", stringsAsFactors = FALSE)
cso.shed <- merge(cso.shed, csoloc, by="outfall")
cso.shed.df <- reshape.df(cso.shed@data, 14, 21)
cso.shed.df$SGItype <- rep(cso.shed@data$SGItype,4)

# HU watershed
# read harbor water quality data
dep_hwq <- readOGR(dsn="./shapefile", layer="harbor_water_quality",stringsAsFactors = FALSE)
head(dep_hwq)
dep_hwq <- spTransform(dep_hwq, crs)
wbdhu12$huid <- paste0("hu", seq(1, length(wbdhu12$TNMID)))
dep_hwq$hu12 <- over(dep_hwq, wbdhu12)$huid 
SGI.dens.hu12 <- aggregate(Asset_ID ~ hu12, data=greinfr, function(x) length(x))
names(SGI.dens.hu12) <- c("huid", "SGIdens")
wbdhu12 <- merge(wbdhu12, SGI.dens.hu12, by="huid")
wbdhu12$SGIdens <- wbdhu12$SGIdens/wbdhu12$AREAACRES
names(wbdhu12)[which(names(wbdhu12)=="huid")] <- "hu12"
dep_hwq <- merge(dep_hwq, wbdhu12[,c("hu12", "SGIdens")], by="hu12")
hwq.ent <- subset(dep_hwq, year ==2008 & pre2>0 & Key=="Ent_top" & !is.na(Value))
head(hwq.ent)
hist(log(hwq.ent$Value))
plot(hwq.ent$SGIdens, log(hwq.ent$Value))
