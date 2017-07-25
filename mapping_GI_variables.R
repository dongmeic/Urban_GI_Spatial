# This script is created to map urban green infratructures and the relevant variables in the New York City
# - Digital elevation model of 5 boroughs/NYC (They are Manhattan, the Bronx, Queens, Brooklyn, and Staten Island)
# - Combined sewer outflow locations points
# - CSO drainage (sewersheds) of NYC
# - Water quality sampling points of NY harbor
# - Stormwater green infrastructure location points
# data is saved on https://files.sesync.org/pydio/ws-urbangi/spatial_data

# created by Dongmei Chen
# date: 07-18-17

# libraries
library(maptools)
library(GISTools)
library(raster)
library(rgdal)
library(plotrix)
library(rasterVis)
library(lattice)
library(classInt)

setwd("/nfs/urbangi-data/spatial_data/output")
infolder <- "/nfs/urbangi-data/spatial_data/"

# projection and roi(region of interest)
crs <- CRS("+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333 
           +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83 
           +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")
lonlat <- CRS("+proj=longlat +datum=NAD83")
utm <- CRS("+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
rproj <- CRS("+proj=utm +zone=18 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
# extent is set as the boundary
e <- extent( c(913175.1, 1067383, 120121.9, 272844.3) )
p <- as(e, 'SpatialPolygons')
crs(p) <- utm
shapefile(p, 'file.shp', overwrite=TRUE)  

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
  GISTools::map.scale(1040000,140000,19685.083,"km",3,2,sfcol='brown')
} #19685 feet 0.5 inches equals to 6 km

# require census tract boundary, green infrastructure points, and NYC boundary
quick.map <- function(spdf,var,legend.title,main.title,color,outname,style) {
  x <- as.numeric(spdf@data[,var])
  if (style == "jenks"){
    plotvar <- x
    nclr <- 8
    plotclr <- brewer.pal(nclr,color)
    class <- classIntervals(plotvar, nclr, style="jenks", dataPrecision=1)
    colcode <- findColours(class, plotclr)
    png(paste(outname,".png", sep=""), width=9, height=8, units="in", res=300)
    par(xpd=FALSE,mfrow=c(1,1),mar=c(0.5,0.5,2.5,0.5))
    if (var == "Impervious" | var == "stormwat_1"){
      plot(bound)
      plot(spdf, col=colcode, add=T)
      plot(greinfr, col="#4daf4a", add=T, pch=16, cex=0.5)
    }else if (class(spdf)[1] == "SpatialPolygonsDataFrame"){
      plot(spdf, col=colcode)
    }else if (class(spdf)[1] == "SpatialPointsDataFrame"){
      plotclr <- plotclr[nclr:1]
      class <- classIntervals(plotvar, nclr, style="jenks", dataPrecision=1)
      colcode <- findColours(class, plotclr)
      plot(tract,border='darkgrey')
      plot(spdf, add=TRUE,col=colcode,pch=16, cex=1.5)
      #points(spdf@coords[,1], spdf@coords[,2], cex=1.5)
    }else{
      cat("The spatial dataframe is not supported.")
    }
    title(main.title,cex.main=1.5)
    legend(925000,238000, legend=names(attr(colcode, "table")),
           fill=attr(colcode, "palette"), cex=1.2, title=legend.title, bty="n")
    add.northarrow()
    add.scale()
    dev.off()
  } else if (style == "quantileCuts") {
    shades <- auto.shading(na.omit(x), cutter = quantileCuts, n=8, cols=brewer.pal(8, color))
    png(paste(outname,".png", sep=""), width=9, height=8, units="in", res=300)
    par(xpd=FALSE,mfrow=c(1,1),mar=c(0.5,0.5,2.5,0.5))
    choropleth(spdf,x,shades)
    title(main.title,cex.main=1.5)
    choro.legend(925000,238000, shades, bty='n', cex=1.2, title=legend.title)
    add.northarrow()
    add.scale()
    dev.off()
  }else{
    cat("The style is either \"quantileCuts\" (only for spatial polygon) or \"jenks\". Please try again.")
  }
}

# convert dataframe to spatial dataframe
df2spdf <- function(col1, col2, colname1, colname2, df){
  xy <- data.frame(df[,c(col1,col2)])
  coordinates(xy) <- c(colname1, colname2)
  proj4string(xy) <- lonlat
  xy.n <- spTransform(xy, crs)
  spdf <- SpatialPointsDataFrame(coords = xy.n, data = df, proj4string = crs)
  return(spdf)
}

# input CSV files
wqsploc.file <- paste0(infolder,"WQ/harbor_sampling_coordinates.csv")
physhs.file <- paste0(infolder,"SE/ACS_13_5YR_S2504_with_ann.csv")
race.file <- paste0(infolder,"SE/ACS_14_5YR_B02001_with_ann.csv")
income.file <- paste0(infolder,"SE/ACS_14_5YR_B19013_with_ann.csv")
agesex.file <- paste0(infolder,"SE/ACS_14_5YR_S0101_with_ann.csv")
eduatt.file <- paste0(infolder,"SE/ACS_14_5YR_S1501_with_ann.csv")
dischar.file <- paste0(infolder,"SE/ACS_14_5YR_S1810_with_ann.csv")

# read shapefiles
bound <- readOGR(dsn=paste0(infolder, "BD"), layer ="nyad_dis", stringsAsFactors=FALSE)
proj4string(bound) <- crs
tract <- readOGR(dsn=paste0(infolder, "BD"), layer ="cb_2015_36_tract_500k_clip_proj", stringsAsFactors=FALSE)
proj4string(tract) <- crs
bg <- readOGR(dsn=paste0(infolder, "BD"), layer ="cb_2015_36_bg_500k_clip_proj", stringsAsFactors=FALSE)
proj4string(bg) <- crs
csoloc <- readOGR(dsn=paste0(infolder, "CSO"), layer ="CSO_locations_clip_proj", stringsAsFactors=FALSE) 
proj4string(csoloc) <- crs
csodra <- readOGR(dsn=paste0(infolder, "CSO"), layer ="combinedsewer_drainage_area_proj", stringsAsFactors=FALSE)
proj4string(csodra) <- crs
sewershed <- readOGR(dsn=paste0(infolder, "CSO"), layer ="Sewershed", stringsAsFactors=FALSE)
proj4string(sewershed) <- crs
greinfr <- readOGR(dsn=paste0(infolder, "GI"), layer ="DEP_GREEN_INFRASTRUCTURE", stringsAsFactors=FALSE)
greinfr <- spTransform(greinfr,crs)

dem <- raster(paste0(infolder,"DEM/dem_mosaic.tif"))
mapTheme <- rasterTheme(region = rev(brewer.pal(7, "BrBG")))
bound_proj <- spTransform(bound, rproj)
png("dem.png", width=9, height=8, units="in", res=300)
par(xpd=FALSE,mfrow=c(1,1),mar=c(0.5,0.5,0.5,0.5))
cutpts <- c(5, 15, 30, 45, 50, 80, 110, 130)
levelplot(dem, margin = FALSE, at=cutpts, cuts=8, pretty=TRUE, par.settings = mapTheme)+layer(sp.polygons(bound_proj, lwd=0.8, col='gray'))
dev.off()

# to check different groups of elevations
brk <- c(0, 20, 80, 120)
arg <- list(at=c(10,50,100), labels=c("Low","Med.","High"))
plot(dem, col=terrain.colors(3), breaks=brk, axis.args=arg)
add.northarrow()
add.scale()

# read socioeconomic data
physhs <- read.csv(physhs.file, stringsAsFactors = FALSE)
physhs.df <- physhs[,c("GEO.id2","HC01_EST_VC01","HC01_EST_VC17")]
colnames(physhs.df) <- c("GEOID","total","old")
physhs.df$pctold <- suppressWarnings(with(physhs.df, ifelse(is.na(total) | is.na(old), NA, round((as.numeric(old)/as.numeric(total)*100), digit=1))))
physhs.df$pctold <- with(physhs.df, ifelse(pctold > 100, NA, pctold))
physhs.spdf <- merge(tract, physhs.df, by="GEOID", all.x =FALSE)
writeOGR(physhs.spdf, dsn=".", layer="house_characteristics", overwrite_layer = TRUE,driver = "ESRI Shapefile")

race <- read.csv(race.file, stringsAsFactors = FALSE)
race.df <- race[,c("GEO.id2","HD01_VD01","HD01_VD02")]
colnames(race.df) <- c("GEOID","total","white")
race.df$pctwht <- suppressWarnings(with(race.df, round((as.numeric(white)/as.numeric(total)*100), digit=1)))
race.spdf <- merge(tract, race.df, by="GEOID", all.x =FALSE)
race.spdf$pop <- as.numeric(race.spdf$total)/1000
writeOGR(race.spdf, dsn=".", layer="race", overwrite_layer = TRUE,driver = "ESRI Shapefile")

income <- read.csv(income.file, stringsAsFactors = FALSE)
income.df <- income[,c("GEO.id2","HD01_VD01")]
colnames(income.df) <- c("GEOID","income")
income.df$income <- suppressWarnings(round(as.numeric(income.df$income)/1000,digits = 1))
income.spdf <- merge(tract, income.df, by="GEOID", all.x =FALSE)
writeOGR(income.spdf, dsn=".", layer="income", overwrite_layer = TRUE,driver = "ESRI Shapefile")

agesex <- read.csv(agesex.file, stringsAsFactors = FALSE)
agesex.df <- agesex[,c("GEO.id2","HC01_EST_VC36", "HC01_EST_VC38", "HC01_EST_VC39")]
colnames(agesex.df) <- c("GEOID","sex.ratio","oldage.dep","child.dep")
agesex.df$sex.ratio <- suppressWarnings(as.numeric(agesex.df$sex.ratio))
agesex.df$oldage.dep <- suppressWarnings(as.numeric(agesex.df$oldage.dep))
agesex.df$child.dep <- suppressWarnings(as.numeric(agesex.df$child.dep))
agesex.spdf <- merge(tract, agesex.df, by="GEOID", all.x =FALSE)
writeOGR(agesex.spdf, dsn=".", layer="age_sex", overwrite_layer = TRUE,driver = "ESRI Shapefile")

eduatt <- read.csv(eduatt.file, stringsAsFactors = FALSE) 
eduatt.df <- eduatt[,c("GEO.id2","HC01_EST_VC50")]
colnames(eduatt.df) <- c("GEOID","edu.att")
eduatt.df$edu.att <- suppressWarnings(as.numeric(eduatt.df$edu.att))
eduatt.spdf <- merge(tract, eduatt.df, by="GEOID", all.x =FALSE)
writeOGR(eduatt.spdf, dsn=".", layer="education", overwrite_layer = TRUE,driver = "ESRI Shapefile")

dischar <- read.csv(dischar.file, stringsAsFactors = FALSE)
dischar.df <- dischar[,c("GEO.id2","HC01_EST_VC48")]
colnames(dischar.df) <- c("GEOID","dis.sta")
dischar.df$dis.sta <- suppressWarnings(as.numeric(dischar.df$dis.sta))
dischar.spdf <- merge(tract, dischar.df, by="GEOID", all.x =FALSE)
writeOGR(dischar.spdf, dsn=".", layer="disability", overwrite_layer = TRUE,driver = "ESRI Shapefile")

# read water quality data
wqspts <- read.csv(wqsploc.file)
wqspts.spdf <- df2spdf(4,3,"Long","Lat",wqspts)

# mapping
# make a complete map
png(paste("mainmap.png", sep=""), width=9, height=8, units="in", res=300)
par(xpd=FALSE,mfrow=c(1,1),mar=c(1.5,0.5,2.5,0.5))
plot(bound, main="Green Infrastructure in New York City")
plot(csodra,col="#984ea3", border ="darkgrey",add=TRUE)
plot(wqspts.spdf, add=TRUE, col="#377eb8", pch=16, cex=1.8)
plot(csoloc, add=TRUE, col="#e41a1c", pch=16, cex=1.3)
plot(greinfr, add=TRUE, col="#4daf4a", pch=16, cex=0.8)
add.northarrow()
add.scale()
text(cbind(912000, 250000), family="Arial Black", "Legend", cex=1.2)
draw.circle(910000, 220000, 1600, col="#377eb8", border = "#377eb8")
text(cbind(937800, 220000), "WQ sampling locations")
draw.circle(910000, 230000, 1156, col="#e41a1c", border = "#e41a1c")
text(cbind(930000, 230000), "CSO locations")
draw.circle(910000, 240000, 711, col="#4daf4a", border = "#4daf4a")
text(cbind(929000, 240000), "SGI locations")
rect(907000, 206000, 913000, 210000, col = "#984ea3", border = "darkgrey")
text(cbind(936000, 208000), "CSO drainage area")
dev.off()

# check infrastructure data
quick.map(csoloc,"volume_15", "CSO volume in 2015", "Combined sewer overflow locations in NYC", "BuPu", "csovolume", "jenks")
quick.map(sewershed,"Impervious", "% impervious", "Impervious percentage by sewershed in NYC", "BuPu", "impervious", "jenks")
quick.map(csodra,"stormwat_1", "Stormwater vol.", "Stormwater volume by CSO drainage area in NYC", "BuPu", "stormwater", "jenks")

# check socioeconomic data
quick.map(physhs.spdf,"pctold", "% old house units", "Percentage of old house units in NYC", "Reds", "oldhouse", "jenks")
quick.map(race.spdf,"pop", "Population (1000)", "Total population in NYC", "Reds", "population", "jenks")
quick.map(race.spdf,"pctwht", "% white people", "Percentage of white people in NYC", "Blues", "whitepeople", "quantileCuts")
quick.map(income.spdf,"income", "Income ($1000)", "Median household income in NYC", "Reds", "income", "jenks")
quick.map(agesex.spdf,"sex.ratio", "M per 100 F", "Sex ratio in NYC", "Blues", "sexratio", "quantileCuts")
quick.map(agesex.spdf,"oldage.dep", "Old-age depend.", "Old-age dependency in NYC", "Blues", "oldage", "quantileCuts")
quick.map(agesex.spdf,"child.dep", "Child depend.", "Child dependencey in NYC", "Blues", "child", "quantileCuts")
quick.map(eduatt.spdf,"edu.att", "Edu. attainment", "Percent with educational attainment in NYC", "Reds", "education", "jenks")
quick.map(dischar.spdf,"dis.sta", "Disability sta.", "Percent with disability status in NYC", "Reds", "disability", "jenks")
