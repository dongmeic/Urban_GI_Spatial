library(tmap)
library(tmaptools)
library(sf)
library(leaflet)
library(dplyr)
library(BAMMtools)
library(raster)
library(grDevices)

setwd("/nfs/urbangi-data/spatial_data/output")
infolder <- "/nfs/urbangi-data/spatial_data/"

wbdhu12 <- readOGR(dsn = paste0(infolder, "WBDHU"), layer = "wbdhu_12", stringsAsFactors = FALSE)
crs <- CRS("+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333
           +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83
           +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")
wbdhu12 <- spTransform(wbdhu12, crs)
bound <- readOGR(dsn = paste0(infolder, "BD"), layer = "nyad_dis")
lonlat <- CRS("+proj=longlat +datum=NAD83")
bound <- spTransform(bound, crs)

nyc.shp <- st_read(paste0(infolder, "BD/nyc_bound.shp"), stringsAsFactors = FALSE)
#gi.shp <- st_read(paste0(infolder, "GI/DEP_GREEN_INFRASTRUCTURE.shp"), stringsAsFactors = FALSE)
gi.shp <- st_read(dsn="./shapefile", layer="GIsites_all", stringsAsFactors = FALSE)
#nyc.shp <- st_transform(nyc.shp, crs = st_crs(gi.shp)$proj4string)
dem <- raster(paste0(infolder,"DEM/dem_mosaic.tif"))
lc2001 <- raster(paste0(infolder,"LC/nlcd_2001_nycad.tif"))
lc2011 <- raster(paste0(infolder,"LC/nlcd_2011_nycad.tif"))
physhs.shp <- st_read(dsn="./shapefile", layer="house_characteristics", stringsAsFactors = FALSE)
eduatt.shp <- st_read(dsn="./shapefile", layer="education", stringsAsFactors = FALSE)
income.shp <- st_read(dsn="./shapefile", layer="income", stringsAsFactors = FALSE)
race.shp <- st_read(dsn="./shapefile", layer="race", stringsAsFactors = FALSE)
agesex.shp <- st_read(dsn="./shapefile", layer="age_sex", stringsAsFactors = FALSE)
disabi.shp <- st_read(dsn="./shapefile", layer="disability", stringsAsFactors = FALSE)
wq.shp <- st_read(dsn="./shapefile", layer="harbor_water_quality", stringsAsFactors = FALSE)

map <- tm_shape(gi.shp)+tm_symbols(size=0.8, col="GItypes", alpha =0.8) + 
  tm_layout(legend.position = c("left","top"), legend.stack = "horizontal")
physhs.shp$old <- as.numeric(physhs.shp$old)

png(paste0("figure/old_housing_units.png"), width=9, height=8, units="in", res=300)
qtm(physhs.shp, fill = "old", format="World", fill.title="", style="col_blind")+
  tm_legend("Old housing units", legend.position = c("left", "top"), legend.text.size = 1.2,
            main.title = "Occupied housing units (1939 or earlier)", legend.title.size = 1.2, 
            main.title.position = "right")
dev.off()

getJenksBreaks(physhs.shp$pctold, 6)
map <- tm_shape(physhs.shp) +
  tm_fill("old", title="", style = "fixed",
          title.size = 1.2,
          breaks = c(0.0, 18.0, 35.1, 51.1, 67.2, 100.0),
          textNA = "Missing",
          colorNA = "white",   # <-------- color for NA values
          palette = c("#f0f9e8",
            "#ccebc5",
            "#a8ddb5",
            "#7bccc4",
            "#43a2ca",
            "#0868ac")) +
  tm_borders() +
  tm_layout("Occupied housing units (1939 or earlier)",
            legend.title.size = 1.2,
            legend.text.size = 1.2,
            legend.position = c("left","top"))
png(paste0("figure/old_housing_units_1.png"), width=9, height=8, units="in", res=300)
par(xpd=FALSE,mfrow=c(1,1),mar=c(0.5,0.5,0.5,0.5))
map
dev.off()

getJenksBreaks(dischar.df$dis.sta, 6)
map <- tm_shape(disabi.shp) +
  tm_fill("dis_sta", title = "Disability status", style = "fixed",
          breaks = c(0.0, 6.5, 10.7, 17.3, 31.0, 57.6),
          textNA = "Missing",
          colorNA = "white",   # <-------- color for NA values
          palette = c("#fef0d9",
            "#fdd49e",
            "#fdbb84",
            "#fc8d59",
            "#e34a33",
            "#b30000")) +
  tm_borders()+tm_shape(gi.shp)+tm_symbols(size=0.5, col="GI_TECHNOL")
 
getJenksBreaks(eduatt.shp$edu_att, 6)
map <- tm_shape(eduatt.shp) +
  tm_fill("edu_att", title = "Educational attainment", style = "fixed",
          breaks = c(0.0, 7.1, 12.4, 20.1, 50.0, 100.0),
          textNA = "Missing",
          colorNA = "white",   # <-------- color for NA values
          palette = c("#edf8fb",
                      "#bfd3e6",
                      "#9ebcda",
                      "#8c96c6",
                      "#8856a7",
                      "#810f7c")) +
  tm_borders()

getJenksBreaks(income.shp$income, 6)
map <- tm_shape(income.shp) +
  tm_fill("income", title = "Income ($1000)", style = "fixed",
          breaks = c(9.3,40.5,63.0,89.8,135.2,232.3),
          textNA = "Missing",
          colorNA = "white",   # <-------- color for NA values
          palette = c("#edf8fb",
            "#bfd3e6",
            "#9ebcda",
            "#8c96c6",
            "#8856a7",
            "#810f7c")) +
  tm_borders()

getJenksBreaks(race.shp$pctwht, 6)
map <- tm_shape(race.shp) +
  tm_fill("pctwht", title = "Percent of white people", style = "fixed",
          breaks = c(0.0,17.1,36.5,57.3,77.4,100.0),
          textNA = "Missing",
          colorNA = "white",   # <-------- color for NA values
          palette = c("#fef0d9",
                      "#fdd49e",
                      "#fdbb84",
                      "#fc8d59",
                      "#e34a33",
                      "#b30000")) +
  tm_borders()

getJenksBreaks(agesex.shp$sex_ratio, 6)
map <- tm_shape(agesex.shp) +
  tm_fill("sex_ratio", title = "Male per 100 females", style = "fixed",
          breaks = c(10.0,95.9,272.1,578.1,986.3,2222.1),
          textNA = "Missing",
          colorNA = "white",   # <-------- color for NA values
          palette = c("#f0f9e8",
                      "#ccebc5",
                      "#a8ddb5",
                      "#7bccc4",
                      "#43a2ca",
                      "#0868ac")) +
  tm_borders()

getJenksBreaks(agesex.shp$oldage_dep, 6)
map <- tm_shape(agesex.shp) +
  tm_fill("oldage_dep", title = "Old age dependency", style = "fixed",
          breaks = c(0.0,20.8,45.8,148.8,371.4,956.2),
          textNA = "Missing",
          colorNA = "white",   # <-------- color for NA values
          palette = c("#fef0d9",
                      "#fdd49e",
                      "#fdbb84",
                      "#fc8d59",
                      "#e34a33",
                      "#b30000")) +
  tm_borders()


getJenksBreaks(agesex.shp$child_dep, 6)
map <- tm_shape(agesex.shp) +
  tm_fill("child_dep", title = "Child dependency", style = "fixed",
          breaks = c(0.7,21.9,34.5,48.5,80.6,171.9),
          textNA = "Missing",
          colorNA = "white",   # <-------- color for NA values
          palette = c("#fef0d9",
                      "#fdd49e",
                      "#fdbb84",
                      "#fc8d59",
                      "#e34a33",
                      "#b30000")) +
  tm_borders()
# map <- tm_shape(dem) +
#   tm_raster("dem_mosaic", palette = topo.colors(6, alpha = 1), title = "Topography")
# map
lf <- tmap_leaflet(map)
lf
