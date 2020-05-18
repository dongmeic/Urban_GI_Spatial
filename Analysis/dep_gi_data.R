library(rgdal)
inpath <- "/Users/dongmeichen/Documents/All/UrbanGI"
gis.path <- "/Users/dongmeichen/Documents/All/UrbanGI/GIS_data"
gi.file <- "DEP_GI_Assets"

# read GI shapefiles
gi.shp <- readOGR(dsn = paste0(inpath, "/DEP"), layer = gi.file, stringsAsFactors = FALSE)
names(gi.shp)
head(gi.shp)
unique(gi.shp$Asset_Type)

# reorganize the GI types
bluebelt <- c("Bluebelt","Constructed Wetland")
bioswale <- c("ROWB", "ROWEB","Bioswale", "Engineered Soil Tree Pit", "Tree Pit", "ROWGS", "ROWSGS")
raingarden <- c("Rain Garden","ROW Rain Garden", "ROWRG", bioswale)
greenroof <- c("Combined Blue/Green Roof", "Green Roof","Rooftop Farm", "Blue Roof")
permeable <- c("Permeable Pavers", "Pervious Concrete","Porous Asphalt",
               "Porous Concrete", "ROW Permeable Pavers", "ROW Porous Concrete",
               "ROW Structural Soil","Gravel Bed", "Pervious Pavement",
               "ROW Permeable Pavement")
detention <- c("Detention System (Connected to Sewer)", 
               "Detention System", "ROW Infiltration Basin with Concrete Top", 
               "ROW Infiltration Basin with Grass Top", 
               "ROW Stormwater Seepage Basin", "ROW Porous Asphalt", 
               "ROW Infiltration Basin with Combination of Concrete and Grass Top", 
               "ROW Subsurface Pipe/Broken Stone", "Subsurface Detention System", 
               "Subsurface Pipe", "Synthetic Turf Field Storage Layer", "Subsurface Storage",
               "Subsurface Pipe/Broken Stone")
rainbarrel <- c("Rainwater Harvesting","Cistern","Rain Barrel","Rainwater Reuse System")

gi.shp$GI_Type <- ifelse(gi.shp$Asset_Type %in% bluebelt, "Bluebelt",
                         ifelse(gi.shp$Asset_Type %in% raingarden, "Rain garden",
                                #ifelse(gi.shp$Asset_Type %in% bioswale, "Bioswales",
                                ifelse(gi.shp$Asset_Type %in% greenroof, "Green roof",
                                       ifelse(gi.shp$Asset_Type %in% permeable, "Porous pavement",
                                              ifelse(gi.shp$Asset_Type %in% detention, "Subsurface infiltration",
                                                     ifelse(gi.shp$Asset_Type %in% rainbarrel, "Rain barrel","Other"))))))

# get longitude and latitude
lonlat <- CRS("+proj=longlat +datum=NAD83")
gi.shp.lonlat <- spTransform(gi.shp, lonlat)
gi.shp$Longitude <- coordinates(gi.shp.lonlat)[, 1]
gi.shp$Latitude <- coordinates(gi.shp.lonlat)[, 2]
gi.shp <- gi.shp[gi.shp$Status_Gro == "Constructed",]
selected_vars <- c("Longitude", "Latitude", "Asset_ID", "Asset_Type", "Asset_Area", "GI_Type", 
                   "Sewer_Type", "Outfall", "Waterbody", "Borough")
gi.shp.cleaned <- gi.shp[, selected_vars]
#names(gi.shp.cleaned)[which(names(gi.shp.cleaned) == "Status_Gro")] <- "Status"
head(gi.shp.cleaned)
writeOGR(gi.shp.cleaned, dsn = "/Users/dongmeichen/Documents/All/UrbanGI/DEP", layer = "DEP_GI_Map2020",
         driver = "ESRI Shapefile", overwrite_layer = TRUE)