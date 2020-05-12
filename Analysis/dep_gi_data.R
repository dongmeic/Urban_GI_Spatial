library(rgdal)
inpath <- "/Users/dongmeichen/Documents/All/UrbanGI"
gis.path <- "/Users/dongmeichen/Documents/All/UrbanGI/GIS_data"
gi.file <- "DEP_GI_Assets"

# read GI shapefiles
gi.shp <- readOGR(dsn = paste0(inpath, "/DEP"), layer = gi.file, stringsAsFactors = FALSE)
names(gi.shp)
head(gi.shp)
unique(gi.shp$Asset_Type)
gi.web <- readOGR(dsn = paste0(gis.path, "/DEP_GREEN_INFRASTRUCTURE.shp"), 
                  layer = "DEP_GREEN_INFRASTRUCTURE", stringsAsFactors = FALSE)
names(gi.web)
head(gi.web)
unique(gi.web$GI_TECHNOL)

# reorganize the GI types
bluebelt <- c("Bluebelt","Constructed Wetland")
raingarden <- c("Rain Garden","ROW Rain Garden", "ROWRG")
bioswale <- c("ROWB", "ROWEB","Bioswale")
greenroof <- c("Combined Blue/Green Roof", "Green Roof","Rooftop Farm")
permeable <- c("Permeable Pavers", "Pervious Concrete","Porous Asphalt",
               "Porous Concrete", "ROW Permeable Pavers", "ROW Porous Concrete",
               "ROW Structural Soil","Gravel Bed", "Pervious Pavement")
detention <- c("Blue Roof", "Detention System (Connected to Sewer)", 
               "Detention System", "Engineered Soil Tree Pit", 
               "ROW Infiltration Basin with Concrete Top", 
               "ROW Infiltration Basin with Grass Top", 
               "ROW Stormwater Seepage Basin", "ROW Porous Asphalt", 
               "ROW Infiltration Basin with Combination of Concrete and Grass Top", 
               "ROW Subsurface Pipe/Broken Stone", "Subsurface Detention System", 
               "Subsurface Pipe", "Synthetic Turf Field Storage Layer", "Tree Pit")
greenstreet <- c("ROWGS", "ROWSGS")
rainbarrel <- c("Rainwater Harvesting","Cistern","Rain Barrel","Rainwater Reuse System")

gi.shp$GItypes <- ifelse(gi.shp$Asset_Type %in% bluebelt, "Bluebelts", 
                         ifelse(gi.shp$Asset_Type %in% raingarden, "Rain gardens",
                                ifelse(gi.shp$Asset_Type %in% bioswale, "Bioswales",
                                       ifelse(gi.shp$Asset_Type %in% greenroof, "Green roofs",
                                              ifelse(gi.shp$Asset_Type %in% permeable, "Permeable",
                                                     ifelse(gi.shp$Asset_Type %in% detention, "Detention",
                                                            ifelse(gi.shp$Asset_Type %in% greenstreet, "Green streets", 
                                                                   ifelse(gi.shp$Asset_Type %in% rainbarrel, "Rain barrels","Others"))))))))
gi.web$GItypes <- ifelse(gi.web$GI_TECHNOL %in% bluebelt, "Bluebelts", 
                         ifelse(gi.web$GI_TECHNOL %in% raingarden, "Rain gardens",
                                ifelse(gi.web$GI_TECHNOL %in% bioswale, "Bioswales",
                                       ifelse(gi.web$GI_TECHNOL %in% greenroof, "Green roofs",
                                              ifelse(gi.web$GI_TECHNOL %in% permeable, "Permeable",
                                                     ifelse(gi.web$GI_TECHNOL %in% detention, "Detention",
                                                            ifelse(gi.web$GI_TECHNOL %in% greenstreet, "Green streets", 
                                                                   ifelse(gi.web$GI_TECHNOL %in% rainbarrel, "Rain barrels","Others"))))))))
# merge both data

