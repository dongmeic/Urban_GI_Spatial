# libraries
library(shiny)
library(rgdal)
library(tmap)
library(leaflet)
library(knitr)
# library(devtools)
# devtools::install_github("rstudio/leaflet")

# # data
# setwd("/nfs/urbangi-data/spatial_data/output")
# save(list = ls(all.names = TRUE), file = "cso_wq_gi.RData", envir = .GlobalEnv)
# infolder <- "/nfs/urbangi-data/spatial_data/"
# crs <- CRS("+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333
#            +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83
#            +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")
# NYCBoundary <- readOGR(dsn=paste0(infolder, "BD"),layer="nyad_dis", stringsAsFactors = FALSE)
# gi <- readOGR(dsn = "./shapefile", layer ="DEP_GI_withDA_042218", stringsAsFactors = FALSE)
# names(gi)[which(names(gi)=="mtgtn_2")] <- "MitigatedArea"
# names(gi)[which(names(gi)=="hu12")] <- "WatershedBoundary"
# names(gi)[which(names(gi)=="sewrshd")] <- "Sewershed"
# head(gi@data)
# ContractArea <- readOGR(dsn = paste0(infolder, "GI"), layer ="GI_Contract_Areas_Sept2017", stringsAsFactors = FALSE)
# head(ContractArea)
# CSOWatershed <- readOGR(dsn=paste0(infolder, "CSO"), layer = "combinedsewer_drainage_area", stringsAsFactors = FALSE)
# WatershedBoundary <- readOGR(dsn = paste0(infolder, "WBDHU"), layer = "wbdhu_12", stringsAsFactors = FALSE)
# WatershedBoundary$huid <- paste0("hu", seq(1, length(WatershedBoundary$TNMID)))
# WatershedBoundary <- spTransform(WatershedBoundary, crs)
# hwq <- readOGR(dsn="./shapefile",layer = "harbor_water_quality", stringsAsFactors = FALSE)
# hwq <- spTransform(hwq, crs)
# hwq$year <- as.numeric(hwq$year)
# hwq$hu12 <- over(hwq, WatershedBoundary)$huid
# monthly_cso <- readOGR(dsn = "./shapefile",layer = "monthly_cso", stringsAsFactors = FALSE)
# CSOoutfall <- readOGR(dsn ="./shapefile",layer="csoloc", stringsAsFactors = FALSE)
# ClimateStations <- readOGR(dsn = "./shapefile",layer = "climate_stations", stringsAsFactors = FALSE)
# WasteWaterTreatmentPlants <- readOGR(dsn = paste0(infolder, "CSO/wastewater_treatment_plants"),
#                                      layer = "wastewater_treatment_plants", stringsAsFactors = FALSE)
# PriorityCSOWatershed <- readOGR(dsn=paste0(infolder, "watershed"),layer="priority_cso_watersheds", stringsAsFactors = FALSE)
# GIPilotPrograms <- readOGR(dsn = paste0(infolder, "GI"),layer = "GI_pilots", stringsAsFactors = FALSE)

knitr::knit(text ='```{r}
            load(url("https://github.com/dongmeic/Urban_GI_Spatial/blob/master/cso_wq_gi/cso_wq_gi.RData?raw=true"))
            ```')

# user interface
choices.br <- c("Select All", unique(gi$Borough))
choices.gi <- c("Select All", unique(gi$GItypes))

in1 <- checkboxGroupInput(inputId = "Borough", 
                          label = "Borough", 
                          choices = choices.br, 
                          selected = "Brooklyn", 
                          inline = TRUE)

in2 <- selectInput(inputId = 'GItypes',
                   label = 'SGI types',
                   choices = choices.gi,
                   selected = "Bioswales",
                   multiple = TRUE)

colorshow.types <- c('Borough', 'GItypes', 'MitigatedArea','WatershedBoundary', 'Sewershed')
in3 <- selectInput(inputId = 'colorshow',
                   label = 'Check GI variables',
                   choices = colorshow.types,
                   selected = "GItypes")
in4 <- checkboxGroupInput(inputId = "WQkey", 
                          label = "Water quality indicators (select one)", 
                          choices = unique(hwq$Key), 
                          selected = "Tra", 
                          inline = TRUE)
in5 <- sliderInput('years',
                   label = 'Year range for harbor water quality',
                   min = 2008,
                   max = 2016,
                   value = c(2016, 2016),
                   sep = "")
in6 <- checkboxGroupInput('keyreg_years',
                          label = 'Year range for key regulators',
                          choices = c(2015,2016),
                          selected = 2016, inline = TRUE)
in7 <- sliderInput('months',
                   label = 'Month range',
                   min = 1,
                   max = 12,
                   value = c(1, 12))
cso_oufall_var <- c("volume_13", "events_13", "volume_14", "events_14", "volume_15", 
                    "events_15", "volume_16", "events_16", "Sewershed", "outfall")
cso.legtitles <- c("CSO volume in 2013", "CSO events in 2013", "CSO volume in 2014",
                   "CSO events in 2014", "CSO volume in 2015", "CSO events in 2015",
                   "CSO volume in 2016", "CSO events in 2016", "Sewershed", "Outfall ID")
in8 <- selectInput(inputId = 'CSOoutfall',
                   label = 'Check CSO yearly outfall',
                   choices = cso_oufall_var,
                   selected = "events_16")

gi.legtitles <- c('Borough', 'Green infrastructure types', 'Mitigated area', 'Watershed boundary', 'Sewershed')
hwq.indicators <- c("Ent_top", "DO_bot", "FC_top", "Tra", "FC_bot", "DO_top", "Ent_bot")
hwq.legtitles <- c("Enterococcus (top)", "Dissolved oxygen (bottom)", 
                   "Fecal coliform (top)", "Transparency", "Fecal coliform (bottom)",
                   "Dissolved oxygen (top)", "Enterococcus (bottom)")
ui <- bootstrapPage(
  title = "Urban GI Project - Map of green infrastructures from NYC DEP",
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  absolutePanel(top = 20, right = 30, draggable = TRUE, in1, in2, in3, in4, in5, in6, in7, in8),
  leafletOutput("map", width = "70%", height = "100%")
)
## server
server <- function(input, output, session) {
  
  observe({
    if ("Select All" %in% input$Borough) {
      # choose all the choices _except_ "Select All"
      selected_choices <- setdiff(choices.br, "Select All")
      updateSelectInput(session, "Borough", selected = selected_choices)
    }
    
    if ("Select All" %in% input$GItypes) {
      # choose all the choices _except_ "Select All"
      selected_choices <- setdiff(choices.gi, "Select All")
      updateSelectInput(session, "GItypes", selected = selected_choices)
    }
  })
  
  GreenInfrastructure <- reactive({
    gi <- gi[gi$Borough %in% input$Borough,]
    gi <- gi[gi$GItypes %in% input$GItypes,]
  })
  
  WaterQuality <- reactive({
    hwq <- hwq[hwq$Key == input$WQkey & hwq$year %in% input$years & hwq$month %in% input$months,]
  })
  
  KeyRegulators <- reactive({
    monthly_cso <- monthly_cso[monthly_cso$Year %in% input$keyreg_years & monthly_cso$Month %in% input$months,]
  })
  
  output$map <- renderLeaflet({
    tmap_leaflet(tm_shape(NYCBoundary)+tm_borders("grey20")+tm_shape(ContractArea)+
                   tm_polygons("Area_ID", palette="Set3", legend.show = FALSE)+
                   tm_shape(CSOWatershed)+tm_borders("brown")+
                   tm_shape(WatershedBoundary)+tm_borders("blue")+
                   tm_shape(PriorityCSOWatershed)+tm_borders(col = "red", lwd = 2)+
                   tm_shape(WasteWaterTreatmentPlants)+tm_symbols(col = "black", shape = 22, size=0.5)+
                   tm_shape(ClimateStations)+tm_symbols(col = "cyan", shape = 22, size=0.3)+
                   tm_shape(WaterQuality())+
                   tm_dots(title = hwq.legtitles[which(hwq.indicators==input$WQkey)], size = 0.2,col="Value", palette = "Blues")+
                   tm_shape(KeyRegulators())+
                   tm_dots(title = "Monthly CSO events", size=0.1, col="Events", palette = "Reds")+
                   tm_shape(CSOoutfall)+
                   tm_dots(title = cso.legtitles[which(cso_oufall_var==input$CSOoutfall)], size=0.05, col=input$CSOoutfall, palette = "RdPu")+
                   tm_shape(GIPilotPrograms)+tm_symbols(border.col="green", col = "lightgreen", size = 0.4)+
                   tm_shape(GreenInfrastructure())+
                   tm_dots(title = gi.legtitles[which(colorshow.types==input$colorshow)],size = 0.08, col = input$colorshow))
    
  })
  
}

shinyApp(ui, server)