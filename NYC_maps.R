# libraries
library(tmap)
library(sf)
library(leaflet)
library(dplyr)
library(RColorBrewer)

setwd("/nfs/urbangi-data/spatial_data/output")
infolder <- "/nfs/urbangi-data/spatial_data/"

# data
vars.shp <- readOGR(dsn=".", layer="socioeco_vars", stringsAsFactors = FALSE)
#gi.shp <- st_read(paste0(infolder, "GI/DEP_GREEN_INFRASTRUCTURE.shp"), stringsAsFactors = FALSE)
#vars.shp <- st_read("socioeco_vars.shp", stringsAsFactors = FALSE)
#wq.shp <- st_read("harbor_water_quality.shp", stringsAsFactors = FALSE)
# brks.df <- data.frame(dis_sta=c(0.0, 6.5, 10.7, 17.3, 31.0, 57.6),
#                       edu_att=c(0.0, 7.1, 12.4, 20.1, 50.0, 100.0),
#                       sex_ratio=c(10.0,95.9,272.1,578.1,986.3,2222.1),
#                       pctwht=c(0.0,17.1,36.5,57.3,77.4,100.0),
#                       oldage_dep=c(0.0,20.8,45.8,148.8,371.4,956.2),
#                       child_dep=c(0.7,21.9,34.5,48.5,80.6,171.9),
#                       pctold=c(0.0,3.4,8.1,20.9,44.3,88.8),
#                       income=c(9.3,40.5,63.0,89.8,135.2,232.3))

# cols.df <- data.frame(blue=c("#f0f9e8","#ccebc5","#a8ddb5","#7bccc4","#43a2ca","#0868ac"),
# red=c("#fef0d9","#fdd49e","#fdbb84","#fc8d59","#e34a33","#b30000"),
# purple=c("#edf8fb","#bfd3e6","#9ebcda","#8c96c6","#8856a7","#810f7c"))

legtitles <- c("Disability status","Educational attainment","Male per 100 females",
               "Percent of white people","Old age dependency","Child dependency",
               "Percent of old housing units", "Income ($1000)")

# user interface
in1 <- selectInput(inputId = 'map_topics',
                   label = 'Socioeconomic variables',
                   choices = c("dis_sta", "edu_att", "sex_ratio", "pctwht", "oldage_dep", "child_dep", "pctold", "income"))
# in2 <- checkboxGroupInput(inputId ="color",
#                           label = "Color options",
#                           choices = list("blue" = 1,
#                                           "red" = 2,
#                                          "purple" = 3),
#                           selected = 1)
# c("Disability", "Education", "Gender", "House units", "Income", "Race", "Elder", "Children")
# in3 <- selectInput(inputId = 'gi_type',
#                    label = 'Green infrastructure types',
#                    choices = unique(gi.shp[['GI_TECHNOL']]))
# in4 <- selectInput(inputId = 'wq_para',
#                    label = 'Water quality parameters',
#                    choices = unique(wq.shp[['Key']]))
# in5 <- checkboxGroupInput(inputId ="checkpoint", 
#                    label = "Add a point layer", 
#                    choices = list("Green infrastructures" = 1, 
#                                   "Water quality sampling sites" = 2, "None" = 3),
#                    selected = 3)
# in6 <- checkboxGroupInput("checkraster", 
#                           label = "Add a raster layer", 
#                           choices = list("Topography" = 1, 
#                                          "Land cover 2001" = 2, 
#                                          "Land cover 2011" = 3, 
#                                          "None" = 4),
#                           selected = 4)
side <- sidebarPanel('Options', in1)
out1 <- plotOutput('map')
main <- mainPanel(out1)
tab <- tabPanel(title = 'Thematic maps for the socioeconomic status in the NYC',
                sidebarLayout(side,main))
ui <- navbarPage('Urban GI Project', tab)

# Server
server <- function(input, output) {
  # var <- reactive({
  #   input$map_topics
  # })
  # legtitle <- reactive({
  #   legtitles[which(names(brks.df)== input$map_topics)]
  # })
  output$map <- renderLeaflet({
    map <- tm_shape(vars.shp) +
      tm_fill(input$map_topics,
              style="jenks",
              textNA = "Missing",
              colorNA = "white") +
      tm_borders()
    tmap_leaflet(map)
  })
}

# Create the Shiny App
shinyApp(ui = ui, server = server)