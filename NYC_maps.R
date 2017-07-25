# libraries
library(tmap)
library(sf)
library(leaflet)
library(dplyr)

setwd("/nfs/urbangi-data/spatial_data/output")
infolder <- "/nfs/urbangi-data/spatial_data/"

# data
gi.shp <- st_read(paste0(infolder, "GI/DEP_GREEN_INFRASTRUCTURE.shp"), stringsAsFactors = FALSE)
physhs.shp <- st_read("house_characteristics.shp", stringsAsFactors = FALSE)
eduatt.shp <- st_read("education.shp", stringsAsFactors = FALSE)
income.shp <- st_read("income.shp", stringsAsFactors = FALSE)
race.shp <- st_read("race.shp", stringsAsFactors = FALSE)
agesex.shp <- st_read("age_sex.shp", stringsAsFactors = FALSE)
disabi.shp <- st_read("disability.shp", stringsAsFactors = FALSE)
wq.shp <- st_read("harbor_water_quality.shp", stringsAsFactors = FALSE)


# user interface
in1 <- selectInput(inputId = 'map_topics',
                   label = 'Socioeconomic variables',
                   choices = c("Disability", "Education", "Gender", "House units", "Income", "Race", "Elder", "Children"))
# in2 <- selectInput(inputId = 'gi_type',
#                    label = 'Green infrastructure types',
#                    choices = unique(gi.shp[['GI_TECHNOL']]))
# in3 <- selectInput(inputId = 'wq_para',
#                    label = 'Water quality parameters',
#                    choices = unique(wq.shp[['Key']]))
# in4 <- checkboxGroupInput(inputId ="checkpoint", 
#                    label = "Add a point layer", 
#                    choices = list("Green infrastructures" = 1, 
#                                   "Water quality sampling sites" = 2, "None" = 3),
#                    selected = 3)
# in5 <- checkboxGroupInput("checkraster", 
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
                sidebarLayout(side, main))
ui <- navbarPage('Urban GI Project', tab)

# Server
server <- function(input, output) {
  # reactive({
  #   var <- input$map_topics
  # })
  #   if (var == "Disability"){
  #     output[['map']] <- renderLeaflet(leaflet()  %>% tm_shape(disabi.shp) +
  #                                                  tm_fill("dis_sta", title = "Disability status", style = "fixed",
  #                                                          breaks = c(0.0, 6.5, 10.7, 17.3, 31.0, 57.6),
  #                                                          textNA = "Missing",
  #                                                          colorNA = "white",   # <-------- color for NA values
  #                                                          palette = c("#fef0d9",
  #                                                                      "#fdd49e",
  #                                                                      "#fdbb84",
  #                                                                      "#fc8d59",
  #                                                                      "#e34a33",
  #                                                                      "#b30000")) +
  #                                                  tm_borders() # %>% if (input[['checkpoint']]==1) tm_shape(gi.shp %>%
  #                                                  #                                                             filter(GI_TECHNOL == input[['gi_type']]))+tm_symbols(size=0.5, col="green") else if (input[['checkpoint']]==2) 
  #                                                  #                                                               tm_shape(wq.shp %>% filter(Key == input[['wq_para']]))+tm_symbols(size=0.5, col="red")
  #     )
  #   }else if(var == "Education"){
  #     output[['map']] <- renderLeaflet(leaflet()  %>% tm_shape(eduatt.shp) +
  #                                                  tm_fill("edu_att", title = "Educational attainment", style = "fixed",
  #                                                          breaks = c(0.0, 7.1, 12.4, 20.1, 50.0, 100.0),
  #                                                          textNA = "Missing",
  #                                                          colorNA = "white",   # <-------- color for NA values
  #                                                          palette = c("#edf8fb",
  #                                                                      "#bfd3e6",
  #                                                                      "#9ebcda",
  #                                                                      "#8c96c6",
  #                                                                      "#8856a7",
  #                                                                      "#810f7c")) +
  #                                                  tm_borders()
  #     )    
  #   }else if(var == "Gender"){
  #     output[['map']] <- renderLeaflet(leaflet()  %>% tm_shape(agesex.shp) +
  #                                                  tm_fill("sex_ratio", title = "Male per 100 females", style = "fixed",
  #                                                          breaks = c(10.0,95.9,272.1,578.1,986.3,2222.1),
  #                                                          textNA = "Missing",
  #                                                          colorNA = "white",   # <-------- color for NA values
  #                                                          palette = c("#f0f9e8",
  #                                                                      "#ccebc5",
  #                                                                      "#a8ddb5",
  #                                                                      "#7bccc4",
  #                                                                      "#43a2ca",
  #                                                                      "#0868ac")) +
  #                                                  tm_borders() 
  #     )    
  #   }else if(var == "House units"){
  #     output[['map']] <- renderLeaflet(leaflet()  %>% tm_shape(physhs.shp) +
  #                                                  tm_fill("old", title = "Occupied housing units (1939 or earlier)", style = "fixed",
  #                                                          breaks = c(0.0, 18.0, 35.1, 51.1, 67.2, 100.0),
  #                                                          textNA = "Missing",
  #                                                          colorNA = "white",   # <-------- color for NA values
  #                                                          palette = c("#f0f9e8",
  #                                                                      "#ccebc5",
  #                                                                      "#a8ddb5",
  #                                                                      "#7bccc4",
  #                                                                      "#43a2ca",
  #                                                                      "#0868ac")) +
  #                                                  tm_borders() 
  #                                   
  #     )    
  #   }else if(var == "Income"){
  #     output[['map']] <- renderLeaflet(leaflet()  %>% tm_shape(income.shp) +
  #                                                  tm_fill("income", title = "Income ($1000)", style = "fixed",
  #                                                          breaks = c(9.3,40.5,63.0,89.8,135.2,232.3),
  #                                                          textNA = "Missing",
  #                                                          colorNA = "white",   # <-------- color for NA values
  #                                                          palette = c("#edf8fb",
  #                                                                      "#bfd3e6",
  #                                                                      "#9ebcda",
  #                                                                      "#8c96c6",
  #                                                                      "#8856a7",
  #                                                                      "#810f7c")) +
  #                                                  tm_borders() 
  #     )   
  #   }else if(var == "Race"){
  #     output[['map']] <- renderLeaflet(leaflet()  %>% tm_shape(race.shp) +
  #                                                  tm_fill("pctwht", title = "Percent of white people", style = "fixed",
  #                                                          breaks = c(0.0,17.1,36.5,57.3,77.4,100.0),
  #                                                          textNA = "Missing",
  #                                                          colorNA = "white",   # <-------- color for NA values
  #                                                          palette = c("#fef0d9",
  #                                                                      "#fdd49e",
  #                                                                      "#fdbb84",
  #                                                                      "#fc8d59",
  #                                                                      "#e34a33",
  #                                                                      "#b30000")) +
  #                                                  tm_borders()
  #     )   
  #   }else if(var == "Elder"){
  #     output[['map']] <- renderLeaflet(leaflet()  %>% tm_shape(agesex.shp) +
  #                                                  tm_fill("oldage_dep", title = "Old age dependency", style = "fixed",
  #                                                          breaks = c(0.0,20.8,45.8,148.8,371.4,956.2),
  #                                                          textNA = "Missing",
  #                                                          colorNA = "white",   # <-------- color for NA values
  #                                                          palette = c("#fef0d9",
  #                                                                      "#fdd49e",
  #                                                                      "#fdbb84",
  #                                                                      "#fc8d59",
  #                                                                      "#e34a33",
  #                                                                      "#b30000")) +
  #                                                  tm_borders()
  #     )   
  #   }else if(var == "Children"){
      output[['map']] <- renderLeaflet(leaflet()  %>% tm_shape(agesex.shp) +
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
                                        tm_borders())
    # }
}

# Create the Shiny App
shinyApp(ui = ui, server = server)