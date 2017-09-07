# libraries
library(shiny)
library(rgdal)
library(tmap)
library(leaflet)
library(knitr)

## data
# setwd("/nfs/urbangi-data/spatial_data/output")
# save(list = ls(all.names = TRUE), file = "gi_maps.RData", envir = .GlobalEnv)
# infolder <- "/nfs/urbangi-data/spatial_data/"
# gi <- readOGR(dsn = paste0(infolder, "GI"), layer ="GI_assets_public", stringsAsFactors = FALSE)
# head(gi)

knitr::knit(text ='```{r}
            load(url("https://github.com/dongmeic/Urban_GI_Spatial/blob/master/gi_maps.RData?raw=true"))
            ```')

## user interface
choices.wb <- c("Select All", unique(gi$Waterbody))

in1 <- checkboxGroupInput(inputId = "Status_Gro", 
                          label = "Construction status", 
                          choices = unique(gi[['Status_Gro']]), 
                          selected = "Constructed", 
                          inline = TRUE)

in2 <- selectInput(inputId = 'Waterbody',
                   label = 'GI by waterbody',
                   choices = choices.wb,
                   selected = "Newtown Creek",
                   multiple = TRUE)

in3 <- selectInput(inputId = 'colorshow',
                          label = 'Color by variables',
                          choices = c('GI_Type', 'CSO_Tribut', 'Borough'),
                          selected = "GI_Type")

colorshow.types <- c('GI_Type', 'CSO_Tribut', 'Borough')
legtitles <- c('Green infrastructure types', 'CSO tributary', 'Borough')

ui <- bootstrapPage(
  title = "Urban GI Project - Map of green infrastructures from NYC DEP",
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 120, left = 10, draggable = TRUE, in1, in2, in3)
)

## server
server <- function(input, output, session) {
  
  observe({
    if ("Select All" %in% input$Waterbody) {
      # choose all the choices _except_ "Select All"
      selected_choices <- setdiff(choices.wb, "Select All")
      updateSelectInput(session, "Waterbody", selected = selected_choices)
    }
  })
  
  filteredData <- reactive({
    gi <- gi[gi$Status_Gro %in% input$Status_Gro,]
    gi <- gi[gi$Waterbody %in% input$Waterbody,]
  })
  
  output$map <- renderLeaflet({
    tmap_leaflet(tm_shape(filteredData()) + tm_dots(title = legtitles[which(colorshow.types==input$colorshow)], size = 0.2, col = input$colorshow))
  })
  
}

shinyApp(ui, server)
