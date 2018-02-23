library(shiny)
library(leaflet)
library(rgdal)
library(tmap)
library(knitr)
library(RColorBrewer)

# Data
#save(list = ls(all.names = TRUE), file = "varmaps.RData", envir = .GlobalEnv)
#setwd("/nfs/urbangi-data/spatial_data/output")
# infolder <- "/nfs/urbangi-data/spatial_data/"
# vars.shp <- readOGR(dsn=".", layer="socioeco_vars", stringsAsFactors = FALSE)
# legtitles <- c("Disability status","Educational attainment","Male per 100 females",
#                "Percent of white people","Old age dependency","Child dependency",
#                "Percent of old housing units", "Income ($1000)")
# varnames <- c("dis_sta", "edu_att", "sex_ratio", "pctwht", "oldage_dep", "child_dep", "pctold", "income")
#load("varmaps.RData")

# githubURL <- "https://github.com/dongmeic/Urban_GI_Spatial/blob/master/varmaps.RData?raw=true"
knitr::knit(text ='```{r}
            load(url("https://github.com/dongmeic/Urban_GI_Spatial/blob/master/varmaps.RData?raw=true"))
            ```')

# User interface
ui <- bootstrapPage(
  title = "Urban GI Project - Thematic maps for the socioeconomic status in NYC",
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 120, left = 10, draggable = TRUE,
                selectInput("variables", "Socioeconomic Variable",
                            varnames
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                )
  )
)

server <- function(input, output, session) {

  output$map <- renderLeaflet({
    tmap_leaflet(tm_shape(vars.shp) + 
                   tm_fill(input$variables,
                           title = legtitles[which(varnames==input$variables)],
                           style="jenks", 
                           textNA = "Missing", 
                           colorNA = "white", 
                           palette = brewer.pal(6, input$colors)) + 
                   tm_borders())
  })

}

shinyApp(ui, server)