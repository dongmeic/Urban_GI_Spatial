# libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rgdal)
library(maptools)
library(knitr)
library(ggrepel)

# # projection
# lonlat <- CRS("+proj=longlat +datum=NAD83")
# crs <- CRS("+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333
#            +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83
#            +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")
# 
# # functions
# df2spdf <- function(col1, col2, colname1, colname2, df){
#   xy <- data.frame(df[,c(col1,col2)])
#   coordinates(xy) <- c(colname1, colname2)
#   proj4string(xy) <- lonlat
#   spdf <- SpatialPointsDataFrame(coords = xy, data = df, proj4string = lonlat)
#   return(spdf)
# }
# 
# # data
# setwd("/nfs/urbangi-data/spatial_data/output")
# infolder <- "/nfs/urbangi-data/spatial_data/"
# harbor_wq <- read.csv('csv/harbor_water_quality.csv', stringsAsFactors = FALSE)
# harbor_wq <- harbor_wq[!is.na(harbor_wq$year),]
# harbor_wq$FC_top_log <- log(harbor_wq$FC_top)
# harbor_wq$Ent_top_log <- log(harbor_wq$Ent_top)
# harbor_wq.td <- gather(harbor_wq, Key, Value, -site, -date, -year, -month, -day)
# harbor_wq.td$year <- as.character(harbor_wq.td$year)
# coords <- read.csv(paste0(infolder, "WQ/DEP/harbor_sampling_coordinates.csv"), stringsAsFactors = FALSE)
# coords <- coords[,-1]
# colnames(coords)[1] <- "site"
# #harbor_wq.df <- merge(coords, harbor_wq.td, by="site")
# #harbor_wq.shp <- df2spdf(3,2,"Long","Lat",harbor_wq.df)
# #writeOGR(harbor_wq.shp, dsn=".", layer="harbor_water_quality", overwrite_layer = TRUE,driver = "ESRI Shapefile")
# bound <- readOGR(dsn = paste0(infolder, "BD"), layer = "nyc_bound")
# bound <- spTransform(bound, lonlat)
# #longlat <- bound %>% fortify() %>% select(long,lat)
# names(bound)[5:6] <- c("long","lat")
# # plot(bound)
# # sites.spdf <- df2spdf(3,2,"Long","Lat",coords)
# # plot(sites.spdf, add=T, col="blue", pch=19)
# # pointLabel(sites.spdf$Long, sites.spdf$Lat, sites.spdf$site, col="red", offset = 0.6)
# wq_inds <- c("Dissolved Oxygen (DO) on the top",
#              "Dissolved Oxygen (DO) on the bottom",
#              "Fecal Coliform (FC) Bacteria on the top",
#              "Fecal Coliform (FC) Bacteria on the bottom",
#              "Enterococci Bacteria on the top",
#              "Enterococci Bacteria on the bottom",
#              "Secchi Transparency")
# # save(list = ls(all.names = TRUE), file = "harborWQ.RData", envir = .GlobalEnv)

knitr::knit(text ='```{r}
           load(url("https://github.com/dongmeic/Urban_GI_Spatial/blob/master/WQ_plots/harborWQ.RData?raw=true"))
           ```')

# User Interface
in1 <- selectInput(inputId = 'pick_wqp',
                   label = 'Select a water quality indicator',
                   choices = unique(harbor_wq.td[['Key']]))
in2 <- sliderInput('slider_months',
                   label = 'Month Range',
                   min = 1,
                   max = 12,
                   value = c(1, 12))
in3 <- selectInput(inputId = 'pick_site',
                   label = 'Select a sampling site',
                   choices = unique(harbor_wq.td[['site']]))
out1 <- plotOutput('wq_plot')
out2 <- plotOutput('map')
out3 <- textOutput('wq_ind')
side <- sidebarPanel('Options', in1, in2, in3, out3)
main <- mainPanel(out1, out2)
tab <- tabPanel(title = 'Harbor water quality in NYC',
                sidebarLayout(side, main))
ui <- navbarPage('Urban GI Project', tab)

# Server
server <- function(input, output) {
  
  reactive_seq <- reactive(
    seq(input[['slider_months']][1],
        input[['slider_months']][2])
  )
  
  output[['wq_plot']] <- renderPlot(
    harbor_wq.td %>%
      filter(Key == input[['pick_wqp']]) %>%
      filter(month %in% reactive_seq()) %>%
      filter(site == input[['pick_site']]) %>%
      ggplot(aes(year, Value)) +
      labs(title="Red points show the mean values")+
      geom_boxplot()+xlab("Sampling year")+
      geom_point(stat = 'summary',
                 fun.y = 'mean',
                 color = "red")
    
  )
  output[['map']] <- renderPlot(
      ggplot()+geom_polygon(data=bound, aes(x=long, y=lat, group=group), fill="grey40", 
                            colour="grey90", alpha=0.6)+
      labs(x="Longitude", y="Latitude", title="Location of the selected sampling site is in green")+
      geom_point(data = coords, aes(Long, Lat), col='red', alpha=0.8)+
      geom_point(data = coords %>%
                   filter(site == input[['pick_site']]), aes(Long, Lat), col='green', size=5)+
      geom_text(data = coords %>%
                  filter(site == input[['pick_site']]), aes(label = site, x = Long, y = Lat), hjust = 0, nudge_x = 0.008, size=7, color = "green")+
      geom_text_repel(data = coords %>%
                  filter(site != input[['pick_site']]), aes(label = site, x = Long, y = Lat), size=5)
  )
  output[['wq_ind']] <- renderText(
    paste0("The selected water quality indicator is ", wq_inds[which(unique(harbor_wq.td[['Key']])==input[['pick_wqp']])])
  )
}

# Create the Shiny App
shinyApp(ui = ui, server = server)
