library(shiny)
library(shinydashboard)
library(leaflet)
library(jsonlite)
library(tidyverse)
library(leaflet.extras)
library(lubridate)


ui <- dashboardPage(skin = "blue",
                    title = "Google Location Map",
                    dashboardHeader(title = "Google Location History Visualization", titleWidth = 300),
                    
                    #idebar with menu and widgets
                    dashboardSidebar(width = 300,
                                     tags$div(
                                       tags$blockquote("Use this shiny app to check where Google has tracked you!"),
                                       tags$h4(img(src='https://github.com/aoliu95/google_location_privacy_app/raw/master/info.png', height = "25px"),
                                               "How to get  Google location data:"),
                                       #tags$p("It's your ",tags$strong("right")," to see how Internet Companies store your data."),
                                       tags$p("1.Simply heads to ", tags$a(href="https://takeout.google.com/", "Google Takeout")," to see and download any of the data Google holds on you."),
                                       tags$p("2.Click on",tags$b("SELECT NONE, "),img(src='https://github.com/aoliu95/google_location_privacy_app/raw/master/NONE.png', height = "30px"),
                                              "then scroll down to",tags$b("Location History"),img(src='https://github.com/aoliu95/google_location_privacy_app/raw/master/Loc1.png', height = "35px"),
                                              "and click on the slider to select it."),
                                       tags$p("3.Scroll to the bottom and click NEXT, then CREATE ARCHIVE, and finally DOWNLOAD when it is ready. You will need to verify by logging into your Google account."),
                                       tags$p("4.This will download a ZIP file to your downloads directory.Upload the downloaded ZIP file found below..."),
                                       style = "padding: 10px;"
                                       
                                     ),
                                     
                                     # Input: Select a file
                                     fileInput("file1", "Upload your Zip file",
                                               multiple = FALSE,
                                               accept = ".zip",
                                               placeholder = "Max file size 100Mb"),
                                     
                                     tags$div(
                                      # p("Point data is capped at the most recent 50,000 locations."),
                                       tags$i("Privacy: We are not going to be another",tags$a(href="https://www.vox.com/policy-and-politics/2018/3/23/17151916/facebook-cambridge-analytica-trump-diagram",
                                                                                                "Cambridge Analytica"),"! Your data would be uploaded to RShiny's cache. It will only be used for the purpose of 
                                                                                                 rendering this visualisation. ANY FILE would be permently wiped once you closed this page.",
                                                                                                  "You are also welcomed to audit the code at ", tags$a(href="https://github.com/aoliu95",
                                                                                                  " A Light Data Lab Github Page")),
                                       style = "padding: 10px;"
                                     ),
                                     
                                     # credit tag
                                     h5("  Made with ❤️ from",
                                        img(src = "https://github.com/aoliu95/airport_shiny/raw/master/black.png", height = "50px"))
                                     
                    ),
                    
                    # Main panel for displaying outputs
                    dashboardBody(
                      
                      tags$head(tags$style("#myMap{height:90vh !important;}")),
                      
                      leafletOutput("myMap")
                      
                    )
)

# Define server logic to read selected file
server <- function(input, output) {
  
  options(shiny.maxRequestSize = 100*1024^2)
  
  output$myMap <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron, group = "Default Maptile") %>% 
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark Maptile") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Maptile") %>%
      setView(24, 27, zoom = 2) %>% 
      addLayersControl(
        baseGroups = c("Default Maptile", "Dark Maptile", "Satellite Maptile"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  observe({
    
    withProgress(message = 'Please wait...',
                 value = 0/4, {
                   
                   req(input$file1)
                   
                   incProgress(1/4, detail = "reading data")
                   
                   fname = unzip(input$file1$datapath, list=TRUE)$Name[1]
                   td = tempdir()
                   unzip(input$file1$datapath, files=fname, exdir=td, overwrite=TRUE)
                   fpath = file.path(td, fname)
                   
                   locationdata <- fromJSON(fpath, simplifyVector = TRUE, simplifyDataFrame = TRUE)
                   
                   newIcons <- iconList(
                     stand = makeIcon("https://github.com/aoliu95/google_location_privacy_app/raw/master/stand.png",
                                      "https://github.com/aoliu95/google_location_privacy_app/raw/master/stand.png", 46, 46),
                     drive = makeIcon("https://github.com/aoliu95/google_location_privacy_app/raw/master/car.png", 
                                      "https://github.com/aoliu95/google_location_privacy_app/raw/master/car.png", 24, 24)
                   )
                   
                   incProgress(1/4, detail = "cleaning data")
                   
                   myData <- locationdata$locations %>% 
                     select(latitudeE7, longitudeE7, `timestampMs`, velocity) %>% 
                     mutate(lat = latitudeE7 / 1E7, lon = longitudeE7 / 1E7) %>% 
                     mutate(timestampMs = as.numeric(timestampMs)) %>%
                     mutate(Date = as.POSIXct(timestampMs/1000, origin="1970-01-01")) %>%
                     select(-latitudeE7, -longitudeE7) %>% 
                     mutate(image = case_when(
                       velocity > 10 ~ "drive",
                       TRUE ~ "stand"
                     )) %>% 
                     mutate(image = factor(image, levels = c("drive","stand")))
                   
                   incProgress(1/4, detail = "rendering map")
                   
                   leafletProxy("myMap", data = myData) %>%
                     fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat)) %>%  
                     addHeatmap(lng = ~lon, lat = ~lat, group = "HeatMap", blur = 20, max = 0.01, radius = 15) %>%
                     addMarkers(data = head(myData, 50000), ~lon, ~lat, icon = ~newIcons[image], clusterOptions = markerClusterOptions(), 
                                label = ~ format(Date, format = "%H:%M %d-%b-%Y"), group = "Points") %>% 
                     addLayersControl(
                       baseGroups = c("Default Maptile", "Dark Maptile", "Satellite Maptile"),
                       overlayGroups = c("HeatMap", "Points"),
                       options = layersControlOptions(collapsed = FALSE)
                     )
                   
                   incProgress(1/4)
                   
                 })
  })
  
}

# Create Shiny app
shinyApp(ui, server)