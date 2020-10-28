library(dplyr)
library(DBI)
library(ggplot2)
library(plotly)
library(shiny)
library(tidyverse)
library(lubridate)
library(leaflet)
library(scales)
library(plotly)

teste <- read.csv(file = "teste.csv")
sumatorio <- read.csv(file = "sumatorio.csv")
analiseGiras <- read.csv(file = "analiseGiras.csv")

#Time doesn't come as POSIXct when reading a csv
sumatorio$time  <- as.POSIXct(sumatorio$time, format="%Y-%m-%d %H:%M:%S")

#Needed for Shiny----

mybins <- seq(0, 65, by=5)
mybins2 <- seq(0, -60, by=-5)
mybins3 <- seq(0, 100, by=10)
mypalette <- colorBin(
  palette="viridis", 
  domain=teste$retiradas,
  na.color="transparent", 
  bins=mybins, reverse=TRUE)
mypalette2 <- colorBin(
  palette="viridis", 
  domain=teste$devolvidas,
  na.color="transparent", 
  bins=mybins2, reverse=FALSE)
mypalette3 <- colorBin(
  palette="viridis", 
  domain=teste$prob,
  na.color="transparent", 
  bins=mybins3, reverse=TRUE)

mytext <- paste("Last update: ", teste[,c("TS")],"<br>",
                "Dock: ", teste[,c("estacao")], "<br/>",
                 "Withdrawn = ",teste[,c("retiradas")], "<br/>",
                 "Returned = ",teste[,c("devolvidas")], "<br/>",
                "N. Bikes in Dock = ",teste[,c("numbicicletas")], "<br/>",
                "N. Empty Docks = ",teste[,c("numdocasvacias")]
                , "<br/>",
                "Max n. Bikes this day  = ",teste[,c("maxBikes")], "<br/>",
                "Daily Max. Prob. have Bike = ",teste[,c("prob")],"%"
                ) %>%
  lapply(htmltools::HTML)


#Shiny UI----

ui <- shinyUI(fluidPage(
  
  titlePanel("GIRAS"),
  
  sidebarPanel( 
    
    helpText("Daily Volumes", style = "font-size:150%"),
    
    sliderInput(inputId = "range", label = "Dates:",
                min = as.Date("2020-09-28","%Y-%m-%d"),
                max = as.Date("2020-10-04","%Y-%m-%d"),
                value=c(as.Date("2020-09-28"),as.Date("2020-10-04")),
                timeFormat="%Y-%m-%d"),
    
    selectInput(inputId = "variavel", label = "Variable:",
                c("N. Bikes in Use" = "bikeEmUso",
                  "Withdrawn Bike" = "retiradas",
                  "Returned Bike" = "devolvidas",
                  "N. Bikes in Dock" = "numbicicletasSum",
                  "N. Empty Docks" = "numdocasvaciasSum")
                )


  ),
  
  mainPanel(
    plotOutput("plot1")
  ),
  
  sidebarPanel( 
    
    helpText("Dock Analysis", style = "font-size:150%"),
    
    sliderInput(inputId = "rangePostos", label = "Day:",
                min = as.Date("2020-09-28","%Y-%m-%d"),
                max = as.Date("2020-10-04","%Y-%m-%d"),
                value=c(as.Date("2020-09-28")),
                timeFormat="%Y-%m-%d"),
    
    sliderInput(inputId = "horaPostos", label = "Hour:",
                min = as.POSIXct("2020-10-15 02:00:00","%H:%M"),
                max = as.POSIXct("2020-10-15 23:00:00","%H:%M"),
                value = c(as.POSIXct("2020-10-15 13:00:00")),
                timeFormat = "%H:%M"),
    
    selectInput(inputId = "legenda", label = "Legend:",
                c("Withdrawn Bike" = "retiradas",
                  "Returned Bike" = "devolvidas",
                  "Finding Bike Max. Probability" = "prob")),
    
    actionButton(inputId = "update", 
                 label = "Update"),
    
    helpText("After changing the parameters, press the update button")
    
  ),

    mainPanel(
      leafletOutput("myMap"),
      helpText("This work is licensed under a Creative Commons Attribution 4.0 License."),
      helpText("Created by Manuel M. Felix")
      
    )
  )
)

#Server----

server <- function(input, output, session){

  bdSumatorio <- reactive({
    sumatorio %>% filter(add_data >= input$range[1] & add_data <= input$range[2]) %>%
      filter(time > as.POSIXct("2020-10-15 03:00:00"))
    })
  
  output$plot1 <- renderPlot({withProgress(

    #It's necessary tu use aes_String for reactive variables
    ggplot(bdSumatorio(), aes_string(y=input$variavel)) +
      geom_line(mapping=aes(x=time, group = add_data, color=as.factor(add_data))) + 
      # theme(plot.background = element_rect(fill = "#ebebe0", color = "#ebebe0")) +
      labs(title="", x="", y="") +
      scale_x_datetime(labels = date_format("%H:%M"), date_breaks = "2 hours", 
                       minor_breaks = "1 hours", 
                       limits = c(as.POSIXct("2020-10-15 03:00:00"),as.POSIXct("2020-10-15 24:00:00 GMT")))+
      scale_color_viridis_d(name = "Dates")+
      theme(axis.text.x = element_text( color = "black", size = 8, angle = 90))
  )

  })
  
  output$myMap <- renderLeaflet({
    # mapGiras
    leaflet(teste) %>%
      setView(lat = 38.736946, lng = 	-9.142685, zoom=13) %>%
      addTiles(group="OSM") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group="Dark") %>%
      addProviderTiles(providers$CartoDB.Positron, group="Light") %>%
      addLayersControl(baseGroups=c('Light','OSM','Dark')) %>%
      addCircleMarkers(~lng,~lat,
                       fillColor = ~mypalette(retiradas), fillOpacity = 0.7, color="white", radius=8, stroke=FALSE,
                       label = mytext,
                       labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>%
      addLegend(pal=mypalette, values=~input$legenda, title="Withdrawn", opacity=0.9, position = "bottomright" )
  })
  
  #We need to transform it into a dataframe in the end, for shiny doesn't work
  #well with tibbles, only dataframes
  bdAnalise <- reactive({withProgress(message = 'Updating',
    analiseGiras %>% filter(add_data == input$rangePostos) %>%
      filter(time >= as.POSIXct("2020-10-15 03:00:00") & time <= input$horaPostos) %>%
      as.data.frame() %>% group_by(desigcomercial) %>% 
      top_n(retiradas) %>% distinct(desigcomercial, .keep_all= TRUE) %>%
      as.data.frame()
    )
  })

  
# Observe Event----  
  
  observeEvent(input$update, {
    mytext <- paste0("Last update: ", bdAnalise()[,c("TS")],"<br>",
                     "Posto: ", bdAnalise()[,c("estacao")],"<br>",
                     "Retiradas = ",bdAnalise()[,c("retiradas")],"<br>",
                     "Devolvidas = ",bdAnalise()[,c("devolvidas")], "<br/>",
                     "N. Bikes in Dock = ",bdAnalise()[,c("numbicicletas")], "<br/>",
                     "N. Empty Docks = ",bdAnalise()[,c("numdocasvacias")]
                     , "<br/>",
                     "Max n. Bikes this day = ",bdAnalise()[,c("maxBikes")], "<br/>",
                     "Daily Max. Prob. have Bike = ",bdAnalise()[,c("prob")],"%"
                     ) %>%
      lapply(htmltools::HTML)
    if(input$legenda == "retiradas"){
    leafletProxy("myMap", data=bdAnalise()) %>% 
      clearControls() %>%
      clearMarkers() %>%
      addCircleMarkers(~lng,~lat,
                       fillColor = ~mypalette(retiradas), 
                       fillOpacity = 0.7, color="white", radius=8, stroke=FALSE,
                       label = mytext,
                       labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>%
      addLegend(pal=mypalette, values=~input$legenda, title="Withdrawn", opacity=0.9, position = "bottomright" )
    }else if(input$legenda == "devolvidas") {
      leafletProxy("myMap", data=bdAnalise()) %>%
        clearControls() %>%
        clearMarkers() %>%
        addCircleMarkers(~lng,~lat,
                         fillColor = ~mypalette2(devolvidas),
                         fillOpacity = 0.7, color="white", radius=8, stroke=FALSE,
                         label = mytext,
                         labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>%
      addLegend(pal=mypalette2, values=~input$legenda, title="Returned", opacity=0.9, position = "bottomright" )
    }else if(input$legenda == "prob") {
      leafletProxy("myMap", data=bdAnalise()) %>%
        clearControls() %>%
        clearMarkers() %>%
        addCircleMarkers(~lng,~lat,
                         fillColor = ~mypalette3(prob),
                         fillOpacity = 0.7, color="white", radius=8, stroke=FALSE,
                         label = mytext,
                         labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>%
      addLegend(pal=mypalette3, values=~input$legenda, title="Probability", opacity=0.9, position = "bottomright" )
    }

        })
}

shinyApp(ui = ui, server = server)

