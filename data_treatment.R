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

con <- DBI::dbConnect(odbc::odbc(), 
                      Driver = "Driver", 
                      Server = "Server", 
                      Database = "DataBaseName", 
                      Trusted_Connection = "True/False")

analiseGiras <- DBI::dbReadTable(con, 'DataBaseName')

analiseGiras$TS <- as.POSIXct(paste(analiseGiras$add_data, analiseGiras$time))

analiseGiras$add_data  <- as.Date(analiseGiras$add_data, format = "%Y-%m-%d")

analiseGiras$add_hora  <- as.POSIXct(analiseGiras$add_hora, format="%H:%M:%S")

analiseGiras$time  <- as.POSIXct(analiseGiras$time, format="%H:%M:%S")

#summation----

summation <- analiseGiras %>% filter(add_data > "2020-09-27" & add_data < "2020-10-05") %>%
  group_by(time, add_data, TS) %>%
  summarise(numbicicletasSum=sum(numbicicletas), numdocasvaciasSum=sum(numdocasvacias))

summation <- subset(summation, !numbicicletasSum == 0 & !numdocasvaciasSum == 14)

summation <- summation[order( summation[,3]),  ]

summation <- summation %>%
  group_by(add_data) %>%
  mutate(volume = numdocasvaciasSum - lag(numdocasvaciasSum, default = numdocasvaciasSum[1]))

summation <- summation %>% group_by(add_data) %>% 
  mutate(retiradas = cumsum(ifelse(volume>0,volume,0)),
         devolvidas = cumsum(ifelse(volume<0,volume,0)))

summation <- as.data.frame(summation)

#IMPORTANT: Every POSIXct value has to be altered for the data the you are editing this file----

summation <- summation %>% filter(time > as.POSIXct("2020-10-16 03:00:00"))

summation$time  <- as.POSIXct(summation$time, format="%Y-%m-%d %H:%M:%S")

summation <- summation %>%
  filter(time > as.POSIXct("2020-10-16 03:00:00")) %>%
  group_by(add_data) %>%
  mutate(
    maxBikes = max(as.numeric(numbicicletasSum))) %>%
  mutate(
    bikeEmUso = (maxBikes-numbicicletasSum),
  )

#analiseGiras----

analiseGiras <- analiseGiras[order( analiseGiras[,11]),  ]

analiseGiras <- analiseGiras %>% 
  filter(add_data > "2020-09-27" & add_data < "2020-10-05") %>%
  group_by(add_data, desigcomercial) %>%
  mutate(volume = numdocasvacias - lag(numdocasvacias, default = numdocasvacias[1]))

#If using AnaliseGirasInicial----

#Uncomment the following comments

# analiseGiras$devolvidas <- NULL
# analiseGiras$retiradas <- NULL
# analiseGiras <- read.csv(file = "analiseGirasInicial.csv")
# #Then, roll the rest 

analiseGiras <- analiseGiras %>%
  filter(time > as.POSIXct("2020-10-16 03:00:00")) %>%
  group_by(add_data, desigcomercial) %>%
  mutate(
    maxBikes = max(as.numeric(numbicicletas)),
    minBikes = min(as.numeric(numbicicletas)),
    nDocas = (numdocasvacias+numbicicletas)) %>%
  mutate(
    prob = (maxBikes/nDocas),
    probMin = (minBikes/nDocas),
    retiradas = cumsum(ifelse(volume>0,volume,0)),
    devolvidas = cumsum(ifelse(volume<0,volume,0))
  )

analiseGiras$prob <- as.numeric(sub("%", "",analiseGiras$prob,fixed=TRUE))*100
analiseGiras$prob <- round(analiseGiras$prob,digits=0)
analiseGiras$probMin <- as.numeric(sub("%", "",analiseGiras$prob,fixed=TRUE))*100
analiseGiras$probMin <- round(analiseGiras$prob,digits=0)

teste <- analiseGiras %>% filter(add_data == "2020-09-28") %>%
  filter(time >= as.POSIXct("2020-10-15 03:00:00") & time <= as.POSIXct("2020-10-15 13:00:00") ) %>% 
  as.data.frame() %>%
  group_by(desigcomercial) %>% top_n(devolvidas) %>%
  distinct(desigcomercial, .keep_all= TRUE) %>% as.data.frame()

#Tests for Shiny----

ggplot(summation, mapping=aes(x=time, y=numbicicletasSum, group = add_data, color=as.factor(add_data))) +
  geom_line() +
  labs(title="", x="", y="") +
  scale_x_datetime(labels = date_format("%H:%M"), date_breaks = "2 hours", 
                   minor_breaks = "1 hours", 
                   limits = c(as.POSIXct("2020-10-16 03:00:00"),as.POSIXct("2020-10-16 24:00:00 GMT")))+
  scale_color_viridis_d(name = "Datas")+
  theme(axis.text.x = element_text( color = "black", size = 8, angle = 90))

mybins <- seq(0, 60, by=5)
mypalette <- colorBin(
  palette="viridis", domain=teste$retiradas, na.color="transparent", 
  bins=mybins, reverse=TRUE)

mytext <- paste("Posto: ", teste[,c("estacao")], "<br/>",
                "Retiradas = ",teste[,c("retiradas")], "<br/>",
                "Devolvidas = ",teste[,c("devolvidas")]) %>%
  lapply(htmltools::HTML)

leaflet(teste) %>%
  setView(lat = 38.736946, lng = 	-9.142685, zoom=13) %>%
  addTiles(group="OSM") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group="Dark") %>%
  addProviderTiles(providers$CartoDB.Positron, group="Light") %>%
  addLayersControl(baseGroups=c('Light','OSM','Dark')) %>%
  addCircleMarkers(~lng,~lat,
                   fillColor = ~mypalette2(devolvidas), fillOpacity = 0.7, color="white", radius=8, stroke=FALSE,
                   label = mytext,
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>%
  addLegend(pal=mypalette2, values=~devolvidas, opacity=0.9, title = "Devolvidas", position = "bottomright")


write.csv(teste,file = "teste.csv")

write.csv(summation,file = "summation.csv")

write.csv(analiseGiras,file = "analiseGiras.csv")

