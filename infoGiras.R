library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)

# setwd("//mfelix/TISpt/RScripts/R_Apps/App1")

infoGiras <- read.csv("http://coiapp.cm-lisboa.pt/api/opendata/public/download/53616c7465645f5f26fbe91cdb44eabe974613c21ed8196cb1b00956bdc85345.csv?format=csv", encoding = "UTF-8")
infoGiras <- infoGiras %>%
  mutate(nrEstacao = as.numeric(substr(infoGiras[,1],1,3)))

estacao <- sub(".*- ","",infoGiras[,1])
infoGiras <- add_column(infoGiras,estacao)

lng <- sub("^[^-]*", "",infoGiras[,4])
lng <- sub("\\,.*", "",lng)
lng<- as.numeric(lng)
infoGiras <- add_column(infoGiras,lng)

lat <- sub("\\].*", "",infoGiras[,4])
lat <- sub(".*, ", "",lat)
lat <- as.numeric(lat)
infoGiras <- add_column(infoGiras,lat)

infoGiras <- subset(infoGiras, select = -c(position))

infoGiras <- infoGiras %>%
  mutate(time = sub(".* ","",Sys.time())) %>%
  select(1:numdocasvacias,time,everything())

infoGiras <- infoGiras %>%
  na.omit()

con <- DBI::dbConnect(odbc::odbc(), 
                      Driver = "SQL Server", 
                      Server = "SAI\\SQLEXPRESS", 
                      Database = "girasR", 
                      Trusted_Connection = "True")

# DBI::dbCreateTable(con, 'giras', dataGiras, row.names = NULL, temporary = FALSE)
DBI::dbWriteTable(con, 'giras_Nova', infoGiras, append = TRUE)

# write.csv(infoGiras,file = "infoGiras.csv")



