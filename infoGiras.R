library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)

dataB <- read.csv("http://coiapp.cm-lisboa.pt/api/opendata/public/download/53616c7465645f5f26fbe91cdb44eabe974613c21ed8196cb1b00956bdc85345.csv?format=csv", encoding = "UTF-8")
dataB <- dataB %>%
  mutate(nrEstacao = as.numeric(substr(dataB[,1],1,3)))

estacao <- sub(".*- ","",dataB[,1])
dataB <- add_column(dataB,estacao)

lng <- sub("^[^-]*", "",dataB[,4])
lng <- sub("\\,.*", "",lng)
lng<- as.numeric(lng)
dataB <- add_column(dataB,lng)

lat <- sub("\\].*", "",dataB[,4])
lat <- sub(".*, ", "",lat)
lat <- as.numeric(lat)
dataB <- add_column(dataB,lat)

dataB <- subset(dataB, select = -c(position))

dataB <- dataB %>%
  mutate(time = sub(".* ","",Sys.time())) %>%
  select(1:numdocasvacias,time,everything())

dataB <- dataB %>%
  na.omit()

con <- DBI::dbConnect(odbc::odbc(), 
                      Driver = "Driver", 
                      Server = "Server", 
                      Database = "DataBaseName", 
                      Trusted_Connection = "True/False")

# In the first instance, create the table
# DBI::dbCreateTable(con, 'DataBaseName', dataB, row.names = NULL, temporary = FALSE)

#Afterwards, write on it
DBI::dbWriteTable(con, 'DataBaseName', dataB, append = TRUE)




