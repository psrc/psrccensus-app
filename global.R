library(shiny)
library(psrccensus)
library(DT)
library(sf)
library(leaflet)
library(tidyverse)
library(odbc)
library(DBI)
library(fontawesome)
library(shinyjs)
library(openxlsx)
library(scales)
library(here)
# library(plotly)

db.table <- 'census.variable_dim'

arc.root <- "https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/" 

db.connect <- function() {
  elmer_connection <- dbConnect(odbc(),
                                Driver = "SQL Server",
                                Server = "AWS-PROD-SQL\\SOCKEYE",
                                Database = "Elmer",
                                # Trusted_Connection = "yes"
                                UID = Sys.getenv("userid"),
                                PWD = Sys.getenv("pwd")
  )
}

read.dt <- function(astring, type =c('table_name', 'sql_query')) {
  elmer_connection <- db.connect()
  
  if (type == 'table_name') {
    dtelm <- dbReadTable(elmer_connection, SQL(astring))
  } else if(type == 'sql_query') {
    dtelm <- dbGetQuery(elmer_connection, SQL(astring))
  }
  
  dbDisconnect(elmer_connection)
  return(dtelm)
}

var.df <- read.dt(db.table, 'table_name')
topic.df <- read.csv(here('data', 'topic-list.csv')) %>% 
  filter(!str_detect(title, 'MEDIAN') & !str_detect(title, '.*AVERAGE.*'))

# source tab files
tab_files <- list.files(path = "tabs", full.names = T, recursive = T)
suppressMessages(lapply(tab_files, source))
