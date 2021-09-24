library(shiny)
library(psrccensus)
library(DT)
library(leaflet)
library(tidyverse)
library(odbc)
library(DBI)
library(fontawesome)
library(shinyjs)

db.table <- 'census.variable_dim'

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

# source tab files
tab_files <- list.files(path = "tabs", full.names = T, recursive = T)
suppressMessages(lapply(tab_files, source))