# read in excel sheets
# join to add standardized topic column
# loop through and create one row per topic tagged to table

library(tidyverse)
library(here)
library(odbc)
library(DBI)

source(here('scripts','topic-list-01-functions.R'))


# Read Elmer Census Dim ---------------------------------------------------


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


# Read in ACS and Census table list ---------------------------------------

a.df <- read.acs.index(Table.ID, Subject.Area) %>% 
  select('table_code' = 'Table.ID',
         'topic' = 'Subject.Area')
c.df <- read.census.index()

# join with topic-list-lookup.xlsx (tabs acs, census10)
lookup.a <- read.xlsx(here('data', 'topic-list-lookup.xlsx'), 'acs')
a <- a.df %>% left_join(lookup.a, by = c('topic' = 'acs_name'))
a2 <- a %>% 
  semi_join(var.df, by = c('table_code' = 'census_table_code')) %>% 
  select(table_code, tags = acs_standard_name)

lookup.c <- read.xlsx(here('data', 'topic-list-lookup.xlsx'), 'census10')
c <- c.df %>% left_join(lookup.c, by = c('topic' = 'census_name'))
c2 <- c %>% 
  semi_join(var.df, by = c('table_code' = 'census_table_code')) %>% 
  select(table_code, tags = census_standard_name)

tag.list <- bind_rows(a2, c2)

new.df <- NULL
for(i in 1:nrow(tag.list)) {
  x <- tag.list[i,]
  x.row <- map(str_split(x$tags, ","), ~data.frame(table_code = x$table_code, tags = str_trim(.x))) # add to df
  ifelse(is.null(new.df), new.df <- x.row, new.df <- bind_rows(new.df, x.row))
}

# add universe data
a.title.univ <- read.acs.index(Table.ID, Table.Title, Universe) %>% 
  rename(table_code = Table.ID, title = Table.Title, universe = Universe)
c.title.univ <- read.census.table.list(Table.Title, Universe) %>% 
  rename(table_code = Table.No., title = Table.Title, universe = Universe)
ac.title.univ <- bind_rows(a.title.univ, c.title.univ)

df <- new.df %>% 
  left_join(ac.title.univ, by = 'table_code')
