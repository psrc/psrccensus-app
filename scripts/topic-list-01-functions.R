# Gather table information to tag with topic(s) for topic dropdown. Export as excel file for
# more customization and standardization.

library(tidyverse)
library(openxlsx)
library(here)

read.acs.index <- function(...) {
  # This function reads both 1 & 5 year ACS Index Reference sheets to extract table code and table topic.
  ref.path <- "J:\\Projects\\Census\\AmericanCommunitySurvey\\Data\\2019 Survey\\Reference"
  file.names <- paste0(paste0('Sequence_Number_and_Table_Lookup_2019_', c("1", "5")), '.xlsx')
  
  tables <- map(file.path(ref.path, file.names), ~read.xlsx(.x, sheet = 'Index'))
  
  table1 <- tables[[1]] %>% select(Table.ID,...)
  table2 <- tables[[2]] %>% select(Table.ID,...)
  
  t1 <- semi_join(table1, table2, by = 'Table.ID')
  t2 <- anti_join(table1, table2, by = 'Table.ID')
  t3 <- anti_join(table2, table1, by = 'Table.ID')
  
  df <- reduce(list(t1, t2, t3), bind_rows)

  return(df)
}

read.census.index <- function() {
  # This function reads the SF1 2010 Census to extract table code and table topic.
  # input or table.list
  c.ref.path <- "J:\\Projects\\Census\\Census2010\\Summary File 1\\Reference"
  c.file.names <- '2010SF1 Data Dictionary.xlsx'

  table <- read.xlsx(file.path(c.ref.path, c.file.names), sheet = 'Input', startRow = 3)

  df <- table %>%
    filter(!is.na(X1)) %>%
    select(1,2,5)
  colnames(df) <- c('table_code', 'title', 'topic_o')

  df <- df %>%
    mutate(topic = str_trim(str_extract(topic_o, "(?<=,).*"))) %>%
    mutate(table_code = str_replace_all(table_code, "\\.", ""))

  return(df)
}

read.census.table.list <- function(...) {
  # This function reads the SF1 2010 Census to extract table code and table topic.
  # input or table.list
  c.ref.path <- "J:\\Projects\\Census\\Census2010\\Summary File 1\\Reference"
  c.file.names <- '2010SF1 Data Dictionary.xlsx'
  
  table <- read.xlsx(file.path(c.ref.path, c.file.names), sheet = 'Table list', startRow = 1)
  
  df <- table %>% select(Table.No., ...) %>% 
    mutate(Table.No. = str_replace_all(Table.No., "\\.", ""))

  return(df)
}

# Other -------------------------------------------------------------------

export.topic.list <- function() {
  # This function exports the ACS and Census topic list to excel for manual editing and refining.
  a <- read.acs.index(Subject.Area) %>% select('table_code' = 'Table.ID', 'topic' = 'Subject.Area')
  c <- read.census.index()
  write.xlsx(list(unique(a$topic), unique(c$topic)), here('data', 'topic-list-lookup.xlsx'))
}


