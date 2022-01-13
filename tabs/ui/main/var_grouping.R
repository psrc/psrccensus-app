var_group <- read.csv(file.path('data', 'variables_groupings.csv')) %>% 
  mutate(table_code = str_extract(.data$variable, "\\w+(?=_)"))