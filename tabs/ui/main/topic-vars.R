# This script produces a named vector for the Topics dropdown list

# Create a df of unique census table codes.
# Add a new column that is tidycensus friendly (i.e. where Decennial codes are padded with 0s. H2 vs H002)
var.df.dist <- var.df %>% 
  select(census_table_code, concept, census_product, name) %>% 
  filter(!is.na(concept) & concept != 'N/A') %>%
  mutate(census_table_code_pad = ifelse(census_product == 'Decennial', 
                                        str_sub(name, start = 1, end = str_length(name)-3), 
                                        census_table_code)) %>% 
  select(-census_product, -name) %>% 
  distinct() %>%
  arrange(census_table_code_pad)

# Create the named vector.
# Labels will have the topic name & table code
vars <- var.df.dist$census_table_code
names(vars) <- map2(str_to_title(var.df.dist$concept), var.df.dist$census_table_code, ~paste0(.x, ": ", .y)) 

