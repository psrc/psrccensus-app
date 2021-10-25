# This script produces a named vector for the Places and MSA dropdown list

## places ----
place.df <- read.dt('Political.incorporated_city_dims', 'table_name') 
places <- place.df$place_geoid
names(places) <- place.df$juris_name

## MSA ----
msa.sql <- "select gd.geography_name, gd.msa_fips
from census.geography_dim gd
where gd.geography_type = 'MSA'"

msa.df.full <- read.dt(msa.sql, 'sql_query')

non.wa.msa <- str_subset(msa.df.full$geography_name, ".*\\,.*")
wa.metro.msa <- str_subset(msa.df.full$geography_name, ".*WA\\sMetro.*")
non.wa.msas <- setdiff(non.wa.msa, wa.metro.msa)

msa.df <- msa.df.full %>% 
  filter(!(geography_name %in% c(non.wa.msas)))

msas <- msa.df$msa_fips
names(msas) <- msa.df$geography_name