library(tidyverse)
library(tidycensus)
library(readxl)
library(openxlsx)

df <- get_acs(geography="county subdivision",
              table = "B01001",
              cache_table = TRUE,
              state = "NH",
              county = 011,
              year = 2013,
              survey = "acs5")

df <- df %>%
  filter(str_detect(NAME, 'Nashua city'))

fips <- data(fips_codes)