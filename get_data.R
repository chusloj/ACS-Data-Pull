library(tidyverse)
library(tidycensus)
library(readxl)
library(openxlsx)

setwd("~/Documents/career/RKG")
census_api_key("2f7688b42a2c229e0662079bf0f4f5400cbb7551")
source("funcs.r")

# Main ----
# open <- Sys.time()


labels <- read_excel("Data Pull_NashuaNH.xlsx",sheet="Data Pull")
labels <- filter(labels, Source=="ACS Data")
labels <- labels[1:3]
tables <- labels[2]
titles <- labels[3]

vars <- load_variables(2018, "acs5", cache = TRUE)
vars <- select(vars, "name","label")
vars <- rename(vars, "variable"="name")


# wb <- createWorkbook()
wb <- loadWorkbook("Data Pull_NashuaNH.xlsx")

j <- nrow(tables)
# j <- 5

for(i in 1:j) {
  df <- get_acs(geography="state",
              table = tables[[i,1]],
              cache_table = TRUE,
              state = "NH",
              year = 2018,
              survey = "acs5")
  
  if(i==1){fin_df <- sexage.func(df)} else {fin_df <- df}
  
  
  sht <- tables[[i,1]]
  addWorksheet(wb,sht)
  writeData(wb,sht,fin_df)
}

saveWorkbook(wb,"Data.xlsx",overwrite = T)


# close <- Sys.time()
# close-open


