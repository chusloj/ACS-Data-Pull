library(tidyverse)
library(tidycensus)
library(readxl)
library(openxlsx)
source("funcs.r")
###



# Parameters ----
setwd("~/Documents/career/RKG/code/RKG-Data-Pull")
data_path <- "~/Documents/career/RKG/data/"
read_file <- "Data Pull_NashuaNH.xlsx"
var_sheet <- "Data Pull"

census_api_key("2f7688b42a2c229e0662079bf0f4f5400cbb7551")

# data(fips_codes)

# inputs ----
acs_year <- 2013
geo_level <- "county subdivision"
st <- "NH"
cnty <- "Hillsborough county"
survey_type <- "acs5"




# labels/vars ----
labels <- read_excel(paste(data_path,read_file,sep=""),sheet=var_sheet)
labels <- filter(labels, Source=="ACS Data")
tables <- labels[2]

vars <- load_variables(acs_year, survey_type, cache = TRUE)
vars <- select(vars, "name","label")
vars <- rename(vars, "variable"="name")




# wb setup ----
# wb <- createWorkbook()
wb <- loadWorkbook(paste(data_path,read_file,sep=""))

# loop ----
for(i in 1:nrow(tables)) {
  df <- get_acs(geography=geo_level,
              table = tables[[i,1]],
              state = st,
              county = cnty,
              year = acs_year,
              survey = survey_type)
  
  df <- df %>%
    filter(str_detect(NAME, 'Nashua city'))
  
  sht <- tables[[i,1]]
  addWorksheet(wb,sht)
  writeData(wb,sht,df)
  
  if(i==1){sexage.func(df)}
}



# save ----
cnty_name <- str_replace(cnty," ","_")


name <- "Nashua City"
write_file <- paste(name,st,acs_year,"Data.xlsx",sep="_")
write_file <- paste(data_path,write_file,sep="")
saveWorkbook(wb,write_file,overwrite = T)
