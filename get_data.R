setwd("~/Documents/career/RKG/code/ACS-Data-Pull")
library(tidyverse)
library(tidycensus)
library(readxl)
library(openxlsx)
source("funcs.r")
source("call.r")
###



# Parameters ----
data_path <- "~/Documents/career/RKG/data/"
read_file <- "Data Pull_NashuaNH.xlsx"
var_sheet <- "Data Pull"

census_api_key("2f7688b42a2c229e0662079bf0f4f5400cbb7551")

# data(fips_codes)

# inputs ----
acs_years <-c(2013,2018)
geo_level <- "county subdivision"
st <- "NH"
cnty <- "Hillsborough county"
survey_type <- "acs5"

cnty_name <- str_replace(cnty," ","_")

name <- "Nashua City" ## CHOOSE st, cnty_name, or insert a custom name

# labels ----
labels <- read_excel(paste(data_path,read_file,sep=""),sheet=var_sheet)
labels <- filter(labels, Source=="ACS Data")
tables <- labels[2]


# wb setup ----
wb <- loadWorkbook(paste(data_path,read_file,sep=""))
# wb <- createWorkbook()


# loop ----
for(t in 1:nrow(tables)){
  col_num <- 1
  df <- get_acs(geography=geo_level,
              table = tables[[t,1]],
              state = st,
              county = cnty,
              year = acs_years[1],
              survey = survey_type)

  if(geo_level=="county subdivision"){
    df <- df %>%
      filter(str_detect(NAME, 'Nashua city'))
  }


  sht <- tables[[t,1]]
  addWorksheet(wb,sht)
  writeData(wb,sht,df,startCol = col_num)

  call.func(t)
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  col_num <- ncol(df)+5
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  df_2 <- get_acs(geography=geo_level,
                table = tables[[t,1]],
                state = st,
                county = cnty,
                year = acs_years[2],
                survey = survey_type)
  
  if(geo_level=="county subdivision"){
    df_2 <- df_2 %>%
      filter(str_detect(NAME, 'Nashua city'))
  }
  
  sht <- tables[[t,1]]
  writeData(wb,sht,df_2,startCol = col_num)
  
  call.func(t)
}


# save ----
write_file <- paste(name,st,"Data.xlsx",sep="_")
write_file <- paste(data_path,write_file,sep="")
saveWorkbook(wb,write_file,overwrite = T)
print(write_file)
