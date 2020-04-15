# install.packages(
#   "tidyverse",
#   "tidycensus",
#   "readxl",
#   "openxlsx"
# )

# After you have run the code above, please either delete the code
# or comment out the code using Ctrl-Shift-C

rm(list=ls())

setwd("C:/Users/JCHUSL01/Downloads/OneDrive_1_4-15-2020/code/ACS-Data-Pull")


library(tidyverse)
library(tidycensus)
library(readxl)
library(openxlsx)
source("funcs.r")
source("call.r")


# %%%%%%%%%%%% BEGIN MESSING WITH THIS %%%%%%%%%%%%



# Parameters/FILEPATHS ----
data_path <- "C:/Users/JCHUSL01/Downloads/OneDrive_1_4-15-2020/data"     # This is the path where "read_file" lives
read_file <- "Data Pull_NashuaNH.xlsx"     # This is the file that contains codes for all tables you want to pull
var_sheet <- "Data Pull"     # This specifies the sheet in "read_file" that contains the information mentioned above




# inputs ----
acs_years <-c(2013,2018)   # Please pick 2 separate years
st <- "NH"     # state
geo_level <- "county"  # geographic level. search "tidycensus" online to see different options
cnty <- "Hillsborough County"   # County
survey_type <- "acs5"   # 5-year ACS, 1-year ACS, etc...

cnty_name <- str_replace(cnty," ","_") # Please do not touch this.
name <- cnty_name

# ******** IMPORTANT PLEASE READ **********
# CHOOSE st, cnty_name, or insert the name of a custom place you're trying to 
# If you do NOT use 'cnty_name', you MUST input the name exactly, CASE SENSITIVE, as it appears
# in the data pulled from the acs


# %%%%%%%%%%% STOP MESSING WITH THIS %%%%%%%%%%%%%%%






# labels ----
labels <- read_excel(paste(data_path,read_file,sep="/"),sheet=var_sheet)
labels <- filter(labels, Source=="ACS Data")
tables <- labels[2]




# wb setup ----
wb <- loadWorkbook(paste(data_path,read_file,sep="/"))
# wb <- createWorkbook()









# loop ----
cnty_require <- c("county","county subdivision","tract","block group")
if(geo_level %in% cnty_require){cnty_val <- cnty} else {cnty_val <- NULL}

for(t in 1:nrow(tables)){
  col_num <- 1
  yr <- acs_years[1]
  df <- get_acs(geography=geo_level,
              table = tables[[t,1]],
              state = st,
              county = cnty_val,
              cache_table = TRUE,
              year = yr,
              survey = survey_type)

  if(geo_level == "county subdivision" | geo_level == "place"){
    df <- df %>%
      filter(str_detect(NAME, name))
  }

  
  vars <- load_variables(yr, survey_type, cache = TRUE)
  vars <- select(vars, "name","label")
  vars <- rename(vars, "variable"="name")

  input_df <- inner_join(vars,df,by = "variable")
  
  sht <- tables[[t,1]]
  addWorksheet(wb,sht)
  writeData(wb,sht,input_df,startCol = col_num)

  call.func(t,input_df)
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  col_num <- ncol(df)+5
  yr <- acs_years[2]
  df_2 <- get_acs(geography=geo_level,
                table = tables[[t,1]],
                state = st,
                county = cnty_val,
                cache_table = TRUE,
                year = yr,
                survey = survey_type)
  
  if(geo_level == "county subdivision" | geo_level == "place"){
    df_2 <- df_2 %>%
      filter(str_detect(NAME, name))
  }

  
  vars <- load_variables(yr, survey_type, cache = TRUE)
  vars <- select(vars, "name","label")
  vars <- rename(vars, "variable"="name")

  input_df_2 <- inner_join(vars,df_2,by = "variable")
  
  
  sht <- tables[[t,1]]
  writeData(wb,sht,input_df_2,startCol = col_num)
  
  call.func(t,input_df_2)
}





# save ----
write_file <- paste(name,st,"Data.xlsx",sep="_")
write_file <- paste(data_path,write_file,sep="")
saveWorkbook(wb,write_file,overwrite = T)
print(write_file)




#If you get warnings that say "unknown or uninitialised [something]", ignore that warning

