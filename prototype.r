# inputs ----
acs_years <-c(2013,2018)
geo_level <- "county"
st <- "NH"
cnty <- "Hillsborough county"
survey_type <- "acs5"

cnty_name <- str_replace(cnty," ","_")

name <- cnty_name ## CHOOSE st, cnty_name, or insert a custom name

# labels ----
labels <- read_excel(paste(data_path,read_file,sep=""),sheet=var_sheet)
labels <- filter(labels, Source=="ACS Data")
tables <- labels[2]


# wb setup ----
wb <- loadWorkbook(paste(data_path,read_file,sep=""))
# wb <- createWorkbook()



df <- get_acs(geography=geo_level,
              table = "B01003",
              state = st,
              county = cnty,
              cache_table = TRUE,
              year = acs_years[1],
              survey = survey_type)
  
  