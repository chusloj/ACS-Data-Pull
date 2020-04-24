# inputs ----
acs_years <-c(2013,2018)
geo_level <- "county"
st <- "NH"
cnty <- "Hillsborough County"
survey_type <- "acs5"

cnty_name <- str_replace(cnty," ","_")

name <- cnty_name ## CHOOSE st, cnty_name, or insert a custom name

# labels ----
labels <- read_excel(paste(data_path,read_file,sep=""),sheet=var_sheet)
labels <- filter(labels, Source=="ACS Data")
tables <- labels[1]


# wb setup ----
wb <- loadWorkbook(paste(data_path,read_file,sep=""))
# wb <- createWorkbook()

cnty_require <- c("county","county subdivision","tract","block group")
if(geo_level %in% cnty_require){cnty_val <- cnty} else {cnty_val <- NULL}

yr <- acs_years[1]
df <- get_acs(geography = geo_level,
              table = "B03002",
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

fin_df <- inner_join(vars,df,by = "variable")
View(fin_df)






# FUNCTION



