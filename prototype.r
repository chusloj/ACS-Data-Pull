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
              table = "B01001",
              state = st,
              county = cnty,
              cache_table = TRUE,
              year = acs_years[1],
              survey = survey_type)
  
vars <- load_variables(acs_years[1], survey_type, cache = TRUE)
vars <- select(vars, "name","label")
vars <- rename(vars, "variable"="name")

fin_df <- inner_join(vars,df,by = "variable")

fin_df <- fin_df %>%
  filter(!str_detect(fin_df$label, "including"))
fin_df <- fin_df %>%
  filter(!str_detect(fin_df$label, "excluding"))
fin_df <- fin_df %>%
  filter(!fin_df$variable=="B02001_001")

fin_df$label <- str_remove_all(fin_df$label, "Estimate!!Total!!")
fin_df$label <- str_remove_all(fin_df$label, " alone")

fin_df <- select(fin_df,"label","estimate")