### FUNCTION LIST ###


sexage.func <- function(df_insert){
  row_count <- nrow(df_insert)
  
  vars <- load_variables(acs_years[1], survey_type, cache = TRUE)
  vars <- select(vars, "name","label")
  vars <- rename(vars, "variable"="name")
  
  fin_df <- inner_join(vars,df_insert,by = "variable")

  fin_df$label <- str_remove_all(fin_df$label, "Estimate!!")
  fin_df$label <- str_replace_all(fin_df$label, "!!", "_")
  fin_df$label <- str_remove_all(fin_df$label, "Male_")
  fin_df$label <- str_remove_all(fin_df$label, "Female_")
  fin_df <- fin_df %>%
    group_by(label) %>%
    summarise(estimate = sum(estimate))
  fin_df$label <- str_replace_all(fin_df$label, "_", " ")
  fin_df$label <- str_remove_all(fin_df$label, "Total ")
  fin_df <- fin_df %>%
    filter(!str_detect(fin_df$label, "Total"))
  fin_df <- fin_df %>%
    filter(!str_detect(fin_df$label, "Male"))
  fin_df <- fin_df %>%
    filter(!str_detect(fin_df$label, "Female"))
  
  nums <- sprintf("%s",seq(1:99))
  
  fin_df <- tibble::add_column(fin_df, ages = "")
  
  fin_df$ages <- suppressWarnings(as.numeric(substr(fin_df$label, start = 1, stop = 2)))
  fin_df$ages[str_detect(fin_df$label,"Under 5")] <- 0
  fin_df$ages[str_detect(fin_df$label,"5 to 9")] <- 5
  
  fin_df <- arrange(fin_df,fin_df$ages)
  
  # THERE IS A PROBLEM HERE
  vals <- c()
  vals <- append( vals,(fin_df %>% group_by(ages < 18) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append( vals,(fin_df %>% group_by(ages >= 18 & ages <= 24) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append( vals,(fin_df %>% group_by(ages >= 25 & ages <= 34) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append( vals,(fin_df %>% group_by(ages >= 35 & ages <= 44) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append( vals,(fin_df %>% group_by(ages >= 45 & ages <= 54) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append( vals,(fin_df %>% group_by(ages >= 55 & ages <= 64) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append( vals,(fin_df %>% group_by(ages > 64) %>% summarise(estimate=sum(estimate)))[[2,2]] )

  col_labels <- c(
    "Under 18",
    "18 to 24 years",
    "25 to 34 years",
    "35 to 44 years",
    "45 to 54 years",
    "55 to 64 years",
    "65+ years"
  )
  
  write_df <- data.frame(labels = col_labels, estimates = vals)
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
}

race.func <- function(df_insert){
  vars <- load_variables(acs_years[1], survey_type, cache = TRUE)
  vars <- select(vars, "name","label")
  vars <- rename(vars, "variable"="name")
  
  fin_df <- inner_join(vars,df_insert,by = "variable")
  
  fin_df <- fin_df %>%
    filter(!str_detect(fin_df$label, "including"))
  fin_df <- fin_df %>%
    filter(!str_detect(fin_df$label, "excluding"))
  fin_df <- fin_df %>%
    filter(!fin_df$variable=="B02001_001")
  
  fin_df$label <- str_remove_all(fin_df$label, "Estimate!!Total!!")
  fin_df$label <- str_remove_all(fin_df$label, " alone")
  
  fin_df <- select(fin_df,"label","estimate")
}
