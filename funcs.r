### FUNCTION LIST ###

sexage.func <- function(df_insert){
  row_count <- nrow(df_insert)
  fin_df <- df_insert

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
  
  # fin_df <- tibble::add_column(fin_df, ages = "")
  fin_df$ages <- NA
  
  # Create the 'ages' column
  fin_df$ages <- suppressWarnings(as.numeric(str_sub(fin_df$label,1,2)))
  
  fin_df$ages[str_detect(fin_df$label,"Under 5")] <- 0
  fin_df$ages[str_detect(fin_df$label,"5 to 9")] <- 5
  
  fin_df <- arrange(fin_df,fin_df$ages)
  
  vals <- c()
  vals <- append( vals,(fin_df %>% group_by(ages < 18) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append( vals,(fin_df %>% group_by(ages >= 18 & ages <= 24) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append( vals,(fin_df %>% group_by(ages >= 25 & ages <= 34) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append( vals,(fin_df %>% group_by(ages >= 35 & ages <= 44) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append( vals,(fin_df %>% group_by(ages >= 45 & ages <= 54) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append( vals,(fin_df %>% group_by(ages >= 55 & ages <= 64) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append( vals,(fin_df %>% group_by(ages >= 65) %>% summarise(estimate=sum(estimate)))[[2,2]] )

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
  row_count <- nrow(df_insert)
  fin_df <- df_insert
  
  fin_df <- fin_df %>%
    filter(!str_detect(fin_df$label, "including"))
  fin_df <- fin_df %>%
    filter(!str_detect(fin_df$label, "excluding"))
  fin_df <- fin_df %>%
    filter(!fin_df$variable=="B02001_001")
  
  fin_df$label <- str_remove_all(fin_df$label, "Estimate!!Total!!")
  fin_df$label <- str_remove_all(fin_df$label, " alone")
  
  fin_df <- select(fin_df,label,estimate)
  
  write_df <- fin_df
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
}







hispan.func <- function(df_insert){
  row_count <- nrow(df_insert)
  fin_df <- df_insert
  
  fin_df <- fin_df %>%
    filter(fin_df$variable=="B03002_002" | fin_df$variable=="B03002_012")
  fin_df$label <- str_remove_all(fin_df$label, "Estimate!!Total!!")
  fin_df <- select(fin_df,label,estimate)

  write_df <- fin_df
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
}






educ.func <- function(df_insert){
  row_count <- nrow(df_insert)
  fin_df <- df_insert
  
  fin_df$label <- str_remove_all(fin_df$label, "Estimate!!Total!!")
  fin_df <- fin_df %>%
    filter(!fin_df$variable=="B15003_001")
  
  fin_df$tab_var <- as.numeric(str_sub(fin_df$variable,-2,-1))
  
  vals <- c()
  vals <- append(vals, (fin_df %>% group_by(tab_var <= 16) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(tab_var >= 17 & tab_var <= 18) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(tab_var >= 19 & tab_var <= 21) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals,fin_df$estimate[fin_df$tab_var==22])
  vals <- append(vals,fin_df$estimate[fin_df$tab_var==23])
  vals <- append(vals,sum(fin_df$estimate[fin_df$tab_var==24 | fin_df$tab_var==25]))
  
  col_labels <- c(
    "No High School Diploma",
    "High School Diploma or Equivalent",
    "Associates or Some College",
    "Bachelor's Degree",
    "Master's Degree",
    "Professional or Doctorate"
  )
  
  write_df <- data.frame(labels = col_labels, estimates = vals)
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
}






pov.func <- function(df_insert){
  row_count <- nrow(df_insert)
  fin_df <- df_insert
  
  
  fin_df <- fin_df %>%
    filter(fin_df$variable=="B17001_001" | fin_df$variable=="B17001_002")
  fin_df$label <- str_remove_all(fin_df$label, "Estimate!!")
  fin_df$label <- str_remove(fin_df$label, "Total!!")
  fin_df <- select(fin_df,label,estimate)
  
  
  write_df <- fin_df
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
}






inc.func <- function(df_insert){
  row_count <- nrow(df_insert)
  fin_df <- df_insert
  
  fin_df$label <- str_remove_all(fin_df$label, "Estimate!!Total!!")
  fin_df <- fin_df %>%
    filter(!fin_df$variable=="B19001_001")
  fin_df$inc <- str_sub(fin_df$label,2,8)
  fin_df$inc <- str_replace(fin_df$inc,",","")
  fin_df$inc[fin_df$variable=="B19001_002"] <- 0
  
  fin_df$inc <- as.numeric(fin_df$inc)
  
  vals <- c()
  vals <- append(vals, (fin_df %>% group_by(inc < 15000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(inc >= 15000 & inc < 25000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(inc >= 25000 & inc < 35000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(inc >= 35000 & inc < 50000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(inc >= 50000 & inc < 75000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(inc >= 75000 & inc < 100000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(inc >= 100000 & inc < 150000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(inc >= 150000 & inc < 200000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(inc >= 200000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  
  
  col_labels <- c(
    "<$15,000",
    "$15,000 - $24,999",
    "$25,000 - $34,999",
    "$35,000 - $49,999",
    "$50,000 - $74,999",
    "$75,000 - $99,999",
    "$100,000 - $149,999",
    "$150,000 - $199,999",
    "$200,000+"
  )
  
  write_df <- data.frame(labels = col_labels, estimates = vals)
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
}






occup.func <- function(df_insert){
  row_count <- nrow(df_insert)
  fin_df <- df_insert
  
  df_append <- get_acs(geography=geo_level,
                       table = "B25003",
                       state = st,
                       county = cnty,
                       cache_table = TRUE,
                       year = yr,
                       survey = survey_type)
  
  
  df_bind <- bind_rows(fin_df,df_append)
  fin_df_bind <- inner_join(vars,df_bind,by = "variable")
  
  fin_df_bind <- select(fin_df_bind,-label.y)
  fin_df_bind <- rename(fin_df_bind,"label"="label.x")
  fin_df_bind$label <- str_remove_all(fin_df_bind$label, "Estimate!!Total!!")
  fin_df_bind <- fin_df_bind %>%
    filter(!(fin_df_bind$variable=="B25002_001" | fin_df_bind$variable=="B25003_001" | fin_df_bind$variable=="B25002_002"))
  
  
  write_df <- fin_df_bind
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
}






ownrent.func <- function(df_insert){
  row_count <- nrow(df_insert)
  fin_df <- df_insert
  
  df_append <- get_acs(geography=geo_level,
                       table = "B25002",
                       state = st,
                       county = cnty,
                       cache_table = TRUE,
                       year = yr,
                       survey = survey_type)
  
  
  df_bind <- bind_rows(fin_df,df_append)
  fin_df_bind <- inner_join(vars,df_bind,by = "variable")
  
  fin_df_bind <- select(fin_df_bind,-label.y)
  fin_df_bind <- rename(fin_df_bind,"label"="label.x")
  fin_df_bind$label <- str_remove_all(fin_df_bind$label, "Estimate!!Total!!")
  fin_df_bind <- fin_df_bind %>%
    filter(!(fin_df_bind$variable=="B25002_001" | fin_df_bind$variable=="B25003_001" | fin_df_bind$variable=="B25002_002"))
  
  
  write_df <- fin_df_bind
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
}

















ownage.func <- function(df_insert){
  row_count <- nrow(df_insert)
  fin_df <- df_insert
  
  
  fin_df <- fin_df %>%
    filter(str_detect(fin_df$label,"Owner"))
  fin_df$label <- str_remove_all(fin_df$label,"Estimate!!Total!!Owner occupied!!Householder ")
  fin_df$age <- str_sub(fin_df$label,1,2)
  fin_df$age[fin_df$variable=="B25007_002"] <- 0
  
  vals <- c()
  vals <- append( vals,(fin_df %>% group_by(age >= 15 & age <= 24) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append( vals,(fin_df %>% group_by(age >= 25 & age <= 34) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append( vals,(fin_df %>% group_by(age >= 35 & age <= 44) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append( vals,(fin_df %>% group_by(age >= 45 & age <= 54) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append( vals,(fin_df %>% group_by(age >= 55 & age <= 64) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append( vals,(fin_df %>% group_by(age >= 65) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  
  col_labels <- c(
    "15 to 24 years",
    "25 to 34 years",
    "35 to 44 years",
    "45 to 54 years",
    "55 to 64 years",
    "65+ years"
  )
  
  
  write_df <- data.frame(labels = col_labels, estimates = vals)
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
}







numoccup.func <- function(df_insert){
  row_count <- nrow(df_insert)
  fin_df <- df_insert
  
  fin_df <- fin_df %>%
    filter(str_detect(fin_df$label,"Owner"))
  fin_df$label <- str_remove_all(fin_df$label,"Estimate!!Total!!Owner occupied!!")
  fin_df$age <- str_sub(fin_df$label,1,1)
  fin_df <- fin_df %>%
    filter(!fin_df$variable=="B25009_002")
  fin_df$label[fin_df$variable=="B25009_007"] <- "5-or-more person household"
  fin_df$estimate[fin_df$variable=="B25009_007"] <- (fin_df %>% group_by(age >= 5) %>% summarise(estimate=sum(estimate)))[[2,2]]
  fin_df <- fin_df %>%
    filter(!(fin_df$variable=="B25009_008" | fin_df$variable=="B25009_009"))
  fin_df <- select(fin_df, !moe)
  
  write_df <- fin_df
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
}










hometypetotal.func <- function(df_insert){
  row_count <- nrow(df_insert)
  fin_df <- df_insert
  
  fin_df$label <- str_remove_all(fin_df$label,"Estimate!!Total!!")
  fin_df <- fin_df %>% filter(!fin_df$variable=="B25024_001")
  
  write_df <- fin_df
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
}








hometypeown.func <- function(df_insert){
  row_count <- nrow(df_insert)
  fin_df <- df_insert
  
  fin_df <- fin_df %>%
    filter(str_detect(fin_df$label,"Owner"))
  fin_df$label <- str_remove_all(fin_df$label,"Estimate!!Total!!")
  fin_df <- fin_df %>% filter(!fin_df$variable=="B25032_002")
  fin_df$label <- str_replace_all(fin_df$label,"!!",": ")
  
  write_df <- fin_df
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
}







hometyperent.func <- function(df_insert){
  row_count <- nrow(df_insert)
  fin_df <- df_insert
  
  fin_df <- fin_df %>%
    filter(str_detect(fin_df$label,"Renter"))
  fin_df$label <- str_remove_all(fin_df$label,"Estimate!!Total!!")
  fin_df <- fin_df %>% filter(!fin_df$variable=="B25032_013")
  fin_df$label <- str_replace_all(fin_df$label,"!!",": ")
  
  write_df <- fin_df
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
}







totalyrhouse.func <- function(df_insert){
  row_count <- nrow(df_insert)
  fin_df <- df_insert
  
  fin_df$label <- str_remove_all(fin_df$label,"Estimate!!Total!!Built ")
  fin_df$year <- str_sub(fin_df$label,1,4)
  fin_df <- fin_df %>% filter(!fin_df$variable=="B25034_001")
  
  vals <- c()
  vals <- append( vals,(fin_df %>% group_by(year >= 2000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append( vals,(fin_df %>% group_by(year >= 1980 & year < 2000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append( vals,(fin_df %>% group_by(year >= 1960 & year < 1980) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append( vals,(fin_df %>% group_by(year < 1960) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  
  col_labels <- c(
    "2000 or later",
    "Between 1980 and 1999",
    "Between 1960 and 1979",
    "1959 or earlier"
  )
  
  write_df <- data.frame(labels = col_labels, estimates = vals)
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
}










ownedyrhouse.func <- function(df_insert){
  row_count <- nrow(df_insert)
  fin_df <- df_insert
  
  fin_df <- fin_df %>%
    filter(str_detect(fin_df$label,"Owner"))
  fin_df$label <- str_remove_all(fin_df$label,"Estimate!!Total!!Owner occupied!!Built ")
  fin_df <- fin_df %>% filter(!fin_df$variable=="B25036_002")
  fin_df$year <- str_sub(fin_df$label,1,4)
  
  vals <- c()
  vals <- append( vals,(fin_df %>% group_by(year >= 2000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append( vals,(fin_df %>% group_by(year >= 1980 & year < 2000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append( vals,(fin_df %>% group_by(year >= 1960 & year < 1980) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append( vals,(fin_df %>% group_by(year < 1960) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  
  col_labels <- c(
    "2000 or later",
    "Between 1980 and 1999",
    "Between 1960 and 1979",
    "1959 or earlier"
  )
  write_df <- data.frame(labels = col_labels, estimates = vals)
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
}










ownbeds.func <- function(df_insert){
  row_count <- nrow(df_insert)
  fin_df <- df_insert
  
  fin_df <- fin_df %>%
    filter(str_detect(fin_df$label,"Owner"))
  fin_df$label <- str_remove_all(fin_df$label,"Estimate!!Total!!Owner occupied!!")
  fin_df <- fin_df %>%
    filter(!fin_df$variable=="B25042_002")
  
  write_df <- fin_df
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
}










value.func <- function(df_insert){
  row_count <- nrow(df_insert)
  fin_df <- df_insert
  
  fin_df <- fin_df %>% filter(!fin_df$variable=="B25075_001")
  fin_df$label <- str_remove_all(fin_df$label,"Estimate!!Total!!")
  fin_df$inc <- str_sub(fin_df$label,2,9)
  fin_df$inc <- str_replace(fin_df$inc,",","")
  fin_df$inc <- str_remove_all(fin_df$inc," t")
  fin_df$inc[fin_df$variable=="B25075_025"] <- "1000000"
  fin_df$inc[fin_df$variable=="B25075_002"] <- "0"
  fin_df$inc <- suppressWarnings(as.numeric(fin_df$inc))
  
  vals <- c()
  vals <- append(vals, (fin_df %>% group_by(inc < 50000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(inc >= 50000 & inc < 100000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(inc >= 100000 & inc < 150000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(inc >= 150000 & inc < 200000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(inc >= 200000 & inc < 250000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(inc >= 250000 & inc < 300000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(inc >= 300000 & inc < 400000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(inc >= 400000 & inc < 500000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(inc > 500000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  
  
  col_labels <- c(
    "Less than $50,000",
    "$50,000 - 99,999",
    "$100,000 - $149,000",
    "$150,000 - $199,999",
    "$200,000 - $249,999",
    "$250,000 - $299,999",
    "$300,000 - $399,999",
    "$400,000 - $499,999",
    "Greater than $500,000"
  )
  
  write_df <- data.frame(labels = col_labels, estimates = vals)
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
}










ownrace.func <- function(df_insert){
  row_count <- nrow(df_insert)+9
  fin_df <- df_insert
  
  letters_r <- c("A","B","C","D","E","F","G")
  letters_h <- c("H","I")
  
  races <- c(
    "White",
    "Black",
    "American Indian",
    "Asian",
    "Pacific Islander",
    "Some Other Race",
    "Two or More Races"
  )
  
  his_list <- c(
    "Not Hispanic or Latino",
    "Hispanic or Latino"
  )
  
  vals_r <- c()
  vals_h <- c()
  
  for(i in letters_r){
    df <- get_acs(geography=geo_level,
                  table = paste("B25003",str_to_upper(i),sep=""),
                  state = st,
                  county = cnty,
                  cache_table = TRUE,
                  year = yr,
                  survey = survey_type)
    
    if(geo_level=="county subdivision"){
      df <- df %>%
        filter(str_detect(NAME, name))
    }
    
    vars <- load_variables(yr, survey_type, cache = TRUE)
    vars <- select(vars, "name","label")
    vars <- rename(vars, "variable"="name")
    
    fin_df <- inner_join(vars,df,by = "variable")
    
    vals_r <- append(vals_r,fin_df$estimate[grep("*_002",fin_df$variable)])
  }
  
  
  
  
  for(i in letters_h){
    df <- get_acs(geography=geo_level,
                  table = paste("B25003",str_to_upper(i),sep=""),
                  state = st,
                  county = cnty,
                  cache_table = TRUE,
                  year = yr,
                  survey = survey_type)
    
    if(geo_level=="county subdivision"){
      df <- df %>%
        filter(str_detect(NAME, name))
    }
    
    vars <- load_variables(yr, survey_type, cache = TRUE)
    vars <- select(vars, "name","label")
    vars <- rename(vars, "variable"="name")
    
    fin_df <- inner_join(vars,df,by = "variable")
    
    vals_h <- append(vals_h,fin_df$estimate[grep("*_002",fin_df$variable)])
  }
  
  
  
  write_df <- data.frame(labels = races, estimates = vals_r)
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
  
  row_count <- (row_count+5) + (nrow(write_df))
  
  write_df <- data.frame(labels = his_list, estimates = vals_h)
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
}









teninc.func <- function(df_insert){
  row_count <- nrow(df_insert)
  fin_df <- df_insert
  
  fin_df <- fin_df %>%
    filter(str_detect(fin_df$label,"Owner occupied!!"))
  fin_df$label <- str_remove_all(fin_df$label,"Estimate!!Total!!Owner occupied!!")
  fin_df$inc <- str_sub(fin_df$label,1,8)
  fin_df$inc <- str_replace(fin_df$inc,",","")
  fin_df$inc <- str_remove_all(fin_df$inc," t")
  fin_df$inc <- str_sub(fin_df$inc,2)
  fin_df$inc[fin_df$variable=="B25118_003"] <- "0"
  fin_df$inc <- as.numeric(fin_df$inc)
  
  vals <- c()
  vals <- append(vals, (fin_df %>% group_by(inc < 15000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(inc >= 15000 & inc < 25000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(inc >= 25000 & inc < 35000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(inc >= 35000 & inc < 50000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(inc >= 50000 & inc < 75000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(inc >= 75000 & inc < 100000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(inc >= 100000 & inc < 150000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(inc >= 150000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  
  
  col_labels <- c(
    "< $15,000",
    "$15,000 - $24,999",
    "$25,000 - $34,999",
    "$35,000 - $49,999",
    "$50,000 - $74,999",
    "$75,000 - $99,999",
    "$100,000 - $149,999",
    "$150,000 or more"
  )
  
  write_df <- data.frame(labels = col_labels, estimates = vals)
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
}









commute.func <- function(df_insert){
  row_count <- nrow(df_insert)
  fin_df <- df_insert
  
  fin_df$label <- str_remove_all(fin_df$label,"Estimate!!Total!!")
  fin_df <- fin_df %>% filter(!fin_df$variable=="B08303_001")
  fin_df$tm <- str_sub(fin_df$label,1,2)
  fin_df$tm[fin_df$variable=="B08303_002"] <- "0"
  fin_df$tm <- suppressWarnings(as.numeric(fin_df$tm))
  
  vals <- c()
  vals <- append(vals, (fin_df %>% group_by(tm < 15) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(tm >= 15 & tm < 30) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(tm >= 30 & tm < 45) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(tm >= 45 & tm < 60) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(tm > 60) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  
  col_labels <- c(
    "Less than 15 Minutes",
    "15 Minutes to less than 30 Minutes",
    "30 Minutes to Less than 45 Minutes",
    "45 Minutes to less than 60 Minutes",
    "Greater than 60 Minutes"
  )
  
  write_df <- data.frame(labels = col_labels, estimates = vals)
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
}