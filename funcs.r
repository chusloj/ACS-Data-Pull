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
  
  # Create the 'ages' column
  fin_df$ages <- parse_number(fin_df$label)
  
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
  fin_df$label <- str_remove_all(fin_df$label, "Estimate!!Total!!")
  
  fin_df <- fin_df%>%
    filter(!str_detect(fin_df$label,"Estimate!!Total"))
  
  fin_df$label <- str_remove_all(fin_df$label, " alone")
  
  write_df <- fin_df
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
}







hispan.func <- function(df_insert){
  row_count <- nrow(df_insert)
  fin_df <- df_insert
  
  fin_df <- fin_df %>%
    filter(str_detect(fin_df$label,"Latino!!"))
  fin_df$label <- str_remove_all(fin_df$label, "Estimate!!Total!!")

  write_df <- fin_df
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
}






educ.func <- function(df_insert){
  row_count <- nrow(df_insert)
  fin_df <- df_insert
  
  fin_df$label <- str_remove_all(fin_df$label, "Estimate!!Total!!")
  fin_df <- fin_df %>%
    filter(!fin_df$label=="Estimate!!Total")
  
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
    filter(!str_detect(fin_df$label,"level!!"))
  fin_df <- fin_df %>%
    filter(!str_detect(fin_df$label,"above"))
  fin_df$label <- str_remove_all(fin_df$label,"Estimate!!")
  fin_df$label <- str_replace(fin_df$label,"!!",": ")
  
  
  write_df <- fin_df
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
}






inc.func <- function(df_insert){
  row_count <- nrow(df_insert)
  fin_df <- df_insert
  
  fin_df$label <- str_remove_all(fin_df$label, "Estimate!!Total!!")
  fin_df <- fin_df %>%
    filter(str_detect(fin_df$label,"\\$"))
  fin_df$inc <- parse_number(fin_df$label)
  fin_df$inc[str_detect(fin_df$label,"Less than \\$")] <- 0
  
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








incage.func <- function(df_insert){
  row_count <- nrow(df_insert)
  fin_df <- df_insert
  
  fin_df$label <- str_remove_all(fin_df$label,"Estimate!!Total!!Householder ")
  fin_df <- fin_df %>% filter(str_detect(fin_df$label,"\\$"))
  
  alllabels <- c()
  allvals <- c()
  brackets <- c("under 25 years","25 to 44 years","45 to 64 years","65 years and over")
  
  for(i in brackets){
    age_df <- fin_df %>% filter(str_detect(fin_df$label,i))
    age_df$label <- gsub(".*!!","",age_df$label)
    age_df$inc <- parse_number(age_df$label)
    age_df$inc[str_detect(age_df$label,"Less than")] <- 0
    
    vals <- c()
    vals <- append(vals, (age_df %>% group_by(inc < 15000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
    vals <- append(vals, (age_df %>% group_by(inc >= 15000 & inc < 25000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
    vals <- append(vals, (age_df %>% group_by(inc >= 25000 & inc < 35000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
    vals <- append(vals, (age_df %>% group_by(inc >= 35000 & inc < 50000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
    vals <- append(vals, (age_df %>% group_by(inc >= 50000 & inc < 75000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
    vals <- append(vals, (age_df %>% group_by(inc >= 75000 & inc < 100000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
    vals <- append(vals, (age_df %>% group_by(inc >= 100000 & inc < 150000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
    vals <- append(vals, (age_df %>% group_by(inc >= 150000 & inc < 200000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
    vals <- append(vals, (age_df %>% group_by(inc >= 200000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
    
    
    col_labels <- c()
    dol <- c(
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
    
    for(j in dol){
      col_labels <- append(col_labels,paste(i,j,sep=": "))
    }
    
    alllabels <- append(alllabels,col_labels)
    allvals <- append(allvals,vals)
  }
  
  write_df <- data.frame(labels = alllabels, estimates = allvals)
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
}










occup_ownrent.func <- function(df_insert){
  row_count <- nrow(df_insert)
  fin_df <- df_insert

  df_append <- get_acs(geography=geo_level,
                       table = "B25003",
                       state = st,
                       county = cnty,
                       # if(geo_level=="place"){} else{county = cnty},
                       cache_table = TRUE,
                       year = yr,
                       survey = survey_type)
  
  if(geo_level!="county"){
    df_append <- df_append %>%
      filter(str_detect(NAME, name))
  }
  
  vars <- load_variables(yr, survey_type, cache = TRUE)
  vars <- select(vars, "name","label")
  vars <- rename(vars, "variable"="name")
  
  fin_df_append <- inner_join(vars,df_append,by = "variable")
  
  
  
  fin_df_bind <- bind_rows(fin_df,fin_df_append)
  fin_df_bind <- fin_df_bind %>%
    filter((str_detect(fin_df_bind$label,"Vacant") |
              str_detect(fin_df_bind$label,"Owner occupied") |
              str_detect(fin_df_bind$label,"Renter occupied")
    ))
  
  fin_df_bind$label <- str_remove_all(fin_df_bind$label,"Estimate!!Total!!")
  
  
  write_df_2 <- fin_df_bind
  writeData(wb,sht,write_df_2,startRow=row_count+5,startCol = col_num)
}












ownage.func <- function(df_insert){
  row_count <- nrow(df_insert)
  fin_df <- df_insert
  
  
  fin_df <- fin_df %>%
    filter(str_detect(fin_df$label,"Owner") | str_detect(fin_df$label,"Renter"))
  fin_df <- fin_df %>%
    filter(str_detect(fin_df$label,"Householder"))
  
  alllabels <- c()
  allvals <- c()
  types <- c("Owner","Renter")
  col_labels <- c(
    "15 to 24 years",
    "25 to 34 years",
    "35 to 44 years",
    "45 to 54 years",
    "55 to 64 years",
    "65+ years"
  )
  
  
  
  for(i in types){
    age_df <- fin_df %>%
      filter(str_detect(fin_df$label,i))
    
    age_df$label <- str_remove_all(age_df$label,"Estimate!!Total!!")
    age_df$label <- str_remove_all(age_df$label,paste(i,"occupied!!Householder ",sep=" "))
    
    age_df$age <- parse_number(age_df$label)
    
    vals <- c()
    vals <- append( vals,(age_df %>% group_by(age >= 15 & age <= 24) %>% summarise(estimate=sum(estimate)))[[2,2]] )
    vals <- append( vals,(age_df %>% group_by(age >= 25 & age <= 34) %>% summarise(estimate=sum(estimate)))[[2,2]] )
    vals <- append( vals,(age_df %>% group_by(age >= 35 & age <= 44) %>% summarise(estimate=sum(estimate)))[[2,2]] )
    vals <- append( vals,(age_df %>% group_by(age >= 45 & age <= 54) %>% summarise(estimate=sum(estimate)))[[2,2]] )
    vals <- append( vals,(age_df %>% group_by(age >= 55 & age <= 64) %>% summarise(estimate=sum(estimate)))[[2,2]] )
    vals <- append( vals,(age_df %>% group_by(age >= 65) %>% summarise(estimate=sum(estimate)))[[2,2]] )
    
    allvals <- append(allvals,vals)
    alllabels <- append(alllabels,paste(i,col_labels,sep=": "))
  }
  
  
  write_df <- data.frame(labels = alllabels, estimates = allvals)
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
}







numoccup.func <- function(df_insert){
  row_count <- nrow(df_insert)
  fin_df <- df_insert
  
  fin_df <- fin_df %>%
    filter(str_detect(fin_df$label,"Total!!"))
  
  own_df <- fin_df %>%
    filter(str_detect(fin_df$label,"Owner"))
  own_df <- own_df %>%
    filter(!own_df$label=="Estimate!!Total!!Owner occupied")
  own_df$label <- str_remove_all(own_df$label,"Estimate!!Total!!Owner occupied!!")
  own_df$per <- parse_number(own_df$label)
  plus <- sum(own_df$estimate[own_df$per>=5])
  own_df <- own_df %>%
    filter(own_df$per<=5)
  own_df$estimate[own_df$per==5] <- plus
  own_df$label[own_df$per==5] <- "5-or-more person household"
  own_df$label <- paste("Owner occupied",own_df$label,sep=": ")
  
  rent_df <- fin_df %>%
    filter(str_detect(fin_df$label,"Renter"))
  rent_df <- rent_df %>%
    filter(!rent_df$label=="Estimate!!Total!!Renter occupied")
  rent_df$label <- str_remove_all(rent_df$label,"Estimate!!Total!!Renter occupied!!")
  rent_df$per <- parse_number(rent_df$label)
  plus <- sum(rent_df$estimate[rent_df$per>=5])
  rent_df <- rent_df %>%
    filter(rent_df$per<=5)
  rent_df$estimate[rent_df$per==5] <- plus
  rent_df$label[rent_df$per==5] <- "5-or-more person household"
  rent_df$label <- paste("Renter occupied",rent_df$label,sep=": ")
  
  fin_df <- rbind(own_df,rent_df)
  
  write_df <- fin_df
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
}










hometypetotal.func <- function(df_insert){
  row_count <- nrow(df_insert)
  fin_df <- df_insert
  
  fin_df <- fin_df %>%
    filter(str_detect(fin_df$label,"Total!!"))
  fin_df$label <- str_remove_all(fin_df$label,"Estimate!!Total!!")
  
  write_df <- fin_df
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
}








hometypeownrent.func <- function(df_insert){
  row_count <- nrow(df_insert)
  fin_df <- df_insert
  
fin_df <- fin_df %>%
    filter(str_detect(fin_df$label,"Owner") | str_detect(fin_df$label,"Renter"))
  fin_df$label <- str_remove_all(fin_df$label,"Estimate!!Total!!")
  fin_df$label <- str_replace_all(fin_df$label,"!!",": ")
  
  write_df <- fin_df
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
}










totalyrhouse.func <- function(df_insert){
  row_count <- nrow(df_insert)
  fin_df <- df_insert
  
  fin_df$label <- str_remove_all(fin_df$label,"Estimate!!Total!!Built ")
  fin_df <- fin_df %>%
    filter(!(str_detect(fin_df$label,"Estimate!!Total")))
  fin_df$year <- parse_number(fin_df$label)
  
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
    filter(str_detect(fin_df$label,"Owner") | str_detect(fin_df$label,"Renter"))
  fin_df <- fin_df %>%
    filter(str_detect(fin_df$label,"Built"))
  fin_df$label <- str_remove_all(fin_df$label,"Estimate!!Total!!")
  
  alllabels <- c()
  allvals <- c()
  types <- c("Owner","Renter")
  
  for(i in types){
    med_df <- fin_df %>% filter(str_detect(fin_df$label,i))
    med_df$label <- str_remove_all(med_df$label,".*!!Built ")
    med_df$year <- parse_number(med_df$label)
    
    vals <- c()
    vals <- append( vals,(med_df %>% group_by(year >= 2000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
    vals <- append( vals,(med_df %>% group_by(year >= 1980 & year < 2000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
    vals <- append( vals,(med_df %>% group_by(year >= 1960 & year < 1980) %>% summarise(estimate=sum(estimate)))[[2,2]] )
    vals <- append( vals,(med_df %>% group_by(year < 1960) %>% summarise(estimate=sum(estimate)))[[2,2]] )
    
    col_labels <- c(
      "2000 or later",
      "Between 1980 and 1999",
      "Between 1960 and 1979",
      "1959 or earlier"
    )
    
    allvals <- append(allvals,vals)
    alllabels <- append(alllabels,paste(i,"occupied: Built",col_labels,sep=" "))
  }
  
  write_df <- data.frame(labels = alllabels, estimates = allvals)
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
}










ownbeds.func <- function(df_insert){
  row_count <- nrow(df_insert)
  fin_df <- df_insert
  
  fin_df <- fin_df %>%
    filter(str_detect(fin_df$label,"Owner occupied!!"))
  fin_df$label <- str_remove_all(fin_df$label,"Estimate!!Total!!Owner occupied!!")
  
  write_df <- fin_df
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
}










value.func <- function(df_insert){
  row_count <- nrow(df_insert)
  fin_df <- df_insert
  
  fin_df <- fin_df %>% filter(str_detect(fin_df$label,"\\$"))
  fin_df$label <- str_remove_all(fin_df$label,"Estimate!!Total!!")
  
  fin_df$inc <- parse_number(fin_df$label)
  fin_df$inc[fin_df$variable=="B25075_002"] <- 0
  
  vals <- c()
  vals <- append(vals, (fin_df %>% group_by(inc < 50000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(inc >= 50000 & inc < 100000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(inc >= 100000 & inc < 150000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(inc >= 150000 & inc < 200000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(inc >= 200000 & inc < 250000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(inc >= 250000 & inc < 300000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(inc >= 300000 & inc < 400000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(inc >= 400000 & inc < 500000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  vals <- append(vals, (fin_df %>% group_by(inc >= 500000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
  
  
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
                  # if(geo_level=="place"){} else{county = cnty},
                  county = cnty,
                  cache_table = TRUE,
                  year = yr,
                  survey = survey_type)
    
    if(geo_level!="county"){
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
                  # if(geo_level=="place"){} else{county = cnty},
                  county = cnty,
                  cache_table = TRUE,
                  year = yr,
                  survey = survey_type)
    
    if(geo_level!="county"){
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
    filter(str_detect(fin_df$label,"Owner occupied!!") | str_detect(fin_df$label,"Renter occupied!!"))
  
  allvals <- c()
  alllabels <- c()
  types <- c("Owner","Renter")
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
  
  
  for(i in types){
    med_df <- fin_df %>%
      filter(str_detect(fin_df$label,i))
    med_df$label <- str_remove_all(med_df$label,".* occupied!!")
    
    med_df$inc <- parse_number(med_df$label)
    med_df$inc[str_detect(med_df$label,"Less than")] <- 0
    
    
    
    vals <- c()
    vals <- append(vals, (med_df %>% group_by(inc < 15000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
    vals <- append(vals, (med_df %>% group_by(inc >= 15000 & inc < 25000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
    vals <- append(vals, (med_df %>% group_by(inc >= 25000 & inc < 35000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
    vals <- append(vals, (med_df %>% group_by(inc >= 35000 & inc < 50000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
    vals <- append(vals, (med_df %>% group_by(inc >= 50000 & inc < 75000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
    vals <- append(vals, (med_df %>% group_by(inc >= 75000 & inc < 100000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
    vals <- append(vals, (med_df %>% group_by(inc >= 100000 & inc < 150000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
    vals <- append(vals, (med_df %>% group_by(inc >= 150000) %>% summarise(estimate=sum(estimate)))[[2,2]] )
    
    allvals <- append(allvals,vals)
    alllabels <- append(alllabels,paste(i,"occupied:",col_labels,sep=" "))
  }
  
  write_df <- data.frame(labels = alllabels, estimates = allvals)
  writeData(wb,sht,write_df,startRow=row_count+5,startCol = col_num)
}









commute.func <- function(df_insert){
  row_count <- nrow(df_insert)
  fin_df <- df_insert
  
  fin_df$label <- str_remove_all(fin_df$label,"Estimate!!Total!!")
  fin_df <- fin_df %>% filter(!str_detect(fin_df$label,"Estimate!!Total"))
  fin_df$tm <- parse_number(fin_df$label)
  fin_df$tm[str_detect(fin_df$label,"Less than ")] <- 0
  
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