### FUNCTION LIST ###


sexage.func <- function(df_insert){
  fin_df <- inner_join(vars,df_insert,by = "variable")
  fin_df <- select(fin_df, -"GEOID", -"NAME")
  fin_df$label <- str_remove_all(fin_df$label, "Estimate!!")
  fin_df$label <- str_replace_all(fin_df$label, "!!", "_")
  fin_df$label <- str_remove_all(fin_df$label, "Male_")
  fin_df$label <- str_remove_all(fin_df$label, "Female_")
  fin_df <- fin_df %>% 
    group_by(label) %>% 
    summarise(estimate = sum(estimate))
  return(fin_df)
}