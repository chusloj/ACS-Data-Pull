### CALL TO FUNCTIONS FROM funcs.r ###


call.func <- function(t){
  
  #Sex by age
  if(tables[[t,1]]=="B01001"){sexage.func(df_func)}
  
  #Race
  if(tables[[t,1]]=="B02001"){race.func(df_func)}
  
}