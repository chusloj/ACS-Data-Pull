### CALL TO FUNCTIONS FROM funcs.r ###

call.func <- function(t,df_func){
  
  #Sex by age
  if(tables[[t,1]]=="B01001"){sexage.func(df_func)}
  
  #Race
  if(tables[[t,1]]=="B02001"){race.func(df_func)}
  
  #Hispanic
  if(tables[[t,1]]=="B03002"){hispan.func(df_func)}
  
  #Education
  if(tables[[t,1]]=="B15003"){educ.func(df_func)}
  
  #Poverty
  if(tables[[t,1]]=="B17001"){pov.func(df_func)}
  
  #Income
  if(tables[[t,1]]=="B19001"){inc.func(df_func)}
  
  #Occupied/vacant
  if(tables[[t,1]]=="B25002"){occup.func(df_func)}
  
  #Own/rent
  if(tables[[t,1]]=="B25003"){ownrent.func(df_func)}
  
  #Ownership by age
  if(tables[[t,1]]=="B25007"){ownage.func(df_func)}
  
  #home occupation number
  if(tables[[t,1]]=="B25009"){numoccup.func(df_func)}
  
  #(total) home occupation by type
  if(tables[[t,1]]=="B25024"){hometypetotal.func(df_func)}
  
  #(owned) home occupation by type
  if(tables[[t,1]]=="B25032"){hometypeown.func(df_func)}
  
  #(rented) home occupation by type
  if(tables[[t,1]]=="B25032"){hometyperent.func(df_func)}
  
  #(total) year home was built
  if(tables[[t,1]]=="B25034"){totalyrhouse.func(df_func)}
  
  #(owned) year home was built
  if(tables[[t,1]]=="B25036"){ownedyrhouse.func(df_func)}
  
  #(owned) number of bedrooms
  if(tables[[t,1]]=="B25042"){ownbeds.func(df_func)}
  
  #value
  if(tables[[t,1]]=="B25075"){value.func(df_func)}
  
  #homeownership by race
  if(tables[[t,1]]=="B25003"){ownrace.func(df_func)}
  
  #tenure in last 12 months by income
  if(tables[[t,1]]=="B25118"){teninc.func(df_func)}
  
  #commuting time
  if(tables[[t,1]]=="B08303"){commute.func(df_func)}
  
}