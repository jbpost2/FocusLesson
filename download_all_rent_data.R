#download all rent data locally

library(tidyverse)
library(tidycensus)

state_abb <- datasets::state.name
my_key <- "e267f117801b2ef741e54620602b0903c5f4d3c8"



#rent variable name
rent <- "DP04_0142PE"

all_rent <- lapply(X = state_abb, FUN = function(x){
  get_acs(variables = rent, 
          geography = "tract", #geometry = T returns the polygon data and allows for maps easily
          geometry = TRUE,
          survey = "acs5",
          show_call = TRUE, 
          state = x,
          key = my_key)
})

names(all_rent)<- state_abb

saveRDS(all_rent, file = 'rent.rds')



