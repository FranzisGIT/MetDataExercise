library(tidyverse)              # use tidyverse library and function set
BOM_data <- read_csv("data/BOM_data.csv")   # read in data from BOM


#CHALLENGE Question 1 filtering for data where ALL of the columns are filled in
numStn_ALLtemp_raindata <-  BOM_data %>% 
  separate(col=Temp_min_max,into=c("Temp_min","Temp_max"), "/") %>%   
  filter(Temp_min!="-", Temp_max != "-", Rainfall != "-") %>%  
  # group_by(Station_number) %>%              # this is redundant when using count function
  count(Station_number)

  
# alternative on Question 1  
# returns the number of rows where ANY data value minimum, maximum temp or rainfall are filled in
numStn_ANYtemp_raindata <-BOM_data %>% 
  separate(col=Temp_min_max,into=c("Temp_min","Temp_max"), "/") %>%   
  filter(Temp_min!="-" | Temp_max != "-" | Rainfall != "-") %>% 
  # group_by(Station_number) %>%              # this is redundant when using count function
  count(Station_number)

# Cleaning up stations data
BOM_stations <- read_csv("data/BOM_stations.csv", col_names=FALSE)  # read in station information from BOM
BOM_stations
# should loop following somehow...
temp1 <- BOM_stations %>% 
  select(X1,X2) %>% 
  spread(X1,X2)
temp2 <- BOM_stations %>% 
  select(X1,X3) %>% 
  spread(X1,X3) %>% 
  bind_rows(temp1)
temp1 <- BOM_stations %>% 
  select(X1,X4) %>% 
  spread(X1,X4) %>% 
  bind_rows(temp2)
temp2 <- BOM_stations %>% 
  select(X1,X5) %>% 
  spread(X1,X5) %>% 
  bind_rows(temp1)
temp1 <- BOM_stations %>% 
  select(X1,X6) %>% 
  spread(X1,X6) %>% 
  bind_rows(temp2)
temp2 <- BOM_stations %>% 
  select(X1,X7) %>% 
  spread(X1,X7) %>% 
  bind_rows(temp1)
temp1 <- BOM_stations %>% 
  select(X1,X8) %>% 
  spread(X1,X8) %>% 
  bind_rows(temp2)
temp2 <- BOM_stations %>% 
  select(X1,X9) %>% 
  spread(X1,X9) %>% 
  bind_rows(temp1)
temp1 <- BOM_stations %>%
  select(X1,X10) %>% 
  spread(X1,X10) %>% 
  bind_rows(temp2)
temp2 <- BOM_stations %>% 
  select(X1,X11) %>% 
  spread(X1,X11) %>% 
  bind_rows(temp1)
temp1 <- BOM_stations %>% 
  select(X1,X12) %>% 
  spread(X1,X12) %>% 
  bind_rows(temp2)
temp2 <- BOM_stations %>% 
  select(X1,X13) %>% 
  spread(X1,X13) %>% 
  bind_rows(temp1)
temp1 <- BOM_stations %>% 
  select(X1,X14) %>% 
  spread(X1,X14) %>% 
  bind_rows(temp2)
temp2 <- BOM_stations %>% 
  select(X1,X15) %>% 
  spread(X1,X15) %>% 
  bind_rows(temp1)
temp1 <- BOM_stations %>% 
  select(X1,X16) %>% 
  spread(X1,X16) %>% 
  bind_rows(temp2)
temp2 <- BOM_stations %>% 
  select(X1,X17) %>% 
  spread(X1,X17) %>% 
  bind_rows(temp1)
temp1 <- BOM_stations %>% 
  select(X1,X18) %>% 
  spread(X1,X18) %>% 
  bind_rows(temp2)
temp2 <- BOM_stations %>% 
  select(X1,X19) %>% 
  spread(X1,X19) %>% 
  bind_rows(temp1)
temp1 <- BOM_stations %>% 
  select(X1,X20) %>% 
  spread(X1,X20) %>% 
  bind_rows(temp2)
temp2 <- BOM_stations %>% 
  select(X1,X21) %>% 
  spread(X1,X21) %>% 
  bind_rows(temp1)

BOM_stations_tidy <- temp2
rm(temp1)
rm(temp2)
BOM_stations_tidy