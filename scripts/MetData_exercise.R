library(tidyverse)              # use tidyverse library and function set
BOM_data <- read_csv("data/BOM_data.csv")   # read in data from BOM
BOM_stations <- read_csv("data/BOM_stations.csv")  # read in station information from BOM

#CHALLENGE Question 1
BOM_data %>% 
  separate(col=Temp_min_max,into=c("Temp_min","Temp_max"), "/") %>%   
  filter(Temp_min!="-", Temp_max != "-", Rainfall != "-") %>%   # filtering for data where ALL of the columns are filled in

  
# alternative on Question 1  
BOM_data %>% 
  separate(col=Temp_min_max,into=c("Temp_min","Temp_max"), "/") %>%   
  filter(Temp_min!="-" | Temp_max != "-" | Rainfall != "-")       # filtering for data where ANY of the columns are filled in

