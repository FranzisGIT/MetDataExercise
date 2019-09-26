library(tidyverse)              # use tidyverse library and function set


#COMMENT on where to find the data if not hosted on gitHUB (specified on gitignore)

BOM_data <- read_csv("data/BOM_data.csv")   # read in data from BOM


#CHALLENGE Question 1: 2 approaches
# a)  returns the number of rows where ALL data value minimum, maximum temp or rainfall are filled in
numStn_ALLtemp_raindata <-  BOM_data %>% 
  separate(col=Temp_min_max,into=c("Temp_min","Temp_max"), "/") %>%   
  filter(Temp_min!="-", Temp_max != "-", Rainfall != "-") %>%  
  # group_by(Station_number) %>%              # this is redundant when using count function
  count(Station_number)

  
# b) alternative on Question 1  
# returns the number of rows where ANY data value minimum, maximum temp or rainfall are filled in
numStn_ANYtemp_raindata <-BOM_data %>% 
  separate(col=Temp_min_max,into=c("Temp_min","Temp_max"), "/") %>%   
  filter(Temp_min!="-" | Temp_max != "-" | Rainfall != "-") %>% 
  # group_by(Station_number) %>%              # this is redundant when using count function
  count(Station_number)

#CHALLENGE Question 2: month with the lowest average daily temperature difference

# look at the BOM data to remind us what it looks like
glimpse(BOM_data)

#first separate Temp_min_max into two separate columns and change them and rainfall into numeric type
BOM_data_tidy <- BOM_data %>% 
  separate(col=Temp_min_max,into=c("Temp_min","Temp_max"), "/") %>%   
  mutate(Temp_min = as.numeric(Temp_min)) %>%
  mutate(Temp_max = as.numeric(Temp_max)) %>%
  mutate(Rainfall = as.numeric(Rainfall)) %>% 
  mutate(Solar_exposure = as.numeric(Solar_exposure)) %>% 
  
  mutate(Temp_diff = Temp_max-Temp_min) #make a new variable of temperature difference

# using the tidied up data frame select which state saw the lowest average daily temperature difference to answer Q3 
Q2Result <- BOM_data_tidy %>% 
   filter(!is.na(Temp_diff)) %>%    # call function is.na to (use ! for IsNot) to exclude NA.
   group_by(Month) %>% 
   summarise(Mean_TDiff = mean(Temp_diff)) %>% 
  filter(Mean_TDiff==min(Mean_TDiff))


#CHALLENGE Question 3: state saw the lowest average daily temperature difference

# look at the BOM_data_tidy to remind us what it looks like
glimpse(BOM_data_tidy)

# there is no state in this data set thus I need to look at the BOM_stations data
BOM_stations <- read_csv("data/BOM_stations.csv")  # read in station information from BOM
BOM_stations              
 
#there is no real header row in thids data set, thus read it in without assigning header labels 

BOM_stations <- read_csv("data/BOM_stations.csv", col_names=FALSE)
BOM_stations
 # ALTERNATIVE TO MY APPROACH more elegant version.... 
  # gather(BOM_stations, station_no, data, 2:21)

#now it made column headers X1 to X21 and all rows can be used as data 
# Cleaning up stations data by transposing and using the first column as new column headers

# should loop following somehow... What I do is creating temporary tibbles stepping through each column (X2 to X21) 
# and speading each column over the headers which are in Column1 (X1). I bind the result together alernating
# between the tibbles temp1 and temp2; at end I rename the last tibble and remove the temp1 and temp2 tibbles

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

# I rename the last temporary tibble and remove the temp1 and temp2 tibbles
BOM_stations_trans <- temp2   
rm(temp1)
rm(temp2)

# tidy up the data types in the various columns, making numberic values numeric
BOM_stations_tidy <-BOM_stations_trans %>% 
  mutate(elev = as.double(elev, length=5)) %>% 
  mutate(end = as.double(end)) %>%   
  mutate(station_no = as.integer(info)) %>% 
  mutate(lat = as.double(lat, length=5)) %>% 
  mutate(lon = as.double(lon, length=5)) %>% 
  mutate(start = as.double(start)) 

#now I have a cleaned up data set I can link to the BOM_data_tidy to get state information for Q3

BOM_Combo <- full_join(BOM_stations_tidy, BOM_data_tidy, by=c("station_no"="Station_number"))

# using the combined data frame select which state saw the lowest average daily temperature difference to answer Q3

Q3Result <- BOM_Combo %>% 
  filter(!is.na(Temp_diff)) %>%    # call function is.na to (use ! for IsNot) to exclude NA.
  group_by(state) %>% 
  summarise(Mean_TDiff = mean(Temp_diff)) %>% 
  filter(Mean_TDiff==min(Mean_TDiff))


# cleaner way try out gather(BOM_stations, station_no, data, 2:21)

# CHALLENGE Question 4: Does the westmost (lowest longitude) or eastmost (highest longitude) weather station 
#have a higher average solar exposure?
Q4Result <- BOM_Combo %>% 
  filter(!is.na(Solar_exposure)) %>%    # call function is.na to (use ! for IsNot) to exclude NA.
  group_by(station_no) %>% 
  summarise(Mean_SolEx = mean(Solar_exposure)) %>%               # create the mean SolExp by station
  full_join(BOM_stations_tidy, by=c("station_no"="station_no")) %>%    # rejoin station info to the grouped data
  filter(lon==max(lon) | lon==(min(lon))) %>%                        # filter out the eastern/ western most statons
# up to here I have picked out the eastern and westernmost stations now I add a  variable easter or westrn most 
#then filter out the lowest Solar exposure to give result
  mutate("Loc"= case_when(lon==min(lon) ~ "westmost", lon==max(lon) ~ "eastmost")) %>% 
  filter(Mean_SolEx==min(Mean_SolEx))


library(tidyverse)  
# ALTERNATIVE TO MY APPROACH more elegant version....
BOM_stationsH <- read_csv("data/BOM_stations.csv", col_names=TRUE)
BOM_stations_trans2 <- BOM_stationsH %>% 
  gather(station_no, data, 2:21) %>% 
  spread(info,data)
  


  

