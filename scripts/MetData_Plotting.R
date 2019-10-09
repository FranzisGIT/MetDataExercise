# plotting BOM data as an exercise in visualisation

library(tidyverse)  # use tidyverse library and function set

# From BOM MetData exercise use the script bits that tidy up the data 
# and combine stations and collected data to recreate BOM_Combo

BOM_data <- read_csv("data/BOM_data.csv")   # read in data from BOM


#first separate Temp_min_max into two separate columns and change them and rainfall into numeric type
BOM_data_tidy <- BOM_data %>% 
  separate(col=Temp_min_max,into=c("Temp_min","Temp_max"), "/") %>%   
  mutate(
    Temp_min = as.numeric(Temp_min),
    Temp_max = as.numeric(Temp_max),
    Rainfall = as.numeric(Rainfall),
    Solar_exposure = as.numeric(Solar_exposure),
    Temp_diff = Temp_max-Temp_min )  #make a new variable of temperature difference



# there is no state in this data set thus I need to look at the BOM_stations data
BOM_stations <- read_csv("data/BOM_stations.csv")  # read in station information from BOM
BOM_stations     


BOM_stations_trans <- BOM_stations %>% 
  gather(station_no, data, 2:21) %>% 
  spread(info,data)



# tidy up the data types in the various columns, making numberic values numeric
BOM_stations_tidy <-BOM_stations_trans %>% 
  mutate(
    elev = as.double(elev, length=5), 
    end = as.double(end),   
    station_no = as.integer(station_no), 
    lat = as.double(lat, length=5), 
    lon = as.double(lon, length=5), 
    start = as.double(start)
  ) 

#now I have a cleaned up data set I can link to the BOM_data_tidy to get state information for Q3

BOM_Combo <- full_join(BOM_stations_tidy, BOM_data_tidy, by=c("station_no"="Station_number"))

# start with challenge of putting it al together https://csiro-data-school.github.io/visualisation/04-theming/index.html
glimpse(BOM_Combo)
# filter for staion no 9225 as Perth
stnPerth <- BOM_Combo %>% 
  filter(station_no==9225)

#Q1 plot each variable against y axis separately
Perth1 <- ggplot(data = stnPerth, 
         mapping = aes(x = Temp_max,         
                       y = Temp_min
         ))+
  geom_point()

Perth2 <- ggplot(data = stnPerth, 
       mapping = aes(x = Temp_max,         
                     y = Rainfall
       ))+
  geom_point()

Perth3 <- ggplot(data = stnPerth, 
       mapping = aes(x = Temp_max,         
                     y = Solar_exposure
       ))+
  geom_point()

#Q2:Put all three into a single plot


Perth_combo <- ggplot(data = stnPerth, 
       mapping = aes(x = Temp_max,         
                     y = Temp_min,
                     colour = Solar_exposure,  # use colour intensity 
                     size = Rainfall           # use symbol size
                     
       ))+
  geom_point()+
  theme(legend.position="bottom")

#Q3 make multipanel figure from Q1 & Q2
# first I need to install the cowplot paccke, then call up the library 
#install.packages("cowplot") # comment this out because once installed it should be there

library(cowplot)

comboPlot1 <- plot_grid(Perth1, Perth2, Perth3, Perth_combo )
ggsave("figures/comboPlot1.jpg", 
             plot=comboPlot1, 
             width=17, 
             height=8,
             units= "cm",
             dpi=600)

#Q4 calculate the average monthly rainfall for each station lineplot to visualise this data and 
#the state each station

AvRain <- BOM_Combo %>% 
  filter(!is.na(Rainfall)) %>%       # exclude nulls
  group_by(station_no, Month) %>%           # group bt station
  summarise(meanRain = mean(Rainfall)) %>%     # calculate mean by station
  full_join(BOM_stations_tidy, by=c("station_no"="station_no"))  # rejoin to station info

# plot data
AvRainPlot <- ggplot(data = AvRain, 
                 mapping = aes(x = Month,         
                               y = meanRain,
                               group=station_no,
                               colour=state
                 ))+
  geom_line()+
  facet_wrap(~state)

AvRainPlot
