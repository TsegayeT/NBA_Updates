install.packages("gganimate")
library(tidyverse)
library(ggplot2)
library(tidyr)
library(readxl)
library(gganimate)
library(lubridate)
library(ggthemes)

#Delete every 13th rw
####Nth.delete <- function(DC_Wards, n)DC_Wards[-(seq(n, to=nrow(DC_Wards),by=n)),]
####DC_Wards <- Nth.delete(DC_Wards, 13)


DC_Wards <- read_excel("C:/Users/ttsegaye/OneDrive - Government of The District of Columbia/Desktop/DC_Wards.xls")
DC_Wards$Years <- str_c(DC_Wards$Month_Str, " ", DC_Wards$Year)

DC_Wards <- DC_Wards %>% filter(Month != 13)

DC_Wards$Years <- my(DC_Wards$Years)
DC_Wards$Years <- as.numeric(DC_Wards$Years)
format(DC_Wards$Years, "%B%Y")
sapply(DC_Wards, class)


DC_Wards %>% 
  ggplot(aes(Years,`Unemployment Rate`))+
  geom_line()+
  geom_point(color = "blue", size =1.5)+
  geom_smooth(color = "white", alpha =0.5)+
  facet_grid(~ `Area Title`)+
  ggthemes::theme_fivethirtyeight()+
  labs(title = "Unemployment Rate by Ward Jan2015-Sept2021",,
       caption = "plot:@_ThomasT; Data: BLS_LAUS")
  transition_time(Years)+
  labs(title = "Year: {frame_time}")
