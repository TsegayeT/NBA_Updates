library(tidyverse)
library(ggplot2)
library(readxl)
library(nbastatR)
library(ggimage)
library(ggthemes)

NBA_Rankings <- read_excel("NBA_Rankings.xlsx")


Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

nba_logos <- nba_teams() %>% 
  filter(isNonNBATeam ==0) %>% 
  select(nameTeam, slugTeam, urlThumbnailTeam) %>% 
  mutate(nameTeam = case_when(
    nameTeam == "Los Angeles Clippers" ~ "LA Clippers",
    TRUE ~nameTeam))

nba_Update <- left_join(NBA_Rankings, nba_logos, by=c("Team"="nameTeam"))



nba_Update %>% 
  ggplot(aes(x = NETRTG, y= SOS))+
  geom_point()+
  geom_image(aes(image = urlThumbnailTeam),
             size = 0.038, by ="width", asp =2.3)+
  theme(text = element_text(size = 20))+
  theme_wsj()+
  scale_x_continuous(breaks = seq(-11,13,1))+
  scale_y_continuous(breaks = seq(0.46,0.55,0.01))+
  geom_hline(yintercept = mean(nba_Update$SOS), color = "red", linetype= "dashed", size =1)+
  geom_vline(xintercept = mean(nba_Update$NETRTG),color = "red", linetype= "dashed", size =1 )+
  labs(title = "Net Rating(x-axis) vs Strength of Schedule(y-axis)")

