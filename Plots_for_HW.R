# Supplemental plots for Heather Welch's marine heat wave and animal movement paper ----

library(tidyverse)
library(lubridate)
library(ggExtra)
install.packages("ggOceanMapsData", repos = c("https://mikkovihtakari.github.io/drat", "https://cloud.r-project.org"))
library(ggOceanMapsData)

comb_data_full <- read.csv("ebird_npdb_cleaned_01_22_21.csv") %>% 
  mutate(date = as_date(date))


minx =180
maxx =245
miny =15
maxy =65

x <- fortify(maps::map("world2",
                       plot=FALSE,fill=TRUE))


# minpoly2=matrix(c(minx,miny, 
#                   minx,maxy,
#                   maxx,maxy,
#                   maxx,miny,
#                   minx,miny),
#                 ncol=2,byrow = T) %>% 
#   as.data.frame() %>% 
#   rename(lon=V1,lat=V2)



# Plot #1 ----
LAAL_plot1 <- comb_data_full %>% 
  filter(species == "laysanAlbatross") %>% 
  ggplot() +
  
  
  geom_point(aes(x = long360, y = lat, color = data), 
             alpha = 0.2) +
  scale_colour_manual(values = c("red", "blue")) +
  
  geom_map(data=x,map=x,aes(map_id=region,x=long,y=lat),
           fill="darkgrey",color="black") +
  xlim(minx,maxx)+ylim(miny,maxy) +
  xlab("longitude")+ylab("latitude") +
  
  # geom_polygon(data = fortify(maps::map("world2",
  #                                       plot=FALSE,fill=TRUE)), 
  #              aes(x=long, y = lat, group=group),
  #              color="black",fill="grey") +
  # geom_polygon(data=minpoly2,aes(x=lon,y=lat), 
  #              color = "black",fill=NA,size=.5)+
  #geom_sf(data=boxx,color="red",fill=NA)+
  # scale_x_continuous(expand=c(0,0),limits = c(-185,-110))+
  # scale_y_continuous(expand=c(0,0),limits = c(15,65)) +
  # coord_equal()+
  # coord_sf(xlim = c(minx,maxx), ylim = c(miny,maxy),expand=F)+
  labs(title = "Laysan albatross") +
  theme_bw(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.2, 0.5))
LAAL_plot1 



LAAL_plot1_full <-  ggMarginal(
  LAAL_plot1, 
  x = "long360", y = "lat",
  groupColour = TRUE, groupFill = F,
  type = "density",
  alpha = .3
)
LAAL_plot1_full

dev.copy2pdf(file="LAAL_plot1_full.pdf", width=12, height=7)




BFAL_plot1 <- comb_data_full %>% 
  filter(species == "black-footedAlbatross") %>% 
  ggplot() +
  
  
  geom_point(aes(x = long360, y = lat, color = data), 
             alpha = 0.2) +
  scale_colour_manual(values = c("red", "blue")) +
  
  geom_map(data=x,map=x,aes(map_id=region,x=long,y=lat),
           fill="darkgrey",color="black") +
  xlim(minx,maxx)+ylim(miny,maxy) +
  xlab("longitude")+ylab("latitude") +

labs(title = "Black-footed albatross") +
  theme_bw(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.2, 0.5))
BFAL_plot1 



BFAL_plot1_full <-  ggMarginal(
  BFAL_plot1, 
  x = "long360", y = "lat",
  groupColour = TRUE, groupFill = F,
  type = "density",
  alpha = .3
)
BFAL_plot1_full

dev.copy2pdf(file="BFAL_plot1_full.pdf", width=12, height=7)


SOSH_plot1 <- comb_data_full %>% 
  filter(species == "sootyShearwater") %>% 
  ggplot() +
  
  
  geom_point(aes(x = long360, y = lat, color = data), 
             alpha = 0.2) +
  scale_colour_manual(values = c("red", "blue")) +
  
  geom_map(data=x,map=x,aes(map_id=region,x=long,y=lat),
           fill="darkgrey",color="black") +
  xlim(minx,maxx)+ylim(miny,maxy) +
  xlab("longitude")+ylab("latitude") +
  
  labs(title = "Sooty shearwater") +
  theme_bw(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.2, 0.5))
SOSH_plot1 



SOSH_plot1_full <-  ggMarginal(
  SOSH_plot1, 
  x = "long360", y = "lat",
  groupColour = TRUE, groupFill = F,
  type = "density",
  alpha = .3
)
SOSH_plot1_full

dev.copy2pdf(file="SOSH_plot1_full.pdf", width=12, height=7)



# Plot 2 ----

LAAL_plot2 <- comb_data_full %>% 
  filter(species == "laysanAlbatross") %>% 
  group_by(date, data) %>% 
  summarize(tot_obs = n()) %>% 
  ggplot() +
  geom_line(aes(date, tot_obs, color = data)) +
  scale_colour_manual(values = c("red", "blue")) +
  labs(title = "Laysan albatross",
       y = "Total observations") +
  theme_bw(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.7, 0.7))
  
LAAL_plot2

dev.copy2pdf(file="LAAL_plot2.pdf", width=9, height=5)


BFAL_plot2 <- comb_data_full %>% 
  filter(species == "black-footedAlbatross") %>% 
  group_by(date, data) %>% 
  summarize(tot_obs = n()) %>% 
  ggplot() +
  geom_line(aes(date, tot_obs, color = data)) +
  scale_colour_manual(values = c("red", "blue")) +
  labs(title = "Black-footed albatross",
       y = "Total observations") +
  theme_bw(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.7, 0.7))

BFAL_plot2

dev.copy2pdf(file="BFAL_plot2.pdf", width=9, height=5)


SOSH_plot2 <- comb_data_full %>% 
  filter(species == "sootyShearwater") %>% 
  group_by(date, data) %>% 
  summarize(tot_obs = n()) %>% 
  ggplot() +
  geom_line(aes(date, tot_obs, color = data)) +
  scale_colour_manual(values = c("red", "blue")) +
  labs(title = "Sooty shearwater",
       y = "Total observations") +
  theme_bw(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.7, 0.7))

SOSH_plot2

dev.copy2pdf(file="SOSH_plot2.pdf", width=9, height=5)


CAsealion_plot2 <- comb_data_full %>% 
  filter(species == "californiaSeaLion") %>% 
  group_by(date, data) %>% 
  summarize(tot_obs = n()) %>% 
  ggplot() +
  geom_line(aes(date, tot_obs, color = data)) +
  scale_colour_manual(values ="blue") +
  labs(title = "CA sea lion",
       y = "Total observations") +
  theme_bw(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.7, 0.7))
CAsealion_plot2

dev.copy2pdf(file="CAsealion_plot2.pdf", width=9, height=5)


NEseal_plot2 <- comb_data_full %>% 
  filter(species == "elephantSeal") %>% 
  group_by(date, data) %>% 
  summarize(tot_obs = n()) %>% 
  ggplot() +
  geom_line(aes(date, tot_obs, color = data)) +
  scale_colour_manual(values ="blue") +
  labs(title = "N. elephant seal",
       y = "Total observations") +
  theme_bw(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.7, 0.7))
NEseal_plot2

dev.copy2pdf(file="NEseal_plot2.pdf", width=9, height=5)



BLSH_plot2 <- comb_data_full %>% 
  filter(species == "blueShark") %>% 
  group_by(date, data) %>% 
  summarize(tot_obs = n()) %>% 
  ggplot() +
  geom_line(aes(date, tot_obs, color = data)) +
  scale_colour_manual(values ="blue") +
  labs(title = "blue shark",
       y = "Total observations") +
  theme_bw(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.7, 0.7))

BLSH_plot2

dev.copy2pdf(file="BLSH_plot2.pdf", width=9, height=5)



 # Plot #3 ----

# Looking weird, especially the long plot

LAAL_plot3_lat <- comb_data_full %>% 
  filter(species == "laysanAlbatross") %>% 
  mutate(mean_lat = mean(lat),
         mean_long360 = mean(long360),
         dev_mean_lat = lat - mean_lat,
         dev_mean_long = long360 - mean_long360) %>% 
  ggplot() +
  geom_line(aes(date, dev_mean_lat, color = data)) +
  scale_colour_manual(values = c("red", "blue")) +
  labs(title = "Laysan albatross",
       y = "Diff from mean latitude") +
  theme_bw(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.7, 0.7))

LAAL_plot3_lat


LAAL_plot3_long <- comb_data_full %>% 
  filter(species == "laysanAlbatross") %>% 
  mutate(mean_lat = mean(lat),
         mean_long360 = mean(long360),
         dev_mean_lat = lat - mean_lat,
         dev_mean_long = long360 - mean_long360) %>% 
  ggplot() +
  geom_line(aes(date, dev_mean_long, color = data)) +
  scale_colour_manual(values = c("red", "blue")) +
  labs(title = "Laysan albatross",
       y = "Diff from mean longitude") +
  theme_bw(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.7, 0.7))

LAAL_plot3_long


