# Supplemental plots for Heather Welch's marine heat wave and animal movement paper ----

library(tidyverse)
library(ggExtra)
install.packages("ggOceanMapsData", repos = c("https://mikkovihtakari.github.io/drat", "https://cloud.r-project.org"))
library(ggOceanMapsData)

comb_data_full <- read.csv("ebird_npdb_cleaned_01_22_21.csv")

to360 <- function(x) {x %% 360}
minx =to360(-185)
maxx = to360(-110)
miny = to360(15)
maxy = to360(65)

minpoly2=matrix(c(minx,miny, 
                  minx,maxy,
                  maxx,maxy,
                  maxx,miny,
                  minx,miny),
                ncol=2,byrow = T) %>% 
  as.data.frame() %>% 
  rename(lon=V1,lat=V2)




LAAL_plot1 <- comb_data_full %>% 
  filter(species == "laysanAlbatross") %>% 
  ggplot() +
  geom_point(aes(x = long360, y = lat, color = data), 
             alpha = 0.2) +
  scale_colour_manual(values = c("red", "blue")) +
  geom_polygon(data = fortify(maps::map("world2",
                                        plot=FALSE,fill=TRUE)), 
               aes(x=long, y = lat, group=group),
               color="black",fill="grey") +
  geom_polygon(data=minpoly2,aes(x=lon,y=lat), 
               color = "black",fill=NA,size=.5)+
  #geom_sf(data=boxx,color="red",fill=NA)+
  # scale_x_continuous(expand=c(0,0),limits = c(-185,-110))+
  # scale_y_continuous(expand=c(0,0),limits = c(15,65)) +
  coord_equal()+
  coord_sf(xlim = c(minx,maxx), ylim = c(miny,maxy),expand=F)+
  labs(title = "Laysan albatross") +
  theme_bw() +
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
    
