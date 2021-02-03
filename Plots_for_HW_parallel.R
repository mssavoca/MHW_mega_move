# Supplemental plots for Heather Welch's marine heat wave and animal movement paper ----
# batch method # 1 (MEDIUM), foreach: will run make all of your plots at once
# batch method # 2 (FASTEST), run in parallel (using multiple cores of your computer at once to make multiple plots at once)
# batch method # 3 (SLOWEST), map

# load librarias
library(tidyverse)
# devtools::install_github("daattali/ggExtra") # whole mess of package bugs going on -> https://github.com/daattali/ggExtra/issues/75
library(ggExtra)
# install.packages("ggOceanMapsData", repos = c("https://mikkovihtakari.github.io/drat", "https://cloud.r-project.org"))
library(ggOceanMapsData)
library(glue)
library(foreach)
library(doParallel, quietly = TRUE)

# define objects
# comb_data_full <- read.csv("ebird_npdb_cleaned_01_22_21.csv")
comb_data_full <- read.csv("/Users/heatherwelch/Dropbox/OLE/data/species_data/ebird_npdb_cleaned_01_22_21.csv")
outdir="/Users/heatherwelch/Dropbox/OLE/github/MHW_mega_move/plots"

minx =180
maxx =250
miny =15
maxy =65

x <- fortify(maps::map("world2",
               plot=FALSE,fill=TRUE))

plotting_function=function(minx,maxx,miny,maxy,spp,outdir,df){
  dat=comb_data_full %>% filter(species==spp)
  
plot1 <- ggplot(dat) +
  geom_point(aes(x = long360, y = lat, color = data), 
             alpha = 0.2) +
  scale_colour_manual("",values = c("red", "blue")) +
  geom_map(data=x,map=x,aes(map_id=region,x=long,y=lat),fill="darkgrey",color="black")+
  xlim(minx,maxx)+ylim(miny,maxy)+
  xlab("longitude")+ylab("latitude")+
  labs(title = glue("{spp}")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.9, 0.9),
        legend.background = element_blank(),
        legend.key=element_blank())

plot1_full <-  ggMarginal(
  plot1, 
  x = "long360", y = "lat",
  groupColour = TRUE, groupFill = F,
  type = "density",
  alpha = .3
)

png(glue("{outdir}/{spp}.png"),width=22,height=22,type = "cairo",units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
print({plot1_full})
dev.off()

}

# 1. foreach ####
# takes my computer 60 seconds
sp=unique(comb_data_full$species)
system.time(print(
  foreach(i=1:length(sp),.export = c("plotting_function","comb_data_full","outdir","minx","maxx","miny","maxy"),.packages = c("ggplot2","glue","ggExtra","tidyverse"),.verbose=T) %do% {
    print(sp[i])
    plotting_function(spp = sp[i],minx=minx,maxx=maxx,miny=miny,maxy=maxy,outdir=outdir,df=comb_data_full)
  }
))

# 2. run in parallel ####
# takes my computer 26 seconds
# THIS ONLY WORKS IN TERMINAL
# steps: 1. open terminal, 2. type "r" to open r in terminal, 3. copy paste lines 6-62 into terminal, 4. copy paste this section into terminal

detectCores() ## run this to see how many cores your computer has; leave a few free for other processes
registerDoParallel(6)
sp=unique(comb_data_full$species)
system.time(print(
  foreach(i=1:length(sp),.export = c("plotting_function","comb_data_full","outdir","minx","maxx","miny","maxy"),.packages = c("ggplot2","glue","ggExtra","tidyverse"),.verbose=T) %dopar% {
     print(sp[i])
    plotting_function(spp = sp[i],minx=minx,maxx=maxx,miny=miny,maxy=maxy,outdir=outdir,df=comb_data_full)
  }
))
# after you run this code, open your activity monitor. you will see you have spawned 6 simultaneous r sessions, each one is making a different plot

# 3. map ####
# takes my computer 61 seconds
# i just learned about this (embarrassing), the tidy way to do a lapply
sp=unique(comb_data_full$species)
system.time(print(
  map(sp,plotting_function,minx=minx,maxx=maxx,miny=miny,maxy=maxy,outdir=outdir,df=comb_data_full)
))


    



