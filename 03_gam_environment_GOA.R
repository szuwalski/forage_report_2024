library(maps)
library("rnaturalearth")
library(interp)
library(RColorBrewer)
library(reshape2) # for melt
library(mgcv)  
library(PBSmapping)
library(mapdata)    #some additional hires data
library(maptools)   #useful tools such as reading shapefiles
library(mapproj)
library(ggplot2)
library(dplyr)
library(patchwork)
library(cowplot)
library(forcats)
library (rgdal)
library(sf)
library(mgcv)
library(gratia)
library(cluster)
library(ggplot2)
library(dplyr)
library(reshape2)
library(parallel)

haul_dat<-read.csv('data/GOA/race_cpue_by_haul.csv',skip=7)
haul_dat$avg_wt<-haul_dat$Weight..kg./haul_dat$Number.of.Fish

ggplot(haul_dat,aes(x=avg_wt))+
  geom_histogram(bins=100)+
  facet_wrap(~Common.Name)+
  theme_bw()

#=================================================================
# make a function that plots for a given species
# spatial maps of all years data
# distribution of recent year + prevalence and biomass estimates
# FILTER THE SALMON, SABLEFISH, POLLOCK, AND OTHER FISH BY SIZE TO REPRESENT FORAGE
#=================================================================

#==try ALL species (fewer) and two covars
#==FILTER OUT EVERYTHING EXCEPT SHELF SURVEY FOR A TRY
names(haul_dat)
in_dat<-haul_dat[haul_dat$Number.CPUE..number.km2.!=0,]

in_dat<-in_dat %>% 
  group_by(Common.Name) %>% 
  filter(n() >= 50)%>%
  ungroup()

use_species<-unique(in_dat$Common.Name)

gam_dat<-filter(haul_dat,Common.Name%in%use_species) 
gam_dat<-filter(haul_dat,Common.Name%in%use_species&Survey=="GOA") 
gam_dat$Presence<-gam_dat$Number.of.Fish>0
gam_dat$Common.Name<-as.factor(gam_dat$Common.Name)
mod4<-bam(Presence~s(Surface.Temperature...C.,by=Common.Name,k=4)+s(Bottom.Depth,by=Common.Name,k=4)+
            s(Gear.Temperature...C.,by=Common.Name,k=4),
          data = gam_dat,family =binomial(link='logit'),discrete=TRUE,nthreads=detectCores())

summary(mod4)


#==SST
smooth_dat<-smooth_estimates(mod4,smooth="Surface.Temperature...C.",partial_match=TRUE,trans=plogis)
smooth_dat$trans_est<-plogis(smooth_dat$est)
smooth_dat$trans_low<-plogis(smooth_dat$est-2*smooth_dat$se)
smooth_dat$trans_upp<-plogis(smooth_dat$est+2*smooth_dat$se)

clust_dat<-dcast(smooth_dat[,c(6,7,8)],Common.Name~Surface.Temperature...C.)
#==calculate dissimilarity matrix
dissim<-daisy(clust_dat[,-1],metric="euclidean")
#==cluster based on dissimilarity matrix
clusts<-hclust(dissim)
plot(clusts)

#==set the number of clusters to retain
clust_num<-4
clust_cut<-cutree(clusts,clust_num)

plot_dat<-melt(clust_dat)
smooth_dat$cluster<-clust_cut[smooth_dat$Common.Name]

fer<-smooth_dat[!duplicated(smooth_dat$Common.Name), ]
table(fer[,c(7,11)])

sst_plot<-ggplot(data=smooth_dat)+
  geom_line(aes(x=Surface.Temperature...C.,y=trans_est,col=Common.Name),lwd=2)+
  geom_ribbon(aes(x=Surface.Temperature...C.,ymin=trans_low,ymax=trans_upp,fill=Common.Name),alpha=.2)+
  theme_bw()+ylab("p(occurence)")+xlab("Sea surface temperature")+labs(col="Species/group",fill="Species/group")+
  facet_wrap(~cluster,nrow=1)

#==depth
smooth_dat<-smooth_estimates(mod4,smooth="Depth",partial_match=TRUE,trans=plogis)
smooth_dat$trans_est<-plogis(smooth_dat$est)
smooth_dat$trans_low<-plogis(smooth_dat$est-2*smooth_dat$se)
smooth_dat$trans_upp<-plogis(smooth_dat$est+2*smooth_dat$se)
smooth_dat$cluster<-clust_cut[smooth_dat$Common.Name]

depth_plot<-ggplot(data=smooth_dat)+
  geom_line(aes(x=Bottom.Depth,y=trans_est,col=Common.Name),lwd=2)+
  geom_ribbon(aes(x=Bottom.Depth,ymin=trans_low,ymax=trans_upp,fill=Common.Name),alpha=.2)+
  theme_bw()+ylab("p(occurence)")+xlab("Depth")+labs(col="Species/group",fill="Species/group")+
  facet_wrap(~cluster,nrow=1)


#==bottom temp
smooth_dat<-smooth_estimates(mod4,smooth="Gear.Temperature...C.",partial_match=TRUE,trans=plogis)
smooth_dat$trans_est<-plogis(smooth_dat$est)
smooth_dat$trans_low<-plogis(smooth_dat$est-2*smooth_dat$se)
smooth_dat$trans_upp<-plogis(smooth_dat$est+2*smooth_dat$se)
smooth_dat$cluster<-clust_cut[smooth_dat$Common.Name]
bottom_plot<-ggplot(data=smooth_dat)+
  geom_line(aes(x=Gear.Temperature...C.,y=trans_est,col=Common.Name),lwd=2)+
  geom_ribbon(aes(x=Gear.Temperature...C.,ymin=trans_low,ymax=trans_upp,fill=Common.Name),alpha=.2)+
  theme_bw()+  theme_bw()+ylab("p(occurence)")+xlab("Bottom temperature")+
  labs(col="Species/group",fill="Species/group")+
  facet_wrap(~cluster,nrow=1)

png("plots/presence_GOA_gam.png",height=10,width=10,res=350,units='in') 
bottom_plot / sst_plot / depth_plot + plot_layout(guides = "collect")
dev.off()
