
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

 ts_data<-read.csv('data/BSAI/EBS Shelf - Biomass by Stratum Standard.csv')
 haul_dat<-read.csv('data/BSAI/race_cpue_by_haul.csv',skip=7, fileEncoding="latin1")
haul_dat$avg_wt<-haul_dat$Weight..kg./haul_dat$Number.of.Fish

names(haul_dat)
ggplot(haul_dat,aes(x=avg_wt))+
  geom_histogram(bins=100)+
  facet_wrap(~Common.Name)+
  theme_bw()

world <- ne_countries(scale = "medium", returnclass = "sf")
lon_1<- -179
lon_2<- -150
lat_1<-  50
lat_2<-  65
#=================================================================
# make a function that plots for a given species
# spatial maps of all years data
# distribution of recent year + prevalence and biomass estimates
# FILTER THE SALMON, SABLEFISH, POLLOCK, AND OTHER FISH BY SIZE TO REPRESENT FORAGE
#=================================================================
unique_spp<-unique(haul_dat$Common.Name)
unique_spp_ts<-unique(ts_data$Common.Name)
sort(unique(unique_spp,unique_spp_ts))
haul_dat$Common.Name2<-haul_dat$Common.Name
haul_dat$Common.Name2[grep('shrimp',haul_dat$Common.Name2)]<-'shrimp'
haul_dat$Common.Name2[grep('prickleback',haul_dat$Common.Name2)]<-'prickleback'
haul_dat$Common.Name2[grep('lanternfish',haul_dat$Common.Name2)]<-'myctophid'
haul_dat$Common.Name2[grep('eelblenny',haul_dat$Common.Name2)]<-'eelblenny'

#==map of all species catches
in_dat<-haul_dat[haul_dat$Number.CPUE..number.km2.!=0,]

in_dat<-in_dat %>% 
  group_by(Common.Name2) %>% 
  filter(n() >= 50)%>%
  ungroup()

p<-ggplot() + 
  geom_tile(data=in_dat, aes(x = Ending.Longitude..dd.,
                             y = Ending.Latitude..dd.,
                             fill = Common.Name2),width=.25,height=.15) +
  facet_wrap(~Common.Name2,ncol=5) +
  geom_sf(data=world) +
  coord_sf(xlim = c(lon_1,lon_2), ylim = c(lat_1,lat_2), expand = FALSE) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10))+
  # strip.text.x = element_text(margin= margin(1,0,1,0)),
  # panel.grid.major = element_blank(), 
  # panel.grid.minor = element_blank(),
  # panel.border = element_blank(),
  # strip.background = element_rect(color="white",fill="white"))+
  theme(legend.position='none',
        legend.background = element_rect(fill='transparent',color=NA),
        legend.box.background = element_rect(fill='transparent',color=NA),
        legend.direction='horizontal')+labs(fill="Numbers/km^2")+
  xlab("Longitude")+
  ylab("Latitude")
png(paste("plots/ebs_all_spp4.png",sep=''),height=8,width=8,res=400,units='in')
print(p)
dev.off()

in_dat3<-filter(haul_dat,Common.Name2%in% unique(in_dat$Common.Name2))%>%
  group_by(Common.Name2,Ending.Longitude..dd.,Ending.Latitude..dd.)%>%
  summarize(dens=mean(Number.CPUE..number.km2.))
in_dat3<-in_dat3[in_dat3$dens!=0,]
in_dat3$dens[in_dat3$dens>500]<-500

in_dat3$scale_dens<-NA
tkm<-unique(in_dat3$Common.Name2)
for(z in 1:length(tkm))
{
  born<-scale((in_dat3$dens[which(!is.na(match(in_dat3$Common.Name2,tkm[z])))]))
  in_dat3$scale_dens[which(!is.na(match(in_dat3$Common.Name2,tkm[z])))]<-born
}
in_dat3$scale_dens[in_dat3$scale_dens>2]<-2

p<-ggplot() + 
  geom_tile(data=in_dat3, aes(x = Ending.Longitude..dd.,
                             y = Ending.Latitude..dd.,
                             fill = (scale_dens)),width=.5,height=.35) +
  facet_wrap(~Common.Name2,ncol=5) +
  geom_sf(data=world) +
  coord_sf(xlim = c(lon_1,lon_2), ylim = c(lat_1,lat_2), expand = FALSE) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10))+
  # strip.text.x = element_text(margin= margin(1,0,1,0)),
  # panel.grid.major = element_blank(), 
  # panel.grid.minor = element_blank(),
  # panel.border = element_blank(),
  # strip.background = element_rect(color="white",fill="white"))+
  theme(legend.position='bottom',
        legend.background = element_rect(fill='transparent',color=NA),
        legend.box.background = element_rect(fill='transparent',color=NA),
        legend.direction='horizontal')+labs(fill="scaled(Numbers/km^2)")+
  xlab("Longitude")+
  ylab("Latitude")+  scale_fill_distiller(palette="RdYlBu") 
png(paste("plots/ebs_all_spp5.png",sep=''),height=8,width=8,res=400,units='in')
print(p)
dev.off()


#==need centers of gravity of catches and survey
#==make recent map with divisions by INFPC to match the estimated biomass
#==add divisions for maps into INFPC areas
#==do abundance and biomass?
#==include error bars
use_spp<-unique(in_dat$Common.Name2)
herring_svgs <- st_read("C:/Users/cody.szuwalski/Work/forage_2023/data/AK_HerringSavingsAreas/AK_HerringSavingsAreas.shp")

for(x in 1:length(use_spp))
{

# in_ts_tot<-filter(ts_data,Species.Common.Name==use_spp[x])%>%
#   group_by(Year)%>%
#   summarise(sum_bio=sum(Stratum.Biomass,na.rm=T),
#             low_bio=sum(Min.Stratum.Biomass,na.rm=T),
#             max_bio=sum(Max.Stratum.Biomass,na.rm=T))
  # bio_tot<-ggplot(data=in_ts_tot)+
  #   geom_line(aes(x=Year,y=sum_bio))+
  #   geom_point(aes(x=Year,y=sum_bio))+
  #   geom_ribbon(aes(x=Year,ymin=low_bio,ymax=max_bio),col='grey',alpha=.3)+
  #   theme_bw()+expand_limits(y=0)+
  #   ylab("Estimated biomass (t)")
  # 
tmp<-filter(haul_dat,Common.Name2==use_spp[x])
tmp2<-tmp%>%
  group_by(Survey,Year)%>%
  summarise(avg_dens=mean(Number.CPUE..number.km2.),
            sd_dens=sd(Number.CPUE..number.km2.))

dens_tot<-ggplot(data=tmp2)+
  geom_line(aes(x=Year,y=avg_dens,col=Survey))+
  geom_point(aes(x=Year,y=avg_dens,col=Survey))+
  #geom_ribbon(aes(x=Year,ymin=low_bio,ymax=max_bio),col='grey',alpha=.3)+
  theme_bw()+expand_limits(y=0)+
  ylab("Average density (n/km^2)")+
  theme(legend.position='none',
        legend.background = element_rect(fill='transparent',color=NA),
        legend.box.background = element_rect(fill='transparent',color=NA))

tmp_dat<-filter(haul_dat, Year==max(haul_dat$Year) & Common.Name2==use_spp[x])
tmp_dat$Number.CPUE..number.km2.[tmp_dat$Number.CPUE..number.km2.==0]<-NA
tmp_dat<-tmp_dat[!is.na(tmp_dat$Number.CPUE..number.km2.),]



recent_map<-ggplot() + 
  geom_tile(data=tmp_dat, aes(x = Ending.Longitude..dd.,
                             y = Ending.Latitude..dd.,
                             fill = log(Number.CPUE..number.km2.)),width=.45,height=.3) +
  # geom_point(data = gam_dat, 
  #            aes(x = lon, y = lat,fill=log_abund_101), 
  #            shape = 16,size=.45) +
  scale_fill_distiller(palette="Spectral") +
  #facet_wrap(~year) +
  geom_sf(data=world) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10))+
  # strip.text.x = element_text(margin= margin(1,0,1,0)),
  # panel.grid.major = element_blank(), 
  # panel.grid.minor = element_blank(),
  # panel.border = element_blank(),
  # strip.background = element_rect(color="white",fill="white"))+
   theme(legend.position=c(.5,.1),
         legend.background = element_rect(fill='transparent',color=NA),
         legend.box.background = element_rect(fill='transparent',color=NA),
         legend.direction='horizontal')+
  labs(fill="log(Number)/km^2")+
  xlab("Longitude")+
  ylab("Latitude")+
  geom_sf(data=herring_svgs,fill='NA',col='black') +
  xlim(lon_1,lon_2)+
  ylim(lat_1,lat_2)


tmp_dat<-filter(haul_dat, Year%in%seq(max(haul_dat$Year)-5,max(haul_dat$Year)-1) & Common.Name2==use_spp[x])
if(use_spp[x]=="myctophid" | use_spp[x]=="squid unid.")
{
 use_yrs<- sort(unique(filter(haul_dat,Common.Name2==use_spp[x])$Year), decreasing = TRUE)
 in_yrs<-use_yrs[1:4]   
 tmp_dat<-filter(haul_dat, Year%in%in_yrs & Common.Name2==use_spp[x])
}
tmp_dat$Number.CPUE..number.km2.[tmp_dat$Number.CPUE..number.km2.==0]<-NA
tmp_dat<-tmp_dat[!is.na(tmp_dat$Number.CPUE..number.km2.),]

recent_past_map<-ggplot() + 
  geom_tile(data=tmp_dat, aes(x = Ending.Longitude..dd.,
                             y = Ending.Latitude..dd.,
                             fill = log(Number.CPUE..number.km2.)),width=.45,height=.3) +
  # geom_point(data = gam_dat, 
  #            aes(x = lon, y = lat,fill=log_abund_101), 
  #            shape = 16,size=.45) +
  scale_fill_distiller(palette="Spectral") +
  facet_wrap(~Year,ncol=2) +
  geom_sf(data=world) +
  coord_sf(xlim = c(lon_1,lon_2), ylim = c(lat_1,lat_2), expand = FALSE) +
  theme_bw()+
  theme(legend.position='none',
        axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10),
        axis.text.y=element_blank())+
  ylab("")+xlab("") + geom_sf(data=herring_svgs,fill='NA',col='black') +
  xlim(lon_1,lon_2)+
  ylim(lat_1,lat_2)
  # strip.text.x = element_text(margin= margin(1,0,1,0)),
  # panel.grid.major = element_blank(), 
  # panel.grid.minor = element_blank(),
  # panel.border = element_blank(),
  # strip.background = element_rect(color="white",fill="white"))+
  # theme(legend.position=c(.65,.35),
  #       legend.background = element_rect(fill='transparent',color=NA),
  #       legend.box.background = element_rect(fill='transparent',color=NA))+
  #labs(fill="Number/km^2")


tmp_dat<-filter(haul_dat, Common.Name2==use_spp[x])
tmp_dat$Number.CPUE..number.km2.[tmp_dat$Number.CPUE..number.km2.>5000]<-5000
tmp_dat$Number.CPUE..number.km2.[tmp_dat$Number.CPUE..number.km2.==0]<-NA
tmp_dat<-tmp_dat[!is.na(tmp_dat$Number.CPUE..number.km2.),]


p<-ggplot() + 
  geom_tile(data=tmp_dat, aes(x = Ending.Longitude..dd.,
                             y = Ending.Latitude..dd.,
                             fill = Number.CPUE..number.km2.),width=.5,height=.5) +
  # geom_point(data = gam_dat, 
  #            aes(x = lon, y = lat,fill=log_abund_101), 
  #            shape = 16,size=.45) +
  scale_fill_distiller(palette="Spectral", na.value="grey") +
  facet_wrap(~Year,ncol=7) +
  geom_sf(data=world) +
  coord_sf(xlim = c(lon_1,lon_2), ylim = c(lat_1,lat_2), expand = FALSE) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10))+
  # strip.text.x = element_text(margin= margin(1,0,1,0)),
  # panel.grid.major = element_blank(), 
  # panel.grid.minor = element_blank(),
  # panel.border = element_blank(),
  # strip.background = element_rect(color="white",fill="white"))+
  theme(legend.position='bottom',
        legend.background = element_rect(fill='transparent',color=NA),
        legend.box.background = element_rect(fill='transparent',color=NA),
        legend.key.width=unit(2,"cm"))+labs(fill="Numbers/km^2")+
  xlab("Longitude")+
  ylab("Latitude")
png(paste("plots/ebs_time_map_",use_spp[x],".png",sep=''),height=10,width=8.5,res=400,units='in')
print(p)
dev.off()

#==plot prevalence a
in_dat_sm<-tmp_dat%>%
  group_by(Year,Survey)%>%
  summarise(prevalence=sum(!is.na(Number.CPUE..number.km2.)),
            tot_station=sum(!is.na(Ending.Longitude..dd.)))

prevs<-ggplot(in_dat_sm,aes(y=prevalence,x=Year,col=Survey))+geom_line()+theme_bw()+geom_point()+
  expand_limits(y=0)

plot_1<-( recent_map |recent_past_map) / (prevs|dens_tot) + plot_layout(heights=c(1,1))

png(paste("plots/ebs_",use_spp[x],".png",sep=''),height=10,width=8,res=400,units='in')
outs<-plot_1 + plot_annotation(tag_levels='a')
try(print(outs),silent=TRUE)
dev.off()
}

#==plot prevalence in the bottom trawl survey
#==add biomass weighted average prevalence??
prev_df<-data.frame(Year=NULL,species=NULL,prevalence=NULL)
for(x in 1:length(use_spp))
{
in_dat<-filter(haul_dat, Common.Name==use_spp[x])
in_dat_sm<-in_dat%>%
  group_by(Year,Survey)%>%
  summarise(prev=sum(Number.CPUE..number.km2.>0),
            tot_station=sum(!is.na(Ending.Longitude..dd.)),
            prevalence=prev/tot_station)
in_dat_sm$species<-use_spp[x]
 prev_df<-rbind(prev_df,in_dat_sm[,c(1,2,5,6)])
}
png(paste("plots/ebs_prevalence.png",sep=''),height=8,width=8,res=400,units='in')
prevs<-ggplot(prev_df,aes(y=prevalence,x=Year,col=species))+geom_line()+theme_bw()+geom_point()+
  expand_limits(y=0)+facet_wrap(~Survey)
print(prevs)
dev.off()

png(paste("plots/ebs_prevalence_big2.png",sep=''),height=8,width=8,res=400,units='in')
prevs<-ggplot(filter(prev_df,species%in%c("Pacific capelin","eulachon")),aes(y=prevalence,x=Year,col=species))+geom_line()+theme_bw()+geom_point()+
  expand_limits(y=0)+
  theme(legend.position=c(.5,.1))
print(prevs)
dev.off()



#==plot total biomass estimates
in_ts_tot<-ts_data%>%
  group_by(Year,Species.Common.Name)%>%
  summarise(sum_bio=sum(Stratum.Biomass,na.rm=T),
            sum_bio_ci_dn=sum(Min.Stratum.Biomass,na.rm=T),
            sum_bio_ci_up=sum(Max.Stratum.Biomass,na.rm=T),
            sum_n=sum(Stratum.Population,na.rm=T),
            sum_n_ci_dn=sum(Min.Stratum.Population,na.rm=T),
            sum_n_ci_up=sum(Max.Stratum.Population,na.rm=T))

in_ts_tot1<-ts_data%>%
  group_by(Year,Species.Common.Name)%>%
  summarise(sum_bio=sum(Stratum.Biomass,na.rm=T),
            sum_bio_ci_dn=sum(Min.Stratum.Biomass,na.rm=T),
            sum_bio_ci_up=sum(Max.Stratum.Biomass,na.rm=T))
in_ts_tot1$variable<-"Biomass (t)"

in_ts_tot2<-ts_data%>%
  group_by(Year,Species.Common.Name)%>%
  summarise(sum_bio=sum(Stratum.Population,na.rm=T)/100000,
            sum_bio_ci_dn=sum(Min.Stratum.Population,na.rm=T)/100000,
            sum_bio_ci_up=sum(Max.Stratum.Population,na.rm=T)/100000)
in_ts_tot2$variable<-"Abundance (100000s)"

in_ts<-rbind(in_ts_tot1,in_ts_tot2)
unique(in_ts$Species.Common.Name)
bio_tot<-ggplot(data=filter(in_ts,Species.Common.Name%in%c("eulachon","Pacific capelin")))+
  geom_line(aes(x=Year,y=sum_bio,col=Species.Common.Name))+
  geom_point(aes(x=Year,y=sum_bio,col=Species.Common.Name))+
  geom_ribbon(aes(x=Year,ymin=sum_bio_ci_dn,ymax=sum_bio_ci_up,fill=Species.Common.Name),alpha=.4)+
  theme_bw()+expand_limits(y=0)+
  facet_wrap(Species.Common.Name~variable,scales='free_y')+
  theme(legend.position='none')+
  ylab('')

bio_tot2<-ggplot(data=filter(in_ts,Species.Common.Name%in%c("magistrate armhook squid","squid unid.")))+
  geom_line(aes(x=Year,y=sum_bio,col=Species.Common.Name))+
  geom_point(aes(x=Year,y=sum_bio,col=Species.Common.Name))+
  geom_ribbon(aes(x=Year,ymin=sum_bio_ci_dn,ymax=sum_bio_ci_up,fill=Species.Common.Name),alpha=.4)+
  theme_bw()+expand_limits(y=0)+
  facet_wrap(Species.Common.Name~variable,scales='free_y')+
  theme(legend.position='none')+
  ylab('')

bio_tot3<-ggplot(data=filter(in_ts,Species.Common.Name%in%c("sidestripe shrimp","shrimp unid.")))+
  geom_line(aes(x=Year,y=sum_bio,col=Species.Common.Name))+
  geom_point(aes(x=Year,y=sum_bio,col=Species.Common.Name))+
  geom_ribbon(aes(x=Year,ymin=sum_bio_ci_dn,ymax=sum_bio_ci_up,fill=Species.Common.Name),alpha=.4)+
  theme_bw()+expand_limits(y=0)+
  facet_wrap(Species.Common.Name~variable,scales='free_y')+
  theme(legend.position='none')+
  ylab('')

bio_tot111<-ggplot(data=filter(in_ts,Species.Common.Name%in%c("eulachon","Pacific capelin",
                                                   "sidestripe shrimp","shrimp unid.",
                                                   "magistrate armhook squid","squid unid.")))+
  geom_line(aes(x=Year,y=sum_bio,col=Species.Common.Name))+
  geom_point(aes(x=Year,y=sum_bio,col=Species.Common.Name))+
  geom_ribbon(aes(x=Year,ymin=sum_bio_ci_dn,ymax=sum_bio_ci_up,fill=Species.Common.Name),
              alpha=.2)+
  theme_bw()+expand_limits(y=0)+
  facet_wrap(~variable,scales='free_y')+
  theme(legend.position=c(.12,.8))+
  ylab('')

png(paste("plots/ebs_est_biomass_n_big2.png",sep=''),height=8,width=8,res=400,units='in')
print(bio_tot)
dev.off()

png(paste("plots/ebs_est_biomass_n_squid.png",sep=''),height=8,width=8,res=400,units='in')
print(bio_tot2)
dev.off()

png(paste("plots/ebs_est_biomass_n_shrimp.png",sep=''),height=8,width=8,res=400,units='in')
print(bio_tot3)
dev.off()
