
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

ts_data<-read.csv('data/GOA/Biomass by Stratum GOA.csv')
haul_dat<-read.csv('data/GOA/race_cpue_by_haul.csv',skip=7, fileEncoding="latin1")
haul_dat$avg_wt<-haul_dat$Weight..kg./haul_dat$Number.of.Fish
unique(haul_dat$Common.Name)
ggplot(haul_dat,aes(x=avg_wt))+
  geom_histogram(bins=100)+
  facet_wrap(~Common.Name)+
  theme_bw()

world <- ne_countries(scale = "medium", returnclass = "sf")
lon_1<-min(haul_dat$Ending.Longitude..dd.,na.rm=T)
lon_2<-max(haul_dat$Ending.Longitude..dd.,na.rm=T)*.99
lat_1<-min(haul_dat$Ending.Latitude..dd.,na.rm=T)
lat_2<-max(haul_dat$Ending.Latitude..dd.,na.rm=T)*1.02

INFPC_long<-data.frame(lon=c(-170,-159,-154,-147,-140))
#=================================================================
# make a function that plots for a given species
# spatial maps of all years data
# distribution of recent year + prevalence and biomass estimates
# FILTER THE SALMON, SABLEFISH, POLLOCK, AND OTHER FISH BY SIZE TO REPRESENT FORAGE
#=================================================================
unique_spp<-unique(haul_dat$Common.Name)
unique_spp_ts<-unique(ts_data$Common.Name)
sort(unique(unique_spp,unique_spp_ts))
#==map of all species catches
in_dat<-haul_dat
in_dat<-in_dat[in_dat$Number.CPUE..number.km2.!=0,]

p<-ggplot() + 
  geom_tile(data=in_dat, aes(x = Ending.Longitude..dd.,
                             y = Ending.Latitude..dd.,
                             fill = Common.Name),width=.25,height=.15) +
  facet_wrap(~Common.Name,ncol=4) +
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
png(paste("plots/goa_all_spp.png",sep=''),height=8,width=8,res=400,units='in')
print(p)
dev.off()

#==map of all species catches
in_dat<-filter(haul_dat,avg_wt<0.5)
in_dat<-in_dat[in_dat$Number.CPUE..number.km2.!=0,]

p<-ggplot() + 
  geom_tile(data=in_dat, aes(x = Ending.Longitude..dd.,
                             y = Ending.Latitude..dd.,
                             fill = Common.Name),width=.25,height=.15) +
  facet_wrap(~Common.Name,ncol=4) +
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
png(paste("plots/goa_all_spp_forage.png",sep=''),height=8,width=8,res=400,units='in')
print(p)
dev.off()

#==need centers of gravity of catches and survey
#==make recent map with divisions by INFPC to match the estimated biomass
#==add divisions for maps into INFPC areas
#==do abundance and biomass?
#==include error bars
for(x in 1:length(unique_spp))
{
in_ts<-filter(ts_data,Common.Name==unique_spp[x])%>%
  group_by(Year,Stratum.Min.Depth..m.,Stratum.INPFC.Area)%>%
  mutate(Stratum.INPFC.Area=factor(Stratum.INPFC.Area))%>%
  mutate(Stratum.INPFC.Area=fct_relevel(Stratum.INPFC.Area,c("Shumagin","Chirikof","Kodiak",
                                        'Yakutat',"Southeastern")))%>%
  summarise(sum_bio=sum(Stratum.Biomass..mt.,na.rm=T))

# shumagin, chiriko, kodiak, yakutat, SE

bio_depth<-ggplot(data=in_ts)+
  geom_line(aes(x=Year,y=sum_bio,col=as.factor(Stratum.Min.Depth..m.)))+
  geom_point(aes(x=Year,y=sum_bio,col=as.factor(Stratum.Min.Depth..m.)))+
  theme_bw()+
  facet_grid(~as.factor(Stratum.INPFC.Area))+
  scale_colour_discrete(name="Minimum depth (m)")+
  ylab("Estimated biomass (t)")



in_ts_tot<-filter(ts_data,Common.Name==unique_spp[x])%>%
  group_by(Year)%>%
  summarise(sum_bio=sum(Stratum.Biomass..mt.,na.rm=T))

bio_tot<-ggplot(data=in_ts_tot)+
  geom_line(aes(x=Year,y=sum_bio))+
  geom_point(aes(x=Year,y=sum_bio))+
  theme_bw()+expand_limits(y=0)+
  ylab("Estimated biomass (t)")




in_dat<-filter(haul_dat, Year==2021 & Common.Name==unique_spp[x])
in_dat$Number.CPUE..number.km2.[in_dat$Number.CPUE..number.km2.==0]<-NA

recent_map<-ggplot() + 
  geom_vline(data=INFPC_long,aes(xintercept=lon))+
  geom_tile(data=in_dat, aes(x = Ending.Longitude..dd.,
                             y = Ending.Latitude..dd.,
                             fill = Number.CPUE..number.km2.),width=.3,height=.2) +
  # geom_point(data = gam_dat, 
  #            aes(x = lon, y = lat,fill=log_abund_101), 
  #            shape = 16,size=.45) +
  scale_fill_distiller(palette="Spectral", na.value="grey") +
  #facet_wrap(~year) +
  geom_sf(data=world) +
  coord_sf(xlim = c(lon_1,lon_2), ylim = c(lat_1,lat_2), expand = FALSE) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10))+
  # strip.text.x = element_text(margin= margin(1,0,1,0)),
  # panel.grid.major = element_blank(), 
  # panel.grid.minor = element_blank(),
  # panel.border = element_blank(),
  # strip.background = element_rect(color="white",fill="white"))+
  # theme(legend.position=c(.65,.35),
  #       legend.background = element_rect(fill='transparent',color=NA),
  #       legend.box.background = element_rect(fill='transparent',color=NA))+
  labs(fill="Number/km^2")+
  xlab("Longitude")+
  ylab("Latitude")



in_dat<-filter(haul_dat, Common.Name==unique_spp[x])
in_dat$Number.CPUE..number.km2.[in_dat$Number.CPUE..number.km2.>50000]<-50000
# hist(in_dat$Number.CPUE..number.km2.)
# sort(in_dat$Number.CPUE..number.km2.)
in_dat$Number.CPUE..number.km2.[in_dat$Number.CPUE..number.km2.==0]<-NA
p<-ggplot() + 
  geom_tile(data=in_dat, aes(x = Ending.Longitude..dd.,
                             y = Ending.Latitude..dd.,
                             fill = Number.CPUE..number.km2.),width=.25,height=.15) +
  # geom_point(data = gam_dat, 
  #            aes(x = lon, y = lat,fill=log_abund_101), 
  #            shape = 16,size=.45) +
  scale_fill_distiller(palette="Spectral", na.value="grey") +
  facet_wrap(~Year,ncol=3) +
  geom_sf(data=world) +
  coord_sf(xlim = c(lon_1,lon_2), ylim = c(lat_1,lat_2), expand = FALSE) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10))+
  # strip.text.x = element_text(margin= margin(1,0,1,0)),
  # panel.grid.major = element_blank(), 
  # panel.grid.minor = element_blank(),
  # panel.border = element_blank(),
  # strip.background = element_rect(color="white",fill="white"))+
  theme(legend.position=c(.80,.0),
        legend.background = element_rect(fill='transparent',color=NA),
        legend.box.background = element_rect(fill='transparent',color=NA))+labs(fill="Numbers/km^2")+
  xlab("Longitude")+
  ylab("Latitude")
png(paste("plots/goa_time_map_",unique_spp[x],".png",sep=''),height=10,width=8,res=400,units='in')
print(p)
dev.off()

#==plot prevalence a
in_dat_sm<-in_dat%>%
  group_by(Year)%>%
  summarise(prevalence=sum(!is.na(Number.CPUE..number.km2.)),
            tot_station=sum(!is.na(Ending.Longitude..dd.)))

prevs<-ggplot(in_dat_sm,aes(y=prevalence,x=Year))+geom_line()+theme_bw()+geom_point()+
  expand_limits(y=0)

plot_1<-bio_depth / recent_map  + plot_layout(heights=c(.25,0.5))

png(paste("plots/goa_",unique_spp[x],".png",sep=''),height=7,width=10,res=400,units='in')
outs<-plot_1 + plot_annotation(tag_levels='a')
try(print(outs),silent=TRUE)
dev.off()
}

#==plot prevalence in the bottom trawl survey
#==add biomass weighted average prevalence??
prev_df<-data.frame(Year=NULL,species=NULL,prevalence=NULL)
for(x in 1:length(unique_spp))
{
in_dat<-filter(haul_dat, Common.Name==unique_spp[x])
in_dat_sm<-in_dat%>%
  group_by(Year)%>%
  summarise(prev=sum(Number.CPUE..number.km2.>0),
            tot_station=sum(!is.na(Ending.Longitude..dd.)),
            prevalence=prev/tot_station)
in_dat_sm$species<-unique_spp[x]
 prev_df<-rbind(prev_df,in_dat_sm[,c(1,4,5)])
}
png(paste("plots/goa_prevalence.png",sep=''),height=8,width=8,res=400,units='in')
prevs<-ggplot(prev_df,aes(y=prevalence,x=Year,col=species))+geom_line()+theme_bw()+geom_point()+
  expand_limits(y=0)
print(prevs)
dev.off()

png(paste("plots/goa_prevalence_big2.png",sep=''),height=8,width=8,res=400,units='in')
prevs<-ggplot(filter(prev_df,species%in%c("Pacific capelin","eulachon")),aes(y=prevalence,x=Year,col=species))+geom_line()+theme_bw()+geom_point()+
  expand_limits(y=0)+
  theme(legend.position=c(.8,.2))
print(prevs)
dev.off()

png(paste("plots/goa_prevalence_squid.png",sep=''),height=8,width=8,res=400,units='in')
prevs<-ggplot(filter(prev_df,species%in%c("magistrate armhook squid","squid unid.")),aes(y=prevalence,x=Year,col=species))+geom_line()+theme_bw()+geom_point()+
  expand_limits(y=0)+
  theme(legend.position=c(.8,.2))
print(prevs)
dev.off()

png(paste("plots/goa_prevalence_shrimp.png",sep=''),height=8,width=8,res=400,units='in')
prevs<-ggplot(filter(prev_df,species%in%c("sidestripe shrimp","shrimp unid.")),aes(y=prevalence,x=Year,col=species))+geom_line()+theme_bw()+geom_point()+
  expand_limits(y=0)+
  theme(legend.position=c(.8,.2))
print(prevs)
dev.off()


#==plot total biomass estimates
in_ts_tot<-ts_data%>%
  group_by(Year,Common.Name)%>%
  summarise(sum_bio=sum(Stratum.Biomass..mt.,na.rm=T),
            sum_bio_ci_dn=sum(Min.Stratum.Biomass..lower.95..CL.,na.rm=T),
            sum_bio_ci_up=sum(Max.Stratum.Biomass..upper.95..CL.,na.rm=T),
            sum_n=sum(Stratum.Population..number.,na.rm=T),
            sum_n_ci_dn=sum(Min.Stratum.Population..lower.95..CL.,na.rm=T),
            sum_n_ci_up=sum(Max.Stratum.Population..upper.95..CL.,na.rm=T))

in_ts_tot1<-ts_data%>%
  group_by(Year,Common.Name)%>%
  summarise(sum_bio=sum(Stratum.Biomass..mt.,na.rm=T),
            sum_bio_ci_dn=sum(Min.Stratum.Biomass..lower.95..CL.,na.rm=T),
            sum_bio_ci_up=sum(Max.Stratum.Biomass..upper.95..CL.,na.rm=T))
in_ts_tot1$variable<-"Biomass (t)"

in_ts_tot2<-ts_data%>%
  group_by(Year,Common.Name)%>%
  summarise(sum_bio=sum(Stratum.Population..number.,na.rm=T)/100000,
            sum_bio_ci_dn=sum(Min.Stratum.Population..lower.95..CL.,na.rm=T)/100000,
            sum_bio_ci_up=sum(Max.Stratum.Population..upper.95..CL.,na.rm=T)/100000)
in_ts_tot2$variable<-"Abundance (100000s)"

in_ts<-rbind(in_ts_tot1,in_ts_tot2)
unique(in_ts$Common.Name)
bio_tot<-ggplot(data=filter(in_ts,Common.Name%in%c("eulachon","Pacific capelin")))+
  geom_line(aes(x=Year,y=sum_bio,col=Common.Name))+
  geom_point(aes(x=Year,y=sum_bio,col=Common.Name))+
  geom_ribbon(aes(x=Year,ymin=sum_bio_ci_dn,ymax=sum_bio_ci_up,fill=Common.Name),alpha=.4)+
  theme_bw()+expand_limits(y=0)+
  facet_wrap(Common.Name~variable,scales='free_y')+
  theme(legend.position='none')+
  ylab('')

bio_tot2<-ggplot(data=filter(in_ts,Common.Name%in%c("magistrate armhook squid","squid unid.")))+
  geom_line(aes(x=Year,y=sum_bio,col=Common.Name))+
  geom_point(aes(x=Year,y=sum_bio,col=Common.Name))+
  geom_ribbon(aes(x=Year,ymin=sum_bio_ci_dn,ymax=sum_bio_ci_up,fill=Common.Name),alpha=.4)+
  theme_bw()+expand_limits(y=0)+
  facet_wrap(Common.Name~variable,scales='free_y')+
  theme(legend.position='none')+
  ylab('')

bio_tot3<-ggplot(data=filter(in_ts,Common.Name%in%c("sidestripe shrimp","shrimp unid.")))+
  geom_line(aes(x=Year,y=sum_bio,col=Common.Name))+
  geom_point(aes(x=Year,y=sum_bio,col=Common.Name))+
  geom_ribbon(aes(x=Year,ymin=sum_bio_ci_dn,ymax=sum_bio_ci_up,fill=Common.Name),alpha=.4)+
  theme_bw()+expand_limits(y=0)+
  facet_wrap(Common.Name~variable,scales='free_y')+
  theme(legend.position='none')+
  ylab('')

bio_tot111<-ggplot(data=filter(in_ts,Common.Name%in%c("eulachon","Pacific capelin",
                                                   "sidestripe shrimp","shrimp unid.",
                                                   "magistrate armhook squid","squid unid.")))+
  geom_line(aes(x=Year,y=sum_bio,col=Common.Name))+
  geom_point(aes(x=Year,y=sum_bio,col=Common.Name))+
  geom_ribbon(aes(x=Year,ymin=sum_bio_ci_dn,ymax=sum_bio_ci_up,fill=Common.Name),
              alpha=.2)+
  theme_bw()+expand_limits(y=0)+
  facet_wrap(~variable,scales='free_y')+
  theme(legend.position=c(.12,.8))+
  ylab('')

png(paste("plots/goa_est_biomass_n_big2.png",sep=''),height=8,width=8,res=400,units='in')
print(bio_tot)
dev.off()

png(paste("plots/goa_est_biomass_n_squid.png",sep=''),height=8,width=8,res=400,units='in')
print(bio_tot2)
dev.off()

png(paste("plots/goa_est_biomass_n_shrimp.png",sep=''),height=8,width=8,res=400,units='in')
print(bio_tot3)
dev.off()
