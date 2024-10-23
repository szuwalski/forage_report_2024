others<-read.csv("data/BSAI/Nontarget Species Bycatch.csv")
squids<-read.csv("data/BSAI/Groundfish Total Catch.csv")
herring<-read.csv("data/BSAI/Prohibited Species Catch.csv")
library(ggplot2)
library(dplyr)
library(reshape2)
library(patchwork)

tmp<-others%>%
  group_by(Year,Trip.Target.Fishery,Species.Group.Name)%>%
  summarise(tot_catch=sum(Nontarget.Estimate..mt.,na.rm=T))

ggplot(tmp)+
  geom_line(aes(x=Year,y=tot_catch,col=Trip.Target.Fishery))+
  facet_wrap(~Species.Group.Name,scales='free_y')+
  theme_bw()

tmp<-filter(others,Species.Group.Name!='Squid')%>%
  group_by(Year,Species.Group.Name)%>%
  summarise(tot_catch=sum(Nontarget.Estimate..mt.,na.rm=T))

png(paste("plots/ebs_non_target_catch_ind.png",sep=''),height=8,width=8,res=400,units='in')
ggplot(tmp)+
  geom_line(aes(x=Year,y=tot_catch,col=Species.Group.Name),lwd=1.3)+
  theme_bw()+
  facet_wrap(~Species.Group.Name,scales='free_y')+
  theme(legend.position='none')+
  ylab("Metric tons")
dev.off()

png(paste("plots/ebs_non_target_catch.png",sep=''),height=8,width=8,res=400,units='in')
ggplot(tmp)+
  geom_area(aes(x=Year,y=tot_catch,fill=Species.Group.Name))+
  theme_bw()+
  theme(legend.position=c(.7,.7))+
  ylab("Metric tons")
dev.off()

tmpsq<-filter(others,Species.Group.Name=='Squid')%>%
  group_by(Year)%>%
  summarise(all_catch=sum(Nontarget.Estimate..mt.,na.rm=T))

stmp<-filter(squids,FMP.Area=='BSAI')%>%
  group_by(Year)%>%
  summarise(all_catch=sum(Catch..mt.,na.rm=T))

stmp<-rbind(stmp,tmpsq)

png(paste("plots/ebs_squid_catch.png",sep=''),height=4,width=6,res=400,units='in')

ggplot(stmp)+
  geom_line(aes(x=Year,y=all_catch),lwd=2)+
  theme_bw()+
  ylab("Metric tons")
dev.off()



tmp<-herring%>%
  group_by(Year,Trip.Target.Name)%>%
  summarise(tot_catch=sum(PSCNQ.Estimate....,na.rm=T))
her1<-ggplot(tmp)+
  geom_line(aes(x=Year,y=tot_catch,col=Trip.Target.Name),lwd=1.3)+
  theme_bw()+
  theme(legend.position=c(.4,.7),
        legend.background = element_rect(fill='transparent',color=NA),
        legend.box.background = element_rect(fill='transparent',color=NA))+
  ylab("Metric tons")

tmp<-herring%>%
  group_by(Year)%>%
  summarise(tot_catch=sum(PSCNQ.Estimate....,na.rm=T))

her2<-ggplot(tmp)+
  geom_line(aes(x=Year,y=tot_catch),lwd=2)+
  theme_bw()+
  ylab("Metric tons")

png(paste("plots/ebs_herring_catch.png",sep=''),height=10,width=8,res=400,units='in')
her2/her1 + plot_layout(heights=c(1,1.5))
dev.off()

basis<-read.csv("retimeseriesforcody/2023NForageFishTimeSeries.csv")
basis$upper<-basis$Forage+1.96*basis$Forage_SE
basis$lower<-basis$Forage-1.96*basis$Forage_SE
png(paste("plots/basis.png",sep=''),height=6,width=8,res=400,units='in')

ggplot(basis)+
  geom_line(aes(x=Year,y=Forage,col=Region),lwd=1.3)+
  geom_point(aes(x=Year,y=Forage,col=Region))+
  geom_segment(aes(x=Year,xend=Year,y=lower,yend=upper,col=Region))+
  theme_bw()+
  theme(legend.position=c(.3,.8))+
  ylab("Kilograms")
dev.off()
