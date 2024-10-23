incid_cat<-read.csv("data/GOA/Nontarget Species Bycatch.csv")
colnames(incid_cat)
library(dplyr)
library(reshape2)
library(ggplot2)
library(patchwork)

#write.csv(filter(incid_cat,metric=="Nontarget Estimate (mt)"),"for_bill.csv")

png(paste("plots/goa_fmp_incidental_catch_stack.png",sep=''),height=8,width=10,res=400,units='in')

ggplot(incid_cat)+
  geom_bar(aes(x=Year,y=Nontarget.Estimate..mt.,fill=Species.Group.Name),position='stack',stat='identity')+
  theme_bw()+
  theme(legend.position=c(.5,.7))+
  labs(fill="Species/group")+
  ylab("Nontarget catch estimate (mt)")
dev.off()


in_dat<-filter(incid_cat,Retained.or.Discarded=="Discarded")%>%
  group_by(Year,Species.Group.Name)%>%
  summarize(discards=sum(Nontarget.Estimate..mt.))

png(paste("plots/goa_fmp_incidental_catch_discard.png",sep=''),height=8,width=10,res=400,units='in')
ggplot(in_dat)+
  geom_line(aes(x=Year,y=discards,col=Species.Group.Name))+
  theme_bw()+
  labs(fill="Species/group")+
  ylab("Nontarget discard catch estimate (mt)")+
  facet_wrap(~Species.Group.Name,scales='free_y')
dev.off()

in_dat<-filter(incid_cat,Retained.or.Discarded=="Retained")%>%
  group_by(Year,Species.Group.Name)%>%
  summarize(discards=sum(Nontarget.Estimate..mt.))

png(paste("plots/goa_fmp_incidental_catch_retain.png",sep=''),height=8,width=10,res=400,units='in')
ggplot(in_dat)+
  geom_line(aes(x=Year,y=discards,col=Species.Group.Name))+
  theme_bw()+
  labs(fill="Species/group")+
  ylab("Nontarget retained catch estimate (mt)")+
  facet_wrap(~Species.Group.Name,scales='free_y')
dev.off()

#===SQUID GAMES
squid<-read.csv("data/GOA/FMP Other Species Bycatch CAS2.csv")
squid<-filter(squid,FMP.Area=='GOA')
squid$FMP.Subarea[squid$FMP.Subarea=="CG"]<-"Central GOA"
squid$FMP.Subarea[squid$FMP.Subarea=="SE"]<-"Southeast GOA"
squid$FMP.Subarea[squid$FMP.Subarea=="WG"]<-"Western GOA"
squid$FMP.Subarea[squid$FMP.Subarea=="WY"]<-"Central GOA"
ret_dis_squid<-squid%>%
  group_by(Year,Retained.Discarded)%>%
  summarize(biomass=sum(Weight.Posted..mt.,na.rm=T))

sq_1<-ggplot(ret_dis_squid,aes(x=Year,y=biomass,col=Retained.Discarded))+
  geom_line(lwd=2)+theme_bw()+theme(legend.position=c(.2,.7))+
  labs(col='')+ylab("Biomass (t)")

#==by target species
target_squid<-squid%>%
  group_by(Year,Trip.Target.Name)%>%
  summarize(biomass=sum(Weight.Posted..mt.,na.rm=T))

target_squid1<-squid%>%
  group_by(Trip.Target.Name)%>%
  summarize(biomass=sum(Weight.Posted..mt.,na.rm=T))%>%
  arrange(desc(biomass))

target_squid1$perc<-target_squid1$biomass/sum(target_squid1$biomass)

sq_2<-ggplot(filter(target_squid,Trip.Target.Name%in%c("Pollock - midwater",
                                                 "Pollock - bottom",
                                                 "Arrowtooth Flounder",
                                                 "Rockfish")),aes(x=Year,y=biomass,col=Trip.Target.Name))+
  geom_line(lwd=2)+theme_bw()+
  labs(col='')+ylab("Biomass (t)")+
  theme(legend.position=c(.2,.6))

##==FMP area
fmp_squid<-squid%>%
  group_by(Year,FMP.Subarea)%>%
  summarize(biomass=sum(Weight.Posted..mt.,na.rm=T))

sq_3<-ggplot(fmp_squid,aes(x=Year,y=biomass,col=FMP.Subarea))+
  geom_line(lwd=2)+theme_bw()+theme(legend.position=c(.2,.6))+
  labs(col='')+ylab("Biomass (t)")

png(paste("plots/goa_squid_catch.png",sep=''),height=8,width=6,res=400,units='in')

sq_1/sq_2/sq_3

dev.off()

#=======================================
# shrimp

squid<-read.csv("data/Nontarget Species Bycatch_shrimp.csv")
names(squid)
squid<-filter(squid,FMP.Area=='GOA')
squid$FMP.Subarea[squid$FMP.Subarea=="CG"]<-"Central GOA"
squid$FMP.Subarea[squid$FMP.Subarea=="SE"]<-"Southeast GOA"
squid$FMP.Subarea[squid$FMP.Subarea=="WG"]<-"Western GOA"
squid$FMP.Subarea[squid$FMP.Subarea=="WY"]<-"Central GOA"
ret_dis_squid<-squid%>%
  group_by(Year,Retained.or.Discarded)%>%
  summarize(biomass=sum(Nontarget.Estimate..mt.,na.rm=T))

sq_1<-ggplot(ret_dis_squid,aes(x=Year,y=biomass,col=Retained.or.Discarded))+
  geom_line(lwd=2)+theme_bw()+theme(legend.position=c(.4,.7))+
  labs(col='')+ylab("Biomass (t)")

#==by target species
target_squid<-squid%>%
  group_by(Year,Trip.Target.Fishery)%>%
  summarize(biomass=sum(Nontarget.Estimate..mt.,na.rm=T))

target_squid1<-squid%>%
  group_by(Trip.Target.Fishery)%>%
  summarize(biomass=sum(Nontarget.Estimate..mt.,na.rm=T))%>%
  arrange(desc(biomass))

target_squid1$perc<-target_squid1$biomass/sum(target_squid1$biomass)

sq_2<-ggplot(filter(target_squid,Trip.Target.Fishery%in%c("Pollock - midwater",
                                                       "Pollock - bottom",
                                                       "Arrowtooth Flounder",
                                                       "Rockfish",
                                                       "Flatehead Sole")),
             aes(x=Year,y=biomass,col=Trip.Target.Fishery))+
  geom_line(lwd=2)+theme_bw()+
  labs(col='')+ylab("Biomass (t)")+
  theme(legend.position=c(.4,.68))

##==FMP area
fmp_squid<-squid%>%
  group_by(Year,FMP.Subarea)%>%
  summarize(biomass=sum(Nontarget.Estimate..mt.,na.rm=T))

sq_3<-ggplot(fmp_squid,aes(x=Year,y=biomass,col=FMP.Subarea))+
  geom_line(lwd=2)+theme_bw()+theme(legend.position=c(.4,.7))+
  labs(col='')+ylab("Biomass (t)")

png(paste("plots/goa_shrimp_catch.png",sep=''),height=8,width=6,res=400,units='in')

sq_1/sq_2/sq_3

dev.off()

#========================================
# herring PSC
#========================================
herring<-read.csv("data/GOA/Prohibited Species Catch.csv")
names(herring)

herring<-filter(herring,FMP.Area=='GOA')
herring$FMP.Subarea[herring$FMP.Subarea=="CG"]<-"Central GOA"
herring$FMP.Subarea[herring$FMP.Subarea=="SE"]<-"Southeast GOA"
herring$FMP.Subarea[herring$FMP.Subarea=="WG"]<-"Western GOA"
herring$FMP.Subarea[herring$FMP.Subarea=="WY"]<-"Central GOA"
tot_herr<-herring%>%
  group_by(Year)%>%
  summarize(PSC=sum(PSCNQ.Estimate....,na.rm=T))

sq_1<-ggplot(tot_herr,aes(x=Year,y=PSC))+
  geom_line(lwd=2)+theme_bw()+theme(legend.position=c(.2,.7))+
  labs(col='')+ylab("Biomass (t)")

#==by target species
target_herring<-herring%>%
  group_by(Year,Trip.Target.Name)%>%
  summarize(PSC=sum(PSCNQ.Estimate....,na.rm=T))

target_herring1<-herring%>%
  group_by(Trip.Target.Name)%>%
  summarize(PSC=sum(PSCNQ.Estimate....,na.rm=T))%>%
  arrange(desc(PSC))

target_herring1$perc<-target_herring1$PSC/sum(target_herring1$PSC)

sq_2<-ggplot(filter(target_herring,Trip.Target.Name%in%c("Pollock - midwater",
                                                       "Pollock - bottom",
                                                       "Arrowtooth Flounder",
                                                       "Shallow Water Flatfish - GOA")),
             aes(x=Year,y=PSC,col=Trip.Target.Name))+
  geom_line(lwd=2)+theme_bw()+
  labs(col='')+ylab("Biomass (t)")+
  theme(legend.position=c(.2,.67))

##==FMP area
fmp_herr<-herring%>%
  group_by(Year,FMP.Subarea)%>%
  summarize(PSC=sum(PSCNQ.Estimate....,na.rm=T))

sq_3<-ggplot(fmp_herr,aes(x=Year,y=PSC,col=FMP.Subarea))+
  geom_line(lwd=2)+theme_bw()+theme(legend.position=c(.2,.6))+
  labs(col='')+ylab("Biomass (t)")

png(paste("plots/goa_herring_catch.png",sep=''),height=8,width=6,res=400,units='in')

sq_1/sq_2/sq_3

dev.off()
