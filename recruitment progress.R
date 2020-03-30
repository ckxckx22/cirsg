# setup 
#example
setwd("~/Documents/github/UPMC/Plots n Graphs/Recruitment progress/")
source("~/Documents/github/UPMC/startup.R")
library(bsrc);library(ggpubr);library(tidyverse);library(ggplot2);library(lubridate)

md$data[which(md$data=="",arr.ind = T)]<-NA
#plotpath="/Users/mogoverde/Desktop" #Morgan WFH
#plotpath="C:/Users/buerkem/OneDrive - UPMC/Desktop" #Morgan Work
plotpath="~/Documents/github/UPMC/Plots n Graphs/Recruitment progress/" #Kexin

# Ksocial recruitment milestone 

## Graph for Ksocial 
recr_k<-read.csv("Ksocial Recruitment milestones.csv",stringsAsFactors = F) %>% as_tibble() %>% mutate(Date = lubridate::mdy(Date),Month=NULL)
# every pt who did scan 
root="~/Box/skinner/data/" #Kexin
list.files(path=paste0(root, "eprime/shark"))->Eshark
list.files(path=paste0(root, "eprime/clock_reversal"))->Eclock
#Remove the non-ID files (only a problem with clock)
as.numeric(Eclock)[-which(is.na(as.numeric(Eclock)))]->Eclock
as.numeric(Eshark)->Eshark
#ID mapping
idmap<-md$data[c("registration_redcapid","registration_wpicid","registration_soloffid")]
names(idmap)<-c("masterdemoid","wpicid","soloffid")
#Get list of shark OR clock IDs
unique(append(Eshark, Eclock))->Eallid
#Make df for ID matching
data.frame(ID=Eallid, extra=T)->Eallid
#Grab new ids as masterdemoids
bsrc.findid(Eallid,idmap = idmap,id.var = "ID")$masterdemoid->Eallmd
#Check
#Should be =0, all IDs should be in Pall
#Get EXPLORE pts from those in Protect
#Get people who have scanned from Box
list.files(path=paste0(root, "matlab task data/bpd_clock"))->Bclock
list.files(path=paste0(root, "matlab task data/bpd_spott"))->Bspott
list.files(path=paste0(root, "eprime/bpd_trust"))->Btrust
#Anyone who was scanned
unique(append(Bclock, Btrust))->BKtasks
unique(append(BKtasks,Bspott))->Bmriid
as.numeric(Bmriid)->Bmriid
as.numeric(Bspott)->Bspott
#Make df for ID matching (SPOTT seperately)
data.frame(ID=Bmriid, extra=T)->Bmriid
data.frame(ID=Bspott, extra=T)->Bspottid
#ID match
bsrc.findid(Bmriid,idmap = idmap,id.var = "ID")->Bmrimddf
#Bmrimddf[-which(Bmrimddf$ID=="120517"),]
Bmrimddf[which(!Bmrimddf$ifexist),] #Should be no one
bsrc.findid(Bspottid,idmap = idmap,id.var = "ID")->Bspottmddf
Bspottmddf[which(!Bspottmddf$ifexist),] #Should be no one

#Plot prep for KSOCIAL
#Get which participants completed each task (K Only)
list.files(path=paste0(root, "matlab task data/ksoc_clock"))->Kclock
list.files(path=paste0(root, "eprime/ksoc_trust"))->Ktrust
#B/Ksocial pts (gathered from BSOCIAL section)  
as.numeric(BKtasks)->BKtasks
#Make df for ID matching
data.frame(ID=BKtasks, extra=T)->BKtasksid
bsrc.findid(BKtasksid,idmap = idmap,id.var = "ID")->BKmddf
BKmddf[which(!BKmddf$ifexist),] #should be none
BKmddf$masterdemoid->BKid
unique(append(append(Kclock,Ktrust),BKid))->Kmri
bsrc.findid(data.frame(ID=Kmri,stringsAsFactors = F),idmap = idmap,id.var = "ID")$masterdemoid->Kmri


# every pt consented to KS
Kconsented<-subset(md$data,registration_ptcstat___ksocial==1)[c('registration_redcapid',"reg_condate_ksocial")]
any(duplicated(Kconsented$registration_redcapid));any(is.na(Kconsented$registration_redcapid))
Kconsented_mdid<-bsrc.findid(as.data.frame(Kconsented),id.var="registration_redcapid",idmap = idmap)

datebreaks<-seq(min(recr_k$Date),max(recr_k$Date),"2 month")
`actualcounts_K?`<-Kconsented %>% as.tibble() %>%
  drop_na() %>% # removed 440159 because this pt is too new and doesn't have a condate 
  mutate(endpoint=datebreaks[findInterval(as.Date(.$reg_condate_ksocial),datebreaks)+1]) %>% #get end point of date
  arrange(endpoint) %>% 
  group_by(endpoint) %>% 
  mutate(counts=max(row_number())) %>% # get the counts within each time period 
  ungroup() %>%
  select(endpoint,counts) %>% distinct() %>% #remove unecessary cols and keep only enddate and counts within each period
  mutate(count_sum=cumsum(.$counts)) %>%
  mutate(counts=NULL) %>% # remove counts 
  rename(Date=endpoint,Consented=count_sum) # for the convenience of next step: left_join

actualcounts_K<-bsrc.findid(as.data.frame(Kconsented),id.var="registration_redcapid",idmap = idmap) %>% as.tibble() %>%
  transmute(registration_redcapid=registration_redcapid,reg_condate_ksocial=reg_condate_ksocial,did_scan=masterdemoid %in% Kmri) %>% 
  drop_na() %>% # removed 440159 because this pt is too new and doesn't have a condate 
  mutate(endpoint=datebreaks[findInterval(as.Date(.$reg_condate_ksocial),datebreaks)+1]) %>% #get end point of date
  arrange(endpoint) %>% # sort by endpoint
  group_by(endpoint,did_scan) %>% 
  tally() %>% # get the number of pt who did the scan
  group_by(endpoint) %>% 
  mutate(count_all = sum(n)) %>% # get the number of pt who ever consented to K  within each time period
  ungroup() %>%
  filter(did_scan) %>%
  transmute(Date=endpoint,Scanned=cumsum(n),Consented=cumsum(count_all))

plotdata_K<-left_join(tibble(Date=datebreaks),recr_k,by="Date") %>%
  left_join(actualcounts_K,by="Date") %>% # get the numbers
  pivot_longer(-Date,names_to = "Number Type",values_to = "counts",values_drop_na = TRUE) # re arreange the dataframe 


ggplot(plotdata_K, aes(x=Date, y=counts, label=counts, color=`Number Type`)) +
  geom_line() +
  geom_text(data = subset(plotdata_K,`Number Type`=="Consented"), aes(label=counts),vjust=-0.5,hjust=1)+ 
  geom_text(data = subset(plotdata_K,`Number Type`=="Scanned"), aes(label=counts),vjust=1.5,hjust=0)+ 
  geom_point(data = subset(plotdata_K,`Number Type`%in% c("Scanned","Consented")))+
  geom_label(data = subset(plotdata_K,`Number Type`=="Target"), aes(label=counts))+
  scale_color_manual(values=c('red','blue', 'gray')) +
  labs(title = "KSocial Recruitment Progress",
       subtitle = paste0("Data updated on ",Sys.Date()),
       x="Month/Year",y = "Number of Participants")+
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust=0.5),
        axis.text.x = element_text(angle = 45, vjust=.5))+
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month")

ggsave(paste0("Ksocial Recruitment Progress_",Sys.Date(),".png"))  


## Graph for Protect 3 
recr_pt3<-read.csv("~/Box/skinner/administrative/Data meeting/Recruitment milestones for MH085651 - Decision Process of Late-Life Suicide_new.csv",stringsAsFactors = F) %>%
  as_tibble() %>% mutate(pj_date = ymd(pj_date))
datebreaks2<-recr_pt3$pj_date
plotdata_pt3<-md$data %>% as.tibble() %>% 
  mutate_all(~replace(.,.=="",NA)) %>%
  filter(registration_ptcstat___protect3==1 & (is.na(reg_p3catchup)|reg_p3catchup=="00")) %>%
  select(registration_redcapid,reg_condate_protect3) %>% 
  arrange(reg_condate_protect3) %>% 
  mutate(endpoint=datebreaks2[findInterval(as.Date(.$reg_condate_protect3),datebreaks2)+1]) %>% #get end point of date
  count(endpoint) %>% # get the counts within each time period 
  transmute(Date=endpoint,Consented=cumsum(n)) %>%
  right_join(recr_pt3[c("pj_date","Target_total_recruitment")],by=c("Date"="pj_date")) %>%
  rename(Target = Target_total_recruitment) %>%
  pivot_longer(-Date,names_to = "Number Type",values_to = "counts") %>%
  add_row(Date=datebreaks2[1],"Number Type"="Consented",counts=0)
  
ggplot(plotdata_pt3, aes(x=Date, y=counts, label=counts, color=`Number Type`)) +
  geom_line() +
  geom_label(data = subset(plotdata_pt3,`Number Type`=="Target"), aes(label=counts))+
  geom_text(data = subset(plotdata_pt3,`Number Type`=="Consented"), aes(label=counts),vjust=-0.5,hjust=1)+ 
  geom_point(data = subset(plotdata_pt3,`Number Type`=="Consented"))+
  geom_line(data = subset(plotdata_pt3,`Number Type`=="Consented")) +
  scale_color_manual(values=c('red','gray')) +
  labs(title = "Protect3 Recruitment Progress",
       subtitle = paste0("Data updated on ",Sys.Date()),
       caption = "Recruitment goals: 220 subjects. They should complete either the behavioral or scan SPOTT.",
       x="Month/Year",y = "Number of Participants")+
  theme(plot.title = element_text(hjust = 0), 
        plot.subtitle = element_text(hjust=0),
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(angle = 45, vjust=.5))+
  scale_x_date(date_labels = "%b %Y", date_breaks = "4 month")

ggsave(paste0("P3 Recruitment Progress_",Sys.Date(),".png"))  
  

## Graph for Scan
recr_ep<-read.csv("Recruitment_explore2.csv",stringsAsFactors = F) %>% as_tibble() %>% mutate(Date = lubridate::mdy(Date),Month=NULL)
datebreaks3<-recr_ep$Date

bsrc.findid(data.frame(ID=Eclock,stringsAsFactors = F),idmap = idmap,id.var = "ID")$masterdemoid->Eclock

message("no one consented to / did behaviorl so far. Plot a bar graph to indicate the number of Ep2-consented/EP1-consented pts. ") 
# plot data for Scan
plotdata_ep_Scan<-md$data %>% as.tibble() %>% 
  mutate_all(~replace(.,.=="",NA)) %>%
  filter(registration_ptcstat___explore==1) %>%
  select(registration_redcapid,reg_condate_explore) %>% 
  arrange(reg_condate_explore) %>% 
  mutate(endpoint=datebreaks3[findInterval(as.Date(.$reg_condate_explore),datebreaks3)+1]) %>% #get end point of date
  count(endpoint) %>% # get the counts within each time period 
  transmute(Date=endpoint,Consntd_Ep1=cumsum(n)) %>%
  right_join(recr_ep[c("Date","Scan_Target")],by="Date") %>%
  pivot_longer(-Date,names_to = "Number Type",values_to = "counts")
#plot data for behv
plotdata_ep_Behv <- tibble(Date=ymd("2020-03-01"),Consented = 0) %>%
  right_join(recr_ep[c("Date","Behav_Target")],by="Date") %>%
  pivot_longer(-Date,names_to = "Number Type",values_to = "counts")

# plotting 
plotbehv<-ggplot(plotdata_ep_Behv, aes(x=Date, y=counts, label=counts, color=`Number Type`)) +
  geom_line() +
  geom_label(data = subset(plotdata_ep_Behv,`Number Type`=="Behav_Target"), aes(label=counts))+
  geom_text(data = subset(plotdata_ep_Behv,`Number Type`=="Consented"), aes(label=counts),vjust=-0.5,hjust=1)+ 
  geom_point(data = subset(plotdata_ep_Behv,`Number Type`=="Consented"))+
  geom_line(data = subset(plotdata_ep_Behv,`Number Type`=="Consented")) +
  scale_color_manual(values=c('gray','blue')) +
  labs(#title = "Explroe2 Recruitment Progress - Behav (Target = 200)",
    #subtitle = paste0("\nData updated on ",Sys.Date(),"\n"),
    #caption = "Recruitment goals: 220 subjects (All should complete behaviroal tasks and 120 should complete scans (SPOTT).)",
    x="",y = "Number of Participants")+
  theme(plot.title = element_text(hjust = 0), 
        plot.subtitle = element_text(hjust=0),
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_blank()) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "4 month")+
  scale_y_continuous(limits = c(0,205))

plotscan<-ggplot(plotdata_ep_Scan, aes(x=Date, y=counts, label=counts, color=`Number Type`,)) +
  geom_line() +
  geom_label(data = subset(plotdata_ep_Scan,`Number Type`=="Scan_Target"), aes(label=counts))+
  geom_text(data = subset(plotdata_ep_Scan,`Number Type`=="Consntd_Ep1"), aes(label=counts),vjust=-0.5,hjust=1)+ 
  geom_point(data = subset(plotdata_ep_Scan,`Number Type`=="Consntd_Ep1"))+
  geom_line(data = subset(plotdata_ep_Scan,`Number Type`=="Consntd_Ep1")) +
  scale_color_manual(values=c('red','gray')) +
  labs( #title = "Explroe2 Recruitment Progress - Scan (Target = 200)",
       #subtitle = paste0("Data updated on ",Sys.Date()),
       x="Month/Year",y = "Number of Participants")+
  theme(plot.title = element_text(hjust = 0), 
        plot.subtitle = element_text(hjust=0),
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(angle = 45, vjust=.5))+
  scale_x_date(date_labels = "%b %Y", date_breaks = "4 month")+
  scale_y_continuous(limits = c(110,205))

ggarrange(plotbehv,plotscan,
          labels = c("Behav (Target = 200)","Scan (Target = 200)"), 
          font.label = list(size = 10),
          ncol = 1) %>%
  annotate_figure(top = text_grob("Explore2 Recruitment Progress",size=14),
                  bottom = text_grob(paste0("Recruitment goals: 400 subjects. \n\t A group of 200 will complete behaviroal tasks and another group of 200 will complete scans. \n\t There can be no overlap between the two groups.\n",
                                            "\nData updated on ",Sys.Date()),hjust=0,x=0.05,size = 10) )
ggsave(paste0("Explore2 Recruitment Progress_",Sys.Date(),".png"))  

#### Archive: this gives both the numbers of the consented and the scanned #####
md$data %>% as.tibble() %>% 
  mutate_all(~replace(.,.=="",NA)) %>%
  filter(registration_ptcstat___explore==1) %>%
  select(registration_redcapid,reg_condate_explore) %>%
  mutate(did_scan=registration_redcapid %in% Eclock) %>%
  mutate(endpoint=datebreaks3[findInterval(as.Date(.$reg_condate_explore),datebreaks3)+1]) %>% #get end point of date
  arrange(endpoint) %>% # sort by endpoint
  group_by(endpoint,did_scan) %>% 
  tally() %>% # get the number of pt who did the scan
  group_by(endpoint) %>% 
  mutate(count_all = sum(n)) %>% # get the number of pt who ever consented to K  within each time period
  ungroup() %>%
  filter(did_scan) %>%
  transmute(Date=endpoint,Scan_Scanned=cumsum(n),Scan_Consented=cumsum(count_all)) %>%
  right_join(recr_ep,by="Date") %>%
  select(-starts_with("Behav")) %>%
  pivot_longer(-Date,names_to = "Number Type",values_to = "counts",values_drop_na = TRUE)
ggplot(plotdata_ep, aes(x=Date, y=counts, label=counts, color=`Number Type`)) +
  geom_line() +
  geom_text(data = subset(plotdata_ep,`Number Type`=="Scan_Consented"), aes(label=counts),vjust=-0.5,hjust=1)+ 
  geom_text(data = subset(plotdata_ep,`Number Type`=="Scan_Scanned"), aes(label=counts),vjust=1.5,hjust=0)+ 
  geom_point(data = subset(plotdata_ep,`Number Type`%in% c("Scan_Scanned","Scan_Consented")))+
  geom_label(data = subset(plotdata_ep,`Number Type`=="Scan_Target"), aes(label=counts))+
  scale_color_manual(values=c('red','blue', 'gray')) +
  labs(title = "Explore2 Recruitment Progress (Target: Scan = 200)",
       subtitle = paste0("Data updated on ",Sys.Date()),
       x="Month/Year",y = "Number of Participants")+
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust=0.5),
        axis.text.x = element_text(angle = 45, vjust=.5))+
  scale_x_date(date_labels = "%b %Y", date_breaks = "4 month")
ggsave(paste0("Explore2 Recruitment Progress_",Sys.Date(),".png"))  
#### Archive: above gives both the numbers of the consented and the scanned #####
  
