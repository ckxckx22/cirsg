#Cirs pull
#startup
source('/Users/mogoverde/WFH/startup.R')
startup()
#setwd
setwd("/Users/mogoverde/WFH")
#Libraries
library(lubridate)
library(dplyr)
#Redcap setup
    md<-bsrc.checkdatabase2(ptcs$masterdemo, batch_size=500L, online=T)
      idmap<-md$data[c("registration_redcapid","registration_wpicid","registration_soloffid")]
      names(idmap)<-c("masterdemoid","wpicid","soloffid")
    pt<-bsrc.checkdatabase2(ptcs$protect, batch_size=500L, online=T)
#Get CIRS data (longitudinal, includes ssi and ham)
    cirs<-data.frame(ID=pt$data$registration_redcapid, bldate=pt$data$bq_date,
              fudate=pt$data$fug_date,event=pt$data$redcap_event_name,
               pt$data[grepl("cirs",names(pt$data))], pt$data[grepl("ham",names(pt$data))],
               pt$data[grepl("ssi_",names(pt$data))])
    cirs[!grepl("miss|complete|___|anchor|total", names(cirs))]->cirs
    #Remove all rows that there is NO cirs data
    cirs[-which(rowSums(is.na(cirs[paste("cirsg_",c(1:13),"_s",sep="")]))==13),]->cirs
    #Get date info for time
    as.character(cirs$bldate)->cirs$bldate
    as.character(cirs$fudate)->cirs$fudate
    cirs$cirs_date=ifelse(cirs$bldate=="", cirs$fudate, cirs$bldate)
    ymd(cirs$cirs_date)->cirs$cirs_date
    #ID map
    as.character(cirs$ID)->cirs$ID
    bsrc.findid(cirs, idmap=idmap, "ID")->cirs
    if(any(!cirs$ifexist)){message("STOP, someone in CIRS df doesn't have ID:",
      print(cirs[which(!cirs$ifexist),"ID"]))}
    #CIRS scoring (must be numeric 1st)
    lapply(cirs[grepl("cirs",names(cirs)) & !grepl("_n$|date",names(cirs))],as.numeric)->
      cirs[grepl("cirs",names(cirs)) & !grepl("_n$|date",names(cirs))]
    rowSums(cirs[grepl("cirs",names(cirs)) & !grepl("_n$|date",names(cirs))])->cirs$cirs_sum
    rowSums(cirs[grepl("cirs",names(cirs)) & !grepl("_n$|date|sum",names(cirs))]>=3,na.rm=T)->cirs$cirs_morethan3
    #Change event to character
    as.character(cirs$event)->cirs$event
#Get eligibility
    data.frame(ID=md$data$registration_redcapid, md$data[grepl("suicid",names(md$data))],
               md$data[grepl("protect",names(md$data))])->eligi
    #Consented for S1-P3
    eligi[which(rowSums(eligi[grepl("registration_ptcstat",names(eligi))],na.rm=T)>0),]->eligi
    #Not terminated from any protocol d/t ineligible (reason 3)
    eligi[which(rowSums(eligi[grepl("term_reason_pro|term_reason_sui",names(eligi))]==3,na.rm=T)==0),]->eligi
    #Not marked as unusable from any protocol
    eligi[which(rowSums(eligi[grepl("excl_",names(eligi))],na.rm = T)==0),]->eligi
#Only take CIRS from eligible people
    cirs[which(cirs$masterdemoid %in% eligi$ID),]->cirs2
    #remove unwanted vars
    cirs2[grepl("ID|wpicid|soloff|ogid|ifexit",names(cirs2))]<-NULL
#Get demographic characteristics
    demo<-data.frame(masterdemoid=md$data$registration_redcapid,group=md$data$registration_group,
               dob=md$data$registration_dob, md$data[grepl("registration_race",names(md$data))],
               ethnicity=md$data$registration_hispanic,gender=md$data$registration_gender, 
               education=md$data$registration_edu, marital_status=md$data$registration_marrs,
               md$data[grepl("condate_pro|condate_sui",names(md$data))])
    demo[grepl("race",names(demo))]==1
    #Only people in CIRS sample
    demo[which(demo$masterdemoid %in% cirs2$masterdemoid),]->demo
    #Figure out race
    for(i in 1:nrow(demo)){
      if(demo[i,"registration_race___999"]==1){demo[i,"race"]<-NA
      }else if(sum(demo[i,grepl("race",names(demo))]==1,na.rm=T)>1){demo[i,"race"]<-6
      }else{
        for(j in c(4:9)){
          if(demo[i,j]==1){demo[i,"race"]<-(j-3)}}}}
    demo[grepl("___",names(demo))]<-NULL
    #Age= age at baseline (since CIRS is given many times)
      #Baseline (1st)
      ymd(demo$reg_condate_suicide)->demo$reg_condate_suicide
      ymd(demo$reg_condate_suicid2)->demo$reg_condate_suicid2
      ymd(demo$reg_condate_protect)->demo$reg_condate_protect
      ymd(demo$reg_condate_protect2)->demo$reg_condate_protect2
      ymd(demo$reg_condate_protect3)->demo$reg_condate_protect3
      transform(demo, mincon=pmin(reg_condate_suicide, reg_condate_suicid2, reg_condate_protect,
                            reg_condate_protect2, reg_condate_protect3, na.rm=T))->demo
      for(i in 1:nrow(demo)){
        if(demo[i,"mincon"] %in% c(demo[i,"reg_condate_suicide"], 
                                   demo[i,"reg_condate_suicid2"], 
                                   demo[i,"reg_condate_protect"])){
          demo[i,"arm_num"]<-1
        }else if(demo[i,"mincon"] %in% demo[i,"reg_condate_protect2"]){demo[i,"arm_num"]<-2
        }else if(demo[i,"mincon"] %in% demo[i,"reg_condate_protect3"]){demo[i,"arm_num"]<-3}}
      ymd(demo$dob)->demo$dob
      age_calc(dob=demo$dob, enddate=demo$mincon, units="years",precise=F)->demo$bl_age
    demo[grepl("condate|dob|mincon",names(demo))]<-NULL
    #Merge into cirs2
    merge(cirs2,demo, by="masterdemoid", all.x=T)->cirs3
  
    
    
    
    
    
    