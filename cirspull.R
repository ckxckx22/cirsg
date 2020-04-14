#Cirs pull
#startup
source('/Users/mogoverde/WFH/startup.R')
startup()
#setwd
setwd("/Users/mogoverde/WFH")
#Libraries
library(lubridate)
library(dplyr)
library(eeptools)
#Redcap setup
    md<-bsrc.checkdatabase2(ptcs$masterdemo, batch_size=500L, online=T)
      idmap<-md$data[c("registration_redcapid","registration_wpicid","registration_soloffid")]
      names(idmap)<-c("masterdemoid","wpicid","soloffid")
    pt<-bsrc.checkdatabase2(ptcs$protect, batch_size=500L, online=T)
    ptdata<-bsrc.findid(pt$data, idmap=idmap, "registration_redcapid")
    ptdata[-which(!ptdata$ifexist),]->ptdata
#Get CIRS data (longitudinal, includes ssi and ham)
    cirs<-data.frame(masterdemoid=ptdata$registration_redcapid, bldate=ptdata$bq_date,
              fudate=ptdata$fug_date,event=ptdata$redcap_event_name,
               ptdata[grepl("cirs",names(ptdata))], ptdata[grepl("ham",names(ptdata))],
               ptdata[grepl("ssi_",names(ptdata))])
    cirs[!grepl("miss|complete|___|anchor|total", names(cirs))]->cirs
    #Remove all rows that there is NO cirs data
    cirs[-which(rowSums(is.na(cirs[paste("cirsg_",c(1:13),"_s",sep="")]))==13),]->cirs
    #Get date info for time
    as.character(cirs$bldate)->cirs$bldate
    as.character(cirs$fudate)->cirs$fudate
    cirs$cirs_date=ifelse(cirs$bldate=="", cirs$fudate, cirs$bldate)
    ymd(cirs$cirs_date)->cirs$cirs_date
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
#Get personality measures (NEO, SIDP)
    #SIDP
      #1. Get sidpdf
      sidpdf<-data.frame(masterdemoid=ptdata$masterdemoid, event=ptdata$redcap_event_name,
                  ptdata[(grepl("^sidp.*score$",names(ptdata)) & !grepl("unable",names(ptdata)))])
      #2. Event map data for sidp (only given at BL, not catchup BL)
        sidpdf[grepl("baseline",sidpdf$event) & ! grepl("catchup", sidpdf$event),]->sidpdf
      #3. Remove NA's (if more than half of values are NA) and check for invalid (4= "Unable to score")
        if(any(!is.na(sidpdf) & sidpdf==4)){message("STOP! Fix code to change 4's to NAs")}
        sidpdf[rowSums(is.na(sidpdf[3:52]))<25,]->sidpdf
      #4. Check if any duplicated IDs
        #Remove if the full row is duplicated (same data) besides event name
        sidpdf[!duplicated(sidpdf[-2]),]->sidpdf
        #Check if any ids are duplicated
        #Remove NA IDs
        any(duplicated(sidpdf$masterdemoid))
      #5. Score
       data.frame(masterdemoid=sidpdf$masterdemoid,
                  extravar=T)->scores
        
      #functions
      sum.sidp<-function(dfx){
        names(dfx)[!grepl("ID", names(dfx))]->nmdfx
        for (i in 1:nrow(dfx)){
            sum(is.na(dfx[i,nmdfx]))->dfx[i,"NAs"]
            if(dfx[i,"NAs"]>0){
              #[number of non-NA criteria met/# of possible criteria-missingness]*possible criteria
              ((sum(dfx[i,nmdfx]>2 & !is.na(dfx[i,nmdfx])))/(sum(table(nmdfx))-(dfx[i,"NAs"])))*(sum(table(nmdfx)))->dfx[i,"SUM"]
            }else{sum(dfx[i, nmdfx]>=2)-> dfx[i,"SUM"]}
          round(dfx$SUM)->dfx$SUM}
        dfx[!grepl("NAs", names(dfx))]->dfx
        return(dfx)}
      #Marks presence/absence of disorder based on # of criteria met
      thresh.sidp<-function(dfy, threshold){
        ifelse(dfy$SUM>=threshold,1,0)->dfy$Presence
        return(dfy)}
      #Antisocial: J3, J5, J6, J2, J7, J4, J8 
        #(J9 for conduct d/o, needs 3 sx's out of 7 AND conduct d/o): 
        sidpdf[grepl("masterdemoid|j3|j5|j6|j2|j7|j4|j8|j9", names(sidpdf))]->assidpdf
        assidpdf[rowSums(is.na(assidpdf[-1]))<4,]->assidpdf
        sum.sidp(assidpdf)->assidpdf
        ifelse(assidpdf$SUM>3 & assidpdf$sidp_iv_j9score>=2,1,0)->assidpdf$Presence
      #Avoidant (needs 4 sx's out of 7)
        sidpdf[grepl("masterdemoid|b2|d3|c3|d5|d6|d1a|a3", names(sidpdf))]->avsidpdf
        avsidpdf[rowSums(is.na(avsidpdf[-1]))<4,]->avsidpdf
        sum.sidp(avsidpdf)->avsidpdf
        thresh.sidp(avsidpdf,4)->avsidpdf
      #Obsessive-Compulsive (needs 4 sx's out of 8)
        sidpdf[grepl("masterdemoid|b6|b4|b1|g5|g7|b5|a7|g6", names(sidpdf))]->ocsidpdf
        ocsidpdf[rowSums(is.na(ocsidpdf[-1]))<4,]->ocsidpdf
        sum.sidp(ocsidpdf)->ocsidpdf
        thresh.sidp(ocsidpdf,4)->ocsidpdf
      #Borderline (needs 5 sx's out of 9)
        sidpdf[grepl("masterdemoid|c7|c5|g1|j1|i5|e4|e5|i1|i4", names(sidpdf))]->bpsidpdf
        bpsidpdf[rowSums(is.na(bpsidpdf[-1]))<5,]->bpsidpdf
        sum.sidp(bpsidpdf)->bpsidpdf
        thresh.sidp(bpsidpdf,5)->bpsidpdf
      #Narcissistic (needs 5 sx's out of 9)
        sidpdf[grepl("masterdemoid|g3|g4|d1n|d11|g2|b3|d4|g8|i2", names(sidpdf))]->nsidpdf
        nsidpdf[rowSums(is.na(nsidpdf[-1]))<5,]->nsidpdf
        sum.sidp(nsidpdf)->nsidpdf
        thresh.sidp(nsidpdf,5)->nsidpdf
      #Schizotypal (needs 5 sx's out of 9)
        sidpdf[grepl("masterdemoid|h5|h7|h_8|f2|h9|f4|f1|c2|d2", names(sidpdf))]->ssidpdf
        ssidpdf[rowSums(is.na(ssidpdf[-1]))<5,]->ssidpdf
        sum.sidp(ssidpdf)->ssidpdf
        thresh.sidp(ssidpdf,5)->ssidpdf
      #Make final dataframe with scores
        #data.frame(ID=sidpdf$ID)->scores
        assidpdf$SUM[match(scores$masterdemoid, assidpdf$masterdemoid)]->scores$ASPD_sxs
        assidpdf$Presence[match(scores$masterdemoid, assidpdf$masterdemoid)]->scores$ASPD_presence
        avsidpdf$SUM[match(scores$masterdemoid, avsidpdf$masterdemoid)]->scores$AVPD_sxs
        avsidpdf$Presence[match(scores$masterdemoid, avsidpdf$masterdemoid)]->scores$AVPD_presence
        ocsidpdf$SUM[match(scores$masterdemoid, ocsidpdf$masterdemoid)]->scores$OCPD_sxs
        ocsidpdf$Presence[match(scores$masterdemoid, ocsidpdf$masterdemoid)]->scores$OCPD_presence
        bpsidpdf$SUM[match(scores$masterdemoid, bpsidpdf$masterdemoid)]->scores$BPD_sxs
        bpsidpdf$Presence[match(scores$masterdemoid, bpsidpdf$masterdemoid)]->scores$BPD_presence
        nsidpdf$SUM[match(scores$masterdemoid, nsidpdf$masterdemoid)]->scores$NPD_sxs
        nsidpdf$Presence[match(scores$masterdemoid, nsidpdf$masterdemoid)]->scores$NPD_presence
        ssidpdf$SUM[match(scores$masterdemoid, ssidpdf$masterdemoid)]->scores$SZPD_sxs
        ssidpdf$Presence[match(scores$masterdemoid, ssidpdf$masterdemoid)]->scores$SZPD_presence
        scores$extravar<-NULL
        #How many
        nrow(scores[which(scores$masterdemoid %in% cirs3$masterdemoid),])
    #NEO
      #1. Get neodf
      neodf<-data.frame(masterdemoid=ptdata$masterdemoid, event=ptdata$redcap_event_name,
                  ptdata[(grepl("^neo",names(ptdata)) & !grepl("s|complete|admin",names(ptdata)))])
      #2. Remove NA's
      neodf[which(rowSums(is.na(neodf[grepl("neo",names(neodf))]))<60),]->neodf
      #3. remove duplicates
      neodf[-which(duplicated(neodf[-2])),]->neodf
        #2 duplicated, remove as both are idecide
        neodf[-duplicated(neodf$masterdemoid),]->neodf
      #4. Score
        #Neuroticism:
        #Negative affect
        rowSums(neodf[paste0("neoffi_", c(1,11,16,31,46))], na.rm=FALSE)->neodf$Neuro_NA
        #Self-reproach
        rowSums(neodf[paste0("neoffi_", c(6,21,26,36,41,51,56))], na.rm=FALSE)->neodf$Neuro_SR
        #Total
        rowSums(neodf[,c("Neuro_NA","Neuro_SR")])->neodf$Neuro_Total
      #Extraversion:
        #Positive affect
        rowSums(neodf[paste0("neoffi_", c(7,12,37,42))], na.rm=FALSE)->neodf$Extra_PA
        #Sociability
        rowSums(neodf[paste0("neoffi_", c(2,17,27,57))], na.rm=FALSE)->neodf$Extra_SOC
        #Activity
        rowSums(neodf[paste0("neoffi_", c(22,32,47,52))], na.rm=FALSE)->neodf$Extra_ACT
        #Total
        rowSums(neodf[,c("Extra_PA","Extra_SOC", "Extra_ACT")])->neodf$Extra_Total
      #Openness:
        #Aesthetic Interests
        rowSums(neodf[paste0("neoffi_", c(13,23,43))], na.rm=FALSE)->neodf$Open_AI
        #Intellectual interests
        rowSums(neodf[paste0("neoffi_", c(48,53,58))], na.rm=FALSE)->neodf$Open_II
        #Unconventionality
        rowSums(neodf[paste0("neoffi_", c(3,8,18,38))], na.rm=FALSE)->neodf$Open_UNC
        #Total
        rowSums(neodf[,c("Open_AI","Open_II", "Open_UNC")])->neodf$Open_Total
      #Agreeableness:
        #Non Antagonistic Orientation
        rowSums(neodf[paste0("neoffi_", c(9,14,19,24,29,44,54,59))], na.rm=FALSE)->neodf$Agree_NAO
        #Prosocial orientation
        rowSums(neodf[paste0("neoffi_", c(4,34,39,49))], na.rm=FALSE)->neodf$Agree_PSO
        #Total
        rowSums(neodf[,c("Agree_NAO","Agree_PSO")])->neodf$Agree_Total
      #Conscientiousness
        #Orderliness
        rowSums(neodf[paste0("neoffi_", c(5,10,15,30,55))], na.rm=FALSE)->neodf$Consc_ORD
        #Goal-striving
        rowSums(neodf[paste0("neoffi_", c(25,35,60))], na.rm=FALSE)->neodf$Consc_GS
        #Dependability
        rowSums(neodf[paste0("neoffi_", c(20,40,45,50))], na.rm=FALSE)->neodf$Consc_DEP
        #Total
        rowSums(neodf[,c("Consc_ORD","Consc_GS", "Consc_DEP")])->neodf$Consc_Total
        neodf$event<-NULL
        neodf[!grepl("neoffi",names(neodf))]->neodf
        #How many
        nrow(neodf[which(neodf$masterdemoid %in% cirs3$masterdemoid),])
#Get Social Support Document, Perceived Burdensomness
      #SSD
        #1. Get ssddf
        ssddf<-data.frame(masterdemoid=ptdata$masterdemoid, event=ptdata$redcap_event_name,
                    ptdata[(grepl("^ssd",names(ptdata)) & !grepl("miss|complete|admin",names(ptdata)))])
        #2. Remove NA's
        ssddf[which(rowSums(is.na(ssddf[grepl("ssd",names(ssddf))]) | ssddf[grepl("ssd",names(ssddf))]=="")<25),]->ssddf
        #3. remove duplicates
        ssddf[-which(duplicated(ssddf[-2])),]->ssddf
          #2 duplicated, remove as both are idecide
          any(duplicated(ssddf$masterdemoid))
          ssddf$event<-NULL
          nrow(ssddf[which(ssddf$masterdemoid %in% cirs3$masterdemoid),])
          
      #PB
        #1. Get pb
        pbdf<-data.frame(masterdemoid=ptdata$masterdemoid, event=ptdata$redcap_event_name,
                    ptdata[(grepl("^pb",names(ptdata)) & !grepl("miss|complete|admin|total|7|8",names(ptdata)))])
        #2. Remove NA's
        pbdf[which(rowSums(is.na(pbdf[grepl("pb",names(pbdf))]))<11),]->pbdf
        #3. remove duplicates
        pbdf[-which(duplicated(pbdf[-2])),]->pbdf
        #Only baseline
        pbdf[which(grepl("baseline",pbdf$event)),]->pbdf
          #one duplicated with different data, remove later
          pbdf[-which(duplicated(pbdf$masterdemoid)),]->pbdf
          pbdf$event<-NULL
          nrow(pbdf[which(pbdf$masterdemoid %in% cirs3$masterdemoid),])
#Merge into main df
    merge(cirs3,scores, by="masterdemoid", all.x = T)->cirs4
    merge(cirs4, neodf, by="masterdemoid", all.x = T)->cirs5
    merge(cirs5, ssddf, by="masterdemoid", all.x = T)->cirs6
    merge(cirs6, pbdf, by="masterdemoid", all.x = T)->cirs7
    