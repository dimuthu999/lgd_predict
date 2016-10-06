
# Freddie -----------------------------------------------------------------
rm(list=ls())
library(plyr)
setwd("C:/Dim/Freddie Per")
filelist = list.files(pattern = ".*.txt")

i=1
for(fn in filelist) {
  cat(fn,i,"\n")
  
  pefdata <- read.table(fn, sep = "|",quote = "", row.names = NULL, stringsAsFactors = FALSE) 
  names(pefdata)<-c("loanid","reportingperiod_t","currentupb_t","delstatus_t","age_t","monthstomaturity_t","repurchase_t","modification_t","zbcode_t","zbdate_t","interestrate_t","differedupb_t","lastduedate_t","insrecovery_t","salesproceeds_t","noninsrecovery_t","expenses_t")
  
  delloanid <- unique(pefdata[pefdata$delstatus_t %in% c("6"),'loanid']) # 3 means 90 to 120 days late
  pefdata <- pefdata[pefdata$loanid %in% delloanid,]
  pefdata['modified']<-ifelse(pefdata$modification_t=="Y",1,0)
  
  zbloans <- pefdata[pefdata$zbcode_t %in% c(3,9),]  
  zbloans$currentupb_t <- NULL
  zbloans$interestrate_t <- NULL
  zbloans <- merge(zbloans, pefdata[,c("loanid","reportingperiod_t","currentupb_t","interestrate_t")],by.x = c("loanid","lastduedate_t"),by.y = c("loanid","reportingperiod_t"),all.x = TRUE)
  zbloans <- zbloans[,c("loanid","reportingperiod_t","age_t","currentupb_t","lastduedate_t","interestrate_t","zbcode_t","insrecovery_t","salesproceeds_t","noninsrecovery_t","expenses_t")]
  names(zbloans)<-c("loanid","forc_date","forc_age","forc_upb","lastduedate_t","interestrate_t","zbcode_t","insrecovery_t","salesproceeds_t","noninsrecovery_t","expenses_t")
  
  del_obs <- pefdata[pefdata$delstatus_t %in% c("6"),]
    prev_default <- ddply(del_obs,.(loanid),summarise,prev_defaults=length(loanid))
    prev_default$prev_defaults <- prev_default$prev_defaults-1
  del_obs <- del_obs[!duplicated(del_obs[,c("loanid")],fromLast = TRUE),]
  
  #observations before 6 month default
  pefdata<-merge(pefdata,del_obs[,c("loanid","age_t")],by=c("loanid"))
  pefdata <- pefdata[pefdata$age_t.x<pefdata$age_t.y,]
  modified_in_past <- ddply(pefdata,.(loanid),summarise,modified_in_past=max(modified,na.rm = TRUE))
  
  del_obs <- del_obs[,c("loanid","reportingperiod_t","currentupb_t","age_t")]
  names(del_obs)<-c("loanid","default_date","default_upb","default_age")
  del_obs <- merge(del_obs,prev_default,by=c("loanid"))
  del_obs <- merge(del_obs,modified_in_past,by=c("loanid"))
  del_obs <- merge(del_obs,zbloans,by=c("loanid"),all.x = TRUE)
  
  
  del_obs$default_date <- as.Date(paste(substr(del_obs$default_date,1,4),"-",substr(del_obs$default_date,5,6),"-01",sep=""))
  del_obs$forc_date <- ifelse(!is.na(del_obs$forc_date),(paste(substr(del_obs$forc_date,1,4),"-",substr(del_obs$forc_date,5,6),"-01",sep="")),NA)
  del_obs$forc_date <- as.Date(del_obs$forc_date)
  del_obs$lastduedate_t <- ifelse(!is.na(del_obs$lastduedate_t),(paste(substr(del_obs$lastduedate_t,1,4),"-",substr(del_obs$lastduedate_t,5,6),"-01",sep="")),NA)
  del_obs$lastduedate_t <- as.Date(del_obs$lastduedate_t)
  
  
  del_obs$forc_age <- as.numeric(del_obs$default_age + floor((del_obs$forc_date-del_obs$default_date)/30))
  
  del_obs$salesproceeds_t <- as.numeric(del_obs$salesproceeds_t)
  del_obs[,c("forc_upb","interestrate_t","expenses_t","noninsrecovery_t","salesproceeds_t","insrecovery_t")]<-
    apply(del_obs[,c("forc_upb","interestrate_t","expenses_t","noninsrecovery_t","salesproceeds_t","insrecovery_t")], 2, function(x){replace(x, is.na(x), 0)})
  
  #Delinquent Interest = (Zero Balance Effective Date - Due Date of Last Paid Installment in months) X Current Actual UPB X 30/360 X (Current Interest Rate - Servicing Fee)/100
  del_obs['del_interest'] <- as.numeric((del_obs$forc_date - del_obs$lastduedate_t)/30)*del_obs$forc_upb * (30/360)*(del_obs$interestrate_t-0.25)/100
  
  del_obs$del_interest <- ifelse(is.na(del_obs$del_interest),0,del_obs$del_interest)
  

  #Net Loss = Default UPB - Net Sales Proceeds + Expenses - MI Recoveries - Non MI Recoveries + Delinquent Interest
  del_obs['lgd_fv'] <- (del_obs$forc_upb - del_obs$salesproceeds_t + (del_obs$expenses_t*(-1))-del_obs$insrecovery_t - del_obs$noninsrecovery_t + del_obs$del_interest)/del_obs$forc_upb
  del_obs$lgd_fv <- ifelse(!is.finite(del_obs$lgd_fv) | is.na(del_obs$lgd_fv),NA,del_obs$lgd_fv)
  del_obs['lgd_pv'] <- del_obs$lgd_fv/((1+del_obs$interestrate_t/100)^as.numeric(del_obs$forc_age-del_obs$default_age))
  del_obs['foreclosed'] <-ifelse(is.na(del_obs$zbcode_t),0,1)
  del_obs <- del_obs[,c("loanid","default_date","default_upb","default_age","prev_defaults","modified_in_past"
                        ,"forc_date","forc_age","forc_upb","lastduedate_t","zbcode_t","lgd_fv","lgd_pv","foreclosed")]
  
  if(i==1){
    write.table(del_obs,file="lgd_dataset_freddie.csv",append=FALSE,sep="|",col.names=TRUE,quote=FALSE,row.names = FALSE,na="")
  } else {
    write.table(del_obs,file="lgd_dataset_freddie.csv",append=TRUE,sep="|",col.names=FALSE,quote=FALSE,row.names = FALSE,na="")
  }
  
  i=i+1
}



# Fannie ------------------------------------------------------------------
rm(list=ls())
library(plyr)
setwd("C:/Dim/Fannie Per")
filelist = list.files(pattern = ".*.txt")

i=1
for(fn in filelist) {
  cat(fn,i,"\n")
  
  pefdata <- read.table(fn, sep = "|",quote = "", row.names = NULL, stringsAsFactors = FALSE) 
  names(pefdata)<-c('loanid','reportingperiod_t','servicer_t','interestrate_t','currentupb_t','age_t','monthstomaturity_t','adjmonthstomaturity_t','maturitydate','msa','delstatus_t','modification_t','zbcode_t','zbdate_t','lastduedate_t','foreclosuredate','dispositiondate','expenses1_t','expenses2_t','expenses3_t','expenses4_t','taxes_t','salesproceeds_t','insrecovery_t','repurchaseproceeds_t','forcproceedes_t')
  
  
  delloanid <- unique(pefdata[pefdata$delstatus_t %in% c("6"),'loanid']) # 3 means 90 to 120 days late
  pefdata <- pefdata[pefdata$loanid %in% delloanid,]
  pefdata['modified']<-ifelse(pefdata$modification_t=="Y",1,0)
  
  zbloans <- pefdata[pefdata$zbcode_t %in% c(3,9),]  
  zbloans$currentupb_t <- NULL
  zbloans$interestrate_t <- NULL
  zbloans <- merge(zbloans, pefdata[,c("loanid","reportingperiod_t","currentupb_t","interestrate_t")],by.x = c("loanid","lastduedate_t"),by.y = c("loanid","reportingperiod_t"),all.x = TRUE)
  zbloans <- zbloans[,c("loanid","reportingperiod_t","age_t","currentupb_t","lastduedate_t","interestrate_t","zbcode_t","expenses1_t","expenses2_t","expenses3_t","expenses4_t","taxes_t","salesproceeds_t","insrecovery_t","repurchaseproceeds_t","forcproceedes_t")]
  names(zbloans)<-c("loanid","forc_date","forc_age","forc_upb","lastduedate_t","interestrate_t","zbcode_t","expenses1_t","expenses2_t","expenses3_t","expenses4_t","taxes_t","salesproceeds_t","insrecovery_t","repurchaseproceeds_t","forcproceedes_t")
  
  del_obs <- pefdata[pefdata$delstatus_t %in% c("6"),]
    prev_default <- ddply(del_obs,.(loanid),summarise,prev_defaults=length(loanid))
    prev_default$prev_defaults <- prev_default$prev_defaults-1
  del_obs <- del_obs[!duplicated(del_obs[,c("loanid")],fromLast = TRUE),]
  
  #observations before 6 month default
  pefdata<-merge(pefdata,del_obs[,c("loanid","age_t")],by=c("loanid"))
  pefdata <- pefdata[pefdata$age_t.x<pefdata$age_t.y,]
  modified_in_past <- ddply(pefdata,.(loanid),summarise,modified_in_past=max(modified,na.rm = TRUE))
  
  del_obs <- del_obs[,c("loanid","reportingperiod_t","currentupb_t","age_t")]
  names(del_obs)<-c("loanid","default_date","default_upb","default_age")
  del_obs <- merge(del_obs,prev_default,by=c("loanid"))
  del_obs <- merge(del_obs,modified_in_past,by=c("loanid"))
  
  del_obs <- merge(del_obs,zbloans,by=c("loanid"),all.x = TRUE)
  
  del_obs$default_date<- as.Date(del_obs$default_date, "%m/%d/%Y")
  del_obs$forc_date <- ifelse(!is.na(del_obs$forc_date),as.Date(del_obs$forc_date, "%m/%d/%Y"),NA)
  del_obs$forc_date <- as.Date(del_obs$forc_date)
  del_obs$lastduedate_t <- ifelse(!is.na(del_obs$lastduedate_t),as.Date(del_obs$lastduedate_t, "%m/%d/%Y"),NA)
  del_obs$lastduedate_t <- as.Date(del_obs$lastduedate_t)

  del_obs$forc_age <- as.numeric(del_obs$default_age + floor((del_obs$forc_date-del_obs$default_date)/30))
  
  del_obs[,c("forc_upb","interestrate_t","expenses1_t","expenses2_t","expenses3_t","expenses4_t","taxes_t","salesproceeds_t","insrecovery_t","repurchaseproceeds_t","forcproceedes_t")]<-
    apply(del_obs[,c("forc_upb","interestrate_t","expenses1_t","expenses2_t","expenses3_t","expenses4_t","taxes_t","salesproceeds_t","insrecovery_t","repurchaseproceeds_t","forcproceedes_t")], 2, function(x){replace(x, is.na(x), 0)})
  
  #Delinquent Interest = (Zero Balance Effective Date - Due Date of Last Paid Installment in months) X Current Actual UPB X 30/360 X (Current Interest Rate - Servicing Fee)/100
  del_obs['del_interest'] <- as.numeric((del_obs$forc_date - del_obs$lastduedate_t)/30)*del_obs$forc_upb * (30/360)*(del_obs$interestrate_t-0.25)/100
  del_obs$del_interest <- ifelse(is.na(del_obs$del_interest),0,del_obs$del_interest)
  #Net Loss = Default UPB - Net Sales Proceeds + Expenses - MI Recoveries - Non MI Recoveries + Delinquent Interest
  del_obs['lgd_fv'] <- (del_obs$forc_upb - del_obs$salesproceeds_t +del_obs$expenses1_t+del_obs$expenses2_t+del_obs$expenses3_t+
                          del_obs$expenses4_t+del_obs$taxes_t-del_obs$insrecovery_t - del_obs$repurchaseproceeds_t - del_obs$forcproceedes_t + del_obs$del_interest)/del_obs$forc_upb
  del_obs$lgd_fv <- ifelse(!is.finite(del_obs$lgd_fv) | is.na(del_obs$lgd_fv),NA,del_obs$lgd_fv)
  del_obs['lgd_pv'] <- del_obs$lgd_fv/((1+del_obs$interestrate_t/100)^as.numeric(del_obs$forc_age-del_obs$default_age))
  del_obs['foreclosed'] <-ifelse(is.na(del_obs$zbcode_t),0,1)
  del_obs <- del_obs[,c("loanid","default_date","default_upb","default_age","prev_defaults","modified_in_past"
                        ,"forc_date","forc_age","forc_upb","lastduedate_t","zbcode_t","lgd_fv","lgd_pv","foreclosed")]
  
  if(i==1){
    write.table(del_obs,file="lgd_dataset_fannie.csv",append=FALSE,sep="|",col.names=TRUE,quote=FALSE,row.names = FALSE,na="")
  } else {
    write.table(del_obs,file="lgd_dataset_fannie.csv",append=TRUE,sep="|",col.names=FALSE,quote=FALSE,row.names = FALSE,na="")
  }
  
  i=i+1
}



# Combination of datasets-------------------------------------------------------------
rm(list=ls())
setwd("C:/Users/dnratnadiwakara/Documents/2016 Foreclosure Delay")

freddie <- read.table("C:/Dim/Freddie Per/lgd_dataset_freddie.csv", sep = "|",quote = "", row.names = NULL, stringsAsFactors = FALSE,header = TRUE) 
freddie['gse']<-'freddie'
freddie$modified_in_past <- as.integer(freddie$modified_in_past)

fannie <- read.table("C:/Dim/Fannie Per/lgd_dataset_fannie.csv", sep = "|",quote = "", row.names = NULL, stringsAsFactors = FALSE,header = TRUE) 
fannie$loanid <- as.character(fannie$loanid)
fannie['gse']<-'fannie'

def_data <- rbind(freddie,fannie)

allloans <- readRDS("CombinedFreddieFannieAct_Nov18.rds")

def_data <- merge(def_data,allloans,by="loanid",all.x = TRUE)

unemp <- readRDS("zipunemp.rds")
names(unemp)<-c("default_date","zip","unemp_on_def")
def_data <- merge(def_data,unemp,by=c("default_date","zip"),all.x = TRUE)
unemp<-NULL

lardata <-readRDS("censusdata_zip_Aug232016.rds")
lardata$zip <- lardata$zip*100
names(lardata)[1]<-"default_year"

def_data['default_year']<-as.double(substr(def_data$default_date,1,4))
def_data <- merge(def_data,lardata,by=c("default_year","zip"),all.x = TRUE)


hpi <- read.csv("C:/Users/dnratnadiwakara/Documents/2016 Foreclosure Delay/HPI3zip.csv")
hpi$zip <- hpi$zip/10
hpi['current_q']<- as.yearqtr(as.Date(hpi$yearquarter,origin="1900-01-01"))
hpi <- hpi[,c('current_q','zip','index')]
names(hpi) <- c('default_q','zip','default_hpi')

def_data['default_q']<-as.yearqtr(as.Date(def_data$default_date))
def_data <- merge(def_data,hpi, by=c('default_q','zip'),all.x = TRUE)

def_data['org_q']<-as.yearqtr(def_data$orgdate)
hpiorg <- as.data.frame(hpi)
names(hpiorg) <- c('org_q','zip','org_hpi')
def_data <- merge(def_data,hpiorg, by=c('org_q','zip'),all.x = TRUE)

def_data['current_housevalue'] <- (def_data$upb*100/def_data$ltv)*(def_data$default_hpi)/def_data$org_hpi
def_data['default_equity'] <- (def_data$current_housevalue - (def_data$default_upb))/(def_data$current_housevalue)

saveRDS(def_data,file="lgd_predict_dataset.rds")