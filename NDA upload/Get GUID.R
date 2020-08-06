# How to use this script: 
## Change paths in the first four lines 
## Run the codes, you will get four dfs: newcon (new consents), recon (re consents), missingdata (the rest), and P3_unmatched (this tells you who weren't found in the worksheet)
## The dfs contain the required columns in the required names and order, plus some cols that help you find the ID if it's not there already 
## "check_HasAllData": (should be all TRUE) if the ID has all data required (excluding Middle Name) to get a GUID
## "check_MatchedRedcapID": (should be all TRUE) If a redcap ID can be found based on initials AND dob. 
## "check_SexMatched": (should be all TRUE) Test if the redcap ID is matched correctly based on the sex at birthed saved in Redcap and the worksheet
# Things to do: 
## Check the "check_..." columns and see if anything/what went wrong 
## Check the comments of pts whose middle name is missing. Do they not have a middle name? Fill in the column "SUBJECTHASMIDDLENAME"
## Check the df P3_unmatched to see if any P3 pts is missing from the worksheet 

### CHANGE THE PATHS TO MATCH YOURS 
setwd("~/Documents/github/UPMC/NDA upload/")
source('~/Documents/github/UPMC/startup.R')
library(tidyverse);library(lubridate)
boxpath= "~/Box/skinner/data/NDA upload/"

### RUN THE CODES BELOW 
# get all P3 pts 
MD_p3<-md$data %>%
  filter(registration_ptcstat___protect3==1) %>%
  mutate_all(~replace(.,.=="",NA))

# load the xlsx sheets 
newcon_raw<-readxl::read_excel(paste0(boxpath,"practice GUID worksheet.xlsx"),sheet = 1)
recon_raw<-readxl::read_excel(paste0(boxpath,"practice GUID worksheet.xlsx"),sheet = 2)
missingdata_raw<-readxl::read_excel(paste0(boxpath,"practice GUID worksheet.xlsx"),sheet = 3)

# get subset of MD_p3 for ID mapping 
ID_init_map<-MD_p3[c("registration_redcapid","registration_initials","registration_dob","registration_birthsex")] %>% mutate(registration_dob=as.POSIXct(ymd(registration_dob)))

# Organize the sheets. Check recon and newcon: if everyone has all data required. Match ID using initials and dob. Confirm the ID is correct using sex at birth 
map2(list(newcon_raw,recon_raw),list("newcon","recon"), function(x,y){ # new consents and reconsents 
  x<-x %>% 
    mutate_if(is.character,~replace(.,.=="",NA)) %>%
    rename(dob = `Date of birth`, COB = `City/Municipality of Birth`, initials = `Initials in Master demo (used for ID match)`, SEX = `Sex at birth`) %>%
    mutate(FIRSTNAME=trimws(`First name at birth`, which = c("both")),MIDDLENAME=trimws(`Middle name at birth`, which = c("both")),LASTNAME=trimws(`Last name at birth`, which = c("both"))) %>%
    mutate(MOB=month(ymd(dob)),DOB=day(ymd(dob)),YOB=year(ymd(dob))) %>%
    mutate(SEX=plyr::mapvalues(SEX,c("Female","Male"),c("F","M"))) %>%
    mutate(SUBJECTHASMIDDLENAME=NA,USEEXISTINGGUID="YES",ID=1:nrow(.)) %>%
    select(FIRSTNAME,MIDDLENAME,LASTNAME,MOB,DOB,YOB,COB,SEX,SUBJECTHASMIDDLENAME,USEEXISTINGGUID,ID,initials,dob) %>% # df is ready. Will find the redcap id below 
    left_join(ID_init_map,by= c("initials"="registration_initials","dob" = "registration_dob")) %>%
    mutate(registration_birthsex=plyr::mapvalues(registration_birthsex,c(1,2),c("F","M"),warn_missing = F)) %>%
    mutate(check_HasAllData = rowSums(is.na(.[c("FIRSTNAME","LASTNAME","MOB","DOB","YOB","COB","SEX")]))==0, 
           check_MatchedRedcapID=!is.na(registration_redcapid),
           check_SexMatched= SEX==registration_birthsex) %>%
    select(-registration_birthsex)
  assign(y,x,envir = globalenv())})

missingdata<-missingdata_raw %>% # IDs that have missing data 
  mutate_if(is.character,~replace(.,.=="",NA)) %>%
  rename(dob = `Date of birth`, COB = `City/Municipality of Birth`, initials = `Initials in Master demo (used to match to ID)`, SEX = `Sex at birth`) %>%
  mutate(FIRSTNAME=trimws(`First name at birth`, which = c("both")),MIDDLENAME=trimws(`Middle name at birth`, which = c("both")),LASTNAME=trimws(`Last name at birth`, which = c("both"))) %>%
  mutate(MOB=month(ymd(dob)),DOB=day(ymd(dob)),YOB=year(ymd(dob))) %>%
  mutate(SEX=plyr::mapvalues(SEX,c("Female","Male"),c("F","M"))) %>%
  mutate(SUBJECTHASMIDDLENAME=NA,USEEXISTINGGUID="YES",ID=1:nrow(.)) %>%
  select(FIRSTNAME,MIDDLENAME,LASTNAME,MOB,DOB,YOB,COB,SEX,SUBJECTHASMIDDLENAME,USEEXISTINGGUID,ID,initials,dob) %>% # df is ready. Will find the redcapid below 
  left_join(ID_init_map,by= c("initials"="registration_initials","dob" = "registration_dob")) %>%
  mutate(registration_birthsex=plyr::mapvalues(registration_birthsex,c(1,2),c("F","M"),warn_missing = F)) %>%
  mutate(check_MatchedRedcapID=!is.na(registration_redcapid),
         check_SexMatched= SEX==registration_birthsex) %>%
  select(-registration_birthsex)

P3_unmatched<-ID_init_map %>% filter(!registration_redcapid %in% c(newcon$registration_redcapid,recon$registration_redcapid,missingdata$registration_redcapid))


