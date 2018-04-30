# REDCAP IN R
This a project that use R to deal with RedCap data. Although now it might have extended beyond that...
- REDREW: main functions
	- New Version 2.0: Refresh data organization method to be similar as EMA; Now automator refresh in background every 3 hours
- AUTOMATOR: automator refresh and back-up functions
- ECOLOGIST: ema data processing and organizing functions 
	- New Version 1.1: 
		- Introduce new data organization method using RData file. Pretty clean and nice to use.
		- Now will preprocess data into separate data frame within list structure.
- ADMINISTRATOR: functions that helps speed up administrative tasks
- RETRO: replicating matlab functions that process behavioral data in R

Find changelogs and function description on the top of each script.

**Required Packages:**
```
library("data.table")
library("lubridate")
library("ggplot2")
library("zoo")
library("plyr")
library("ggrepel")
library("redcapAPI")
library("RCurl")
library("data.table")
library("REDCapR")
library(pracma)
library(rprime)
library(tidyr)
library(stringdist)
```
# README for EMA DATA FILE: 
In Box Sync/Skinner/Data/EMA Data, there is a file: "emadata.all.rdata"
Within it there are: 
- [fulldata.ema] All the EMA data, including raw, processed progress, upload to redcap, info of the subject;
- [metadata.ema] All the metadata (variable names and exact questions etc,.) for EMA instruments, Beginning of the Day, During the Day, End of the Day and Micro-Burst.

You can load these data to your R environment and use them whatever you desire so. 

A good practice is to only attach data:
```
attach("~/emadata.all.rdata")
yourdf<-fulldata.ema
...
detach("file: ~/emadata.all.rdata")
```

Below are the orgnization for objects within emadata.all.rdata:

- [fulldata.ema] is a list object contains the following: 
	- $pdata: progress data (aggregated by single day)
	- $info: participant info
	- $raw: raw data, untouched data
	- $rdata: redcap upload data, basically slimed down (aggregated by week), wide format of $pdata
	- $procdata: list object, contains processed data, separated into 4 data frames each contains item level processed data for different instruments: 
		- $bod: Beginning of the Day
		- $dod: During the Day / Random Prompt
		- $eod: End of the Day
		- $mb: Follow-Up / Micro-burst
	- $update.date: date of when the data is updated

- [metadata.ema] is a list object contains the following:
	- $bod: Beginning of the Day
		- $metadata: full metadata date frame
		- $valuevariname: list of variable names (as string) that contains actual value [excluding INFORMATION type questions]
		- $updated.date: date of when the metadata is updated
	- $dod: During the Day / Random Prompt
		- same structure as $bod
	- $eod: End of the Day
		- same structure as $bod
	- $mb: Follow-Up / Micro-burst
		- same structure as $bod
		
You may use functions in ECOLOGIST.R (found on git) to help navigate this data, currently, the function include:
```
bsrc.ema.progress.graph()  #graph each participant’s progress and their count number 
dnpl.ema.missinggraph()    #graph missingness on each survey using the $data
dnpl.ema.meanbyweek()      #calculate each form’s mean completion rate by week
```
