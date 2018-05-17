# REDCAP IN R REPOSITORY
This a project that use R to deal with RedCap data. Although now it might have extended beyond that...

Instead of scripts, now 4 main scripts are consolidated into one package:
- bsrc:
	- REDREW: main functions
		- New Major Revision (Ver 2.0):  
			- Refresh data organization method to be similar as EMA; 
			- Now automator refresh in background every 3 hours
	- AUTOMATOR: automator refresh and back-up functions
	- ECOLOGIST: ema data processing and organizing functions 
		- New Major Revision (Ver 1.1):  
			- Introduce new data organization method using RData file. Pretty clean and nice to use;
			- Now will preprocess data into separate data frame within list structure
	- ADMINISTRATOR: functions that helps speed up administrative tasks
	- MIGRATOR: functions for migrating data from access 
	- THOR: codes specifically deals with contents on thorndike

Functions in the following are still staying in script because they are not fully tested and functional
- RETRO: replicating matlab functions that process behavioral data in R

Find changelogs and function description on the first section of each script. For functions that are within the 'bsrc' package, go the 'R' folder to find the actual scripts.

To install the 'bsrc' package, you will need the package 'devtools'
```
library("devtools")
devtools::install_github("DecisionNeurosciencePsychopathology/redcap_in_r")
objects("package:bsrc")
```
**Before use of the package, you will need a profile for each RedCap connection**
You can use the bsrc.switcher() function to create one

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

The bsrc.attachngrab() in REDREW.R, will allow you to import the content in the rdata in a more efficient way

- Load them as list:
	
```
	list.load<-invisible(bsrc.attachngrab(rdpath=rdpath,returnas = "list"))
	curdb<-list.load[[1]]
```
- Load them as environment:
```
	envir.load<-invisible(bsrc.attachngrab(rdpath=rdpath,returnas = "envir"))
	objectlist<-object(envir=envir.load)
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
