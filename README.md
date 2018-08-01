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
	- MIGRATOR: functions for migrating data from access; as well as from redcap to redcap.
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
(Skip this part if you have a startup script, which should include making the following)
You can use the following function to create one using necessary information:
```
bsrc.switcher(name=NULL,      #Any name you wish to give it to the profile, just easy way to identify
	      redcap_uri=NULL,#RedCap URL for the redcap system *required
	      token=NULL,     #RedCap token.                    *required
	      rdpath=NULL,    #local .rdata path, let it remain NULL if wish to use fully online mode
	      protocol.cur=F  #do not use global variable protocol.cur to generate profile
	      ) 
```

**To attach data**
To attach data as an environment or list, you can use the following function. The argument "returnas" can be set to either "list" or "envir." By default it should use "envir" for compatibility with other functions
```
curdb<-bsrc.attachngrab(protocol=ptcs$bsocial,returnas="envir")
```
The environment (or list) object should include the following objects: 
- data #Main data all in one data frame
- metadata #Meta data for mapping purposes
- eventmapping #Event mapping data for mapping purposes 
- update.date #When is this updated 
- update.time #What time is this updated
- success     #If the update was successful

Most of the function within this package will accept this environment (or list [limited support]) object as their data source. If not supplied, they will pull from RedCap server always using the credential. To minimize time cause, always supply this to a function when applicable. 
**Required Packages:**
Since now this repo can be loaded as package, the required packages are automatically loaded during installation of 'bsrc'
But here's a list of the packages:
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
bsrc::bsrc.ema.progress.graph()  #graph each participant’s progress and their count number 
bsrc::dnpl.ema.missinggraph()    #graph missingness on each survey using the $data
bsrc::dnpl.ema.meanbyweek()      #calculate each form’s mean completion rate by week
```

# README for Thorndike specific code:
These can only be used with Thorndike (by direct commandline or SSH).

To update RedCap scan database follow 3 steps:

1, Use start-up function to grab needed info from thorndike:
```
thorndike.startup(protocol="scandb",mode="offline")
#By default the function assign information list "protocol.cur" to global environment, which is on search path for step 3.  
```
2, Generate index.list object:
```
#Currently only accepts smartfind=T, later will add actual configuration files for higher precision.
index.list<-bsrc::thorndike.getfuncproclist(rootdir="/Volumes/bek",smartfind=T,dir.pattern="MR_Proc",proc.pattern="*nfsw*.nii.gz",id.pattern="[0-9]{4,6}",mprage.pattern="mprage",mapping=NULL,...)
```
3, Update RedCap using default parameters or alter with your own parameters:
```
#censor argument will take following pattern in list: id.rcstudy; rcstudy is the RedCap study name. 
bsrc::thorndike.updaterc(protocol=protocol.cur,index.list=NULL,preset=T,preproc.pattern="preproc", censor=c("120517.bsocial"),upload=T,output=F,...)
```
