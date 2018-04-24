#REDREW:
Main functions

#AUTOMATOR 
Automator functions & script

#ADMINISTRATOR 
Routine administrative tasks scripts

#ECOLOGIST
EMA data process, management and presentation functions
 
#EMA Data Organization
In Box Sync/Skinner/Data/EMA Data, there is a file: emadata.all.rdata

Within it there are: 
All the EMA data, including raw, processed progress, upload to redcap, info of the subject;
All the metadata for EMA instruments, Beginning of the Day, During the Day, End of the Day and Micro-Burst.

\You can load these data to your R environment. 
\The specific object that contains all data: [fulldata.ema]
\The specific object that contains all metadata: [metadata.ema]

For each object, see details below:

[fulldata.ema] 
is a list object contains the following: 
$pdata             #progress data (aggregated by single day)
$info              #participant info
$raw               #raw data, untouched data
$rdata             #redcap upload data, basically slimed down (aggregated by week), wide format of $pdata
$procdata          #processed data, separated into 4 dfs: $bod; $dod; $eod; and $mb each contains processed data of surveys
$update.date       #updated date of the data

[metadata.ema]
is a list object contains the following:
$bod  #Beginning of the Day
$dod  #During the Day / Random Prompt
$eod  #End of the Day
$mb   #Follow-Up / Micro-burst

Each of those list objects contains the follow:
$metadata           #full metadata date frame
$valuevariname      #string list of variable names that contains value
$updated.date       #when is this updated 


You may use functions in ECOLOGIST.R to help navigate this data, currently, the function include:
bsrc.ema.progress.graph()  #graph each participant’s progress and their count number 
dnpl.ema.missinggraph()    #graph missingness on each survey using the $data
dnpl.ema.meanbyweek()      #calculates each form’s mean completion rate by week

 
