# BSRC
README ongoing. Updated on Apr 12 2020

## Overview
This package is used by the Longitudinal Research Program in Late-Life Suicide to manage and process our research data. 

* REDREW: main functions 
* AUTOMATOR: automator refresh and back-up functions  
* ECOLOGIST: EMA data processing and organizing functions  
* ADMINISTRATOR: functions that helps speed up administrative tasks  
* MIGRATOR: functions for migrating data from access; as well as from redcap to redcap.  
* THOR (no longer in use): codes specifically deals with contents on thorndike

## REDREW
### Frequently used functions 
#### `bsrc.checkdatabase2`

#### `bsrc.getform`

#### `bsrc.findid(df,idmap=NULL,id.var="ID",onlyoutput=NULL)`  
Map IDs in a \<dataframe> onto MasterDemoID. Column "ifexist" in the output data frame indicates if an ID can be successful mapped. If `false`, it's likely that this person's ID is entered wrongly, or the record is test data, or this person is not our study's subject.   

#### `bsrc.getchoicemapping`

#### `bsrc.checkbox`


### Supplementary functions 
#### `bsrc.attachngrab`  

#### `bsrc.conredcap2`

