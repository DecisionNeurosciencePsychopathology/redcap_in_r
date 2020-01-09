# Access Form Cleaning Pipeline
## Setup 
rootdir 
variable map 
* 'baseline': when checking duplicated IDs, if baseline then check id if not baseline then check ID+CDATE
* checks: 
allsub
Logs: Out-of-range values, raplce, branching  
Database: pt or bsoc
* functions: report_wrong(), mdy()
## Introduction
## Steps 
### STEP1: Select a Access form - baseline(bl) first and then other forms- match with redcap variables, splited ordinary variables with checkbox variables, removed calculated variables   

### STEP4 fix data with systematic issues (eg: shifted range) identified in 'var_map' 
### STEP5 Excluding checkbox variables: Report out-of-range values AND if replace_w_na=T, replace them with NA.
### STEP7 for checkbox (fix_what1)
### STEP8 match checkbox variabels with other variabels using matching_id or IDDATE
### STEP9 branching (fix_what2) 

### OTHER: double check accross forms (eg: ham)