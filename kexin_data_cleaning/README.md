# Access Forms Cleaning Pipeline (Protect)
Revised: Jan 9 2020  
## Setup 
rootdir 
logdir: directory of folder to save duplicated IDs that are removed from database
variable map  
* 'baseline': when checking duplicated IDs, if baseline then check id if not baseline then check ID+CDATE
    * baseline: TRUE or FALSE
* 'is.checkbox': TRUE, FALSE, NA. If NA, the variable will not be transferred.  
* checks (in STEP1): 
    * is.checkbox: one access_var in a certain form cannot be both checkbox and non-checkbox 
    * path: no NA, the path is valid
    * All variabels in the Access form can be found in the variable map, and all access_var in the variable map can match a column name of the Access form. Get a warning message if not.  
    * ID: No NA
    * CDATE: No NA 
    * If is.checkbox for CDATE is NA in a form, will get a warning message. 
    * if a non-checkbox access_var matches multiple redcap variables, get a warning message. 
Access form: in .csv format 
allsub: subjects of our lab  
skipotherforms
Logs: Out-of-range values, raplce, branching   
Database: pt or bsoc  
* functions: report_wrong(), mdy()
## Introduction
SPECIAL: 
At the end, use "warnings()" to retrieve all warning messages 
## Steps 
### STEP1
**Load Access form, split it into non-checkbox variables and checkbox variables, map non-checkbox Access variables to Redcap variables**  
1.1. Get `formname`, `vm`, `ifbl` (if is a baseline form)  
1.2. Read form, filter out subjects that are not ours.    
1.3. Convert CDATE to "yyyy-mm-dd" format. Create a copy fo the column. Combine ID+CDATE to make a new column called IDDATE. Check if any ID is duplicated for baseline forms or IDDATE for non-baseline forms. Remove the rows that contain duplicated IDs or IDDATEs and save the data seperately.  
1.4. Save chkbx vars to 'raw_nonch' and non-chkbx vars to df: 'raw_chk'  
1.5. Calculated fields will not be transferred. Remove them from the dataframe.   
1.6. variable map: replace access variables with redcap variable names. After var mapping, new colnames should contain only CDATE, IDDATE, and redcap variables, and there should be no duplicated column name.  
**Return**
* `RAWDATA`: Access variables to be transferred
* `raw_nonch`: Non-checkbox variables to be transferred. Variabels are mapped to be Redcap variables 
* `raw_chk`: Checkbox variables to be transferred. 
* `vm`: Varmap 

### STEP4 fix data with systematic issues (eg: shifted range) identified in 'var_map' 
### STEP5 Excluding checkbox variables: Report out-of-range values AND if replace_w_na=T, replace them with NA.
### STEP7 for checkbox (fix_what1)
### STEP8 match checkbox variabels with other variabels using matching_id or IDDATE
### STEP9 branching (fix_what2) 

### OTHER: double check accross forms (eg: ham)