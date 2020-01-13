# Access Forms Cleaning Pipeline (Protect)
Revised on Jan 9 2020  
## Setup 
`rootdir`: Directory of the Access forms to be transferred  
* Access form: in .csv format  

`logdir`: Directory of the folder to save duplicated IDs that are removed from database  

`variable map`  
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

`allsub`: subjects of our lab  

`skipotherforms`: If `TRUE`, skip non-baseline forms  

Logs: Out-of-range values, raplce, branching. The logs are initialized before cleaning the forms.   

`curdb`: pt or bsoc  

Functions: 
* `report_wrong()`: Functiont to report wrong values  
* `mdy()`: Convert date type data to the correct format "yyyy-mm-dd"  
  

## Steps 
### STEP1
#### Load Access form, split it into non-checkbox variables and checkbox variables, map non-checkbox Access variables to Redcap variables
1.1. Get `formname`, `vm`, `ifbl` (if is a baseline form)  
1.2. Read form, filter out subjects that are not ours.    
1.3. Convert `CDATE` to "yyyy-mm-dd" format. Create a copy fo the column. Combine ID+CDATE to make a new column called `IDDATE`. Check if any `ID` is duplicated for baseline forms or `IDDATE` for non-baseline forms. Remove the rows that contain duplicated `ID`s or `IDDATE`s and save the data seperately.  
1.4. Save chkbx vars to 'raw_nonch' and non-chkbx vars to df: 'raw_chk'  
1.5. Calculated fields will not be transferred. Remove them from the dataframe.   
1.6. variable map: replace access variables with redcap variable names. After var mapping, new colnames should contain only CDATE, IDDATE, and redcap variables, and there should be no duplicated column name.  
#### Return
* `RAWDATA`: Access variables to be transferred
* `raw_nonch`: Non-checkbox variables to be transferred. Variabels are mapped to be Redcap variables 
* `raw_chk`: Checkbox variables to be transferred. 
* `vm`: Variable map for the form being cleaned  

### STEP2 
#### Fix data with systematic issues identified in 'var_map' 'fixed_what1'
2.1. <u>range_fix</u>: Values in Redcap is shifted from the values in Access, eg: 1 in Access refer to 0 in Redcap. Use mapvalues() to fix the issue.  
2.2. <u>range_allowed</u>: Range allowed in Access is different from the range allowed in Redcap. If the value is out-of-range, write the info in log_out_of_range. Replace the value with NA and write in log_replace    
2.3 <u>date</u>: Convert date data into format "yyyy-mm-dd". Original data must be in format "month date year". After converting, check if date is within (-100,+20) years of today; stop if not.   
2.4 <u>value_set</u>: Import a certain value for every row in the form  
2.5 <u>value_set2</u>: If a variable equals a certain value, give another certain variable a certain value, otherwise, give it another value.  
#### Return 
* `fresh_nonch`: Cleaned `raw_nonch`
* `log_out_of_range`: Log of out-of-range values 
* `log_replace`: Log of values replaced with NA 

### STEP3 
#### Report out-of-range values and replace them with NA
Get the range using `bsrc.getchoicemapping()`. Raplace them with NA. Report out-of-range values.
#### Return
* `fresh_nonch`: Cleaned `fresh_nonch`
* `log_out_of_range`: Log of out-of-range values 
* `log_replace`: Log of values replaced with NA 

### STEP4 
#### Fix checkbox variables (fix_what)
4.2 <u>redcap_check</u>: Convert a non-checkbox access variable to a set of redcap checkbox variables   
4.3 <u>access_check</u>: Convert a checkbox access variable to a set of redcap checkbox variables  
4.4 <u>both_check1</u>: Convert a checkbox access variable to a set of redcap checkbox variables  
4.5 <u>both_check2</u>: Convert a checkbox access variable to a set of redcap checkbox variables   
4.6 <u>condition</u>: Assign an Access variable to Redcap variable only if another variable equals a certain value.  
4.7 <u>special_6</u>: range_fix and then convert the variable to multiple redcap variables  
4.8 <u>special_7</u>: Q3,Q3a; Q3NEW,Q3aNEW Two access variables go to one redcap, only one should have value  
#### Return
* `fresh_chk`: Cleaned `fresh_chk`

### STEP5 
Full join checkbox variabels with other variabels using matching_id (for baseline forms) or IDDATE (for other forms)
#### Return 
* `fresh_alldata`: Combine fresh_chk and fresh_nonch

### STEP6 
#### Branching (fix_what2) 
Check if the branching logic (var map instruction) is met, certain variable (var map access_var) must be blank if the branching logic is not met. If the variable has a value that matchs a certain pattern (var map value6), replace the value with NA. If not matching a certain pattern, report it and replace it with NA. Mannually check the rows that doesn't follow the branching logic.  
#### Return 
* `fresh_alldata`: Cleaned `fresh_alldata`
* `log_replace`: Log of replaced values 
* `log_branching`: Log of branching checks 

### OTHER: double entry, check accross forms (eg: ham)
At the end, use "warnings()" to retrieve all warning messages 
