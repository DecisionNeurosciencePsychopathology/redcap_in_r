#Actual Pipeline:
# Input
ptcs = ptcs[c("masterdemo")]

#Step 1:
# [ID] ID/DEMO duplicate check against Master Demo  
# General rule before generating IDs for folks who are not in the master demo, one must construct a data frame with existing ID and some basic uniquely identifiable information such as initial, data of birth, last four of the social, gender and race. 
masterdemo<-bsrc.checkdatabase2(protocol = ptcs$masterdemo,online = T,batch_size=10000L)
idmap<-masterdemo$data[""]
bsrc.findid(df,idmap=NULL,id.var="ID",onlyoutput=NULL)

#Step 2:
# [EVT] Event mapping based on entry date and a ID/Date/Protocol mapping

#Step 3:
# [CLEAN] Data cleaning and separation

#Step 4:
# [VERIFY] Variable mapping, Data validation and Structure manipulation
# The data prior to uploading requires the following three steps:
#   Variable mapping: most of the assessments now takes a new form of variable names, we will need to change them. This step requires a variable mapping data frame for each assessment.
# Data validation: some of the assessment requires validation as there might be errors. When encountered an error, one should 

#Step 5:
# [Upload] Uploading data

#Step 6:
# [AUTO] Automatic synchronization and updating of existing data  
