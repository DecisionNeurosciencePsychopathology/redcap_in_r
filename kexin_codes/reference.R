
choice_map<-bsrc.getchoicemapping(variablenames = "registration_gender",protocol = ptcs$masterdemo)

df_new$registration_gender [!df_new$registration_gender %in% choice_map$choice.code]
