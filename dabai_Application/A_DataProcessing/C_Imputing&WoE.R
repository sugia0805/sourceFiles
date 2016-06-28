dabaiData

dabaiDataBackup <- copy(dabaiData)
dabaiData<- copy(dabaiDataBackup)
#################################################

massiveMissing <- ggImpute(dabaiData, fullImpute = F, removeMassiveMissing = F)
setnames(dabaiData, "ProjectID", "financingprojectid")

dabaiData[, business_apply_BELONGNAME:=as.factor(business_apply_BELONGNAME)]
dabaiData[, age:=(as.Date(business_apply_OCCURDATE)-as.Date(ind_info_BIRTHDAY))/365]
dabaiData[, workYears:=(as.Date(business_apply_OCCURDATE)-as.Date(paste0(ind_info_WORKBEGINDATE, "/01")))/365]
dabaiData[, c("business_apply_OCCURDATE","ind_info_BIRTHDAY","ind_info_FULLNAME","ind_info_WORKBEGINDATE"):= NULL]


dabaiData <- dabaiData[flgDPD != -1, ]
autoWOE <- woeAutoBin(dabaiData, "flgDPD", exclude = "financingprojectid")
write.csv(autoWOE$woeTable, paste0(boxdata, "autoDabai.csv"))
