dabai_target

masterDataTest


################
dabaiData <-masterDataTest[, c("audit_lnproduct_APPLYAMT",
                   "audit_lnproduct_APPLYTERM",
                   "audit_lnproduct_AVEMOB",
                   "audit_lnproduct_CARDUSERRATE",
                   "audit_lnproduct_CUSGROUP",
                   "audit_lnproduct_HAVUNSECREDEBTRATIO",
                   "audit_lnproduct_HAVUNSECURBEXPO",
                   "audit_lnproduct_INARTIFICIAL",
                   "audit_lnproduct_INTHAT",
                   "audit_lnproduct_ISCOMMISSION",
                   "audit_lnproduct_ISHAVECREDITCAR",
                   "audit_lnproduct_ISHOURSE",
                   "audit_lnproduct_JOBS",
                   "audit_lnproduct_LIMCRDCARD",
                   "audit_lnproduct_MDEBTRATIO",
                   "audit_lnproduct_MINCOME",
                   "audit_lnproduct_MORTGAGEBAL",
                   "audit_lnproduct_MORTGAGEPAY",
                   "audit_lnproduct_MPAYUNSECURD",
                   "audit_lnproduct_NOMDEBTRATIO",
                   "audit_lnproduct_OVERDUELN",
                   "audit_lnproduct_TOTMOB",
                   "audit_lnproduct_UNSECURBEXPO",
                   "audit_lnproduct_UNSECURDEBTRATIO",
                   "audit_lnproduct_UNSECURELNBAL",
                   "audit_lnproduct_USECRDCARD",
                   "business_apply_BASERATE",
                   "business_apply_BELONGNAME",
                   "business_apply_BUSINESSSUM",
                   "business_apply_CREDITCITY",
                   "business_apply_CUSTOMERSORT",
                   "business_apply_CUSTOMERTIME",
                   "business_apply_MONTH",
                   "business_apply_OTHERPURPOSE",
                   "business_apply_PURPOSE",
                   "business_apply_SALESCANAL",
                   "business_apply_TEMPSAVEFLAG",
                   "business_apply_TERMMONTH",
                   "customer_contact_KNOWLOAN",
                   "customer_contact_RELATIONSHIP_CUSTOMIZED",
                   "customer_contact_SEX",
                   "customer_credit_APPROVECOUNT1",
                   "customer_credit_AVERAGENEEDRET",
                   "customer_credit_AVGCOOPYEARS",
                   "customer_credit_AVGUSESUM",
                   "customer_credit_BACKSTATUS1",
                   "customer_credit_BACKSTATUS2",
                   "customer_credit_BACKSTATUS3",
                   "customer_credit_BACKTIMES2",
                   "customer_credit_CREDITACCOUNTNUM",
                   "customer_credit_CREDITLOANCOUNT",
                   "customer_credit_CREDITSUM",
                   "customer_credit_CREDITUSERATE",
                   "customer_credit_CREDITWORSTSTATE",
                   "customer_credit_CURRENTUSESUM1",
                   "customer_credit_CURRENTUSESUM2",
                   "customer_credit_DUECOUNT6MONTH",
                   "customer_credit_FIRSTSEMICREDITMONTH",
                   "customer_credit_FIRSTTLOANMONTH",
                   "customer_credit_HASHOUSEFUND",
                   "customer_credit_HASSOCIALSECURITY",
                   "customer_credit_HOUSINGLOANNUM",
                   "customer_credit_ISHAVECREDITCAR",
                   "customer_credit_LOANBALANCE",
                   "customer_credit_LOANOVERDUEONMONTH",
                   "customer_credit_LOANPAYMENTS",
                   "customer_credit_LOANPAYMENTS2",
                   "customer_credit_LOANREMAIN1",
                   "customer_credit_LOANREMAIN2",
                   "customer_credit_LOANWORSTSTATE",
                   "customer_credit_OTHERLOANNUM",
                   "customer_credit_OVERCREDIT",
                   "customer_credit_OVERSTATUS",
                   "customer_credit_OVERTIMES",
                   "customer_credit_OVERTIMES1",
                   "customer_credit_PLEDGEPAYMENTS",
                   "customer_credit_PLEDGEPAYMENTS2",
                   "customer_credit_PLEDGEREMAIN1",
                   "customer_credit_PLEDGEREMAIN2",
                   "customer_credit_RECARD",
                   "customer_info_CUSTOMERTYPE",
                   "ind_info_BIRTHDAY",
                   "ind_info_CHILDRENNO",
                   "ind_info_CHILDRENSTATUS",
                   "ind_info_COMMADD",
                   "ind_info_EDUEXPERIENCE",
                   "ind_info_FAMILYADD",
                   "ind_info_FAMILYSTATUS",
                   "ind_info_FULLNAME",
                   "ind_info_HEADSHIP",
                   "ind_info_ISCHILDREN",
                   "ind_info_MARRIAGE",
                   "ind_info_MONTHRENT",
                   "ind_info_NATIONALITY",
                   "ind_info_NATIVEADD",
                   "ind_info_PAYDAYDATE",
                   "ind_info_PAYDAYWAY",
                   "ind_info_SECURITYTYPE",
                   "ind_info_SELFMONTHINCOME",
                   "ind_info_SEX",
                   "ind_info_TOTALWORKAGE",
                   "ind_info_UNITNATURE",
                   "ind_info_WORKADD",
                   "ind_info_WORKBEGINDATE",
                   "debt_info_selfClaimDebt",
                   "ProjectID"
), with=F]

dabaiData[, ProjectID:=as.character(ProjectID)]
dabai_target[, projectid:=as.character(projectid)]
dabaiData <- merge(dabaiData, dabai_target, by.x="ProjectID", by.y="projectid")
table(dabaiData$flgDPD)

# 发现了有50个存量数据
# setdiff(dabai_target$projectid, dabaiData$ProjectID)
# 
# projectIDMapping[ProjectID==61231,]
# business_apply[business_apply_JIMUID==113137]

# k <- projectIDMapping[ProjectID %in% setdiff(dabai_target$projectid, dabaiData$ProjectID),]
# ok <- dabaiPostLoan[, c("projectid","loan_date"), with=F]
# ok <- ok[!duplicated(loan_date),]
# k[, ProjectID:=as.character(ProjectID)]
# ok[, projectid:=as.character(projectid)]
# nok <- merge(k, ok, by.x="ProjectID", by.y="projectid", all.x=T)



dabaiFeaturesAnalysis <- featureAnalysis(dabaiData, exclude="flgDPD")

# 去掉一些在初始变量分析是missing rate不高,但是在样本中全部缺失的变量
missingAfterJoin <- unique(dabaiFeaturesAnalysis[dabaiFeaturesAnalysis$MissPctg==1, ]$VarName)

dabaiData[, c(missingAfterJoin):=NULL]
dabaiFeaturesAnalysis2 <- featureAnalysis(dabaiData, exclude=c("flgDPD","ProjectID"))

write.csv(dabaiFeaturesAnalysis2, paste0(boxdata, "dabaiAnalysis.csv"))

dabaiData[, business_apply_TERMMONTH:=NULL]




#################################################
dabaiData