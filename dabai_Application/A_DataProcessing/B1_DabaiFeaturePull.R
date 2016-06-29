audit_lnproduct <- dbGetQuery(anshuoCon, "select * from cmss.AUDIT_LNPRODUCT")
business_apply <- dbGetQuery(anshuoCon, "select * from cmss.BUSINESS_APPLY")
customer_contact <- dbGetQuery(anshuoCon, "select * from cmss.CUSTOMER_CONTACT")
customer_credit <- dbGetQuery(anshuoCon, "select * from cmss.CUSTOMER_CREDIT")
customer_info <- dbGetQuery(anshuoCon, "select * from cmss.CUSTOMER_INFO")
customer_relative <- dbGetQuery(anshuoCon, "select * from cmss.CUSTOMER_RELATIVE")
debt_info <- dbGetQuery(anshuoCon, "select * from cmss.DEBT_INFO")
ind_info <- dbGetQuery(anshuoCon, "select * from cmss.IND_INFO")


audit_lnproduct <- readRDS(paste0(boxdata, "Dabai/audit_lnproduct.RDS"))
business_apply <- readRDS(paste0(boxdata, "Dabai/business_apply.RDS"))
customer_contact <- readRDS(paste0(boxdata, "Dabai/customer_contact.RDS"))
customer_credit <- readRDS(paste0(boxdata, "Dabai/customer_credit.RDS"))
customer_info <- readRDS(paste0(boxdata, "Dabai/customer_info.RDS"))
customer_relative <- readRDS(paste0(boxdata, "Dabai/customer_relative.RDS"))
debt_info <- readRDS(paste0(boxdata, "Dabai/debt_info.RDS"))
ind_info <- readRDS(paste0(boxdata, "Dabai/ind_info.RDS"))

# serialNo
audit_lnproduct <- data.table(audit_lnproduct)

# serialNo
business_apply <- data.table(business_apply)
business_apply <- business_apply[BUSINESSTYPE == 1007000402, ]

# customerID, relationship
customer_contact <- data.table(customer_contact)

# serialNo
customer_credit <- data.table(customer_credit)

# customerID
customer_info <- data.table(customer_info)

# customerID, relativeID
customer_relative <- data.table(customer_relative)

# debtID
debt_info <- data.table(debt_info)

# customerID
ind_info <- data.table(ind_info)


#############################################################################
names(audit_lnproduct) <- paste0("audit_lnproduct_", names(audit_lnproduct))
names(business_apply) <- paste0("business_apply_", names(business_apply))
names(customer_contact) <- paste0("customer_contact_", names(customer_contact))
names(customer_credit) <- paste0("customer_credit_", names(customer_credit))
names(customer_info) <- paste0("customer_info_", names(customer_info))
names(customer_relative) <- paste0("customer_relative_", names(customer_relative))
names(debt_info) <- paste0("debt_info_", names(debt_info))
names(ind_info) <- paste0("ind_info_", names(ind_info))

ind_info[ind_info_COMMADD=='@@@mn7FTeCegonk03vwBpsh8DJPs6kI/6dYDvtwA76a/9g=@@@/dYtEKAa5GrlZhmBx2H9Dg==@@@mPAe3Ho2jH1/D6zvGhFPLw==@@@txnbrDwNAMdLaGr/CsvXoognrW/Na5NouxqJgm1316I+XawnYAbj/nhXPzvwbghI', ind_info_COMMADD:=1]
ind_info[ind_info_COMMADD=='@@@oWAGotW/TvafcEe557Mhd2mKevkWAOqYB/htU9fPNmw=@@@Wzdj1Q0KBAA9B9FqY2UB9A==@@@vEVLvUN0JzYj8a1TV446gw==@@@Yw5h0FoW0NwzOsKbMxj6Ffs0or9tdpvC9EYu6cnvLho=', ind_info_COMMADD:=2]
ind_info[ind_info_COMMADD=='@@@oWAGotW/TvafcEe557Mhd2mKevkWAOqYB/htU9fPNmw=@@@Wzdj1Q0KBAA9B9FqY2UB9A==@@@vEVLvUN0JzYj8a1TV446gw==@@@YFXufehE+EXCq2xVsfbwqN9gDNkqxGs1Y8Jg01iJcGo=', ind_info_COMMADD:=3]

ind_info[ind_info_COMMADD=='@@@oWAGotW/TvafcEe557Mhd2mKevkWAOqYB/htU9fPNmw=@@@Wzdj1Q0KBAA9B9FqY2UB9A==@@@vEVLvUN0JzYj8a1TV446gw==@@@MXyor4PsthbbocA1+amaW9OVgX85H/4WizNQ+wlkJDE=', ind_info_COMMADD:=1]
ind_info[ind_info_COMMADD=='@@@mn7FTeCegonk03vwBpsh8DJPs6kI/6dYDvtwA76a/9g=@@@/dYtEKAa5GrlZhmBx2H9Dg==@@@mPAe3Ho2jH1/D6zvGhFPLw==@@@ShYT6JrHhSqZWo2I7nb81wnvfcwRzO3HOrojyy0qOOIMNe/3bkoCiBvejuqOLmdc\r\n', ind_info_COMMADD:=2]
ind_info[ind_info_COMMADD=='@@@mn7FTeCegonk03vwBpsh8DJPs6kI/6dYDvtwA76a/9g=@@@/dYtEKAa5GrlZhmBx2H9Dg==@@@mPAe3Ho2jH1/D6zvGhFPLw==@@@txnbrDwNAMdLaGr/CsvXoognrW/Na5NouxqJgm1316I+XawnYAbj/nhXPzvwbghI\r\n', ind_info_COMMADD:=1]
ind_info[ind_info_COMMADD=='01', ind_info_COMMADD:=1]

masterData <- merge(business_apply, audit_lnproduct, by.x="business_apply_SERIALNO", by.y = "audit_lnproduct_OBJECTNO", all.x=T)
masterData <- merge(masterData, customer_credit, by.x="business_apply_SERIALNO", by.y = "customer_credit_RELATIVECERTID2", all.x=T)


masterCust <- merge(customer_info, ind_info, by.x="customer_info_CUSTOMERID", by.y="ind_info_CUSTOMERID", all.x=T)



#############################################################################
# masterData_Analysis <- featureAnalysis(masterData, exclude=c("business_apply_SERIALNO","business_apply_RELATIVESERIALNO",
#                                                              "business_apply_CUSTOMERID","business_apply_CUSTOMERNAME",
#                                                              "business_apply_OCCURDATE","business_apply_BUSINESSSUBTYPE"))
# write.csv(masterData_Analysis, paste0(boxdata, "masterData_analysis.csv"))
# 
# masterCust_Analysis <- featureAnalysis(masaterCust, exclude=c("customer_info_CUSTOMERID","customer_info_CUSTOMERNAME","customer_info_CERTID",
#                                                               "customer_info_CUSTOMERPASSWORD","customer_info_INPUTORGID","customer_info_INPUTUSERID",
#                                                               "CERTTYPE"))
# write.csv(masterCust_Analysis, paste0(boxdata, "masterCust_analysis.csv"))
# 
# customerRelative_Analysis <- featureAnalysis(customer_relative, exclude=c("customer_relative_CUSTOMERID"))
# customerContact_Analysis <- featureAnalysis(customer_contact, exclude=c("customer_contact_CUSTOMERID"))
# debitInfo_Analysis <- featureAnalysis(debt_info, exclude=c("debt_info_DEBTID"))
# 
# write.csv(customerRelative_Analysis, paste0(boxdata, "customerRelative.csv"))
# write.csv(customerContact_Analysis, paste0(boxdata, "customerContact.csv"))
# write.csv(debitInfo_Analysis, paste0(boxdata, "debitInfo.csv"))
# 

#############################################################################
masterData <- merge(masterData, masterCust, by.x="business_apply_CUSTOMERID", by.y="customer_info_CUSTOMERID", all.x=T)

# 去掉87个保单贷=1的申请
# masterData <- masterData[is.na(business_apply_ISBDLOAN) | business_apply_ISBDLOAN!=1, ]

projectIDMapping <- martq("select ProjectID, FinancingID from rdb_data_opush.t_retail_loan_performance_2 group by 1,2")

masterDataTest <- merge(masterData, projectIDMapping, by.x="business_apply_JIMUID", by.y="FinancingID", all.x=T)


################################################################
# 弱变量
################################################################
# customer contact
# 1为工作朋友关系, 0为亲属关系
customer_contact[, relationship:=ifelse(customer_contact_RELATIONSHIP %in% c('07030','07020','07010','0040','0020'), 1, 0)]

customer_contact_pivot <- customer_contact[, .("customer_contact_KNOWLOAN"=Mode(customer_contact_KNOWLOAN, na.rm=T),
                                               "customer_contact_SEX"=Mode(customer_contact_SEX, na.rm=T),
                                               "customer_contact_RELATIONSHIP_CUSTOMIZED"=Mode(relationship, na.rm=T)
                                               )
                                           , by="customer_contact_CUSTOMERID"]

masterDataTest <- merge(masterDataTest, customer_contact_pivot, by.x="business_apply_CUSTOMERID", by.y="customer_contact_CUSTOMERID", all.x=T)

# customer relative
# customer_relative_pivot <- customer_relative[, .("customer_relative_FAMIADDSTATUS"=Mode(customer_relative_FAMIADDSTATUS, na.rm=T)
#                                               )
#                                               , by="customer_relative_CUSTOMERID"]
# masterDataTest <- merge(masterDataTest, customer_relative_pivot, by.x="business_apply_CUSTOMERID", by.y="customer_relative_CUSTOMERID", all.x=T)


# debt info, 做成"是否自报负债"变量
# 0为车房贷,1为消费贷,2为其他贷
# debt_info[, loantypeCUSTOMIZED:=ifelse(debt_info_LOANTYPE %in% c("0130","070","0140","080"), 0, ifelse(debt_info_LOANTYPE == "0150", 1, 2))]
# debt_info[, ensuretypeCUSTOMIZED:=ifelse(debt_info_ENSURETYPE %in% c("030","040"), "030", debt_info_ENSURETYPE)]
# debt_info[, guaranteestatusCUSTOMIZED:=ifelse(debt_info_GUARANTEESTATUS %in% c("02","090"), 0, 1)]
# 
# debt_info_pivot <- debt_info[, .("debt_info_majorDebtType"=Mode(debt_info_DEBTTYPE, na.rm=T),
#                                  "debt_info_majorLoanType"=Mode(loantypeCUSTOMIZED, na.rm=T),
#                                  "debt_info_ensuretypeCUSTOMIZED"=Mode(ensuretypeCUSTOMIZED, na.rm=T),
#                                  "debt_info_guaranteestatusCUSTOMIZED"=Mode(guaranteestatusCUSTOMIZED, na.rm=T)
#                                  )
#                              , by="debt_info_CUSTOMERID"]
# 
# 
# masterDataTest <- merge(masterDataTest, customer_relative_pivot, by.x="business_apply_CUSTOMERID", by.y="customer_relative_CUSTOMERID", all.x=T)
# 
# 
masterDataTest[, debt_info_selfClaimDebt:=ifelse(business_apply_CUSTOMERID %in% debt_info$debt_info_CUSTOMERID, 1, 0)]






################
masterDataTest
# customer_contact
# customer_relative
# debt_info

k<-anshuoq("SELECT  *  from  cmss.code_library ")


