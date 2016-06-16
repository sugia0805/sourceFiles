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
business_apply <- business_apply[BUSINESSTYPE==1007000402, ]

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


masterData <- merge(business_apply, audit_lnproduct, by.x="business_apply_SERIALNO", by.y = "audit_lnproduct_SERIALNO", all.x=T)
masterData <- merge(masterData, customer_credit, by.x="business_apply_SERIALNO", by.y = "customer_credit_SERIALNO", all.x=T)


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

# 去掉87+1808个保单贷=1或isbdloan缺失的申请
masterData <- masterData[business_apply_ISBDLOAN==2, ]

projectIDMapping <- martq("select ProjectID, FinancingID from rdb_data_opush.t_retail_loan_performance_2 group by 1,2")

masterDataTest <- merge(masterData, projectIDMapping, by.x="business_apply_JIMUID", by.y="FinancingID", all.x=T)

################
masterDataTest
customer_contact
customer_relative
debt_info

