dabai201601 <- read.csv(paste0(boxdata, "Dabai/post_approval/Dabai201601.csv"))
dabai201602 <- read.csv(paste0(boxdata, "Dabai/post_approval/Dabai201602.csv"))
dabai201603 <- read.csv(paste0(boxdata, "Dabai/post_approval/Dabai201603.csv"))
dabai201604 <- read.csv(paste0(boxdata, "Dabai/post_approval/Dabai201604.csv"))
dabai201605 <- read.csv(paste0(boxdata, "Dabai/post_approval/Dabai201605.csv"))
dabai201511 <- read.csv(paste0(boxdata, "Dabai/post_approval/Dabai20151112.csv"))
dabai201508 <- read.csv(paste0(boxdata, "Dabai/post_approval/Dabai20150810.csv"))



dabaiPostLoan <- rbind(dabai201601, dabai201602, dabai201603, dabai201604, dabai201605, 
                       dabai201508, dabai201511)
dabaiPostLoan <- data.table(dabaiPostLoan)
dabaiPostLoan[, daysFromDD:=as.Date(statc_dt)-as.Date(loan_date)]
dabaiPostLoan <- dabaiPostLoan[!is.na(daysFromDD),]

length(unique(dabaiPostLoan$projectid))

LostContactMapping <- read.csv(paste0(boxjimu, "Models/1606Dabai/Data/LostContactMapping.csv"))
LostContactMapping <- data.table(LostContactMapping)


######################################################################################################################
in180days <- dabaiPostLoan[, .(maxDaysFromDD = max(daysFromDD)), by="projectid"]
hasEnoughPerformance <- in180days[maxDaysFromDD>=180, ]

dabaiPostLoanGT180 <- dabaiPostLoan[projectid %in% hasEnoughPerformance$projectid, ]
dabaiPostLoanGT180[, EverDPD15in180:=ifelse(Cur_Overdue_Days==15 & daysFromDD<=180, 1, 0)]
dabaiPostLoanGT180[, EverDPD07:=ifelse(Cur_Overdue_Days==7, 1, 0)]

dabai_pivotGT180 <- dabaiPostLoanGT180[, .("Ever15in180"=sum(EverDPD15in180),
                                           "Ever7" =sum(EverDPD07)), 
                                       by="projectid"]
dabai_pivotGT180[, flgDPD:=ifelse(Ever15in180>0, 1, ifelse(Ever7==0, 0, -1))]

table(dabai_pivotGT180$flgDPD)
# -1   0   1 
# 82 789 100 

dabai_pivotGT180 <- merge(dabai_pivotGT180, LostContactMapping[, c("ProjectID","jimuid"), with=F], by.x="projectid", by.y="ProjectID", all.x = T)
dabai_pivotGT180[flgDPD==0 & !is.na(jimuid), flgDPD:=-1]
table(dabai_pivotGT180$flgDPD)
# -1   0   1 
# 85 786 100 




dabaiPostLoanLT180 <- dabaiPostLoan[!(projectid %in% hasEnoughPerformance$projectid), ]
dabaiPostLoanLT180 <- merge(dabaiPostLoanLT180, in180days, by="projectid", all.x=T)
dabaiPostLoanLT180[, EverDPD15in180:=ifelse(Cur_Overdue_Days==15, 1, 0)]
dabaiPostLoanLT180[, EverDPD03:=ifelse(Cur_Overdue_Days==3, 1, 0)]


dabai_pivotLT180 <- dabaiPostLoanLT180[, .("Ever15in180"=sum(EverDPD15in180),
                                         "EverDPD3" =sum(EverDPD03),
                                         "MaxPerf" =max(maxDaysFromDD)), 
                                     by="projectid"]

dabai_pivotLT180[, flgDPD:=ifelse(Ever15in180>0, 1, ifelse(EverDPD3==0 & MaxPerf >= 95, 0, -1))]

table(dabai_pivotLT180$flgDPD)
# -1    0    1 
# 1981 1046   82 

dabai_pivotLT180 <- merge(dabai_pivotLT180, LostContactMapping[, c("ProjectID","jimuid"), with=F], by.x="projectid", by.y="ProjectID", all.x = T)
dabai_pivotLT180[flgDPD==0 & !is.na(jimuid), flgDPD:=-1]
table(dabai_pivotLT180$flgDPD)
# -1    0    1 
# 1981 1046   82 


dabai_target <- rbind(dabai_pivotLT180[, c("projectid","flgDPD"), with=F], 
                      dabai_pivotGT180[, c("projectid","flgDPD"), with=F])



######################################################################################
dabai_target