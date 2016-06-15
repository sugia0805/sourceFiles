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

in90days <- dabaiPostLoan[, .(maxDaysFromDD = max(daysFromDD)), by="projectid"]
hasEnoughPerformance <- in90days[maxDaysFromDD>=90, ]

dabaiPostLoanGT90 <- dabaiPostLoan[projectid %in% hasEnoughPerformance$projectid, ]
dabaiPostLoanGT90[, EverDPD15in90:=ifelse(Cur_Overdue_Days==15 & daysFromDD<=90, 1, 0)]
dabaiPostLoanGT90[, EverDPD07:=ifelse(Cur_Overdue_Days==7, 1, 0)]

dabaiPostLoanLT90 <- dabaiPostLoan[!(projectid %in% hasEnoughPerformance$projectid), ]
dabaiPostLoanLT90[, EverDPD15in90:=ifelse(Cur_Overdue_Days==15, 1, 0)]
dabaiPostLoanLT90[, EverDPD07:=-1]


dabai_pivotGT90 <- dabaiPostLoanGT90[, .("Ever15in90"=sum(EverDPD15in90),
                                 "Ever7" =sum(EverDPD07)), 
                             by="projectid"]

dabai_pivotLT90 <- dabaiPostLoanLT90[, .("Ever15in90"=sum(EverDPD15in90),
                                         "Ever7" =sum(EverDPD07)), 
                                     by="projectid"]

dabai_pivotGT90[, flgDPD:=ifelse(Ever15in90>0, 1, ifelse(Ever7==0, 0, -1))]
dabai_pivotLT90[, flgDPD:=ifelse(Ever15in90>0, 1, ifelse(Ever7==0, 0, -1))]

dabai_target <- rbind(dabai_pivotLT90, dabai_pivotGT90)

dabai_target <- dabai_target[, c("projectid","flgDPD"), with=F]



######################################################################################
dabai_target