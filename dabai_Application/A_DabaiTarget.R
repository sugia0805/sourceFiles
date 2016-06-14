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

dabaiPostLoan[, EverDQ15:=ifelse(Cur_Overdue_Days==15, 1, 0)]
dabaiPostLoan[, EverDQ7:=ifelse(Cur_Overdue_Days==7, 1, 0)]
dabai_pivot <- dabaiPostLoan[, .("Ever15"=sum(EverDQ15),
                                 "Ever7" =sum(EverDQ7)), 
                             by="projectid"]

dabai_pivot[, flgDPD:=ifelse(Ever15>0, 1, ifelse(Ever7==0, 0, -1))]
