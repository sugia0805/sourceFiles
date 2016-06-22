dabaiMapping <- anshuoq("SELECT SERIALNO, CUSTOMERID, JIMUID, OCCURDATE, BUSINESSTYPE  from  cmss.business_apply 
                        where businesstype = 1007000402")

# Individual Scores
dabaiScore <- anshuoq("SELECT * from cmss.evaluate_data")
dabaiScore <- merge(dabaiScore, dabaiMapping, by.x="OBJECTNO", by.y="SERIALNO")
dabaiScoreTest <- merge(dabaiScore, projectIDMapping, by.x="JIMUID", by.y="FinancingID", all.x=T)

dabaiModelNo <- anshuoq("select *  from  cmss.evaluate_model where modelno = '034'")
dabaiModelNo[ITEMNO=='2.1', VALUECODE:="AvgCCOutstanding"]
dabaiModelNo[ITEMNO=='2.2', VALUECODE:="CCUtilization"]
dabaiModelNo[ITEMNO=='2.3', VALUECODE:="RecognizedIncome"]
dabaiModelNo[ITEMNO=='2.4', VALUECODE:="BadRate3Months"]
dabaiModelNo[ITEMNO=='3.4', VALUECODE:="hasHouse"]
dabaiModelNo[ITEMNO=='3.5', VALUECODE:="hasCard"]

dabaiScoreTest <- merge(dabaiScoreTest, dabaiModelNo, by="ITEMNO")

dabaiScoreWide <- dcast.data.table(dabaiScoreTest, 
                                   OBJECTNO + OCCURDATE + BUSINESSTYPE + ProjectID ~ ITEMNO, 
                                   fun.agg=max, 
                                   value.var = "ITEMVALUE")

dabai_target[, projectid:=as.character(projectid)]
dabaiScoreWide <- merge(dabaiScoreWide, dabai_target, by.x="ProjectID", by.y="projectid", all.x=T)



# total Scores
dabaiTotalScore <- anshuoq("select * from cmss.evaluate_record")
dabaiTotalScore <- merge(dabaiTotalScore, dabaiMapping, by.x="OBJECTNO", by.y="SERIALNO")
dabaiTotalScoreTest <- merge(dabaiTotalScore, projectIDMapping, by.x="JIMUID", by.y="FinancingID", all.x=T)


#join total score to individual score
dabaiScoreMaster <- merge(dabaiScoreWide, dabaiTotalScoreTest[, c("OBJECTNO","EVALUATESCORE"), with=F], 
                          by="OBJECTNO")
# 去除拆标的OBJECTNO
dabaiScoreMaster <- dabaiScoreMaster[!duplicated(OBJECTNO),]

# 去除6月之后的record
dabaiScoreMaster <- dabaiScoreMaster[as.Date(OCCURDATE)<as.Date('2016-06-01'),]

# 加上渠道来源，也许可以分渠道研究评分卡
# dabaiScoreMaster <- merge(dabaiScoreMaster, test, by="ProjectID", all.x=T)


##########################################################################################################
### 开始研究啦
##########################################################################################################
# KS by month
dabaiAnalysis_KS <- dabaiScoreMaster[flgDPD %in% c(0,1), ]
dabaiAnalysis_KS <- dabaiAnalysis_KS[!is.na(EVALUATESCORE),]

db1 <- dabaiAnalysis_KS[OCCURDATE<=as.Date('2015-9-30'),]
db2 <- dabaiAnalysis_KS[OCCURDATE>=as.Date('2015-10-1') & OCCURDATE<=as.Date('2015-10-31'),]
db3 <- dabaiAnalysis_KS[OCCURDATE>=as.Date('2015-11-1') & OCCURDATE<=as.Date('2015-11-30'),]
db4 <- dabaiAnalysis_KS[OCCURDATE>=as.Date('2015-12-1') & OCCURDATE<=as.Date('2015-12-31'),]
db5 <- dabaiAnalysis_KS[OCCURDATE>as.Date('2016-1-1') & OCCURDATE<=as.Date('2016-1-31'),]
db6 <- dabaiAnalysis_KS[OCCURDATE>as.Date('2016-2-1'),]

# KS
myKS(db1, "EVALUATESCORE", "ScoreBand", bandKS = seq(0,1,0.2))
myKS(db2, "EVALUATESCORE", "ScoreBand", bandKS = seq(0,1,0.2))
myKS(db3, "EVALUATESCORE", "ScoreBand", bandKS = seq(0,1,0.2))
myKS(db4, "EVALUATESCORE", "ScoreBand", bandKS = seq(0,1,0.2))
myKS(db5, "EVALUATESCORE", "ScoreBand", bandKS = seq(0,1,0.2))
myKS(db6, "EVALUATESCORE", "ScoreBand", bandKS = seq(0,1,0.2))
myKS(dabaiAnalysis_KS, "EVALUATESCORE", "ScoreBand", bandKS = seq(0,1,0.2))

# avgScore
mean(db1$EVALUATESCORE)
mean(db2$EVALUATESCORE)
mean(db3$EVALUATESCORE)
mean(db4$EVALUATESCORE)
mean(db5$EVALUATESCORE)
mean(db6$EVALUATESCORE)
mean(dabaiAnalysis_KS$EVALUATESCORE)



# PSI by month
dabaiAnalysis_PSI <- copy(dabaiScoreMaster)
setnames(dabaiAnalysis_PSI, 
         c("1.1","1.2","1.3","2.1","2.2","2.3","2.4","2.5","3.1","3.2","3.3","3.4","3.5","3.6"),
         c("age","sex","edu","AvgCCOutstanding",	"CCUtilization",	"RecognizedIncome",	"BadRate3Months",	"CreditTerm",	"MailAddress",	"WorkDuty",	"MaxOverDue",	"hasHouse",	"hasCard",	"MarriageStatus"
))


dbPSI1 <- dabaiAnalysis_PSI[OCCURDATE<=as.Date('2015-10-31'), ]
dbPSI1[, isCurrMon:=ifelse(OCCURDATE<=as.Date('2015-9-30'), 0, 1)]
PSIResult1 <- myPSI(dbPSI1[isCurrMon==0,], dbPSI1[isCurrMon==1,], "EVALUATESCORE")
PSIResult1$PSI

dbPSI2 <- dabaiAnalysis_PSI[OCCURDATE<=as.Date('2015-11-30'), ]
dbPSI2[, isCurrMon:=ifelse(OCCURDATE<=as.Date('2015-10-31'), 0, 1)]
PSIResult2 <- myPSI(dbPSI2[isCurrMon==0,], dbPSI2[isCurrMon==1,], "EVALUATESCORE")
PSIResult2$PSI

dbPSI3 <- dabaiAnalysis_PSI[OCCURDATE<=as.Date('2015-12-31'), ]
dbPSI3[, isCurrMon:=ifelse(OCCURDATE<=as.Date('2015-11-30'), 0, 1)]
PSIResult3 <- myPSI(dbPSI3[isCurrMon==0,], dbPSI3[isCurrMon==1,], "EVALUATESCORE")
PSIResult3$PSI

dbPSI4 <- dabaiAnalysis_PSI[OCCURDATE<=as.Date('2016-1-31'), ]
dbPSI4[, isCurrMon:=ifelse(OCCURDATE<=as.Date('2015-12-31'), 0, 1)]
PSIResult4 <- myPSI(dbPSI4[isCurrMon==0,], dbPSI4[isCurrMon==1,], "EVALUATESCORE")
PSIResult4$PSI

dbPSI5 <- dabaiAnalysis_PSI[OCCURDATE<=as.Date('2016-2-29'), ]
dbPSI5[, isCurrMon:=ifelse(OCCURDATE<=as.Date('2016-1-31'), 0, 1)]
PSIResult5 <- myPSI(dbPSI5[isCurrMon==0,], dbPSI5[isCurrMon==1,], "EVALUATESCORE")
PSIResult5$PSI

dbPSI6 <- dabaiAnalysis_PSI[OCCURDATE<=as.Date('2016-3-31'), ]
dbPSI6[, isCurrMon:=ifelse(OCCURDATE<=as.Date('2016-2-29'), 0, 1)]
PSIResult6 <- myPSI(dbPSI6[isCurrMon==0,], dbPSI6[isCurrMon==1,], "EVALUATESCORE")
PSIResult6$PSI

dbPSI7 <- dabaiAnalysis_PSI[OCCURDATE<=as.Date('2016-4-30'), ]
dbPSI7[, isCurrMon:=ifelse(OCCURDATE<=as.Date('2016-3-31'), 0, 1)]
PSIResult7 <- myPSI(dbPSI7[isCurrMon==0,], dbPSI7[isCurrMon==1,], "EVALUATESCORE")
PSIResult7$PSI

dbPSI8 <- dabaiAnalysis_PSI[OCCURDATE<=as.Date('2016-5-31'), ]
dbPSI8[, isCurrMon:=ifelse(OCCURDATE<=as.Date('2016-4-30'), 0, 1)]
PSIResult8 <- myPSI(dbPSI8[isCurrMon==0,], dbPSI8[isCurrMon==1,], "EVALUATESCORE")
PSIResult8$PSI





PSIResultVS9_1 <- myPSI(dbPSI1[isCurrMon==0,], dbPSI1[isCurrMon==1,], "EVALUATESCORE")
PSIResultVS9_1$PSI

PSIResultVS9_2 <- myPSI(dbPSI1[isCurrMon==0,], dbPSI2[isCurrMon==1,], "EVALUATESCORE")
PSIResultVS9_2$PSI

PSIResultVS9_3 <- myPSI(dbPSI1[isCurrMon==0,], dbPSI3[isCurrMon==1,], "EVALUATESCORE")
PSIResultVS9_3$PSI

PSIResultVS9_4 <- myPSI(dbPSI1[isCurrMon==0,], dbPSI4[isCurrMon==1,], "EVALUATESCORE")
PSIResultVS9_4$PSI

PSIResultVS9_5 <- myPSI(dbPSI1[isCurrMon==0,], dbPSI5[isCurrMon==1,], "EVALUATESCORE")
PSIResultVS9_5$PSI

PSIResultVS9_6 <- myPSI(dbPSI1[isCurrMon==0,], dbPSI6[isCurrMon==1,], "EVALUATESCORE")
PSIResultVS9_6$PSI

PSIResultVS9_7 <- myPSI(dbPSI1[isCurrMon==0,], dbPSI7[isCurrMon==1,], "EVALUATESCORE")
PSIResultVS9_7$PSI

PSIResultVS9_8 <- myPSI(dbPSI1[isCurrMon==0,], dbPSI8[isCurrMon==1,], "EVALUATESCORE")
PSIResultVS9_8$PSI





##########################################################################################################
### 验证哪些是大白系列产品
##########################################################################################################

test0 <- anshuoq("select distinct JIMUID, businesstype from cmss.business_apply")
test <- martq("select ProjectID, ChannelDetail from rdb_data_opush.t_retail_loan_performance_2 group by 1,2")

projectIDMapping <- martq("select ProjectID, FinancingID from rdb_data_opush.t_retail_loan_performance_2 group by 1,2")

test0 <- merge(test0, projectIDMapping, by.x="JIMUID", by.y="FinancingID", all.x=T)

test0 <- merge(test0, test, by="ProjectID")
test0 <- test0[!duplicated(ProjectID)]
# table(test0[BUSINESSTYPE==1000000640,]$ChannelDetail)
# table(test0[BUSINESSTYPE==1007000120,]$ChannelDetail)
# table(test0[BUSINESSTYPE==1007000180,]$ChannelDetail)
# table(test0[BUSINESSTYPE==1007000220,]$ChannelDetail)
# table(test0[BUSINESSTYPE==1007000230,]$ChannelDetail)
table(test0[BUSINESSTYPE==1007000402,]$ChannelDetail)
# table(test0[BUSINESSTYPE==1007000404,]$ChannelDetail)
# table(test0[BUSINESSTYPE==1007010220,]$ChannelDetail) #chanrong
# table(test0[BUSINESSTYPE==1007020220,]$ChannelDetail) #rong360
# table(test0[BUSINESSTYPE==1007090220,]$ChannelDetail) #chanrong
# table(test0[BUSINESSTYPE==2205000220,]$ChannelDetail) #rongxindai-fangcun
# table(test0[BUSINESSTYPE==2205000290,]$ChannelDetail)




