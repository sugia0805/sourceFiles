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

banding(dabaiAnalysis_KS, "EVALUATESCORE", "ScoreBand")
test <- dabaiAnalysis_KS[!is.na(ScoreBand)]
testPivot <- test[, .("bad"=sum(flgDPD),
                      "total"=.N),
                  by="ScoreBand"]
testPivot[, badRate:=bad/total]






















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




