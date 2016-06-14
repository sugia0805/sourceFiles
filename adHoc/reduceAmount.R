appStatus <- martq("select
a.projectid,
case when (a.applyStatus=1 or a.applyStatus=3 or a.applyStatus=8 or a.applyStatus=10) then 1 else 0 end as passed,
case when a.projectId>0 then 1 else 0 end as disbursed,
a.applicationAmount,
a.approvedAmt
      from rdb_data_dm.dm_fact_rm_application a
      ")
appStatus[, projectid:=as.character(projectid)]

disbursed <- martq("select projectid, min(drawdownamount) as ddAmount
                   from rdb_data_opush.t_retail_loan_performance_2
                   group by 1")

totalCust <- merge(appStatus, disbursed, by="projectid", all.x=T)
totalCust <- totalCust[passed==1,]

reduceAmt <- totalCust[, 
                       .("takeup"=sum(disbursed),
                         "cnt"=.N),
                       by=c("applicationAmount","approvedAmt")]
reduceAmt[, takeupRate:=takeup/cnt]
write.csv(reduceAmt, paste0(boxdata, "reduceAmt.csv"))
