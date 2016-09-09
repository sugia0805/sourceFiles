
source("../sourceFiles//sourceFiles/woeFunctions.R")
source("../sourceFiles//sourceFiles/ExcelFinance.R")


monthDiff <- Vectorize(function(startMth, endMth){
  return((as.numeric(substr(endMth,1,4))-as.numeric(substr(startMth,1,4)))*12+(as.numeric(substr(endMth,5,6))-as.numeric(substr(startMth,5,6))))
})

Mode <- function(x, na.rm=F) {
  if(na.rm){
    x <- x[!is.na(x)]
  }
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

myRAM <- Vectorize(function(TicketSize, UpfrontFee=0.02, Tenor, feeRate, COF, CORate, PrepayRate){
  NormalPaidPrincipal = 0
  
  # Revenue & C/O, Prepay at before 1st payment
  Revenue = UpfrontFee + (PrepayRate/Tenor)*(1-NormalPaidPrincipal)*0.04
  PerformingDDPrincipal = 1 - CORate*log(2)/log(Tenor+1) - PrepayRate/Tenor
  
  # Revenue & C/O Prepay loop thru life cycle
  for(i in 1:Tenor){
    if(i<=2){
      Revenue = Revenue + PerformingDDPrincipal*feeRate + (PrepayRate/Tenor)*(1-NormalPaidPrincipal)*0.04
    }else{
      Revenue = Revenue + PerformingDDPrincipal*feeRate + (PrepayRate/Tenor)*(1-NormalPaidPrincipal)*0.03
    }
    
    PerformingDDPrincipal = PerformingDDPrincipal - CORate*(log(i+1)-log(i))/log(Tenor+1) - PrepayRate/Tenor - PerformingDDPrincipal/Tenor
    NormalPaidPrincipal = NormalPaidPrincipal + PPMT(COF/12, i, Tenor, -1)
  }
  
  GrossMargin = Revenue - CORate
  return(GrossMargin*TicketSize)
  
})

cutoffSimulator <- function(param_scoredOOS, param_pricingTable, cutoff = -Inf, bands=7, flgDPD_DPD90_FlowRate=0.5){
  
  param_RAM_scoredOOS <- param_scoredOOS[NewScore>=cutoff, ]
  param_Declined_scoredOOS <- param_scoredOOS[NewScore<cutoff, ]
  param_LineAssignmentTable <- param_pricingTable[, .("LineAssignment"=min(LineAssignment), "MinTenor"=min(Tenor), "MaxTenor"=max(Tenor)), 
                                                  by="RiskBand"]

  portfolioDPD90 <- sum(as.numeric(as.character(param_RAM_scoredOOS$flgDPD)))/nrow(param_RAM_scoredOOS)*flgDPD_DPD90_FlowRate

  # 给risk score并成band
  if(is.numeric(bands)){
    banding(param_RAM_scoredOOS, "NewScore", "scoreBand", nBands = bands)
  }else if(is.vector(bands)){
    banding(param_RAM_scoredOOS, "NewScore", "scoreBand", vec = bands)
  }else{
    return("bands参数只接受vector和numeric变量!")
  }
  param_RAM_scoredOOS <- rbind(param_RAM_scoredOOS, param_Declined_scoredOOS[, scoreBand:=0])
  
  # 给每个account按照所在risk band加上approved的line assignment, 同时给出tenor的上下限. 给出最终批复的金额和期限
  param_RAM_scoredOOS <- merge(param_RAM_scoredOOS, param_LineAssignmentTable, by.x="scoreBand", by.y="RiskBand", all.x=T)
  
  
  # 判定最终放款的期限和tenor
  param_RAM_scoredOOS[, DDAmount:=ifelse(scoreBand==0, 0, 
                                         ifelse(ApplicationAmount<LineAssignment, ApplicationAmount, LineAssignment))]
  param_RAM_scoredOOS[, DDTenor:=ifelse(scoreBand==0, 0, 
                                        ifelse(ApplicationTenor<MinTenor, MinTenor, 
                                               ifelse(ApplicationTenor>MaxTenor, MaxTenor, ApplicationTenor)))]
  # param_RAM_scoredOOS[, c("ApplicationAmount", "ApplicationTenor","MinTenor","MaxTenor"):=NULL]
  
  
  # 将risk band做group by, 做成risk segments
  param_RAM_bandedOOS_DPD <- param_RAM_scoredOOS[, .("maxScore"=max(NewScore), 
                                                     "minScore"=min(NewScore),
                                                     "totalCnt"=.N,
                                                     "DPD"=sum(as.numeric(as.character(flgDPD)))
  ),
  by=c("scoreBand")]
  param_RAM_bandedOOS_DPD[, DPD90_Rate:=(DPD/totalCnt) * flgDPD_DPD90_FlowRate]
  
  # 将tenor, scoreband做group by, 做成npv segments
  param_RAM_bandedOOS_NPV <- param_RAM_scoredOOS[, .("avgDDAmount"=mean(DDAmount), "totalCnt"=.N), by=c("scoreBand","DDTenor")]
  
  # 将risk segment的DPD数据加入NPV segment
  param_RAM_bandedOOS <- merge(param_RAM_bandedOOS_NPV, param_RAM_bandedOOS_DPD[, -c("totalCnt","DPD"), with=F], by.x="scoreBand", by.y="scoreBand")
  
  # 给NPV大表的各个risk band的每个tenor加上五档pricing
  param_RAM_bandedOOS <- merge(param_RAM_bandedOOS, param_pricingTable, by.x=c("scoreBand","DDTenor"), by.y=c("RiskBand","Tenor"), all.x = T)
  
  # 算RAM
  param_RAM_bandedOOS[scoreBand>0, RAMperAcct:=myRAM(TicketSize = LineAssignment, UpfrontFee = 0.02, Tenor = DDTenor, feeRate = FeeRate, 
                                                     COF = COF, CORate = DPD90_Rate, PrepayRate = PrepayRate)]
  param_RAM_bandedOOS[scoreBand>0, waTotalCnt:=totalCnt*FeeBandPctg]
  param_RAM_bandedOOS[scoreBand>0, waRAM:=waTotalCnt*RAMperAcct]
  
  totalRAM = sum(param_RAM_bandedOOS$waRAM, na.rm=T)
  waRAM = totalRAM/sum(param_RAM_bandedOOS$waTotalCnt, na.rm=T)
  
  return(list("RAMTable"=param_RAM_bandedOOS, "RAMperAcct"=waRAM, "RAMTotal"=totalRAM, "PortfolioDPD90"=portfolioDPD90))
  
}

######################################################################################################## Parse 
# Parse strings, default pattern: targetText:Value(<>) 
# 先用str_extract取targetText之后的一段string，然后用strStart, strEnd截targetText的值
strParseV <- Vectorize(function(obj, targetText, regexpStopping=".*<", strStart=":", strEnd="\\(") {
  sub<-str_extract(obj, paste0(targetText, regexpStopping))
  positionStart<-str_locate(sub, strStart)[1]+nchar(strStart)
  positionEnd<-str_locate(sub, strEnd)[1]-1
  return(substr(sub,positionStart,positionEnd))
})

strParse <- function(obj, targetText, regexpStopping=".*<", strStart=":", strEnd="\\(") {
  sub<-str_extract(obj, paste0(targetText, regexpStopping))
  positionStart<-str_locate(sub, strStart)[1]+nchar(strStart)
  positionEnd<-str_locate(sub, strEnd)[1]-1
  return(substr(sub,positionStart,positionEnd))
}


###################################################################################################### grouping 

grouping <- function(dataVector, groups=10){
  total <- length(dataVector)
  rankVector <- rank(as.numeric(dataVector), ties.method = "min")
  groupVector <- ceiling((rankVector * groups)/(total+1))
  midFrame <- data.table(cbind(dataVector, groupVector))
  groupMax <- midFrame[, 
                       .("groupMax"=max(dataVector)),
                       by="groupVector"]
  midFrame <- merge(midFrame, groupMax, by="groupVector")
  midFrame <- midFrame[!duplicated(dataVector),]
  return(midFrame[, c("dataVector", "groupMax"), with=F])
}


# 造个scoreband放在新的column里
banding <- function(trainDT, scoreColumn, bandName, flgDPDColumn = NULL, nBands=10, vec=NULL){
  if(is.null(vec)){
    trainDT[, bandName:=as.integer(findInterval(get(scoreColumn), quantile(get(scoreColumn), probs=seq(0, 1, 1/nBands), na.rm=T), rightmost.closed = T)), with=F]
  }else{
    trainDT[, bandName:=as.integer(findInterval(get(scoreColumn), vec=vec, rightmost.closed = T)), with=F]
  }
  scoreTable1 <- trainDT[, .("maxScore"=max(get(scoreColumn))
                       ,"minScore"=min(get(scoreColumn))
                       ,"totalCust"=.N
                       ),
                   by=c(bandName)]
  if(!is.null(flgDPDColumn)){
    scoreTable2 <- trainDT[, .("badCust"=sum(as.numeric(as.character(get(flgDPDColumn))))),
                           by=c(bandName)]
    scoreTable <- merge(scoreTable1, scoreTable2, by=bandName)
    scoreTable[, badRate:=badCust/totalCust]
    return(scoreTable)
  }
  

  return(scoreTable1)
}

myPSI <- function(DTOld, DTNew, binColName, bandPSI=seq(0,1,0.1)){
  DTOld <- DTOld[!is.na(get(binColName)),]
  bandingOldPopulation <- banding(DTOld, binColName, "binPSI", bands=bandPSI)
  
  comparisonDF <- copy(bandingOldPopulation)
  
  # 把bandingOldPopulation做成Woe assign里面的binningDF格式方便套用
  bandingOldPopulation[, varName:=binColName]
  bandingOldPopulation[, type:="continuous"]
  setnames(bandingOldPopulation, c("maxScore", "binPSI"), c("maxValue", "WoE"))
  woeTransName <- paste0("w_", binColName)
  
  binnedDTNew <- woeAssign(DTNew, binColName, bandingOldPopulation)
  setnames(binnedDTNew, woeTransName, "binPSI")
  DTNew_pivot <- binnedDTNew[!is.na(binPSI), .("totalCust_New" = .N), by="binPSI"]
  
  # 把新旧population合并起来
  comparisonDF <- merge(comparisonDF[, c("binPSI","maxScore","minScore","totalCust"), with=F], DTNew_pivot, by="binPSI")
  
  # 开始算PSI!
  comparisonDF[, pctgOld:=totalCust/sum(totalCust)]
  comparisonDF[, pctgNew:=totalCust_New/sum(totalCust_New)]
  comparisonDF[, NewMinusOld:=pctgNew-pctgOld]
  comparisonDF[, LogNewDivideOld:=log(pctgNew/pctgOld)]
  comparisonDF[, kPSI:=NewMinusOld * LogNewDivideOld]
  
  PSI <- sum(comparisonDF$kPSI)
  
  result <- list(PSITable=comparisonDF, PSI=PSI)
}

# 拉平data.table 
# test<-dcast.data.table(DT, mainID(用来left join的那个) ~ 变量名, fun.agg=max, value.var = "变量值")
# tvardictWide <- dcast.data.table(tvardict, financingprojectid + createtime ~ var_code, fun.agg=min, value.var = "var_val")


####################################################################################################行/列分析
# 把features的min/max/percentile/Nmiss etc.做成一个data.frame
featureAnalysis <- function(DT, exclude, cateConv=F) {
  result <- data.frame(VarName=character(),
                   sType=character(),
                   N=integer(),
                   Nmiss=integer(),
                   MissPctg=integer(),
                   Mean=double(),
                   Stdev=double(),
                   Min=double(),
                   Median=double(),
                   Max=double(),
                   P01=double(),
                   P05=double(),
                   P10=double(),
                   P25=double(),
                   P75=double(),
                   P90=double(),
                   P95=double(),
                   P99=double(),
                   stringsAsFactors=FALSE)


  for(varName in names(DT)){
    if(!(varName %in% exclude)){
      if(!is.factor(unlist(DT[, varName, with=F])) & 
         !is.double(unlist(DT[, varName, with=F])) &
         length(table(as.numeric(unlist(DT[, varName, with=F]))))>0){
        DT[, varName:=as.numeric(get(varName)), with=F]
      }
      print(varName)
      if(cateConv & nrow(as.data.table(table(DT[, varName, with=F],useNA="ifany")))<=10){
        #如果变量的所有可选值不大于10，可以选择转成categorical，randomForest不用转
        DT[, varName:=as.factor(get(varName)), with=F]
      }
      oneVar<-unlist(DT[, varName, with=F])
      if(typeof(oneVar) %in% c("double","integer") & !is.factor(oneVar)){
        newRow<-data.frame(varName, 
                           typeof(oneVar),
                           length(oneVar), 
                           length(oneVar[is.na(oneVar)]),
                           length(oneVar[is.na(oneVar)])/length(oneVar),
                           mean(oneVar, na.rm = T), 
                           sd(oneVar, na.rm = T), 
                           min(oneVar, na.rm = T), 
                           median(oneVar, na.rm = T), 
                           max(oneVar, na.rm = T), 
                           quantile(oneVar, c(.01), na.rm=T),
                           quantile(oneVar, c(.05), na.rm=T),
                           quantile(oneVar, c(.10), na.rm=T),
                           quantile(oneVar, c(.25), na.rm=T),
                           quantile(oneVar, c(.75), na.rm=T),
                           quantile(oneVar, c(.90), na.rm=T),
                           quantile(oneVar, c(.95), na.rm=T),
                           quantile(oneVar, c(.99), na.rm=T)
        )
      }else{
        newRow<-data.frame(varName, 
                           typeof(oneVar),
                           length(oneVar), 
                           length(oneVar[is.na(oneVar)]),
                           length(oneVar[is.na(oneVar)])/length(oneVar),
                           NA, 
                           NA, 
                           NA, 
                           NA, 
                           NA, 
                           NA, 
                           NA, 
                           NA, 
                           NA, 
                           NA, 
                           NA, 
                           NA, 
                           NA 
        )
      }
      names(newRow)<-names(result)
      result<-rbind(result, newRow)
    }
  }
  names(result)<-c("VarName","sType","N","Nmiss","MissPctg","Mean","Stdev",
                   "Min","Median","Max","P01","P05","P10","P25","P75","P90","P95","P99")
  return(result)
}

sampleMissingAnalysis <- function(DT, IDRow) {
  result<-data.frame()
  for(i in 1:nrow(DT)){
    result<-rbind(result, c(unlist(DT[i, IDRow, with=F]), sum(is.na(DT[i, ]))))
  }
  names(result)<-c("rowNum","missingCnt")
  return(result)
}


######################################################################################################## Impute
# 判断是否alpha-numeric，不是的话就是中文字符
isAlphaNum <- Vectorize(function(str){
  result<-ifelse(length(charToRaw(substr(str, 1, 1)))==1, 1, 0)
  return(result)
})

# 把NA什么的替换成0或者你想要的
naBlankInfer <- function(DT, column, inferFrom=c(NA, 'None', ''), inferTo=0){
  DT[get(column) %in% inferFrom, column:=as.character(inferTo), with=F]
}

# fullImpute=F就把所有的NA都变成默认值
ggImpute <- function(DT, fullImpute=TRUE, removeMassiveMissing=TRUE){
  tooMuchMissingList <- c()
  imputeValueTable <- data.frame(col=as.character("name"), imputeValue=as.character("Value"), stringsAsFactors = F)
  for(col in names(DT)){
    imputeColVec<-unlist(DT[, col, with=F])
    if(sum(is.na(imputeColVec))/sum(!is.na(imputeColVec))>=2/3){
      tooMuchMissingList<-append(tooMuchMissingList, col)
      if(removeMassiveMissing)
        next
    }
    if(fullImpute){
      if(is.numeric(imputeColVec)){
        if(length(unique(imputeColVec))>10){
          imputeValue<-median(imputeColVec, na.rm=T)
        }else if( sum(imputeColVec==Mode(imputeColVec), na.rm=T) < 0.6*sum(!is.na(imputeColVec)) ){
          imputeValue<-mean(imputeColVec, na.rm=T)
        }else{
          imputeValue<-Mode(imputeColVec)
        }
        naBlankInfer(DT, col, inferTo = imputeValue)
        imputeValueTable<-rbind(imputeValueTable, data.frame(col, imputeValue))
        next
      }else if(is.factor(imputeColVec)){
        if( sum(imputeColVec==Mode(imputeColVec), na.rm=T) < 0.6*sum(!is.na(imputeColVec)) ){
          imputeValue<-"999"
        }else{
          imputeValue<-Mode(imputeColVec)
        }
        naBlankInfer(DT, col, inferTo = imputeValue)
        imputeValueTable<-rbind(imputeValueTable, data.frame(col, imputeValue))
        next
      }else{
        imputeValue<-Mode(imputeColVec)
        naBlankInfer(DT, col, inferTo = imputeValue)
        imputeValueTable<-rbind(imputeValueTable, data.frame(col, imputeValue))
        next
      }
    }else{
      naBlankInfer(DT, col, inferTo = MISSING_DEFAULT)
    }
  }
  return(list(removeList=tooMuchMissingList, imputeValues=imputeValueTable))
}
  
typeConverter <- function(DT, convList, typeTo){
  for(varName in convList){
    if(typeTo=="character"){
      DT[, varName:=as.character(get(varName)), with=F]
    }else if(typeTo=="factor"){
      DT[, varName:=as.factor(get(varName)), with=F]
    }else if(typeTo=="numeric"){
      DT[, varName:=as.numeric(as.character(get(varName))), with=F]
    }
  }
}  





