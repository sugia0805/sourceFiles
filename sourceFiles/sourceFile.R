MISSING_DEFAULT <- -99999
source("./sourceFiles/generalFunction.R")
boxjimu <- "E:/Seafiles/Jimu/"
boxdata <- "E:/Seafiles/Jimu/Data/"

# general
library(RMySQL)
library(ROracle)
library(data.table)
library(TTR)
# library(reshape2) # for melt and cast
library(pmml)
library(stringr)
library(pROC)
library(ROCR)
library(PRROC)
library(caret)
options(sqldf.driver="SQLite")
# rf
library(randomForest)
library(varSelRF)

# logistic & elastic net
library(glmnet)
library(Information)
library(smbinning)

# Model validation
library(hmeasure)
library(InformationValue)

# connect with SAS
# library(sas7bdat)
library(foreign)
# library(SASxport)



all_cons <- dbListConnections(MySQL())
for(con in all_cons)
  dbDisconnect(con)

# Jimu data queries
drv <- dbDriver("MySQL")
# dmAnalCon <- dbConnect(drv, user="dumiao_analysis", password="analysis4321",
#               dbname="dumiao_analysis", host="172.19.1.221", port=9900, encoding = getOption("utf8"))
ruleEngCon <- dbConnect(drv, user="dan.xu", password="bTH68b2MjQu8JZA",
                   dbname="rule_engineer", host="172.16.2.28", port=3311, encoding = getOption("utf8"))
dmYewuCon <- dbConnect(drv, user="dan.xu", password="bTH68b2MjQu8JZA",
                     dbname="dumiao", host="172.16.2.28", port=3311, encoding = getOption("utf8"))

dataMartCon <- dbConnect(drv, user="dan.xu", password="bTH68b2MjQu8JZA",
                   host="172.16.3.55", port=3324, encoding = getOption("utf8"))

jdzCon <- dbConnect(drv, user="dan.xu", password="bTH68b2MjQu8JZA",
                         host="172.16.4.42", port=3310, encoding = getOption("utf8"))

drvORA <- dbDriver("Oracle")
ORAhost <- "172.16.3.18"
ORAport <- 1521
ORAsid <- "orcl_std"
connect.string <- paste(
  "(DESCRIPTION=",
  "(ADDRESS=(PROTOCOL=tcp)(HOST=", ORAhost, ")(PORT=", ORAport, "))",
  "(CONNECT_DATA=(SID=", ORAsid, ")))", sep = "")
anshuoCon <- dbConnect(drvORA, username="xudan", password="y446ISd6MXAM", dbname = connect.string)

aeq <- function(query) {
  dbGetQuery(dmAnalCon, "SET NAMES 'GBK'")
  resultDF <- dbGetQuery(dmAnalCon, query)
  data.table(resultDF)
}

ruleq <- function(query) {
  dbGetQuery(ruleEngCon, "SET NAMES 'GBK'")
  resultDF <- dbGetQuery(ruleEngCon, query)
  data.table(resultDF)
}

dmq <- function(query) {
  dbGetQuery(dmYewuCon, "SET NAMES 'GBK'")
  resultDF <- dbGetQuery(dmYewuCon, query)
  data.table(resultDF)
}

martq <- function(query) {
  dbGetQuery(dataMartCon, "SET NAMES 'GBK'")
  resultDF <- dbGetQuery(dataMartCon, query)
  data.table(resultDF)
}

jdzq <- function(query) {
  dbGetQuery(dataMartCon, "SET NAMES 'GBK'")
  resultDF <- dbGetQuery(jdzCon, query)
  data.table(resultDF)
}

anshuoq <- function(query) {
  resultDF <- dbGetQuery(anshuoCon, query)
  data.table(resultDF)
}

