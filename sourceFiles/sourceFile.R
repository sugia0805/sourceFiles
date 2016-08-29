MISSING_DEFAULT <- -98765
INF_DEFAULT <- "9999999999"
EPSILON_DEFAULT <- 0.0000001
# project ids mapping rules
# Project ID
# JimuID = project_ID = financingprojectid

# install.packages("quantmod", "/usr/local/lib/R/site-library")

source("../sourceFiles/sourceFiles/generalFunction.R")
boxdata <- "/home/dx/Data/"

# general
library(RMySQL)
library(data.table)
library(TTR)
# library(reshape2)
# library(pmml)
library(stringr)
library(ROCR)
library(rJava) 
library(RJDBC)
library(xlsx)

options(sqldf.driver="SQLite")

# rf
library(randomForest)
library(varSelRF)

# logistic & elastic net
library(glmnet)
library(ClustOfVar)
library(car)

# Model validation
library(hmeasure)
library(InformationValue)



all_cons <- dbListConnections(MySQL())
for(con in all_cons)
  dbDisconnect(con)

#########################################################################
# Jimu data queries

##################################
# drivers
drv <- dbDriver("MySQL")

cp = c("/home/dx/hive_libs/hive-jdbc-1.2.1.jar", 
       "/home/dx/hive_libs/hadoop-common-2.7.2.jar", 
       "/home/dx/hive_libs/libthrift-0.9.2.jar", 
       "/home/dx/hive_libs/hive-service-1.2.1.jar", 
       "/home/dx/hive_libs/httpclient-4.4.jar", 
       "/home/dx/hive_libs/httpcore-4.4.jar", 
       "/home/dx/hive_libs/hive-jdbc-1.2.1-standalone.jar")
.jinit(classpath=cp)
hiveDrv <- JDBC("org.apache.hive.jdbc.HiveDriver", "/home/dx/hive_libs/hive-jdbc-1.2.1.jar", identifier.quote="`")


##################################
# connections
ruleEngCon <- dbConnect(drv, user="dan.xu", password="bTH68b2MjQu8JZA",
                   dbname="rule_engineer", host="person.retail.mysql.zhaowei.local", port=3311, encoding = getOption("utf8"))
dmYewuCon <- dbConnect(drv, user="dan.xu", password="bTH68b2MjQu8JZA",
                     dbname="dumiao", host="person.retail.mysql.zhaowei.local", port=3311, encoding = getOption("utf8"))

dataMartCon <- dbConnect(drv, user="dan.xu", password="bTH68b2MjQu8JZA",
                   host="172.16.3.55", port=3324, encoding = getOption("utf8"))

jdzCon <- dbConnect(drv, user="dan.xu", password="bTH68b2MjQu8JZA",
                         host="172.16.4.42", port=3310, encoding = getOption("utf8"))

if(sourceName=="Dabai"){
  library(ROracle)
  drvORA <- dbDriver("Oracle")
  ORAhost <- "172.16.3.18"
  ORAport <- 1521
  ORAsid <- "orcl_std"
  connect.string <- paste(
    "(DESCRIPTION=",
    "(ADDRESS=(PROTOCOL=tcp)(HOST=", ORAhost, ")(PORT=", ORAport, "))",
    "(CONNECT_DATA=(SID=", ORAsid, ")))", sep = "")
  anshuoCon <- dbConnect(drvORA, username="xudan", password="y446ISd6MXAM", dbname = connect.string)
  anshuoq <- function(query) {
    resultDF <- dbGetQuery(anshuoCon, query)
    data.table(resultDF)
  }
}

hiveConn <- dbConnect(hiveDrv, "jdbc:hive2://172.19.4.13:10000/dsb","hdpstat","hdpstat")


#################################
# queries

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
  dbGetQuery(dataMartCon, "SET NAMES 'UTF8'")
  resultDF <- dbGetQuery(dataMartCon, query)
  data.table(resultDF)
}

jdzq <- function(query) {
  dbGetQuery(jdzCon, "SET NAMES 'GBK'")
  resultDF <- dbGetQuery(jdzCon, query)
  data.table(resultDF)
}

hiveq <- function(query) {
  dbGetQuery(hiveConn, "SET NAMES 'GBK'")
  resultDF <- dbGetQuery(hiveConn, query)
  data.table(resultDF)
}
