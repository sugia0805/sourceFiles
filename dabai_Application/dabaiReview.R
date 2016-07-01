dabaiReview <- read.csv(paste0(boxdata, "jmsd_perf_vintage_201605.csv"))
dabaiReview <- data.table(dabaiReview)

dabaiReview <- dabaiReview[SubChannel=='直销',]

dabaiReview[, GCTotal:=GCOAmt*Flag_90]

dabaiGC <- dabaiReview[, .("totalGC"=max(GCOAmt), 
                           "GCDate"=min(as.Date(GCOdate, "%D-%M-%Y"))), 
                       by=c("FinancingID_2", "ddmonth", "City", "CustSegment")]



dabaiReview[, DOB:=as.Date(bsn_dt)-as.Date(DrawdownStamp)]
dabaiReview[, c("Channel","ChannelDetail","FinancingName","ProductType"):=NULL]
dabaiReview <- dabaiReview[bsn_dt != '2016-06-26']

dabaiFID <- dabaiReview[DOB>30 & DOB<60, ]
