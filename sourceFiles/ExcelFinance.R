
#' Excel: PMT (should make result data frame, but need to change propertyAnalysis())
#' 
#' Returns the payment amount for a loan based on a constant interest rate and a constant payment schedule.
#' The payment returned by PMT includes principal and interest but no taxes, 
#' reserve payments, or fees sometimes associated with loans.
#' Outgoing payments are displayed by negative numbers and 
#' incoming payments by positive numbers.
#' #http://support2.microsoft.com/kb/214005#appliesto
#' @param rate  Required. The interest rate per period. Needs to be defined as the same periods as nper.
#' @param per   Required. The period for which you want to find the interest and must be in the range 1 to nper.
#' @param nper    Required. The total number of payment periods in an annuity
#' @param pv    Required. The present value, or the lump-sum amount that a series of future payments is worth right now.
#' @param fv  Optional. Final value after the last payment. The default value is 0.
#' @param type Optional. Maturity of the payments. The default value is 0 meaning end of period. 1 means beginning
#' @keywords Excel
#' @keywords Finance
#' @export
#' @examples
#' PMT(0.005, 240, 100000, 0, 0)
#' PMT(0:10/100, 10, 10000) # should fix this to return data frame

PMT <- function(rate, nper,pv, fv=0, type=0){
  pmt = ifelse(rate!=0,
               (rate*(fv+pv*(1+ rate)^nper))/((1+rate*type)*(1-(1+ rate)^nper)),
               (-1*(fv+pv)/nper )
  )
  
  return(pmt)
}




#' Excel: IPMT
#' 
#' Internally calls PMT. Returns the interest payment for a given period for an investment 
#' based on periodic, constant payments and a constant interest rate
#' This can be an annuity can be a facility (for example a mortgage) 
#' or an investment (for example a constant saving deposit).
#' The interest rate and the payments are constant.
#' Outgoing payments are displayed by negative numbers and incoming payments by positive numbers.
#' @param rate  Required. The interest rate per period
#' @param per   Required. The period for which you want to find the interest and must be in the range 1 to nper.
#' @param nper    Required. The total number of payment periods in an annuity
#' @param pv    Required. The present value, or the lump-sum amount that a series of future payments is worth right now.
#' @param fv  Optional. Final value after the last payment. The default value is 0.
#' @param type Optional. Maturity of the payments. The default value is 0 meaning end of period. 1 means beginning
#' @keywords finance
#' @keywords excel
#' @export 
#' @examples
#' IPMT(.1/12, 1, 3*12, 8000) #interest due the first month
#' IPMT(.1, 3, 3, 8000) #Interest due in the last year for a loan with payments made yearly

IPMT <- function(rate, per, nper, pv, fv=0, type=0){
  ipmt = -( ((1+rate)^(per-1)) * (pv*rate + PMT(rate, nper,pv, fv=0, type=0)) - PMT(rate, nper,pv, fv=0, type=0))
  return(ipmt)
}


#' Excel: PPMT
#' 
#' Internally calls PMT and IPMT. Returns the payment on the principal for a given period for an investment based on periodic, 
#' constant payments and a constant interest rate. 
#' Make sure that you are consistent about the units you use for specifying rate and nper. 
#' If you make monthly payments on a four-year loan at 12 percent annual interest, use 12%/12 for rate and 4*12 for nper. 
#' If you make annual payments on the same loan, use 12% for rate and 4 for nper.
#' @param rate  Required. The interest rate per period
#' @param per   Required. The period for which you want to find the interest and must be in the range 1 to nper.
#' @param nper    Required. The total number of payment periods in an annuity
#' @param pv    Required. The present value, or the lump-sum amount that a series of future payments is worth right now.
#' @param fv  Optional. Final value after the last payment. The default value is 0.
#' @param type Optional. Maturity of the payments. The default value is 0 meaning end of period. 1 means beginning
#' @keywords finance
#' @keywords excel
#' @export
#' @examples
#' PPMT(.1/12, 1*3, 3, 8000)
#' PPMT(.1/12, 1, 2*12, 2000) # Payment on principle for the first month of loan

PPMT <- function(rate, per, nper, pv, fv=0, type=0){
  ppmt = PMT(rate, nper,pv, fv=0, type=0) - IPMT(rate, per, nper, pv, fv=0, type=0)
  return(ppmt)
}


#' Excel: NPER
#' 
#' This function calculates the number of periods in a financial problem, in the case of
#' repaying a loan this is the number of repayments. 
#' Returns the number of periods for an investment based on periodic, 
#' constant payments and a constant interest rate.
#' @param rate  Required. The interest rate per period
#' @param pmt   Required. The period for which you want to find the interest and must be in the range 1 to nper.
#' @param pv    Required. The present value, or the lump-sum amount that a series of future payments is worth right now.
#' @param fv  Optional. Final value after the last payment. The default value is 0. It is the future value, or a cash balance you want to attain after the last payment is made. If fv is omitted, 
#' it is assumed to be 0 (the future value of a loan, for example, is 0).
#' @param type Optional. Maturity of the payments. The default value is 0.
#' @keywords finance
#' @keywords excel
#' @export
#' @examples
#' NPER(0.005, -100, 10000, 0, 0)
#' NPER(.12/12, -100, -1000)
NPER <- function(rate, pmt, pv, fv=0, type=0){
  nper = ifelse(rate!=0,
                log10((pmt*(1+rate*type)-fv*rate)/(pmt*(1+rate*type)+pv*rate)) / log10(1+rate),
                -1*(fv+pv)/pmt )
  return(nper)
}


#' Excel: Present Value of constant cash flows
#' 
#' Returns the present value of an investment. The present value is the total amount that a series of future payments is worth now. For example, 
#' when you borrow money, the loan amount is the present value to the lender. 
#' All the formulas for PV, FV, NPER, and PMT come fromt the following
#' If rate is not 0 then: PV*((1+ rate)^NPER)+ PMT*(1+rate*type)*(((1+ rate)^NPER)-1)/rate+FV = 0
#' .............. But, when rate = 0 then: (PMT*NPER)+PV+FV = 0
#' 
#' @param rate  Required. The interest rate per period. Needs to be defined as the same periods as nper.
#' @param nper    Required. The total number of payment periods in an annuity
#' @param Pmt    Required. The payment made each period and cannot change over 
#' the life of the annuity. Typically, pmt includes principal and interest 
#' but no other fees or taxes. For example, the monthly payments on a $10,000, 
#' four-year car loan at 12 percent are $263.33. You would enter -263.33 
#' into the formula as the pmt. If pmt is omitted, you must include the fv argument. 
#' @param fv   Optional. The future value, or a cash balance you 
#' want to attain after the last payment is made. If fv is omitted, 
#' it is assumed to be 0 (the future value of a loan, for example, is 0). 
#' For example, if you want to save $50,000 to pay for a special project 
#' in 18 years, then $50,000 is the future value. You could then make a 
#' conservative guess at an interest rate and determine how much you must save each month. 
#' If fv is omitted, you must include the pmt argument.
#' @param type Optional. Maturity of the payments. The default value is 0 meaning end of period. 1 means beginning
#' @keywords Excel
#' @keywords Finance
#' @keywords discounting
#' @keywords finance
#' @export
#' @examples
#' PV.excel(.08/12, 12*20, 500, 0)
#' PV.excel(1:10/12, 12*20, 500, 0)


PV.excel <- function(rate, nper, pmt, fv=0, type=0){
  if(length(pmt)>1){
    stop("The payment made each period cannot change over the life of the annuity")
  }
  pv = ifelse(rate!=0,
              sapply(rate, function(r)
                - (fv + pmt*(1+r*type)*(((1+r)^nper)-1)/r)/(1+r)^nper),
              -(fv + (pmt*nper))
  )
  
  return(data.frame('Rate'=rate, 'Periods'=nper, 'PV'=pv))
}



#' Excel: Future Value of constant cash flows
#' 
#' Returns the future value of an investment based on periodic, constant payments and a constant interest rate.
#' @param rate  Required. The interest rate per period. Needs to be defined as the same periods as nper.
#' @param nper    Required. The total number of payment periods in an annuity
#' @param Pmt    Required. The payment made each period and cannot change over 
#' the life of the annuity. Typically, pmt includes principal and interest 
#' but no other fees or taxes. For example, the monthly payments on a $10,000, 
#' four-year car loan at 12 percent are $263.33. You would enter -263.33 
#' into the formula as the pmt. If pmt is omitted, you must include the fv argument. 
#' @param pv   Optional.  Is the present value, or the lump-sum amount that a series of future payments is worth right now. If pv is omitted, 
#' it is assumed to be 0 (zero), and you must include the pmt argument.
#' @param type Optional. Maturity of the payments. The default value is 0 meaning end of period. 1 means beginning
#' @keywords Excel
#' @keywords Finance
#' @keywords discounting
#' @keywords finance
#' @export
#' @examples
#' FV.excel(0, 12*20, 500, 0)
#' FV.excel(0:10/1200, 12*20, 500, 0)

FV.excel <- function(rate, nper, pmt, pv=0, type=0){
  if(length(pmt)>1){
    stop("The payment made each period cannot change over the life of the annuity")
  }
  fv = ifelse(rate!=0,
              sapply(rate, function(r)
                (pmt*(1+r*type)*(1-(1+ r)^nper)/r)-pv*(1+r)^nper ),
              -1*(pv+pmt*nper)
  )
  return(data.frame('Rate'=rate, 'Periods'=nper, 'FV'=fv))
}