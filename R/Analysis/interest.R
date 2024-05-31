library(ggplot2)
library(readr)
library(tidyverse)
library(gridExtra)
library(tikzDevice)
library(dplyr)

#-------------------------------------------------------------------------------
#unitReserve calculation
#-------------------------------------------------------------------------------
#rates
spot_rate <- read_delim("data/FSARiskFreeCurve.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
spot_rate<-na.omit(spot_rate)
spot_rate<-mutate(spot_rate, rate=rate/100)

#cashflow
unitCashflows_final = cbind(unitCashflows[,1] - 40, unitCashflows[,2])
colnames(unitCashflows_final)<-c("year", "cashflow")

#Sørg for at cashflow har c("year","cashflow") kolonner og rate har c("year","rate) kolonner
reserve<-function(maxtime,interest_rate,cashflow_data){
  rate_cont = approxfun(unlist(interest_rate[,1]), unlist(interest_rate[,2]))
  bond_price = sapply(seq(0, maxtime), function(x) exp(-integrate(rate_cont, 0, x)$val))
  bond_price_cont = approxfun(seq(0, maxtime), bond_price)
  output = approxfun(unlist(cashflow_data[,1]),unlist(cashflow_data[,2]) * bond_price_cont(unlist(cashflow_data[,1])))
  return( integrate(output, 0, maxtime)$val )
}

reserve(maxtime=25,interest_rate=spot_rate,cashflow_data=unitCashflows_final)

#-------------------------------------------------------------------------------
#DV01
#-------------------------------------------------------------------------------
spot_rate_DV01<-spot_rate
spot_rate_DV01$DV01<-spot_rate$rate+0.0001
reserve(maxtime=25,interest_rate=spot_rate_DV01[,c(1,3)],cashflow_data=unitCashflows_final)

true_reserve<-reserve(maxtime=25,interest_rate=spot_rate,cashflow_data=unitCashflows_final)
oneBasisPoint_reserve<-reserve(maxtime=25,interest_rate=spot_rate_DV01[,c(1,3)],cashflow_data=unitCashflows_final)

DV01<- ((true_reserve-oneBasisPoint_reserve)/0.0001)/true_reserve

sum(25*(spot_rate$rate-spot_rate_DV01$DV01)/(1+spot_rate$rate)*true_reserve)




