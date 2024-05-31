#****************************************************
#*************** SEE semiMarkovValidation.R **********
#*#***************************************************
library(tidyverse)
options(scipen =99)
Rcpp::sourceCpp("logic/RK1SemiMarkov.cpp")

age = 20
startTime = 0.0
endTime = 47
PerTimeUnit = 12#52 #52
karensPeriod = 1/4
startDuration = 0.

# temp = RK1(startTime, startDuration, endTime, stepAmountPerTimeUnit, age)
# p01 = read.csv("p01.csv", header = F)
# p00 = read.csv("p00.csv", header = F)
# cashflow = read_csv("./data/output/unitDisabilityBenefitCashflow_3MonthGracePeriod_20YearsOldActive2024.csv")
# cashflow = unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, stepAmountPerTimeUnit, karensPeriod, age, 0, 1)
# rate_cont = approxfun(unlist(spot_rate[,1]), unlist(spot_rate[,2]))
# bond_price = sapply(seq(0, 50), function(x) exp(-integrate(rate_cont, 0, x)$val))
# bond_price_cont = approxfun(seq(0, 50), bond_price)
# integrand = approxfun(unlist(cashflow[,1]),unlist(cashflow[,2]) * bond_price_cont(unlist(cashflow[,1])))
# integrate(integrand, 0, 49.9)$val

benchmark0 = rbenchmark::benchmark(
  "cashflow_2" = {cashflow_2 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 2, age, karensPeriod, 0, 1)},
  "cashflow_4" = {cashflow_4 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 4, age, karensPeriod, 0, 1)},
  "cashflow_8" = {cashflow_8 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 8, age, karensPeriod, 0, 1)},
  "cashflow_16" = {cashflow_16 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 16, age, karensPeriod, 0, 1)},
  "cashflow_32" = {cashflow_16 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 32, age, karensPeriod, 0, 1)},
  "cashflow_64" = {cashflow_16 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 64, age, karensPeriod, 0, 1)},
  replications = 1
  )

benchmark1 = rbenchmark::benchmark(
  "cashflow_2" = {cashflow_2 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 2, age, karensPeriod, 1, 1)},
  "cashflow_3" = {cashflow_3 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 3, age, karensPeriod, 1, 1)},
  "cashflow_4" = {cashflow_4 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 4, age, karensPeriod, 1, 1)},
  "cashflow_5" = {cashflow_5 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 5, age, karensPeriod, 1, 1)},
  "cashflow_6" = {cashflow_6 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 6, age, karensPeriod, 1, 1)},
  "cashflow_7" = {cashflow_7 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 7, age, karensPeriod, 1, 1)},
  "cashflow_8" = {cashflow_8 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 8, age, karensPeriod, 1, 1)},
  "cashflow_9" = {cashflow_9 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 9, age, karensPeriod, 1, 1)},
  "cashflow_10" = {cashflow_10 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 10, age, karensPeriod, 1, 1)},
  "cashflow_11" = {cashflow_11 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 11, age, karensPeriod, 1, 1)},
  "cashflow_12" = {cashflow_12 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 12, age, karensPeriod, 1, 1)},
  "cashflow_13" = {cashflow_13 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 13, age, karensPeriod, 1, 1)},
  "cashflow_14" = {cashflow_14 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 14, age, karensPeriod, 1, 1)},
  "cashflow_15" = {cashflow_15 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 15, age, karensPeriod, 1, 1)},
  "cashflow_16" = {cashflow_16 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 16, age, karensPeriod, 1, 1)},
  "cashflow_17" = {cashflow_17 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 17, age, karensPeriod, 1, 1)},
  "cashflow_18" = {cashflow_18 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 18, age, karensPeriod, 1, 1)},
  "cashflow_19" = {cashflow_19 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 19, age, karensPeriod, 1, 1)},
  "cashflow_20" = {cashflow_20 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 20, age, karensPeriod, 1, 1)},
  replications = 1
)

benchmark2 = rbenchmark::benchmark(
  "cashflow_52" = {cashflow_52 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 52, age, karensPeriod, 1, 1)},
  replications = 1
)

spot_rate <- read_delim("data/FSARiskFreeCurve.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
spot_rate<-na.omit(spot_rate)
spot_rate<-mutate(spot_rate, rate=rate/100)

reserve = function(cashflow, spot_rate){
  endTime = cashflow[nrow(cashflow), 1]
  
  rate_cont = approxfun(unlist(spot_rate[,1]), unlist(spot_rate[,2]))
  bond_price = sapply(seq(0, 50), function(x) exp(-integrate(rate_cont, 0, x)$val))
  bond_price_cont = approxfun(seq(0, 50), bond_price)
  integrand = approxfun(unlist(cashflow[,1]),unlist(cashflow[,2]) * bond_price_cont(unlist(cashflow[,1])))
  integrate(integrand, 0, endTime)$val
}

# write.table(cashflow_2, "cashflow_2.csv", col.names=FALSE, row.names =F)
# write.table(cashflow_5, "cashflow_5.csv", col.names=FALSE, row.names =F)
# write.table(cashflow_10, "cashflow_10.csv", col.names=FALSE, row.names =F)
# write.table(cashflow_15, "cashflow_15.csv", col.names=FALSE, row.names =F)
# write.table(cashflow_20, "cashflow_20.csv", col.names=FALSE, row.names =F)
# write.table(cashflow_52, "cashflow_52.csv", col.names=FALSE, row.names =F)

res = tibble(
  res_2 = reserve(cashflow_2, spot_rate),
  res_3 = reserve(cashflow_3, spot_rate),
  res_4 = reserve(cashflow_4, spot_rate),
  res_5 = reserve(cashflow_5, spot_rate),
  res_6 = reserve(cashflow_6, spot_rate),
  res_7 = reserve(cashflow_7, spot_rate),
  res_8 = reserve(cashflow_8, spot_rate),
  res_9 = reserve(cashflow_9, spot_rate),
  res_10 = reserve(cashflow_10, spot_rate),
  res_11 = reserve(cashflow_11, spot_rate),
  res_12 = reserve(cashflow_12, spot_rate),
  res_13 = reserve(cashflow_13, spot_rate),
  res_14 = reserve(cashflow_14, spot_rate),
  res_15 = reserve(cashflow_15, spot_rate),
  res_16 = reserve(cashflow_16, spot_rate),
  res_17 = reserve(cashflow_17, spot_rate),
  res_18 = reserve(cashflow_18, spot_rate),
  res_19 = reserve(cashflow_19, spot_rate),
  res_20 = reserve(cashflow_20, spot_rate),
  res_52 = reserve(cashflow_52, spot_rate)
)

ggplot() +
  geom_line(aes(x = cashflow_2[,1], y = cashflow_2[,2]))+
  geom_line(aes(x = cashflow_52[,1], y = cashflow_52[,2]))

# sum(cashflow_slow - cashflow_fast) == 0
