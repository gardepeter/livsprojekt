#****************************************************
#*************** SEE semiMarkovValidation.R **********
#*#***************************************************
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

rbenchmark::benchmark(
  "cashflow_2" = {cashflow_2 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 2, age, karensPeriod, 0, 1)},
  "cashflow_4" = {cashflow_4 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 4, age, karensPeriod, 0, 1)},
  "cashflow_8" = {cashflow_8 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 8, age, karensPeriod, 0, 1)},
  "cashflow_16" = {cashflow_16 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 16, age, karensPeriod, 0, 1)},
  replications = 1
  )

# sum(cashflow_slow - cashflow_fast) == 0
