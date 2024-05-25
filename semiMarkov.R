#****************************************************
#*************** SEE semiMarkovValidation.R **********
#*#***************************************************
# library(tidyverse)
options(scipen =99)
Rcpp::sourceCpp("logic/RK1SemiMarkov.cpp")

age = 20
startTime = 0.0
endTime = 10.0 #50.
stepAmountPerTimeUnit = 12#52 #52
startIncrement = 0
karensPeriod = 1/4
startDuration = startIncrement / stepAmountPerTimeUnit

# cashflow = unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, stepAmountPerTimeUnit, karensPeriod, age, 0, 1)
# rate_cont = approxfun(unlist(spot_rate[,1]), unlist(spot_rate[,2]))
# bond_price = sapply(seq(0, 50), function(x) exp(-integrate(rate_cont, 0, x)$val))
# bond_price_cont = approxfun(seq(0, 50), bond_price)
# integrand = approxfun(unlist(cashflow[,1]),unlist(cashflow[,2]) * bond_price_cont(unlist(cashflow[,1])))
# integrate(integrand, 0, 49.916)$val

system.time({
  # cashflow_slow = unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, stepAmountPerTimeUnit, age, karensPeriod, 0, 1)
  cashflow_fast = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, stepAmountPerTimeUnit, age, karensPeriod, 0, 1)
})

# sum(cashflow_slow - cashflow_fast) == 0
