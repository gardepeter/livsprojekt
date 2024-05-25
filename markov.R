options(scipen = 99)

Rcpp::sourceCpp("logic/RK4Markov.cpp")
age = 20
start = 0
termination = 50
startDuration = 0.
gracePeriod = 1/4
stepAmountPerTimeUnit = 52

probabilities_RK1 = RK1(start, termination, stepAmountPerTimeUnit, age)
colnames(probabilities_RK1) <- c("p_00", "p_01", "p_02",
                             "p_10", "p_11", "p_12",
                             "p_20", "p_21", "p_22")

system.time({
  cashflow = markovUnitCashflowDisabilityWithKarens(start, startDuration, termination, stepAmountPerTimeUnit, age, gracePeriod, 0, 1)
})
# write.csv(cashflow, "unitDisabilityBenefitCashflow_3MonthGracePeriod_20YearsOldActive2024_markov.csv", row.names=FALSE)
