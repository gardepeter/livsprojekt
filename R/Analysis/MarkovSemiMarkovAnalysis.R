Rcpp::sourceCpp("logic/RK1SemiMarkov.cpp")
Rcpp::sourceCpp("logic/RK4Markov.cpp")

age = 20
startTime = 0.0
endTime = 47
gracePeriod = 1/4
startDuration = 0.

reps = 5

semi = rbenchmark::benchmark(
  "cashflow_2" = {cashflow_2 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 2, age, gracePeriod, 0, 1)},
  "cashflow_4" = {cashflow_4 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 4, age, gracePeriod, 0, 1)},
  "cashflow_8" = {cashflow_8 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 8, age, gracePeriod, 0, 1)},
  "cashflow_12" = {cashflow_12 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 12, age, gracePeriod, 0, 1)},
  "cashflow_20" = {cashflow_20 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 20, age, gracePeriod, 0, 1)},
  "cashflow_52" = {cashflow_52 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 52, age, gracePeriod, 0, 1)},
  replications = reps
)
semiMarkov_res = cbind(semi[,1], semi[,3] / reps)
write.table(semiMarkov_res, "semiMarkov_res.csv", row.names = F, col.names = F, sep = ",")

markov = rbenchmark::benchmark(
  "cashflow_2_markov" = {cashflow_2_markov = markovUnitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 2, age, gracePeriod, 0, 1)},
  "cashflow_4_markov" = {cashflow_4_markov = markovUnitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 4, age, gracePeriod, 0, 1)},
  "cashflow_8_markov" = {cashflow_8_markov = markovUnitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 8, age, gracePeriod, 0, 1)},
  "cashflow_12_markov" = {cashflow_12_markov = markovUnitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 12, age, gracePeriod, 0, 1)},
  "cashflow_20_markov" = {cashflow_20_markov = markovUnitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 20, age, gracePeriod, 0, 1)},
  "cashflow_52_markov" = {cashflow_52_markov = markovUnitCashflowDisabilityWithKarens(startTime, startDuration, endTime, 52, age, gracePeriod, 0, 1)},
  replications = reps
)

markov_res = cbind(markov[,1], markov[,3] / reps)
write.table(markov_res, "markov_res.csv", row.names = F, col.names = F, sep = ",")


