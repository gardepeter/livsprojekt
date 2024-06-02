Rcpp::sourceCpp("logic/RK1SemiMarkov.cpp")
Rcpp::sourceCpp("logic/RK4Markov.cpp")

############################## RUN TIMES ###################################

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

#################### RESERVES UNDER DIFFERING GRACE PERIODS ###################
library(tidyverse)

age = 20
startTime = 0.0
endTime = 47
stepAmountPerTimeUnit = 12
startDuration = 0.

cashflow_grace_0_markov = markovUnitCashflowDisabilityWithKarens(startTime, startDuration, endTime, stepAmountPerTimeUnit, age, 0/4, 0, 1)
cashflow_grace_0.25_markov = markovUnitCashflowDisabilityWithKarens(startTime, startDuration, endTime, stepAmountPerTimeUnit, age, 1/4, 0, 1)
cashflow_grace_0.5_markov = markovUnitCashflowDisabilityWithKarens(startTime, startDuration, endTime, stepAmountPerTimeUnit, age, 2/4, 0, 1)
cashflow_grace_0.75_markov = markovUnitCashflowDisabilityWithKarens(startTime, startDuration, endTime, stepAmountPerTimeUnit, age, 3/4, 0, 1)
cashflow_grace_1_markov = markovUnitCashflowDisabilityWithKarens(startTime, startDuration, endTime, stepAmountPerTimeUnit, age, 4/4, 0, 1)

cashflow_grace_0_semiMarkov = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, stepAmountPerTimeUnit, age, 0/4, 0, 1)
cashflow_grace_0.25_semiMarkov = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, stepAmountPerTimeUnit, age, 1/4, 0, 1)
cashflow_grace_0.5_semiMarkov = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, stepAmountPerTimeUnit, age, 2/4, 0, 1)
cashflow_grace_0.75_semiMarkov = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, stepAmountPerTimeUnit, age, 3/4, 0, 1)
cashflow_grace_1_semiMarkov = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, stepAmountPerTimeUnit, age, 4/4, 0, 1)

cashflow_differentGracePeriods_markov = cbind(cashflow_grace_0_markov[,1] + age,
                                                  cashflow_grace_0_markov[,2],
                                                 cashflow_grace_0.25_markov[,2],
                                                 cashflow_grace_0.5_markov[,2],
                                                 cashflow_grace_0.75_markov[,2],
                                                 cashflow_grace_1_markov[,2])
colnames(cashflow_differentGracePeriods_markov) = c("time", "0", "1/4", "1/2", "3/4", "1")

cashflowPlot_differentGracePeriods_markov = as_tibble(cashflow_differentGracePeriods_markov) %>%
  mutate(model = "Markov") %>%
  pivot_longer(!c(time, model), names_to = "GracePeriod", values_to = "CashFlow")

cashflow_differentGracePeriods_semiMarkov = cbind(cashflow_grace_0_semiMarkov[,1] + age,
                                                      cashflow_grace_0_semiMarkov[,2],
                                                      cashflow_grace_0.25_semiMarkov[,2],
                                                      cashflow_grace_0.5_semiMarkov[,2],
                                                      cashflow_grace_0.75_semiMarkov[,2],
                                                      cashflow_grace_1_semiMarkov[,2])
colnames(cashflow_differentGracePeriods_semiMarkov) = c("time", "0", "1/4", "1/2", "3/4", "1")

cashflowplot_differentGracePeriods_semiMarkov = as_tibble(cashflow_differentGracePeriods_semiMarkov) %>%
  mutate(model = "Semi markov") %>%
  pivot_longer(!c(time, model), names_to = "GracePeriod", values_to = "CashFlow")

cashflowPlot = rbind(cashflowPlot_differentGracePeriods_markov, cashflowPlot_differentGracePeriods_semiMarkov)

cashflowPlot$GracePeriod <- factor(cashflowPlot$GracePeriod, levels=c('1', '3/4', '1/2', '1/4', "0"))

ggplot(cashflowPlot, aes(x = time, y = CashFlow, color = GracePeriod)) +
  geom_line() +
  facet_wrap(~model) +
  labs(x="Age (years)", y="Unit cash flow", color="Grace period (years)")


