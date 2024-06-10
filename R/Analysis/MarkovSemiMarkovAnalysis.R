Rcpp::sourceCpp("logic/SemiMarkov.cpp")
Rcpp::sourceCpp("logic/Markov.cpp")

################### RESERVES ################################
spot_rate <- read_delim("data/FSARiskFreeCurve.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
spot_rate<-na.omit(spot_rate)
spot_rate<-mutate(spot_rate, rate=rate/100)

reserve = function(cashflow, spot_rate, subdivisions=100, rel.tol=.Machine$double.eps^0.25){
  endTime = cashflow[nrow(cashflow), 1]
  
  rate_cont = approxfun(unlist(spot_rate[,1]), unlist(spot_rate[,2]))
  bond_price = sapply(seq(0, 50), function(x) exp(-integrate(rate_cont, 0, x)$val))
  bond_price_cont = approxfun(seq(0, 50), bond_price)
  integrand = approxfun(unlist(cashflow[,1]),unlist(cashflow[,2]) * bond_price_cont(unlist(cashflow[,1])))
  integrate(integrand, 0, endTime, subdivisions = subdivisions, rel.tol = rel.tol)$val
}


############################## RUN TIMES ###################################

age = 30
startTime = 0.0
endTime = 37
gracePeriod = 1/4
startDuration = 0.

reps = 1

semi = rbenchmark::benchmark(
  "cashflow_4" = {cashflow_4 = semiMarkovDisabilityUnitBenefitCashflow(startTime, startDuration, endTime, 4, age, gracePeriod, 1, 1)},
  "cashflow_8" = {cashflow_8 = semiMarkovDisabilityUnitBenefitCashflow(startTime, startDuration, endTime, 8, age, gracePeriod, 1, 1)},
  "cashflow_12" = {cashflow_12 = semiMarkovDisabilityUnitBenefitCashflow(startTime, startDuration, endTime, 12, age, gracePeriod, 1, 1)},
  "cashflow_24" = {cashflow_24 = semiMarkovDisabilityUnitBenefitCashflow(startTime, startDuration, endTime, 24, age, gracePeriod, 1, 1)},
  "cashflow_36" = {cashflow_36 = semiMarkovDisabilityUnitBenefitCashflow(startTime, startDuration, endTime, 36, age, gracePeriod, 1, 1)},
  "cashflow_52" = {cashflow_52 = semiMarkovDisabilityUnitBenefitCashflow(startTime, startDuration, endTime, 52, age, gracePeriod, 1, 1)},
  replications = reps
)
semiMarkov_res = cbind(semi[,1], semi[,3] / reps)
# write.table(semiMarkov_res, "semiMarkov_res.csv", row.names = F, col.names = F, sep = ",")

markov = rbenchmark::benchmark(
  "cashflow_4_markov" = {cashflow_4_markov = markovDisabilityUnitBenefitCashflow(startTime, startDuration, endTime, 4, age, gracePeriod, 1, 1)},
  "cashflow_8_markov" = {cashflow_8_markov = markovDisabilityUnitBenefitCashflow(startTime, startDuration, endTime, 8, age, gracePeriod, 1, 1)},
  "cashflow_12_markov" = {cashflow_12_markov = markovDisabilityUnitBenefitCashflow(startTime, startDuration, endTime, 12, age, gracePeriod, 1, 1)},
  "cashflow_24_markov" = {cashflow_24_markov = markovDisabilityUnitBenefitCashflow(startTime, startDuration, endTime, 24, age, gracePeriod, 1, 1)},
  "cashflow_36_markov" = {cashflow_36_markov = markovDisabilityUnitBenefitCashflow(startTime, startDuration, endTime, 36, age, gracePeriod, 1, 1)},
  "cashflow_52_markov" = {cashflow_52_markov = markovDisabilityUnitBenefitCashflow(startTime, startDuration, endTime, 52, age, gracePeriod, 1, 1)},
  replications = reps
)

markov_res = cbind(markov[,1], markov[,3] / reps)
# write.table(markov_res, "markov_res.csv", row.names = F, col.names = F, sep = ",")

library(tidyverse)

cashflows = tibble(
  cashflow_4 = reserve(cashflow_4, spot_rate),
  cashflow_8 = reserve(cashflow_8, spot_rate),
  cashflow_12 = reserve(cashflow_12, spot_rate),
  cashflow_24 = reserve(cashflow_24, spot_rate),
  cashflow_36 = reserve(cashflow_36, spot_rate),
  cashflow_52 = reserve(cashflow_52, spot_rate),
  
  cashflow_4_markov =  reserve(cashflow_4_markov, spot_rate),
  cashflow_8_markov = reserve(cashflow_8_markov, spot_rate),
  cashflow_12_markov = reserve(cashflow_12_markov, spot_rate),
  cashflow_24_markov = reserve(cashflow_24_markov, spot_rate),
  cashflow_36_markov = reserve(cashflow_36_markov, spot_rate),
  cashflow_52_markov = reserve(cashflow_52_markov, spot_rate)
)
colnames(cashflow_4) = c("X1", "X2")
colnames(cashflow_8) = c("X1", "X2")
colnames(cashflow_12) = c("X1", "X2")
colnames(cashflow_24) = c("X1", "X2")
colnames(cashflow_36) = c("X1", "X2")
colnames(cashflow_52) = c("X1", "X2")

cashflow_4[,1] = cashflow_4[,1] + age
cashflow_8[,1] = cashflow_8[,1]  + age
cashflow_12[,1] = cashflow_12[,1] + age
cashflow_24[,1] = cashflow_24[,1] + age
cashflow_36[,1] = cashflow_36[,1] + age
cashflow_52[,1] = cashflow_52[,1] + age

ggplot() + 
  geom_step(data = data.frame(cashflow_4), aes(x = X1, y = X2, color = factor('4', levels = c('4', '8', '12', '24', '36', '52')))) + 
  geom_step(data = data.frame(cashflow_8), aes(x = X1, y = X2, color = factor('8', levels = c('4', '8', '12', '24', '36', '52')))) + 
  geom_step(data = data.frame(cashflow_12), aes(x = X1, y = X2, color = factor('12', levels = c('4', '8', '12', '24', '36', '52')))) + 
  geom_step(data = data.frame(cashflow_24), aes(x = X1, y = X2, color = factor('24', levels = c('4', '8', '12', '24', '36', '52')))) + 
  geom_step(data = data.frame(cashflow_36), aes(x = X1, y = X2, color = factor('36', levels = c('4', '8', '12', '24', '36', '52')))) + 
  geom_step(data = data.frame(cashflow_52), aes(x = X1, y = X2, color = factor('52', levels = c('4', '8', '12', '24', '36', '52')))) + 
  scale_colour_manual(values = c('4' = 'red', 
                                '8' = 'orange', 
                                '12' = 'green', 
                                '24' = 'brown', 
                                '36' = 'purple', 
                                '52' = 'blue')) +
  
  # xlim(30, 40) +
  labs(x = "Age",
       y = "Unit cash flow",
       color = "Steps")

#################### RESERVES UNDER DIFFERING GRACE PERIODS ###################
library(tidyverse)

age = 30
startTime = 0.0
endTime = 37
stepAmountPerTimeUnit = 24
startDuration = 0.

cashflow_grace_0_markov = markovDisabilityUnitBenefitCashflow(startTime, startDuration, endTime, stepAmountPerTimeUnit, age, 0/4, 0, 1)
cashflow_grace_0.25_markov = markovDisabilityUnitBenefitCashflow(startTime, startDuration, endTime, stepAmountPerTimeUnit, age, 1/4, 0, 1)
cashflow_grace_0.5_markov = markovDisabilityUnitBenefitCashflow(startTime, startDuration, endTime, stepAmountPerTimeUnit, age, 2/4, 0, 1)
cashflow_grace_0.75_markov = markovDisabilityUnitBenefitCashflow(startTime, startDuration, endTime, stepAmountPerTimeUnit, age, 3/4, 0, 1)
cashflow_grace_1_markov = markovDisabilityUnitBenefitCashflow(startTime, startDuration, endTime, stepAmountPerTimeUnit, age, 4/4, 0, 1)

cashflow_grace_0_semiMarkov = semiMarkovDisabilityUnitBenefitCashflow(startTime, startDuration, endTime, stepAmountPerTimeUnit, age, 0/4, 0, 1)
cashflow_grace_0.25_semiMarkov = semiMarkovDisabilityUnitBenefitCashflow(startTime, startDuration, endTime, stepAmountPerTimeUnit, age, 1/4, 0, 1)
cashflow_grace_0.5_semiMarkov = semiMarkovDisabilityUnitBenefitCashflow(startTime, startDuration, endTime, stepAmountPerTimeUnit, age, 2/4, 0, 1)
cashflow_grace_0.75_semiMarkov = semiMarkovDisabilityUnitBenefitCashflow(startTime, startDuration, endTime, stepAmountPerTimeUnit, age, 3/4, 0, 1)
cashflow_grace_1_semiMarkov = semiMarkovDisabilityUnitBenefitCashflow(startTime, startDuration, endTime, stepAmountPerTimeUnit, age, 4/4, 0, 1)

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
  mutate(model = "semi-Markov") %>%
  pivot_longer(!c(time, model), names_to = "GracePeriod", values_to = "CashFlow")

cashflowPlot = rbind(cashflowPlot_differentGracePeriods_markov, cashflowplot_differentGracePeriods_semiMarkov)

cashflowPlot$GracePeriod <- factor(cashflowPlot$GracePeriod, levels=c('1', '3/4', '1/2', '1/4', "0"))

ggplot(cashflowPlot, aes(x = time, y = CashFlow, color = GracePeriod)) +
  geom_line() +
  facet_wrap(~model) +
  labs(x="Age", y="Unit cash flow", color="Grace period")

ggplot(cashflowPlot %>% 
         filter(GracePeriod %in% c("0", "1/4")) %>%
         mutate(GracePeriod = paste0( "Grace period ",GracePeriod)), aes(x = time, y = CashFlow, color = model)) +
  geom_line() +
  facet_wrap(~GracePeriod) +
  labs(x="Age", y="Unit cash flow", color="")

reserve(cashflow_grace_0_markov, spot_rate)
reserve(cashflow_grace_0.25_markov, spot_rate)
reserve(cashflow_grace_0.5_markov, spot_rate)

reserve(cashflow_grace_0_semiMarkov, spot_rate)
reserve(cashflow_grace_0.25_semiMarkov, spot_rate)
reserve(cashflow_grace_0.5_semiMarkov, spot_rate)

DV01 = function(cashFlow, spotRate, epsilon){
  spot_rate_DV01<-spot_rate
  spot_rate_DV01$rate<-spot_rate$rate+epsilon
  
  trueReserve = reserve(cashFlow, spot_rate)
  oneBasisPointReserve = reserve(cashFlow, spot_rate_DV01)
  
  DV01 = -(trueReserve-oneBasisPointReserve)/epsilon
  AD = DV01 / trueReserve
  
  return(c(DV01, AD))
}

DV01(cashflow_grace_0.5_semiMarkov, spot_rate, 0.000000001)

####################### DIFFERING START DURATIONS ###########################
age = 30
startTime = 0.0
endTime = 37
gracePeriod = 1/4
stepAmountPerTimeUnit = 24
startDurations = c(0, 0.5, 1, 1.5, 2, 2.5)

cashflow = list()
for(i in 1:length(startDurations) ){
  cashflow[[i]] = list(
    markov =  markovDisabilityUnitBenefitCashflow(startTime, startDurations[i], endTime, stepAmountPerTimeUnit, age, gracePeriod, 1, 1),
    semiMarkov = semiMarkovDisabilityUnitBenefitCashflow(startTime, startDurations[i], endTime, stepAmountPerTimeUnit, age, gracePeriod, 1, 1)
  )
}

lapply(cashflow, function(x) list(markov = reserve(x$markov, spot_rate, 5000, .Machine$double.eps^.05),
                                  semiMarkov = reserve(x$semiMarkov, spot_rate) ))
plot_markov = cbind(cashflow[[1]]$markov,
             cashflow[[2]]$markov[,2],
             cashflow[[3]]$markov[,2],
             cashflow[[4]]$markov[,2],
             cashflow[[5]]$markov[,2],
             cashflow[[6]]$markov[,2])
colnames(plot_markov) = c("age", paste0("Start duration ", startDurations))

plot_semi = cbind(cashflow[[1]]$semiMarkov,
             cashflow[[2]]$semiMarkov[,2],
             cashflow[[3]]$semiMarkov[,2],
             cashflow[[4]]$semiMarkov[,2],
             cashflow[[5]]$semiMarkov[,2],
             cashflow[[6]]$semiMarkov[,2])
colnames(plot_semi) = c("age", paste0("Start duration ", startDurations))

plot = rbind(
  plot_markov %>%
    as_tibble() %>%
    pivot_longer(!age, names_to = "startDuration", values_to = "cashflow") %>%
    mutate(model = "Markov"),
  plot_semi %>%
    as_tibble() %>%
    pivot_longer(!age, names_to = "startDuration", values_to = "cashflow") %>%
    mutate(model = "semi-Markov")
) %>% mutate(age = age + 30)

ggplot(plot, aes(age, cashflow, color = model)) +
  geom_line() +
  facet_wrap(~startDuration, ncol = 3) +
  labs(x = "Age",
       y = "Unit cash flow",
       color = "Model")

