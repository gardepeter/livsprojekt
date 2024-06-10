#options(scipen = 99)
library(tidyverse)

Rcpp::sourceCpp("logic/SemiMarkov.cpp")
Rcpp::sourceCpp("logic/Markov.cpp")

########## UTILITIES ##################
f = function(matrix, index, amount){
  vector = c()
  for(i in 1:amount){
    vector[i] = matrix[index + i, i]
  }
  return(unlist(vector))
}

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

############## SUM OF DIAGANOL VALIDATION ##############
age = 20
startTime = 0.0
endTime = 40.0
stepAmountPerTimeUnit = 12
startIncrement = 0 * stepAmountPerTimeUnit
startDuration = startIncrement / stepAmountPerTimeUnit

semiMarkovTransitionProbabilities(startTime, startDuration, endTime, stepAmountPerTimeUnit, age)

p00 = readr::read_csv("p00.csv", col_names = F) %>%
  mutate_all(~replace(., is.na(.), 0))
p01 = readr::read_csv("p01.csv", col_names = F)%>%
  mutate_all(~replace(., is.na(.), 0))
p02 = readr::read_csv("p02.csv", col_names = F)%>%
  mutate_all(~replace(., is.na(.), 0))

diagonal_vector_00 = f(p00, startIncrement, stepAmountPerTimeUnit * (endTime - startTime))
diagonal_vector_01 = f(p01, startIncrement, stepAmountPerTimeUnit * (endTime - startTime))
diagonal_vector_02 = f(p02, startIncrement, stepAmountPerTimeUnit * (endTime - startTime))

p10 = readr::read_csv("p10.csv", col_names = F)%>%
  mutate_all(~replace(., is.na(.), 0))
p11 = readr::read_csv("p11.csv", col_names = F)%>%
  mutate_all(~replace(., is.na(.), 0))
p12 = readr::read_csv("p12.csv", col_names = F)%>%
  mutate_all(~replace(., is.na(.), 0))

diagonal_vector_10 = f(p10, startIncrement, stepAmountPerTimeUnit * (endTime - startTime))
diagonal_vector_11 = f(p11, startIncrement, stepAmountPerTimeUnit * (endTime - startTime))
diagonal_vector_12 = f(p12, startIncrement, stepAmountPerTimeUnit * (endTime - startTime))

#SHOULD be zero
tolerance = 0.01
sum(abs(diagonal_vector_00 + diagonal_vector_01 + diagonal_vector_01 - 1) > tolerance)
sum(abs(diagonal_vector_10 + diagonal_vector_11 + diagonal_vector_12 - 1) > tolerance)


################ PLOTS FOR VALIDATION ####################
age = 40
startTime = 0.0
endTime = 25.0
stepAmountPerTimeUnit = 24
startIncrement = c(0, 1) * stepAmountPerTimeUnit
karensPeriod = 1/4
startDuration = startIncrement / stepAmountPerTimeUnit

cashflow_0 = semiMarkovDisabilityUnitBenefitCashflow(startTime, startDuration[1], endTime, stepAmountPerTimeUnit, age, karensPeriod, 1, 1)
cashflow_1 = semiMarkovDisabilityUnitBenefitCashflow(startTime, startDuration[2], endTime, stepAmountPerTimeUnit, age, karensPeriod,  1, 1)
cashflow_0_markov = markovDisabilityUnitBenefitCashflow(startTime, startDuration[1], endTime, stepAmountPerTimeUnit, age, karensPeriod, 1, 1)
cashflow_1_markov = markovDisabilityUnitBenefitCashflow(startTime, startDuration[2], endTime, stepAmountPerTimeUnit, age, karensPeriod,  1, 1)

plot_pre = tibble(age = cashflow_0[,1] + age, cashflow_0 = cashflow_0[,2], cashflow_1 = cashflow_1[, 2], model = "semi-Markov") 
plot_pre_markov = tibble(age = cashflow_0_markov[,1] + age, cashflow_0 = cashflow_0_markov[,2], cashflow_1 = cashflow_1_markov[, 2], model = "Markov") 
#write.csv(plot_pre, "unitCashflows.csv", row.names = F)

plot = rbind(plot_pre, plot_pre_markov) %>%
  pivot_longer(!c(age, model), values_to = "value", names_to = "cashflow")

new_labels <- c("cashflow_0" = "Start duration zero", "cashflow_1" = "Start duration one")

ggplot(plot, aes(age, value, color = model))+
  geom_line() + 
  scale_y_continuous(breaks = seq(0, 1.2, length.out = 7), limits = c(0, 1.2)) +
  facet_grid(~cashflow, labeller = labeller(cashflow = new_labels))+
  theme(axis.title.y=element_blank()) +
  labs(color = "Model")

################ RESERVES FOR VALIDATION ####################
age = 40
startTime = 0.0
endTime = 25.0
stepAmountPerTimeUnit = 12
karensPeriod = 1/4
startDuration = c(0, 1/2, 1, 3/2, 2)

cashflow_0 = semiMarkovDisabilityUnitBenefitCashflow(startTime, startDuration[1], endTime, stepAmountPerTimeUnit, age, karensPeriod, 1, 1)
cashflow_0.5 = semiMarkovDisabilityUnitBenefitCashflow(startTime, startDuration[2], endTime, stepAmountPerTimeUnit, age, karensPeriod,  1, 1)
cashflow_1 = semiMarkovDisabilityUnitBenefitCashflow(startTime, startDuration[3], endTime, stepAmountPerTimeUnit, age, karensPeriod,  1, 1)
cashflow_1.5 = semiMarkovDisabilityUnitBenefitCashflow(startTime, startDuration[4], endTime, stepAmountPerTimeUnit, age, karensPeriod,  1, 1)
cashflow_2 = semiMarkovDisabilityUnitBenefitCashflow(startTime, startDuration[5], endTime, stepAmountPerTimeUnit, age, karensPeriod,  1, 1)

reserve(cashflow_0, spot_rate)
reserve(cashflow_0.5, spot_rate)
reserve(cashflow_1, spot_rate)
reserve(cashflow_1.5, spot_rate)
reserve(cashflow_2, spot_rate)
