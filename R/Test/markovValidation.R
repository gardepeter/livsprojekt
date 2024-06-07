library(tidyverse)
Rcpp::sourceCpp("logic/Markov.cpp")

age = 40
startTime = 0.0
endTime = 25.0
stepAmountPerTimeUnit = 12
startIncrement = c(0, 1) * stepAmountPerTimeUnit
karensPeriod = 1/4
startDuration = startIncrement / stepAmountPerTimeUnit

cashflow_0 = markovDisabilityUnitBenefitCashflow(startTime, startDuration[1], endTime, stepAmountPerTimeUnit, age, karensPeriod, 1, 1)
cashflow_1 = markovDisabilityUnitBenefitCashflow(startTime, startDuration[2], endTime, stepAmountPerTimeUnit, age, karensPeriod,  1, 1)

plot_pre = tibble(age = cashflow_0[,1] + age, cashflow_0 = cashflow_0[,2], cashflow_1 = cashflow_1[, 2]) 
#write.csv(plot_pre, "unitCashflows.csv", row.names = F)

plot = plot_pre %>%
  pivot_longer(!age, values_to = "value", names_to = "cashflow")

new_labels <- c("cashflow_0" = "Start duration zero", "cashflow_1" = "Start duration one")

ggplot(plot, aes(age, value))+
  geom_line() + 
  scale_y_continuous(breaks = seq(0, 1.2, length.out = 7), limits = c(0, 1.2)) +
  facet_grid(~cashflow, labeller = labeller(cashflow = new_labels))+
  theme(axis.title.y=element_blank())