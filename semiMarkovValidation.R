#options(scipen = 99)
library(tidyverse)

Rcpp::sourceCpp("logic/RK1SemiMarkov.cpp")

########## UTILITIES ##################
f = function(matrix, index, amount){
  vector = c()
  for(i in 1:amount){
    vector[i] = matrix[index + i, i]
  }
  return(unlist(vector))
}

############## SUM OF DIAGANOL VALIDATION ##############
age = 20
startTime = 0.0
endTime = 40.0
stepAmountPerTimeUnit = 12
startIncrement = 0 * stepAmountPerTimeUnit
startDuration = startIncrement / stepAmountPerTimeUnit

RK1(startTime, startDuration, endTime, stepAmountPerTimeUnit, age)

p00 = readr::read_csv("p00.csv", col_names = F)
p01 = readr::read_csv("p01.csv", col_names = F)
p02 = readr::read_csv("p02.csv", col_names = F)

diagonal_vector_00 = f(p00, startIncrement, stepAmountPerTimeUnit * (endTime - startTime))
diagonal_vector_01 = f(p01, startIncrement, stepAmountPerTimeUnit * (endTime - startTime))
diagonal_vector_02 = f(p02, startIncrement, stepAmountPerTimeUnit * (endTime - startTime))

p10 = readr::read_csv("p10.csv", col_names = F)
p11 = readr::read_csv("p11.csv", col_names = F)
p12 = readr::read_csv("p12.csv", col_names = F)

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
stepAmountPerTimeUnit = 12
startIncrement = c(0, 1) * stepAmountPerTimeUnit
karensPeriod = 1/4
startDuration = startIncrement / stepAmountPerTimeUnit

cashflow_0 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration[1], endTime, stepAmountPerTimeUnit, age, karensPeriod, 1, 1)
cashflow_1 = RK1_unitCashflowDisabilityWithKarens(startTime, startDuration[2], endTime, stepAmountPerTimeUnit, age, karensPeriod,  1, 1)

plot_pre = tibble(age = cashflow_0[,1] + age, cashflow_0 = cashflow_0[,2], cashflow_1 = cashflow_1[, 2]) 
#write.csv(plot_pre, "unitCashflows.csv", row.names = F)

plot = plot_pre %>%
  pivot_longer(!age, values_to = "value", names_to = "cashflow")

new_labels <- c("cashflow_0" = "Expected cash flow (d=0)", "cashflow_1" = "Expected cash flow (d=1)")

ggplot(plot, aes(age, value))+
  geom_line() + 
  scale_y_continuous(breaks = seq(0, 1.2, length.out = 7), limits = c(0, 1.2)) +
  facet_grid(~cashflow, labeller = labeller(cashflow = new_labels))+
  theme(axis.title.y=element_blank())

