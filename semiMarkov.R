#****************************************************
#*************** SEE semiMarkovValidation.R **********
#*#***************************************************
library(tidyverse)
options(scipen =99)
Rcpp::sourceCpp("logic/RK1SemiMarkov.cpp")

age = 20
startTime = 0.0
endTime = 50.0
stepAmountPerTimeUnit = 12 #WARNING very large amount of stepAmounts!
startIncrement = 0
karensPeriod = 3
startDuration = startIncrement / stepAmountPerTimeUnit

# cashflow = unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, stepAmountPerTimeUnit, karensPeriod, age, 0, 1)
# write.csv(cashflow, "unitCashflow20YearOldActive.csv", row.names = F)
# RK1(startTime, startDuration, endTime, stepAmountPerTimeUnit, age)
# p00 = readr::read_csv("p00.csv", col_names = F) #correct? [1,3]?

unitCashflowDisabilityWithKaren = 
  RK1_unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, stepAmountPerTimeUnit, age, karensPeriod, 0, 1)
