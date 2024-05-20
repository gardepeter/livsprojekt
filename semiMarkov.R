#****************************************************
#*************** SEE semiMarkovValidation.R **********
#*#***************************************************
library(tidyverse)

Rcpp::sourceCpp("logic/RK1SemiMarkov.cpp")

age = 20
startTime = 0.0
endTime = 25.0
stepAmountPerTimeUnit = 12 #TODO run with 52 (weeks in a year)
startIncrement = 1 * stepAmountPerTimeUnit
karensPeriod = 3

startDuration = startIncrement / stepAmountPerTimeUnit

RK1(startTime, startDuration, endTime, stepAmountPerTimeUnit, age)

cashflow = unitCashflowDisabilityWithKarens(startTime, startDuration, endTime, stepAmountPerTimeUnit, karensPeriod, age)

temp00 = readr::read_csv("p00.csv", col_names = F)
temp01 = readr::read_csv("p01.csv", col_names = F)
temp02 = readr::read_csv("p02.csv", col_names = F)

#Dødeintensiter for semi markov
# deathInt = readxl::read_excel("data/deathIntensity.xlsx")
# intensities = unlist(deathInt[21:111,14])
# 
# for(age in 20 + 0:(length(intensities) - 1) ){
#   print(paste0("else if( ", age, " <= x < ", age + 1, " ){"))
#   print(paste0("   return ", intensities[age - 19], ";"))
#   print(paste0("}"))
# }




