#options(scipen = 99)
library(Rcpp)
library(RcppArmadillo)

Rcpp::sourceCpp("logic/RK1SemiMarkov.cpp")
startTime = 5.0
stepAmount = 52
endTime = 45.0
startDuration = 26 / stepAmount

RK1(startTime, startDuration, endTime, stepAmount)

