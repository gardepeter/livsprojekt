library(Rcpp)
library(RcppArmadillo)

source("logic/helperFunctions.R")
Rcpp::sourceCpp("logic/cpp/helperFunctions.cpp")

#en meget flot ændring

printHelloWorld()
fibonacci(10)
prodOfOnes(10)

#Anna ændring