library(Rcpp)

source("logic/helperFunctions.R")
Rcpp::sourceCpp("logic/cpp/helperFunctions.cpp")

printHelloWorld()
fibonacci(10)
prodOfOnes(10)

#Anna ændring