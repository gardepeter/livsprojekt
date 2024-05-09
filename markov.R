options(scipen = 99)
library(Rcpp)
library(RcppArmadillo)

Rcpp::sourceCpp("logic/cpp/RungaKutta/RK4.cpp")
start = 0
termination = 47
RK4(start, termination, (termination - start) * 12)
