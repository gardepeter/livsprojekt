options(scipen = 99)
library(Rcpp)
library(RcppArmadillo)

Rcpp::sourceCpp("logic/RK1SemiMarkov.cpp")
RK1(1.0, 1.0, 3.0, 10)
