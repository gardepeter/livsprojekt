options(scipen = 99)
library(Rcpp)
library(RcppArmadillo)

Rcpp::sourceCpp("logic/RK1SemiMarkov.cpp")
#RK1(1, 5, 10)
