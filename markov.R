options(scipen = 99)
library(Rcpp)
library(RcppArmadillo)

Rcpp::sourceCpp("logic/cpp/RungaKutta/RK4Markov.cpp")
start = 0
termination = 50
probabilities = RK4(start, termination, (termination - start) * 12)
colnames(probabilities) <- c("p_00", "p_01", "p_02",
                             "p_10", "p_11", "p_12",
                             "p_20", "p_21", "p_22")
write.csv(probabilities, "probabilities.csv", row.names=FALSE)
