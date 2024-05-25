options(scipen = 99)

Rcpp::sourceCpp("logic/RK4Markov.cpp")
age = 20
start = 0
termination = 50
probabilities_RK1 = RK1(start, termination, (termination - start) * 12, age)
colnames(probabilities_RK1) <- c("p_00", "p_01", "p_02",
                             "p_10", "p_11", "p_12",
                             "p_20", "p_21", "p_22")
#write.csv(probabilities, "probabilities.csv", row.names=FALSE)

probabilities_RK4 = RK4(start, termination, (termination - start) * 12, age)
colnames(probabilities_RK4) <- c("p_00", "p_01", "p_02",
                             "p_10", "p_11", "p_12",
                             "p_20", "p_21", "p_22")


# write.csv(probabilities, "probabilities.csv", row.names=FALSE)
