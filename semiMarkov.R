#options(scipen = 99)
Rcpp::sourceCpp("logic/RK1SemiMarkov.cpp")

startTime = 5.0
endTime = 10.0
stepAmount = 12 * (endTime - startTime)
startDuration = 12 / stepAmount

RK1(startTime, startDuration, endTime, stepAmount)

#temp = readr::read_csv("p00.csv", col_names = F)

