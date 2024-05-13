#options(scipen = 99)
Rcpp::sourceCpp("logic/RK1SemiMarkov.cpp")

startTime = 5.0
endTime = 10.0
stepAmount = 52 * (endTime - startTime)
startDuration = 12 / stepAmount

RK1(startTime, startDuration, endTime, stepAmount)

temp00 = readr::read_csv("p00.csv", col_names = F)
temp01 = readr::read_csv("p01.csv", col_names = F)
temp02 = readr::read_csv("p02.csv", col_names = F)

temp = temp00+temp01+temp02
