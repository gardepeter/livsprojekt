#options(scipen = 99)
Rcpp::sourceCpp("logic/RK1SemiMarkov.cpp")

startTime = 5.0
endTime = 45.0
stepAmount = 25 * (endTime - startTime) #TODO run with 52 (weeks in a year)
startIncrement = 12
startDuration = startIncrement / stepAmount

RK1(startTime, startDuration, endTime, stepAmount)

temp00 = readr::read_csv("p00.csv", col_names = F)
temp01 = readr::read_csv("p01.csv", col_names = F)
temp02 = readr::read_csv("p02.csv", col_names = F)


f = function(matrix, index, amount){
  vector = c()
  for(i in 1:amount){
    vector[i] = matrix[index + i - 1, i]
  }
  return(unlist(vector))
}

iterations = stepAmount
temp_vector_00 = f(temp00, startIncrement, iterations)
temp_vector_01 = f(temp01, 1, iterations)
temp_vector_02 = f(temp02, 1, iterations)

temp_vec = temp_vector_00 + temp_vector_01 + temp_vector_02

sum(temp_vec < 0.9) #Should be 
length(temp_vec)

