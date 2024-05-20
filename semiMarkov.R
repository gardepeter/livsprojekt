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

temp_vector_00 = f(temp00, startIncrement, stepAmount)
temp_vector_01 = f(temp01, startIncrement, stepAmount)
temp_vector_02 = f(temp02, startIncrement, stepAmount)

temp_vec = temp_vector_00 + temp_vector_01 + temp_vector_02

sum(temp_vec < 0.9) #Should be 
length(temp_vec)

temp10 = readr::read_csv("p10.csv", col_names = F)
temp11 = readr::read_csv("p11.csv", col_names = F)
temp12 = readr::read_csv("p12.csv", col_names = F)

temp_vector_10 = f(temp10, startIncrement, stepAmount)
temp_vector_11 = f(temp11, startIncrement, stepAmount)
temp_vector_12 = f(temp12, startIncrement, stepAmount)

temp_vec1 = temp_vector_10 + temp_vector_11 + temp_vector_12

#Dødeintensiter for semi markov
# deathInt = readxl::read_excel("data/deathIntensity.xlsx")
# intensities = unlist(deathInt[21:111,14])
# 
# for(age in 20 + 0:(length(intensities) - 1) ){
#   print(paste0("else if( ", age, " <= x < ", age + 1, " ){"))
#   print(paste0("   return ", intensities[age - 19], ";"))
#   print(paste0("}"))
# }

#DURATION zero test

startTime = 0.0
endTime = 5.0
stepAmount = 12 * (endTime - startTime)
startIncrement = 0
startDuration = startIncrement / stepAmount

RK1(startTime, startDuration, endTime, stepAmount)
