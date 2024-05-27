library(tidyverse)
options(scipen = 9)
load("./data/resources/fits.rda")

microStateIntensity = function(microStateAmount, macroState, iMicroState, jMicroState){
  glmObject = fits[[microStateAmount]]$micro_fits[[macroState]][[iMicroState]][[jMicroState]]
  return( glmObject[[1]] )
}

for(fit_i in 2:6){
  microStates = c(2,3,5,7,10)[fit_i - 1]
  for(csv_i in 1:2){
    temp = matrix(nrow = microStates, ncol = microStates, 0)
    for(i in 1:microStates){
      for(j in 1:microStates){
        if(i == j) next
        temp[i, j] = microStateIntensity(fit_i, macroState, i, j)[csv_i]
      }
    }
    write.table( temp, paste0("./data/resources/beta", csv_i - 1, "microStates", microStates, ".csv"), sep=",",  col.names=F, row.names = F)
  }
}


Rcpp::sourceCpp("logic/AggregateMarkov.cpp")
test()

temp0 = fits[[3]]$A_big[[1]][[2]]
temp = matrix(nrow = 3, ncol = 3, 0)
for(i in 1:4){
  temp[c(1,1,3,3)[i],c(1,3,1,3)[i]] = temp0[c(1,1,5,5)[i], c(1,5,1,5)[i]]
}
temp[2,2] = sum(temp0[2:4,2:4])/3

fromMacroState = 2
toMacroState = 3
fromMicroState = 1
glm = fits[[2]]$exit_fits[[fromMacroState]][[toMacroState]][[fromMicroState]]
predict(glm, tibble(x = 36, exposure = 1), type = "response")

fits[[2]]$M[[1]]

Rcpp::sourceCpp("logic/AggregateMarkovV2.cpp")
test(3, 10, 50, 52)

