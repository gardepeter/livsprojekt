library(tidyverse)

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

