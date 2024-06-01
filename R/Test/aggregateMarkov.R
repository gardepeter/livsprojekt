library(tidyverse)
options(scipen = 9)
load("./data/resources/fits.rda")

################### CODEGEN FOR M_jj ###############################
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

################### CODEGEN FOR PI ###############################

for(fit_i in 2:6){
  microStates = c(2,3,5,7,10)[fit_i - 1]
    temp = coef(fits[[fit_i]]$initial_fits[[2]])
    write.table( temp, paste0("./data/resources/startCondition/etaMicroStates", microStates, ".csv"), sep=",",  col.names=F, row.names = F)
}

################### CODEGEN FOR M ###############################
for(fit_i in 3:3){
  microStates = c(2,3,5,7,10)[fit_i - 1]
    temp = matrix(nrow = microStates + 2, ncol = microStates + 2, 0)
    for(csv_i in 1:2){
    
    for(i in 1:microStates){
      for(j in 1:microStates){
        if(i == j) next
        temp[i + 1, j + 1] = microStateIntensity(fit_i, 2, i, j)[csv_i]
      }
    }
      
    for(i in 1:(microStates + 2)){
      macroStateFrom = 0
      mIcroStateFrom = 0
      if(i == 1){
        macroStateFrom = 1
        mIcroStateFrom = 1
      } else if(i == microStates + 2){
        macroStateFrom = 3
        mIcroStateFrom = 1
      } else {
        macroStateFrom = 2
        mIcroStateFrom = i - 1
      }
      
      for(j in 1:(microStates + 2)){
        if(i == j || macroStateFrom == 3 ) next
        
        macroStateTo = 0
        if(j == 1){
          macroStateTo = 1
          mIcroStateTo = 1
        } else if(j == microStates + 2){
          macroStateTo = 3
          mIcroStateTo = 1
        } else {
          macroStateTo = 2
          mIcroStateTo = j - 1
        }
        if(macroStateTo == macroStateFrom ) next
        
        glmObject = fits[[fit_i]]$exit_fits[[macroStateFrom]][[macroStateTo]][[mIcroStateFrom]]

        temp[i, j] = glmObject[[1]][csv_i]
      }
    }
       write.table( temp, paste0("./data/resources/full/fullIntensityBeta", csv_i - 1, "microStates", microStates, ".csv"), sep=",",  col.names=F, row.names = F)
  }
}

########################### CODEGEN TEST ####################################

fromMacroState = 1
toMacroState = 2
fromMicroState = 1
fits[[4]]$exit_fits[[fromMacroState]][[toMacroState]]

predict(glm, tibble(x = 36, exposure = 1), type = "response")

fits[[2]]$M[[1]]

############################# AGG. MARKOV TEST ##########################
# Rcpp::sourceCpp("logic/AggregateMarkov.cpp")
# test()
# 
# temp0 = fits[[3]]$A_big[[1]][[2]]
# temp = matrix(nrow = 3, ncol = 3, 0)
# for(i in 1:4){
#   temp[c(1,1,3,3)[i],c(1,3,1,3)[i]] = temp0[c(1,1,5,5)[i], c(1,5,1,5)[i]]
# }
# temp[2,2] = sum(temp0[2:4,2:4])/3

Rcpp::sourceCpp("logic/AggregateMarkov.cpp")
testingIntMat()
testingIntSubMat()
t = 0
s = 50
grace = 1/4
age = 50

termA = t(matrix(nrow = 7, ncol = 1, 1)) %*% matrix(nrow = 7, ncol = 5, 1) %*% testBeta(t, s, age, 12)
termB = t(matrix(nrow = 7, ncol = 1, 1)) %*% testBetaFull(t, s - grace, age, 12) %*% matrix(nrow = 7, ncol = 5, 1) %*% testBeta(s - grace, s, age, 12)

- (termA %*% c(1,0,0,0,0) - termB %*% c(1,0,0,0,0)) - (termA %*% c(0,1,0,0,0) - termB %*% c(0,1,0,0,0))
- (termA %*% c(0,0,1,0,0) - termB %*% c(0,0,1,0,0)) - (termA %*% c(0,0,0,1,0) - termB %*% c(0,0,0,1,0))
- (termA %*% c(0,0,0,0,1) - termB %*% c(0,0,0,0,1)) 

testBeta(t, s, age, 12)


prodIntegralSolverTemp(0, 40, 0, 100)






