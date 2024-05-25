#include "RcppArmadillo.h"
#include "MarkovIntensities.hpp"

double intensityOutOfState(int j, double s, double age){
  double sum = 0;
  
  for(int k = 0; k < states; k++){
    if(k == j){
      continue;
    }
    sum += mu(j, k, s, age);
  }
  
  return sum;
}

double sumOfIntensitiesWeightedByProbability(arma::mat& probabilities, int i, int j, double s, double age){
  double weightedSum = 0;
  
  for(int l = 0; l < states; l++){
    if(l == j){
      continue;
    }
    weightedSum += probabilities(i, l) * mu(l, j, s, age);
  }
  
  return weightedSum;
}

double transformationKolmogorov(arma::mat& probabilities, int i, int j, double s, double age){
  return - probabilities(i, j) * intensityOutOfState(j, s, age)
          + sumOfIntensitiesWeightedByProbability(probabilities, i, j, s, age);
}

arma::mat RK4Step(arma::mat& probabilities, double t, double stepLength, int states, double age){
  arma::mat k1(states, states);
  arma::mat k2(states, states);
  arma::mat k3(states, states);
  arma::mat k4(states, states);

  for(int i = 0; i < states; i++){
    for(int j = 0; j < states; j++){
      k1(i , j) = stepLength * transformationKolmogorov(probabilities, i, j, t, age);
    }
  }

  for(int i = 0; i < states; i++){
    for(int j = 0; j < states; j++){
      arma::mat tempProbabilities = probabilities + 0.5 * k1;
      k2(i , j) = stepLength * transformationKolmogorov(tempProbabilities, i, j, t + 0.5 * stepLength, age);
    }
  }

  for(int i = 0; i < states; i++){
    for(int j = 0; j < states; j++){
      arma::mat tempProbabilities = probabilities + 0.5 * k2;
      k3(i , j) = stepLength * transformationKolmogorov(tempProbabilities, i, j, t + 0.5 * stepLength, age);
    }
  }

  for(int i = 0; i < states; i++){
    for(int j = 0; j < states; j++){
      arma::mat tempProbabilities = probabilities + k3;
      k4(i , j) = stepLength * transformationKolmogorov(tempProbabilities, i, j, t + stepLength, age);
    }
  }

  arma::mat newProbabilities = probabilities + 0.125 * (k1 + 2*k2 + 2*k3 + k4);

  return newProbabilities;
}

arma::rowvec matrixToVector(arma::mat& matrix, int states){
  arma::rowvec res(states * states);
  
  for(int i = 0; i < states; i++){
    for(int j = 0; j < states; j++){
      res(states * i + j) = matrix(i, j);
    }
  }
  
  return res;
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat RK4(int startTime, int endTime, int stepAmount, double age) {
  loadCsvFile();
  if(endTime <= startTime || stepAmount <= 1){
    return arma::mat();
  }
  
  double stepLength = 1. / (double)stepAmount;
  
  arma::mat probabilities = arma::eye(states, states);
  arma::mat probabilitySaved(stepAmount, states * states);
  probabilitySaved.row(0) = matrixToVector(probabilities, states);
  
  for(int n = 1; n < stepAmount; n++){
    arma::mat tempMatrix = RK4Step(probabilities, startTime + n * stepLength, stepLength, states, age);
     for(int i = 0; i < states; i++){
       probabilities.row(i) = tempMatrix.row(i);
     }
     probabilitySaved.row(n) = matrixToVector(probabilities, states);
  }
  
  return probabilitySaved;
}



