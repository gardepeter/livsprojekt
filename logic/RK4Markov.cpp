#include "RcppArmadillo.h"
#include "MarkovIntensities.hpp"

double intensityOutOfState(int j, double s){
  double sum = 0;
  
  for(int k = 0; k < states; k++){
    if(k == j){
      continue;
    }
    sum += mu(j, k, s);
  }
  
  return sum;
}

double sumOfIntensitiesWeightedByProbability(arma::mat& probabilities, int i, int j, double s){
  double weightedSum = 0;
  
  for(int l = 0; l < states; l++){
    if(l == j){
      continue;
    }
    weightedSum += probabilities(i, l) * mu(l, j, s);
  }
  
  return weightedSum;
}

double transformationKolmogorov(arma::mat& probabilities, int i, int j, double s){
  return - probabilities(i, j) * intensityOutOfState(j, s)
          + sumOfIntensitiesWeightedByProbability(probabilities, i, j, s);
}

arma::mat RK4Step(arma::mat& probabilities, double t, double stepLength, int states){
  arma::mat k1(states, states);
  arma::mat k2(states, states);
  arma::mat k3(states, states);
  arma::mat k4(states, states);

  for(int i = 0; i < states; i++){
    for(int j = 0; j < states; j++){
      if(i == j){
        k1(i , j) = 0.;
      }
      k1(i , j) = stepLength * transformationKolmogorov(probabilities, i, j, t);
    }
  }

  for(int i = 0; i < states; i++){
    for(int j = 0; j < states; j++){
      if(i == j){
        k2(i , j) = 0.;
      }
      arma::mat tempProbabilities = probabilities + 0.5 * k1;
      k2(i , j) = stepLength * transformationKolmogorov(tempProbabilities, i, j, t + 0.5 * stepLength);
    }
  }

  for(int i = 0; i < states; i++){
    for(int j = 0; j < states; j++){
      if(i == j){
        k3(i , j) = 0.;
      }
      arma::mat tempProbabilities = probabilities + 0.5 * k2;
      k3(i , j) = stepLength * transformationKolmogorov(tempProbabilities, i, j, t + 0.5 * stepLength);
    }
  }

  for(int i = 0; i < states; i++){
    for(int j = 0; j < states; j++){
      if(i == j){
        k4(i , j) = 0.;
      }
      arma::mat tempProbabilities = probabilities + k3;
      k4(i , j) = stepLength * transformationKolmogorov(tempProbabilities, i, j, t + stepLength);
    }
  }

  arma::mat newProbabilities = probabilities + 0.125 * (k1 + 2*k2 + 2*k3 + k4);

  for(int i = 0; i < states; i++){
    double rowSum = 1.;
    for(int j = 0; j < states; j++){
      if(i == j){
        continue;
      }
      rowSum -= newProbabilities(i, j);
    }
    newProbabilities(i, i) = rowSum;
  }

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
arma::mat RK4(int startTime, int endTime, int stepAmount) {
  //loadCsvFile(); 
  if(endTime <= startTime || stepAmount <= 1){
    return arma::mat();
  }
  
  double stepLength = 1. / (double)stepAmount;
  
  arma::mat probabilities = arma::eye(states, states);
  arma::mat probabilitySaved(stepAmount, states * states);
  probabilitySaved.row(0) = matrixToVector(probabilities, states);
  
  for(int n = 1; n < stepAmount; n++){
    arma::mat tempMatrix = RK4Step(probabilities, startTime + n * stepLength, stepLength, states);
     for(int i = 0; i < states; i++){
       probabilities.row(i) = tempMatrix.row(i);
     }
     probabilitySaved.row(n) = matrixToVector(probabilities, states);
  }
  
  return probabilitySaved;
}



