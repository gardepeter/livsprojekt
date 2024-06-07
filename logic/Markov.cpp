#include "RcppArmadillo.h"
#include "hpp/MarkovIntensities.hpp"

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

arma::rowvec matrixToVector(arma::mat& matrix, int states){
  arma::rowvec res(states * states);
  
  for(int i = 0; i < states; i++){
    for(int j = 0; j < states; j++){
      res(states * i + j) = matrix(i, j);
    }
  }
  
  return res;
}

arma::mat RK1Step(arma::mat& probabilities, double s, double stepLength, int states, double age){
  arma::mat delta(states, states);
  
  for(int i = 0; i < states; i++){
    for(int j = 0; j < states; j++){
      delta(i , j) = transformationKolmogorov(probabilities, i, j, s, age);
    }
  }
  
  return probabilities + stepLength * delta;
}

arma::mat markovTransitionProbabilities(int startTime, int endTime, int stepAmountPerTimeUnit, double age, bool forward){
  loadCsvFile();

  double stepLength = 1. / (double)stepAmountPerTimeUnit;
  int stepAmount = std::abs(endTime - startTime) * stepAmountPerTimeUnit;
  
  arma::mat probabilities = arma::eye(states, states);
  arma::mat probabilitySaved(stepAmount, states * states);
  probabilitySaved.row(0) = matrixToVector(probabilities, states);
  
  for(int n = 1; n < stepAmount; n++){
    arma::mat tempMatrix = RK1Step(probabilities, (forward ? 1. : -1.) * (n - 1) * stepLength, stepLength, states, age + (forward ? 0. : (double)startTime));
    for(int i = 0; i < states; i++){
      probabilities.row(i) = tempMatrix.row(i);
    }
    probabilitySaved.row(n) = matrixToVector(probabilities, states);
  }
  
  return probabilitySaved;
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat markovTransitionProbabilities(int startTime, int endTime, int stepAmountPerTimeUnit, double age) {
  if(endTime <= startTime || stepAmountPerTimeUnit <= 1){
    return arma::mat();
  }
  
  return markovTransitionProbabilities(startTime, endTime, stepAmountPerTimeUnit, age, true);
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat markovDisabilityUnitBenefitCashflow(int startTime, double startDuration, int endTime, int stepAmountPerTimeUnit, double age, double gracePeriod, int i, int j) {
  loadCsvFile();
  arma::mat probabilities = markovTransitionProbabilities(startTime, endTime, stepAmountPerTimeUnit, age);
  
  
  int cashflowSteps = stepAmountPerTimeUnit * (endTime - startTime);
  double stepAmountLength = 1 / (double)stepAmountPerTimeUnit;
  int stepsFromZeroToStartDuration = (int)round(startDuration * stepAmountPerTimeUnit);
  int gracePeriodSteps = (int)round((double)stepAmountPerTimeUnit * gracePeriod) + 1; 

  arma::mat probabilitiesBackward = markovTransitionProbabilities(startTime, startTime - gracePeriodSteps, stepAmountPerTimeUnit, age, false);
  
  arma::mat cashflow(cashflowSteps, 2);
  for(int n = 0; n < cashflowSteps; n++ ){
    cashflow(n, 0) = n * stepAmountLength;
    
    if(age + n * stepAmountLength >= RETIREMENT_AGE){
      break;
    }
    
    if( n % (int)round( cashflowSteps * 0.01 ) == 0){
      progressBar((double)n / (double)cashflowSteps);
    }
    
    if(stepsFromZeroToStartDuration + n < gracePeriodSteps ){
      continue;
    }
    cashflow(n, 1) = probabilities(std::max(n, startTime), states * i + j)
      * (n < startTime ? probabilitiesBackward( n, states * j + j) : 1.)
      * probabilities(n, states * j + j);
  }

  return cashflow;
}


