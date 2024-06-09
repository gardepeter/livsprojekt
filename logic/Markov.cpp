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

arma::mat prodIntegralSolver(double s, double t, double age, int stepAmount, int states){
  if(std::abs(t - s) < EPSILON){
    return arma::eye(states, states);
  }
  double stepLength = (t - s) / (double)stepAmount;
  
  arma::mat res = arma::eye(states, states);
  arma::mat k( states, states );
  for(int iteration = 1; iteration < stepAmount + 1; iteration++){
    res += res * markovIntensityMatrix(age, s + stepLength * (double)(iteration - 1)) * stepLength;
  }
  
  return res;
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
    
    double max = (stepAmountLength * ((double)n) - gracePeriod) > startTime ? (stepAmountLength * ((double)n) - gracePeriod) : startTime;
    arma::mat probabilityFactor =  prodIntegralSolver(max, stepAmountLength * ((double)n), age, (int)round(10 * (stepAmountLength * ((double)n) - max) ) + 1, states);
    
    cashflow(n, 1) = probabilities((int)( (max - startTime) * stepAmountPerTimeUnit), states * i + j)
      *  probabilityFactor(j, j);
  }

  return cashflow;
}


