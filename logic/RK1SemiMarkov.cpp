#include "RcppArmadillo.h"
#include "SemiMarkovIntensities.hpp"
const double EPSILON = 0.00001;

double intensityOutOfState(int j, double s, double d, double age){
  double sum = 0;
  
  for(int k = 0; k < states; k++){
    if(k == j){
      continue;
    }
    sum += mu(j, k, s, d, age);
  }
  
  return sum;
}

// d=x*h
double leftIntegral(arma::cube& probabilities, double t0, int i, int j, double s, int d_step, double stepLength, int iteration, double age){ 
  double trapezoidalSum=0;
  for(int n=1; n <= d_step; n++){ // this is for the y-coordinate of the probability matrix
    // s=t0+xh 
    trapezoidalSum += 0.5*(probabilities(n, iteration, states * i + j) - probabilities(n-1, iteration, states * i + j))
    *(intensityOutOfState(j,s,n*stepLength, age)+intensityOutOfState(j,s,stepLength*(n-1), age));
  }
  return trapezoidalSum;
}

//TODO // the variable d is not in use and could be removed 
// the function should work as long as u=x*h and i have not checked if it works if not 
double trapezoidalRightEstimation(arma::cube& probabilities, double t0, double u, int i, int j, int l, double s, double stepLength, int iteration, double age){ 
  double trapezoidalSum=0;
  for(int n=1; n <= round((u+s-t0)/stepLength); n++){ // this is for the y-coordinate of the probability matrix
    // s=t0+xh 
    trapezoidalSum += 0.5*(probabilities(n, iteration, states * i + l) - probabilities(n-1, iteration, states * i + l))
    *(mu(l,j,s,n*stepLength, age)+mu(l,j,s,stepLength*(n-1), age));
  }
  return trapezoidalSum;
}

double rightSumOfIntegrals(arma::cube& probabilities, double t0, double u, int i, int j, double s, double stepLength, int iteration, double age){
  double rightSumResult = 0;
  for(int l = 0; l < states; l++){
    if(l == j){
      continue;
    }
    rightSumResult += trapezoidalRightEstimation(probabilities, t0, u, i, j, l, s, stepLength, iteration, age);
  }
  return rightSumResult;
}

void RK1Step(arma::cube& probabilities, double startTime, double startDuration, int iteration, double stepLength, int states, double age){
  for(int i = 0; i < states; i++){
    for(int j = 0; j < states; j++){
      double rightSum = rightSumOfIntegrals(probabilities, startTime, startDuration, i, j, startTime + stepLength * (iteration  - 1), stepLength, iteration - 1, age);
      
      for(int d_step = 1; d_step <= (int)floor(startDuration/stepLength) + iteration; d_step++){
        probabilities(d_step, iteration, states * i + j) = probabilities(d_step - 1, iteration - 1, states * i + j)
          + stepLength * ( - leftIntegral(probabilities, startDuration, i, j, startTime + stepLength * iteration, d_step-1, stepLength, iteration - 1, age) + rightSum); //TODO investegate if d_step - 1 or not
      }
    }
  }
}

void boundaryCondition(arma::cube& probabilities, int locationOfOne){
  for(int dim = 0; dim < states; dim++){
    probabilities(locationOfOne, 0, (states + 1) * dim) = 1.;
  }
}

bool isNotMultipla(double x, double y){
  return abs(x*y - trunc(x*y)) >= EPSILON;
}

void saveCube(arma::cube& probabilities, int states){
  for(int i = 0; i < states - 1; i++){
    for(int j = 0; j < states; j++){
      probabilities.slice(states * i + j).save("p" + std::to_string(i) + std::to_string(j) + ".csv", arma::csv_ascii);
    }
  }
}

arma::cube RK1_Cpp(double startTime, double startDuration, double endTime, int stepAmountPerTimeUnit, double age) {
  double stepLength = 1. / (double)stepAmountPerTimeUnit;
  
  int stepsFromZeroToStartDuration = (int)round(startDuration * stepAmountPerTimeUnit);
  int nrow = stepsFromZeroToStartDuration + stepAmountPerTimeUnit * (endTime - startTime);
  int ncol = stepAmountPerTimeUnit * (endTime - startTime);
  arma::cube probabilities(nrow, ncol, states * states);
  
  boundaryCondition(probabilities, stepsFromZeroToStartDuration );
  
  for(int iteration = 1; iteration < stepAmountPerTimeUnit * (endTime - startTime); iteration++){
    RK1Step(probabilities, startTime, startDuration, iteration, stepLength, states, age);
  }

  return probabilities;
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
int RK1(double startTime, double startDuration, double endTime, int stepAmountPerTimeUnit, double age) {
  if( endTime <= startTime ||  stepAmountPerTimeUnit <= 1 || isNotMultipla(startDuration, (double)stepAmountPerTimeUnit)){
    return -1;
  }
  
  arma::cube probabilities = RK1_Cpp(startTime, startDuration, endTime, stepAmountPerTimeUnit, age);
  
  saveCube(probabilities, states);
  return 0;
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::vec unitCashflowDisabilityWithKarens(double startTime, double startDuration, double endTime, int stepAmountPerTimeUnit, double gracePeriod, double age, int i, int j) {
  arma::cube probabilities = RK1_Cpp(startTime, startDuration, endTime, stepAmountPerTimeUnit, age);
  
  arma::mat P11 =  probabilities.slice(states * i + j);
  
  int cashflowLength = stepAmountPerTimeUnit * (endTime - startTime);
  int stepsFromZeroToStartDuration = (int)round(startDuration * stepAmountPerTimeUnit);
  
  arma::vec cashflow(cashflowLength);
  for(int i = 0; i < cashflowLength; i++ ){
    if(stepsFromZeroToStartDuration + i < gracePeriod + 1){
      continue;
    }
    cashflow(i) = P11(stepsFromZeroToStartDuration + i, i) - P11(gracePeriod + 1, i); // Strict ineq. as gracePeriod <= 3 month
  }
  
  return cashflow;
}






