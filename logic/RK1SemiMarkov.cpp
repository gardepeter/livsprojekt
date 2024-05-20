#include "RcppArmadillo.h"
#include "SemiMarkovIntensities.hpp"
const double EPSILON = 0.00001;

double intensityOutOfState(int j, double s, double d){
  double sum = 0;
  
  for(int k = 0; k < states; k++){
    if(k == j){
      continue;
    }
    sum += mu(j, k, s, d);
  }
  
  return sum;
}

// d=x*h
double leftIntegral(arma::cube& probabilities, double t0, int i, int j, double s, int d_step, double stepLength, int iteration){ 
  double trapezoidalSum=0;
  for(int n=1; n <= d_step; n++){ // this is for the y-coordinate of the probability matrix
    // s=t0+xh 
    trapezoidalSum += 0.5*(probabilities(n, iteration, states * i + j) - probabilities(n-1, iteration, states * i + j))
    *(intensityOutOfState(j,s,n*stepLength)+intensityOutOfState(j,s,stepLength*(n-1)));
  }
  return trapezoidalSum;
}

//TODO // the variable d is not in use and could be removed 
// the function should work as long as u=x*h and i have not checked if it works if not 
double trapezoidalRightEstimation(arma::cube& probabilities, double t0, double u, int i, int j, int l, double s, double stepLength, int iteration){ 
  double trapezoidalSum=0;
  for(int n=1; n< round((u+s-t0)/stepLength); n++){ // this is for the y-coordinate of the probability matrix
    // s=t0+xh 
    trapezoidalSum += 0.5*(probabilities(n, iteration, states * i + l) - probabilities(n-1, iteration, states * i + l))
    *(mu(l,j,s,n*stepLength)+mu(l,j,s,stepLength*(n-1)));
  }
  return trapezoidalSum;
}

double rightSumOfIntegrals(arma::cube& probabilities, double t0, double u, int i, int j, double s, double stepLength, int iteration){
  double rightSumResult = 0;
  for(int l = 0; l < states; l++){
    if(l == j){
      continue;
    }
    rightSumResult += trapezoidalRightEstimation(probabilities, t0, u, i, j, l, s, stepLength, iteration);
  }
  return rightSumResult;
}

void RK1Step(arma::cube& probabilities, double startTime, double startDuration, int iteration, double stepLength, int states){
  for(int i = 0; i < states; i++){
    for(int j = 0; j < states; j++){
      double rightSum = rightSumOfIntegrals(probabilities, startTime, startDuration, i, j, startTime + stepLength * (iteration  - 1), stepLength, iteration - 1);
      
      for(int d_step = 1; d_step < (int)floor(startDuration/stepLength) + iteration; d_step++){
        probabilities(d_step, iteration, states * i + j) = probabilities(d_step - 1, iteration - 1, states * i + j)
          + stepLength * ( - leftIntegral(probabilities, startDuration, i, j, startTime + stepLength * (d_step-1), (d_step-1), stepLength, iteration - 1) + rightSum); //TODO investegate if d_step - 1 or not
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


//For debugging uses
// void saveCube(arma::cube& probabilities, int states){
//   for(int j = 0; j < states; j++){
//       probabilities.slice(j).save("p0" + std::to_string(j) + ".csv", arma::csv_ascii);
//   }
// }
void saveCube(arma::cube& probabilities, int states){
  for(int i = 0; i < states; i++){
    for(int j = 0; j < states; j++){
      probabilities.slice(states * i + j).save("p" + std::to_string(i) + std::to_string(j) + ".csv", arma::csv_ascii);
    }
  }
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
int RK1(double startTime, double startDuration, double endTime, int stepAmount) {
  if( endTime <= startTime ||  stepAmount <= 1 || isNotMultipla(startDuration, (double)stepAmount)){
    return -1;
  }

  double stepLength = 1. / (double)stepAmount;
  
  int stepsFromZeroToStartDuration = (int)round(startDuration * stepAmount);
  int nrow = stepsFromZeroToStartDuration + stepAmount;
  int ncol = stepAmount;
  arma::cube probabilities(nrow, ncol, states * states);
  
  boundaryCondition(probabilities, stepsFromZeroToStartDuration);
  
  for(int iteration = 1; iteration < stepAmount; iteration++){
    RK1Step(probabilities, startTime, startDuration, iteration, stepLength, states);
  }
  
  saveCube(probabilities, states);
  return 0;
}

