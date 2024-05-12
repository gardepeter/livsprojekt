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
double leftIntegral(arma::cube& probabilities, double t0, int i, int j, double s, double d, double stepLength){ 
  double trapezoidalSum=0;
  for(int n=1; n< round(d/stepLength); n++){ // this is for the y-coordinate of the probability matrix
    // s=t0+xh 
    trapezoidalSum += (1/2)*(probabilities(round((s-t0)/stepLength),n,states * i + j)+probabilities(round((s-t0)/stepLength),n-1,states * i + j))
    *(intensityOutOfState(j,s,n*stepLength)+intensityOutOfState(j,s,stepLength*(n-1)));
  }
  return trapezoidalSum;
}

//TODO // the variable d is not in use and could be removed 
// the function should work as long as u=x*h and i have not checked if it works if not 
double trapezoidalRightEstimation(arma::cube& probabilities, double t0, double u, int i, int j, int l, double s, double stepLength){ // code is written in A(x,y,z) form
  double trapezoidalSum=0;
  for(int n=1; n< round((u+s-t0)/stepLength); n++){ // this is for the y-coordinate of the probability matrix
    // s=t0+xh 
    trapezoidalSum += (1/2)*(probabilities(round((s-t0)/stepLength),n,states * i + l)+probabilities(round((s-t0)/stepLength),n-1,states * i + l))
    *(mu(l,j,s,n*stepLength)+mu(l,j,s,stepLength*(n-1)));
  }
  return trapezoidalSum;
}

double rightSumOfIntegrals(arma::cube& probabilities, double t0, double u, int i, int j, double s, double stepLength){
  double rightSumResult=0;
  for(int l=0; l<states; l++){
    if(l !=j){
      rightSumResult += trapezoidalRightEstimation(probabilities, t0, u, i,j,l,s, stepLength);
    }
  }
  return 0.;
}

double transformationKolmogorov(arma::cube& probabilities, double t0, double u, int i, int j, double s, double d, double stepLength){
  return - leftIntegral(probabilities, t0, i, j, s, d, stepLength)
          + rightSumOfIntegrals(probabilities, t0,u , i, j, s, stepLength);
}

void RK1Step(arma::cube& probabilities, double startTime, double startDuration, int iteration, double stepLength, int states){
  for(int i = 0; i < states; i++){
    for(int j = 0; j < states; j++){
      for(unsigned int d = 0; d * stepLength < startDuration + iteration * stepLength; d++){
        probabilities(d, iteration, states * i + j) = probabilities(d, iteration - 1, states * i + j)
          + stepLength * transformationKolmogorov(probabilities, startTime, startDuration, i, j, startTime + stepLength * iteration, startTime + stepLength * iteration, stepLength);
      }
    }
  }
}

void boundaryCondition(arma::cube& probabilities, double startDuration, double stepLength){
  for(int dim = 0; dim < states; dim++){
    for(unsigned int d = 0; d * stepLength < startDuration; d++){
      probabilities(d, 0, (states + 1) * dim) = 1.;
    }
  }
}

bool isNotMultipla(double x, double y){
  return abs(x*y - trunc(x*y)) >= EPSILON;
}


//For debugging uses
// void saveCube(arma::cube& probabilities, int states){
//   probabilities.slice(0).save("p00.csv", arma::csv_ascii);
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
  if(endTime <= startTime || stepAmount <= 1 || isNotMultipla(startDuration, (double)stepAmount)){
    return  -1;
  }
  
  double stepLength = 1. / (double)stepAmount;
  
  int nrow = (int)round(startDuration * stepAmount) + stepAmount;
  int ncol = stepAmount;
  arma::cube probabilities(nrow, ncol, states * states);
  
  boundaryCondition(probabilities, startDuration, stepLength);
  
  for(int iteration = 1; iteration < stepAmount; iteration++){
    RK1Step(probabilities, startTime, startDuration, iteration, stepLength, states);
  }
  
  // saveCube(probabilities, states);
  return 0;
}

