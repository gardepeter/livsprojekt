#include "RcppArmadillo.h"
#include "SemiMarkovIntensities.h"

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

//TODO
double trapezoidalEstimation(arma::cube& probabilities, int i, int j, double s, double d){
  return 0.;
}

//TODO
double leftIntegral(arma::cube& probabilities, int i, int j, double s, double d){
  return 0.;
}

//TODO
double rightSumOfIntegrals(arma::cube& probabilities, int i, int j, double s, double d){
  return 0.;
}

//TODO
double transformationKolmogorov(arma::cube& probabilities, int i, int j, double s, double d){
  return - leftIntegral(probabilities, i, j, s, d)
          + rightSumOfIntegrals(probabilities, i, j, s, d);
}

//TODO
void RK1Step(arma::cube& probabilities, double t, double stepLength, int states){
  
}

void saveCube(arma::cube& probabilities, int states){
  for(int i = 0; i < states; i++){
    for(int j = 0; j < states; j++){
      probabilities.slice(states * i + j).save("p" + std::to_string(i) + std::to_string(j) + ".csv", arma::csv_ascii);
    }
  }
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
void RK1(int startTime, int endTime, int stepAmount) {
  if(endTime <= startTime || stepAmount <= 1){
    return;
  }
  
  double stepLength = 1. / (double)stepAmount;
  
  arma::cube probabilities;
  
  for(int n = 1; n < stepAmount; n++){
    RK1Step(probabilities, startTime + n * stepLength, stepLength, states);
  }
  
  saveCube(probabilities, states);
}
