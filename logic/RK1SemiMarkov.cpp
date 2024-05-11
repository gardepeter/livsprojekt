#include "RcppArmadillo.h"
#include "SemiMarkovIntensities.hpp"
double stepSize=1/12; //midlertidigt for at se ting virker
double u=12; 

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
double leftIntegral(arma::cube& probabilities, int t0, int i, int j, double s, double d){
  return 0.;
}

//TODO
/*
double rightSumOfIntegrals(arma::cube& probabilities, int i, int j, double s, double d){
  return 0.;
}
*/
//TODO (need to choose right probs though) (for right integral (needed l)) if this doesn't work i cry
double trapezoidalRightEstimation(arma::cube& probabilities, int t0, int i, int j, int l, double s, double d){
  double trapezoidalSum=0;
  for(int n=1; n< floor(d/stepSize); n++){
    //posiable problems: n=0 and we get a problem with (n-1) part. so look changed to start from 1
    //trapezoidalSum += (1/2)*(probabilities.slice(states * i + l)[,std::round((s-t_0)/stepSize)-1]+probabilities.slice(states * i + l)[,std::round((s-t_0)/stepSize)-1])
    //*(mu(l,j,s,n*stepSize)-mu(l,j,s,stepSize*(n-1)));
  }
  return 0.;
  //return trapezoidalSum
}

//TODO EASY!!!
double rightSumOfIntegrals(arma::cube& probabilities, int t0, int i, int j, double s, double d){
  double rightSumResult=0;
  for(int l=0; l<states; l++){
    if(l !=j){
      rightSumResult += trapezoidalRightEstimation(probabilities, t0, i,j,l,s,d);
    }
  }
  return 0.;
}




//TODO
double transformationKolmogorov(arma::cube& probabilities, int t0, int i, int j, double s, double d){
  return - leftIntegral(probabilities, t0, i, j, s, d)
          + rightSumOfIntegrals(probabilities, t0, i, j, s, d);
}

//TODO
void RK1Step(arma::cube& probabilities, int t0, int startDuration, double t, double stepLength, int states){
  
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
void RK1(int startTime, int startDuration, int endTime, int stepAmount) {
  if(endTime <= startTime || stepAmount <= 1){
    return;
  }
  
  double stepLength = 1. / (double)stepAmount;
  
  int durationStepAmount = stepAmount + ceil(stepAmount / startDuration); //TODO, maybe not the correct amount of steps in duration axis.
  arma::cube probabilities(stepAmount, durationStepAmount, states * states);
  
  for(int n = 1; n < stepAmount; n++){
    RK1Step(probabilities, startTime, startDuration, startTime + n * stepLength, stepLength, states);
  }
  
  saveCube(probabilities, states);
}

