#include "RcppArmadillo.h"
#include "AggregateMarkovIntensities.hpp"
#include <iostream>

arma::mat prodIntegralStep(arma::mat& priorMatrix, double x, double stepLength, arma::cube& param, double age){
  return priorMatrix + stepLength * intensityMatrix(age, x * stepLength, param );
}

arma::mat prodIntegralSolver(double s, double t, double age, int stepAmountPerTimeUnit, arma::cube& param){
  double stepLength = 1/(double)stepAmountPerTimeUnit;
  int stepAmount = (int)round((t - s) * stepAmountPerTimeUnit);
  
  arma::mat res = arma::eye(param.n_rows, param.n_rows);
  
  for(int iteration = 1; iteration < stepAmount; iteration++){
    res = prodIntegralStep(res, (double)iteration, stepLength, param, age);
  }
  
  return res;
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat test(double s, double t, double age, int stepAmountPerTimeUnit){
  loadBeta(5);
  return prodIntegralSolver(s, t, age, stepAmountPerTimeUnit, beta);
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat test2(double age, double x){
  loadBeta(5);
  return intensityMatrix(age, x, beta);
}
