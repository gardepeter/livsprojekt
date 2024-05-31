#include "RcppArmadillo.h"
#include "AggregateMarkovIntensities.hpp"
#include <iostream>
#include <cmath>

const int scenario=2;


arma::mat prodIntegralSolver(double s, double t, double age, int stepAmountPerTimeUnit, arma::cube& param){
  double stepLength = 1/(double)stepAmountPerTimeUnit;
  int stepAmount = (int)round((t - s) * stepAmountPerTimeUnit);
  
  arma::mat res = arma::eye(param.n_rows, param.n_rows);
  arma::cube k(param.n_rows, param.n_rows, 4);
  for(int iteration = 1; iteration < stepAmount + 1; iteration++){
    k.slice(0) = res * intensityMatrix(age, s + stepLength * (double)(iteration - 1), param ) * stepLength;
    k.slice(1) = (res + 0.5 * k.slice(0)) * intensityMatrix(age, s + stepLength * (double)(iteration - 1) + 0.5 * stepLength, param ) * stepLength;
    k.slice(2) = (res + 0.5 * k.slice(1)) * intensityMatrix(age, s + stepLength * (double)(iteration - 1) + 0.5 * stepLength, param ) * stepLength;
    k.slice(3) = (res + k.slice(2)) * intensityMatrix(age, s + stepLength * (double)iteration, param ) * stepLength;
    
    res += 0.125 * (k.slice(0) + 2 * k.slice(1) + 2 * k.slice(2) + k.slice(3));
  }
  
  return res;
}

//stolen from previous version... is it going to be deleted?
int di(int macrostate, int scenario){
  //sick
  if(macrostate != 1){
    return 1;
  }
  
  arma::vec microStateMapping = {1, 2, 3, 5, 7, 10};
  return microStateMapping(scenario - 1);
}
arma::mat Pi(int t,int u, int macroStates){
  arma::mat pi(1,di(macroStates,scenario));
  //fill in pi
  //
  //
  return pi;
}
arma::mat colOfOnes(int macroState){
  arma::mat columnVector(di(macroState,scenario),1);
  for(int i=0; i<di(macroState,scenario);i++){
    columnVector(i,1)=1;
  }
  return columnVector;
}
arma::mat EMatrix(int macroState){
  int dbar=0;
  for(int i=0; i<states;i++){
    dbar +=di(i,scenario);
  }
  arma::mat E(1,1);
  for(int jtilde=0; jtilde<di(macroState,scenario);jtilde++){
    //need to put 1 at the right spot
    
    arma::mat ejfat(dbar,1);
    
    arma::mat ejtilde(di(macroState,scenario),1);
    
  }
  return E;
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat testBeta(double s, double t, double age, int stepAmountPerTimeUnit){
  loadBeta(5);
  return prodIntegralSolver(s, t, age, stepAmountPerTimeUnit, beta);
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat testBetaFull(double s, double t, double age, int stepAmountPerTimeUnit){
  loadBeta(5);
  return prodIntegralSolver(s, t, age, stepAmountPerTimeUnit, betaFull);
}

[[Rcpp::depends(RcppArmadillo)]]
[[Rcpp::export]]
arma::mat test2(double age, double x){
  loadBeta(5);
  return intensityMatrix(age, x, beta);
}

// arma::mat A(double x){
//   arma::mat temp(2,2);
//   temp(0,1) = 2*x / 100;
//   temp(1,0) = 4*pow(x, 2) / 100;
//   temp(0,0) = -temp(0,1);
//   temp(1,1) = -temp(1,0);
//   return temp;
// }
// 
// // [[Rcpp::depends(RcppArmadillo)]]
// // [[Rcpp::export]]
// arma::mat prodIntegralSolverTemp(double s, double t, double age, int stepAmountPerTimeUnit){
//   double stepLength = 1 /(double)stepAmountPerTimeUnit;
//   int stepAmount = (int)round((t - s) * stepAmountPerTimeUnit);
//   
//   arma::mat res = arma::eye(2, 2);
//   
//   for(int iteration = 1; iteration < stepAmount + 1; iteration++){
//     res += res * A(s + stepLength * (double)(iteration - 1)) * stepLength;
//   }
//   
//   return res;
// }l