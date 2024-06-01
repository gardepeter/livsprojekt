#include "RcppArmadillo.h"
#include "AggregateMarkovIntensities.hpp"
#include <iostream>
#include <cmath>
#include <algorithm>

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
arma::mat Pi(double t,double u, int macroStates){
  arma::mat pi(1,di(macroStates,scenario));
  //fill in pi
  //remember t in our notes is age + t
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
  arma::mat E(dbar,di(macroState,scenario));
  
  for(int j=0; j< di(macroState,scenario); j++){
    E.col(j).fill(1);
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

arma::mat initialCondition(double startTime,
                           double startDuration,
                           double age,
                           int stepAmountPerTimeUnit, 
                           int macroState, 
                           arma::cube& param){
  //we get the M_ii depending on the parameters we put in
  arma::mat tempVariable=
    Pi(startTime, startDuration, macroState)* 
    prodIntegralSolver(startTime-startDuration, startTime,age,stepAmountPerTimeUnit, param);
  arma::mat upperFractionPart= tempVariable*EMatrix(macroState).t();
  double lowerFractionPart = as_scalar(tempVariable* colOfOnes(macroState)); // nævneren og bør være en skalar
  
  return upperFractionPart/lowerFractionPart;
}

arma::mat leftSigmaProdIntegral(double s, double t,double age, int stepAmountPerTimeUnit, double karensPeriod, arma::cube& params){
   arma::mat result=prodIntegralSolver(s,std::max(t-karensPeriod,s), age, stepAmountPerTimeUnit, params);
  return result;
}

//this is being a bitch for some reason
arma::mat rightSigmaProdIntegral(double s, double t,double age, int stepAmountPerTimeUnit, double karensPeriod, int macroState, arma::cube& params){
  //the intensities should be for M_11
  arma::mat result=prodIntegralSolver(std::max(s-karensPeriod) ,t,s, age, stepAmountPerTimeUnit, params);
  return result;
}
arma::mat sigmaIntegral(double s, double t,double age, int stepAmountPerTimeUnit, double karensPeriod, int macroState, arma::cube& params){
  arma:: mat result=leftSigmaProdIntegral(s,t,age,stepAmountPerTimeUnit,karensPeriod,params)*
                    EMatrix(1)*//make some changes so it gets the right parameters
                    rightSigmaProdIntegral(s,t,age,stepAmountPerTimeUnit, karensPeriod,macroState,params);
  return result;
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