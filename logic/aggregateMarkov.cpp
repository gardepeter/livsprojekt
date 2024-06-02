#include "RcppArmadillo.h"
#include "AggregateMarkovIntensities.hpp"
#include <iostream>
#include <cmath>
#include <algorithm>

arma::mat prodIntegralSolver(double s, double t, double age, int stepAmountPerTimeUnit, int microStateAmount, arma::cube& beta, arma::mat& eta){
  double stepLength = 1/(double)stepAmountPerTimeUnit;
  int stepAmount = (int)round((t - s) * stepAmountPerTimeUnit);
  
  arma::mat res = arma::eye(beta.n_rows, beta.n_rows);
  arma::cube k(beta.n_rows, beta.n_rows, 4);
  for(int iteration = 1; iteration < stepAmount + 1; iteration++){
    k.slice(0) = res * intensityMatrix(age, s + stepLength * (double)(iteration - 1), microStateAmount, beta, eta) * stepLength;
    k.slice(1) = (res + 0.5 * k.slice(0)) * intensityMatrix(age, s + stepLength * (double)(iteration - 1) + 0.5 * stepLength, microStateAmount, beta, eta ) * stepLength;
    k.slice(2) = (res + 0.5 * k.slice(1)) * intensityMatrix(age, s + stepLength * (double)(iteration - 1) + 0.5 * stepLength, microStateAmount, beta, eta ) * stepLength;
    k.slice(3) = (res + k.slice(2)) * intensityMatrix(age, s + stepLength * (double)iteration, microStateAmount, beta, eta ) * stepLength;
    
    res += 0.125 * (k.slice(0) + 2 * k.slice(1) + 2 * k.slice(2) + k.slice(3));
  }
  
  return res;
}

arma::mat Pi(double t,double u, int macroStates, int microStateAmount){
  arma::mat pi(1,di(macroStates, microStateAmount));
  //fill in pi
  //remember t in our notes is age + t
  //
  return pi;
}

arma::mat EMatrix(int macroState, int microStateAmount){
  int dbar=0;
  for(int i=0; i<states;i++){
    dbar +=di(i, microStateAmount);
  }
  arma::mat E(dbar,di(macroState, microStateAmount));
  
  for(int j=0; j< di(macroState, microStateAmount); j++){
    E.col(j).fill(1);
  }
  return E;
}

arma::mat initialCondition(double startTime,
                           double startDuration,
                           double age,
                           int stepAmountPerTimeUnit, 
                           int macroState, 
                           arma::cube& param,
                           int microStateAmount){
  //we get the M_ii depending on the parameters we put in
  arma::mat tempVariable = Pi(startTime, startDuration, macroState, microStateAmount) 
    * prodIntegralSolver(startTime-startDuration, startTime,age,stepAmountPerTimeUnit, microStateAmount, beta, eta);
  
  arma::mat upperFractionPart= tempVariable * EMatrix(macroState, microStateAmount).t();
  
  arma::mat colOfOnes(di(macroState,microStateAmount), 1, arma::fill::ones);
  
  double lowerFractionPart = arma::as_scalar(tempVariable * colOfOnes);
  
  return upperFractionPart/lowerFractionPart;
}

arma::mat leftSigmaProdIntegral(double s, double t,double age, int stepAmountPerTimeUnit, double karensPeriod, int microStateAmount, arma::cube& beta, arma::mat eta){
   arma::mat result=prodIntegralSolver(t,std::max(s-karensPeriod, t), age, stepAmountPerTimeUnit, microStateAmount, beta, eta);
  return result;
}

arma::mat rightSigmaProdIntegral(double s, double t,double age, int stepAmountPerTimeUnit, double karensPeriod, int microStateAmount, int macroState, arma::cube& beta, arma::mat eta){
  //the intensities should be for M_11
  return prodIntegralSolver(std::max(s-karensPeriod, t), s, age, stepAmountPerTimeUnit, microStateAmount, beta, eta);
}

arma::mat sigmaIntegral(double s, double t,double age, int stepAmountPerTimeUnit, double karensPeriod, int macroState, arma::cube& beta, arma::mat& eta, int microStateAmount){
  return leftSigmaProdIntegral(s,t,age,stepAmountPerTimeUnit,karensPeriod, microStateAmount, beta, eta)
    * EMatrix(1, microStateAmount)
    * rightSigmaProdIntegral(s,t,age,stepAmountPerTimeUnit, karensPeriod,macroState,microStateAmount, beta, eta); //SELECT CORRECT PARAMS!
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat testingIntMat(double age, double s, int microStateAmount){
  loadBeta(microStateAmount);
  loadEta(microStateAmount);
  return intensityMatrix(age, s, microStateAmount, beta, eta);
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat testProdInt(double s, double t, double age){
  loadBeta(5);
  loadEta(5);
  return prodIntegralSolver(s, t, age, 12, 5, beta, eta);
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppThread)]]
// [[Rcpp::export]]
arma::mat cashflowAggregateMarkov(double startTime, 
                                  double startDuration, 
                                  double startAge,
                                  double endTime,
                                  int stepAmountPerTimeUnit,
                                  double gracePeriod,
                                  int microStateAmount){
  int cashflowSteps = stepAmountPerTimeUnit * (endTime - startTime);
  arma::mat cashflow(cashflowSteps, 2);
  
  try{
    double stepLength = 1. / (double)stepAmountPerTimeUnit;
    int gracePeriodSteps = (int)round((double)stepAmountPerTimeUnit * gracePeriod);
    for(int iteration = 0; iteration < cashflowSteps; iteration++){ 
      
      cashflow(iteration, 0) = iteration * stepLength;
      
      if(startAge + iteration * stepLength >= RETIREMENT_AGE){
        break;
      }
      
      if( iteration % (int)round( cashflowSteps * 0.01 ) == 0){
        progressBar((double)iteration / (double)cashflowSteps);
      }
      
      if(iteration < gracePeriodSteps){
        continue;
      }
      
      // cashflowStep()
    }
  }
  catch(const std::runtime_error& e){
    std::cout << e.what() << std::endl;
  }
  
  return cashflow;
}