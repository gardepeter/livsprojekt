#include "RcppArmadillo.h"
#include "AggregateMarkovIntensities.hpp"
#include <iostream>
#include <cmath>
#include <algorithm>

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

int di(int macrostate, int dMicroStates){
  if(macrostate != 1){
    return 1;
  }
  
  return dMicroStates;
}
arma::mat Pi(double t,double u, int macroStates, int dMicroStates){
  arma::mat pi(1,di(macroStates, dMicroStates));
  //fill in pi
  //remember t in our notes is age + t
  //
  return pi;
}

arma::mat EMatrix(int macroState, int dMicroStates){
  int dbar=0;
  for(int i=0; i<states;i++){
    dbar +=di(i, dMicroStates);
  }
  arma::mat E(dbar,di(macroState, dMicroStates));
  
  for(int j=0; j< di(macroState, dMicroStates); j++){
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
                           int dMicroStates){
  //we get the M_ii depending on the parameters we put in
  arma::mat tempVariable = Pi(startTime, startDuration, macroState, dMicroStates) 
    * prodIntegralSolver(startTime-startDuration, startTime,age,stepAmountPerTimeUnit, param);
  
  arma::mat upperFractionPart= tempVariable * EMatrix(macroState, dMicroStates).t();
  
  arma::mat colOfOnes(di(macroState,dMicroStates), 1, arma::fill::ones);
  
  double lowerFractionPart = arma::as_scalar(tempVariable * colOfOnes);
  
  return upperFractionPart/lowerFractionPart;
}

arma::mat leftSigmaProdIntegral(double s, double t,double age, int stepAmountPerTimeUnit, double karensPeriod, arma::cube& params){
   arma::mat result=prodIntegralSolver(t,std::max(s-karensPeriod, t), age, stepAmountPerTimeUnit, params);
  return result;
}

arma::mat rightSigmaProdIntegral(double s, double t,double age, int stepAmountPerTimeUnit, double karensPeriod, int macroState, arma::cube& params){
  //the intensities should be for M_11
  return prodIntegralSolver(std::max(s-karensPeriod, t), s, age, stepAmountPerTimeUnit, params);
}

arma::mat sigmaIntegral(double s, double t,double age, int stepAmountPerTimeUnit, double karensPeriod, int macroState, arma::cube& params, int dMicroStates){
  return leftSigmaProdIntegral(s,t,age,stepAmountPerTimeUnit,karensPeriod,params)
    * EMatrix(1, dMicroStates)
    * rightSigmaProdIntegral(s,t,age,stepAmountPerTimeUnit, karensPeriod,macroState,params); //SELECT CORRECT PARAMS!
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
                                  int dMicroStates){
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