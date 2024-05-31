#include "RcppArmadillo.h"
#include "SemiMarkovIntensities.hpp"
#include <windows.h> 
#include <RcppThread.h>

const int START_PARALLEL_PROC = 100;

double intensityOutOfState(int j, double s, double d, double age, double t0){
  double sum = 0;
  
  for(int k = 0; k < states; k++){
    if(k == j){
      continue;
    }
    sum += mu(j, k, s - t0, d, age);
  }
  
  return sum;
}

// d=x*h
double leftIntegral(arma::field<arma::sp_mat>& probabilities, double t0, int i, int j, double s, int d_step, double stepLength, int iteration, double age){ 
  double trapezoidalSum=0;
  for(int n=1; n <= d_step; n++){ // this is for the y-coordinate of the probability matrix
    // s=t0+xh 
    trapezoidalSum += 0.5*(probabilities(states * i + j)(n, iteration) - probabilities(states * i + j)(n-1, iteration))
    *(intensityOutOfState(j, s, n*stepLength, age, t0)+intensityOutOfState(j, s, stepLength*(n-1), age, t0));
  }
  return trapezoidalSum;
}

//TODO // the variable d is not in use and could be removed 
// the function should work as long as u=x*h and i have not checked if it works if not 
double trapezoidalRightEstimation(arma::field<arma::sp_mat>& probabilities, double t0, double u, int i, int j, int l, double s, double stepLength, int iteration, double age){ 
  double trapezoidalSum=0;
  for(int n=1; n <= round((u+s-t0)/stepLength); n++){ // this is for the y-coordinate of the probability matrix
    // s=t0+xh 
    trapezoidalSum += 0.5*(probabilities(states * i + l)(n, iteration) - probabilities(states * i + l)(n-1, iteration))
    *(mu(l, j, s - t0, n*stepLength, age)+mu(l, j, s - t0, stepLength*(n-1), age));
  }
  return trapezoidalSum;
}

double rightSumOfIntegrals(arma::field<arma::sp_mat>& probabilities, double t0, double u, int i, int j, double s, double stepLength, int iteration, double age){
  double rightSumResult = 0;
  for(int l = 0; l < states; l++){
    if(l == j){
      continue;
    }
    rightSumResult += trapezoidalRightEstimation(probabilities, t0, u, i, j, l, s, stepLength, iteration, age);
  }
  return rightSumResult;
}

void RK1Step(arma::field<arma::sp_mat>& probabilities, double startTime, double startDuration, int iteration, double stepLength, int states, double age){
  for(int i = 0; i < states; i++){
    for(int j = 0; j < states; j++){
      double rightSum = rightSumOfIntegrals(probabilities, startTime, startDuration, i, j, startTime + stepLength * (iteration  - 1), stepLength, iteration - 1, age);
      
      if(iteration <= START_PARALLEL_PROC){
        for(int d_step = 1; d_step <= (int)floor(startDuration/stepLength) + iteration; d_step++){
          probabilities(states * i + j)(d_step, iteration) = probabilities(states * i + j)(d_step - 1, iteration - 1)
          + stepLength * ( - leftIntegral(probabilities, startDuration, i, j, startTime + stepLength * iteration, d_step-1, stepLength, iteration - 1, age) + rightSum); //TODO investegate if d_step - 1 or not
        }
      } else{
        RcppThread::parallelFor(1, (int)floor(startDuration/stepLength) + iteration + 1, [&] (size_t d_step) {
          probabilities(states * i + j)(d_step, iteration) = probabilities(states * i + j)(d_step - 1, iteration - 1)
          + stepLength * ( - leftIntegral(probabilities, startDuration, i, j, startTime + stepLength * iteration, d_step-1, stepLength, iteration - 1, age) + rightSum); //TODO investegate if d_step - 1 or not
        });
      }
    
    }
  }
}

void boundaryCondition(arma::field<arma::sp_mat>& probabilities, int locationOfOne){
  for(int dim = 0; dim < states; dim++){
    probabilities((states + 1) * dim)(locationOfOne, 0) = 1.;
  }
}

void boundaryCondition(arma::sp_mat& probabilities, int locationOfOne){
  for(int dim = 0; dim < states; dim++){
    probabilities(locationOfOne, (states + 1) * dim) = 1.;
  }
}

void boundaryCondition(arma::mat& probabilities, int locationOfOne){
  for(int dim = 0; dim < states; dim++){
    probabilities(locationOfOne, (states + 1) * dim) = 1.;
  }
}

bool isNotMultipla(double x, double y){
  return abs(x*y - trunc(x*y)) >= EPSILON;
}

void saveCube(arma::field<arma::sp_mat>& probabilities, int states){
  for(int i = 0; i < states - 1; i++){
    for(int j = 0; j < states; j++){
      probabilities(states * i + j).save("p" + std::to_string(i) + std::to_string(j) + ".csv", arma::csv_ascii);
    }
  }
}

void progressBar(double percent){
  int barWidth = 70;
  
  std::cout << "[";
  int pos = barWidth * percent;
  for (int i = 0; i < barWidth; ++i) {
    if (i < pos) std::cout << "=";
    else if (i == pos) std::cout << ">";
    else std::cout << " ";
  }
  std::cout << "] " << int(percent * 100.0) << " %\r";
  std::cout.flush();
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppThread)]]
// [[Rcpp::export]]
arma::mat RK1_unitCashflowDisabilityWithKarens(double startTime, double startDuration, double endTime, int stepAmountPerTimeUnit, double age, double gracePeriod, int i, int j) {
  loadCsvFile();
  double stepLength = 1. / (double)stepAmountPerTimeUnit;

  int stepsFromZeroToStartDuration = (int)round(startDuration * stepAmountPerTimeUnit);
  int cashflowSteps = stepAmountPerTimeUnit * (endTime - startTime);
  int nrow = stepsFromZeroToStartDuration + cashflowSteps;
  int gracePeriodSteps = (int)round((double)stepAmountPerTimeUnit * gracePeriod);// Plus one (strict ineq.) as gracePeriod <= 1/4 month
  
  arma::sp_mat probabilities(nrow, states * states);
  boundaryCondition(probabilities, stepsFromZeroToStartDuration );
  
  arma::mat cashflow(cashflowSteps, 2);
  cashflow(0, 0) = 0.;
  if(stepsFromZeroToStartDuration >= gracePeriodSteps && age < RETIREMENT_AGE){
    cashflow(0, 1) = probabilities(stepsFromZeroToStartDuration, states * i + j)
    - probabilities(gracePeriodSteps, states * i + j); 
  }
  
  try{
    for(int iteration = 1; iteration < cashflowSteps; iteration++){ 
      cashflow(iteration, 0) = iteration * stepLength;
      if(age + iteration * stepLength >= RETIREMENT_AGE){
        break;
      }
      
      arma::field<arma::sp_mat> probabilitiesTemp(states * states);
      probabilitiesTemp.for_each( [&](arma::sp_mat& X) { X.set_size(stepsFromZeroToStartDuration + iteration + 1, iteration + 1); } );
  
      for(int row = 0; row <= stepsFromZeroToStartDuration + iteration; row++){
          for(int i_state = 0; i_state < states; i_state++){
            for(int j_state = 0; j_state < states; j_state++){
              probabilitiesTemp(states * i_state + j_state)(row, iteration - 1) = probabilities(row, states * i_state + j_state);
          }
        }
      }
     
      RK1Step(probabilitiesTemp, startTime, startDuration, iteration, stepLength, states, age);
  
      for(int row = 0; row <= stepsFromZeroToStartDuration + iteration; row++){
        for(int i_state = 0; i_state < states; i_state++){
          for(int j_state = 0; j_state < states; j_state++){
             probabilities(row, states * i_state + j_state) = probabilitiesTemp(states * i_state + j_state)(row, iteration);
          }
        }
      }
      
      if(stepsFromZeroToStartDuration + iteration >= gracePeriodSteps){
        cashflow(iteration, 1) = probabilities(stepsFromZeroToStartDuration + iteration, states * i + j)
        - probabilities(gracePeriodSteps, states * i + j); // Strict ineq. as gracePeriod <= 3 month
      }
      
      if( iteration % (int)round( cashflowSteps * 0.01 ) == 0){
        progressBar((double)iteration / (double)cashflowSteps);
      }
    }
  }
  catch(const std::runtime_error& e){
    std::cout << e.what() << std::endl;
  }
  
  return cashflow;
}

arma::field<arma::sp_mat> RK1_Cpp(double startTime, double startDuration, double endTime, int stepAmountPerTimeUnit, double age) {
  double stepLength = 1. / (double)stepAmountPerTimeUnit;
  
  int stepsFromZeroToStartDuration = (int)round(startDuration * stepAmountPerTimeUnit);
  int nrow = stepsFromZeroToStartDuration + stepAmountPerTimeUnit * (endTime - startTime);
  int ncol = stepAmountPerTimeUnit * (endTime - startTime);
  
  arma::field<arma::sp_mat> probabilities(states * states);
  probabilities.for_each( [&](arma::sp_mat& X) { X.set_size(nrow, ncol); } );
  
  boundaryCondition(probabilities, stepsFromZeroToStartDuration );
  
  try{
    for(int iteration = 1; iteration < stepAmountPerTimeUnit * (endTime - startTime); iteration++){
      RK1Step(probabilities, startTime, startDuration, iteration, stepLength, states, age);
    }
  }
  catch(const std::runtime_error& e){
    std::cout << e.what() << std::endl;
  }

  return probabilities;
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppThread)]]
// [[Rcpp::export]]
int RK1(double startTime, double startDuration, double endTime, int stepAmountPerTimeUnit, double age) {
  if( endTime <= startTime ||  stepAmountPerTimeUnit <= 1 || isNotMultipla(startDuration, (double)stepAmountPerTimeUnit)){
    return -1;
  }
  loadCsvFile();
  
  arma::field<arma::sp_mat> probabilities = RK1_Cpp(startTime, startDuration, endTime, stepAmountPerTimeUnit, age);
  
  saveCube(probabilities, states);
  return 0;
}

// LEGACY CODE
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppThread)]]
// [[Rcpp::export]]
arma::mat unitCashflowDisabilityWithKarens(double startTime, double startDuration, double endTime, int stepAmountPerTimeUnit, double age, double gracePeriod, int i, int j) {
  loadCsvFile();
  arma::field<arma::sp_mat> probabilities = RK1_Cpp(startTime, startDuration, endTime, stepAmountPerTimeUnit, age);
  arma::sp_mat P =  probabilities(states * i + j);

  int cashflowSteps = stepAmountPerTimeUnit * (endTime - startTime);
  double stepAmountLength = 1 / (double)stepAmountPerTimeUnit;
  int stepsFromZeroToStartDuration = (int)round(startDuration * stepAmountPerTimeUnit);
  int gracePeriodSteps = (int)round((double)stepAmountPerTimeUnit * gracePeriod); // Strict ineq. as gracePeriod <= 1/4

  arma::mat cashflow(cashflowSteps, 2);
  for(int n = 0; n < cashflowSteps; n++ ){
    cashflow(n, 0) = n * stepAmountLength;
    if(age + n * stepAmountLength >= RETIREMENT_AGE){
      break;
    }

    if(stepsFromZeroToStartDuration + n < gracePeriodSteps){
      continue;
    }
    cashflow(n, 1) = P(stepsFromZeroToStartDuration + n, n) - P(gracePeriodSteps, n);
  }

  return cashflow;
}
