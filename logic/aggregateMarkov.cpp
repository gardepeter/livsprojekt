#include "RcppArmadillo.h"
#include "AggregateMarkovIntensities.hpp"

const int RETIREMENT_AGE = 67;

// STEP 1

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat test(){
  loadBeta(10);
  return intensityMatrix(5.);
}

arma::mat prodIntegralSolver(double s, double t, arma::mat& beta0, arma::mat& beta1){
  return arma::mat();
}

arma::mat initialConditionalDistribution(){
  return arma::mat();
}

// STEP 2
//TODO: can and should be parralized
arma::cube allProbabilitySolver(double startTIme, double endTime){
  return arma::cube();
}

arma::mat probabilitySolver(double startTIme, double endTime, arma::cube& allProbabilities){
  return arma::mat();
}
// STEP 3
//// i)
arma::mat diagonolMatrix(double startTime, double endTime, arma::cube& microStateProbabilities){
  return arma::mat();
}

//// ii)
arma::vec integrand(double t, 
                    double v, 
                    double tL,
                    arma::mat& P,
                    arma::mat& MThilde,
                    arma::mat& PBar,
                    arma::mat& R,
                    int dBar){
  return arma::vec();
}

arma::vec rightTerm(double t, 
                    double u, 
                    double tL,
                    arma::mat& PBar,
                    arma::mat& R,
                    int dBar){
  return arma::vec();
}

arma::vec integral(double t, 
                   double v, 
                   double tL,
                   arma::mat& P,
                   arma::mat& MThilde,
                   arma::mat& PBar,
                   arma::mat& R,
                   int dBar){
  return arma::vec();
}

arma::vec cashflowStep(double t, 
                       double v, 
                       double tL,
                       arma::mat& P,
                       arma::mat& MThilde,
                       arma::mat& PBar,
                       arma::mat& R,
                       int dBar){
  return arma::vec();
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
arma::mat cashflowAggregateMarkov(double startTime, 
                                   double startDuration, 
                                   double startAge,
                                   double endTime,
                                   int stepAmountPerTimeUnit,
                                   double gracePeriod,
                                   int dMicroStates){
  
  double stepLength = 1. / (double)stepAmountPerTimeUnit;
  int cashflowSteps = stepAmountPerTimeUnit * (endTime - startTime);
  int gracePeriodSteps = (int)round((double)stepAmountPerTimeUnit * gracePeriod) + 1;// Plus one (strict ineq.) as gracePeriod <= 1/4 month
  
  arma::mat cashflow(cashflowSteps, 2);

  try{
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
      
      // make cashflow step
      // cashflowStep()
    }
  }
  catch(const std::runtime_error& e){
    std::cout << e.what() << std::endl;
  }
  
  return cashflow;
}

