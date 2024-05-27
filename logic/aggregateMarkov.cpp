#include "RcppArmadillo.h"

// STEP 1
arma::mat prodIntegralSolver(double s, double t, arma::mat& M){
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


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppThread)]]
// [[Rcpp::export]]
arma::cube cashflowAggregateMarkov(double startTime, 
                                   double startDuration, 
                                   double endTime,
                                   int stepAmountPerTimeUnit,
                                   int dBar){
  return arma::cube();
}

