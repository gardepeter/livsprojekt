#include "RcppArmadillo.h"
#include "AggregateMarkovIntensities.hpp"

const int scenario=2;

// STEP 1
arma::mat prodIntegralStep(){
  
}

arma::mat prodIntegralSolver(double s, double t, arma){
  
  return arma::mat();
}

int di(int macrostate, int scenario){
  if(macrostate != 1){
    return 1;
  }

  arma::vec microStateMapping = {1, 2, 3, 5, 7, 10};
  return microStateMapping(scenario - 1);
}

arma::mat initialConditionalDistribution(){
  return arma::mat();
}

// STEP 2
//TODO: can and should be parralized
// should take some short of cube in as an input
arma::cube allProbabilitySolver(double startTIme, double endTime){
  return arma::cube();
}

arma::mat probabilitySolver(double startTIme, double endTime, arma::cube& allProbabilities){
  return arma::mat();
}
// STEP 3
//// i)
arma::mat diagonolMatrix(double startTime, 
                         double endTime,
                         int dbar,
                         arma::cube& microStateProbabilities){
  //creates a matrix that is big enough to have each probability matrix in its diagonal
  arma::mat result(dbar,dbar);
  int state=0; // start with active 
  for(int l=0; l<states; l++){ // for each state
    int displacement=0;
    //we look at the matrix P for each macro state where only for l=1 (sick) the dimension changes
    for(int i=0; i<di(state,scenario); i++){
      for(int j=0; j<di(state,scenario); j++ ){
        // we copy matrix P_kk into the diagonal
        result(i+displacement,j+displacement)=microStateProbabilities(state,i,j); 
      }
    }
    //update the displacement such that we can copy another matrix into the diagonal
    displacement += di(state,scenario);
  }
  
  return result;
}

//// ii)
arma::vec integrand(double t, 
                    double v, 
                    double tL,
                    arma::mat& P,
                    arma::mat& MTilde,
                    arma::mat& PBar,
                    arma::mat& R,
                    int dBar){
  //created 1_dbar and fills it in
  arma::mat oneColumndbar(dBar,1);
  for(int i=0; i<dBar; i++){
    oneColumndbar(i,1)=1;
  }
  // the result where we have assumed that the matrices is already calculated for the specific t and v
  arma::mat result=P*MTilde*R*oneColumndbar;
  return result;
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
  int cashflowSteps = stepAmountPerTimeUnit * (endTime - startTime);
  arma::mat cashflow(cashflowSteps, 2);
  
  try{
    double stepLength = 1. / (double)stepAmountPerTimeUnit;
    int gracePeriodSteps = (int)round((double)stepAmountPerTimeUnit * gracePeriod) + 1;// Plus one (strict ineq.) as gracePeriod <= 1/4 month
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

