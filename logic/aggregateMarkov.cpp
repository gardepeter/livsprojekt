#include "RcppArmadillo.h"

// STEP 1
const int scenario=2;
const int states=3;

int di(int macrostate, int scenario){
  result=1;
  if(scenario==2){
    if(macrostate==1){
      result=2;
    }
  }else if(scenario==3){
    if(macrostate==1){
      result=3;
    }
  }else if(scenario==4){
    if(macrostate==1){
      result=5;
    }
  }else if(scenario==4){
    if(macrostate==1){
      result=7;
    }
  }
  else if(scenario==4){
    if(macrostate==1){
      result=10;
    }
  }
  return result;
}

arma::mat prodIntegralSolver(double s, double t, arma::mat& M){
  return arma::mat();
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
  arma::mat result=mat(dbar,dbar);
  int state=0; // start with active 
  for(int l=0; i<states; l++){ // for each state
    int displacement=0;
    //we look at the matrix P for each macro state where only for l=1 (sick) the dimension changes
    for(int i=0; i<di(state,scenario); i++){
      for(int j=0; j<di(state,scenario); j++ ){
        // we copy matrix P_kk into the diagonal
        result(i+displacement,j+displacement)=P(state,i,j); 
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
                    arma::mat& MThilde,
                    arma::mat& PBar,
                    arma::mat& R,
                    int dBar){
  //created 1_dbar and fills it in
  arma::mat oneColumndbar= mat(dbar,1);
  for(int i=0; i<dbar; i++){
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

