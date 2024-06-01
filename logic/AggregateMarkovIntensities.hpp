#ifndef SemiMarkovIntensities
#define SemiMarkovIntensities

#include <cmath>
#include "MarkovIntensities.hpp"

arma::cube beta;
arma::mat eta;

int di(int macrostate, int microStateAmount){
  if(macrostate != 1){
    return 1;
  }
  
  return microStateAmount;
}

void loadBeta(int microStates){
  if(microStates != 2 && microStates != 3 && microStates != 5 && microStates != 7 && microStates != 10){
    throw std::runtime_error("ERROR: Invalid microState amount (livsprojekt.logic.AggregateMarkovIntensities)");
  }
  
  std::string pathFull0 = "../livsprojekt/data/resources/full/fullIntensitybeta0microStates" + std::to_string(microStates) + ".csv";
  std::string pathFull1 = "../livsprojekt/data/resources/full/fullIntensitybeta1microStates" + std::to_string(microStates) + ".csv";
  
  beta.set_size(microStates + 2, microStates + 2, 2);
  
  beta.slice(0).load(pathFull0, arma::csv_ascii);
  beta.slice(1).load(pathFull1, arma::csv_ascii);
}

void loadEta(int microStates){
  if(microStates != 2 && microStates != 3 && microStates != 5 && microStates != 7 && microStates != 10){
    throw std::runtime_error("ERROR: Invalid microState amount (livsprojekt.logic.AggregateMarkovIntensities)");
  }
  
  std::string path0 = "../livsprojekt/data/resources/startCondition/etaMicroStates" + std::to_string(microStates) + ".csv";
 
  eta.set_size(microStates - 1, 2);
  
  eta.load(path0, arma::csv_ascii);
} 

double piFit(int macroState, int microState, int microStateAmount, double s, arma::mat& param){
  if(macroState != 1){
    return 1.;
  }
  
  double numerator = 1;
  if(microState != di(macroState, microStateAmount)){
    numerator = std::exp( param(microState, 0) + param(microState, 1) * s );
  }
  
  double denominator = 1;
  for(int i = 0; i < di(macroState, microStateAmount) - 1; i++){
    denominator += std::exp( param(i, 0) + param(i, 1) * s );
  }
  
  return numerator / denominator;
}

arma::mat intensityMatrix(double age, double x, int microStateAmount, arma::cube& beta, arma::mat& eta){
  arma::mat M = exp(beta.slice(0) + beta.slice(1) * (age + x) );
  M -= arma::diagmat(M.diag());
  
  for(unsigned int i = 1; i < eta.n_rows; i++){
    M(0, i) *= piFit(1, i, microStateAmount, x, eta);
  }
  
  return M - arma::diagmat(arma::sum(M, 1));
}

arma::mat subIntensityMatrix(int i, double age, double x, int microStateAmount, arma::cube& beta, arma::mat& eta){
  arma::mat res(1,1);
  arma::mat fullIntensity = intensityMatrix(age, x, microStateAmount, beta, eta);
  if(i == 0){
    res(0,0) = fullIntensity(0,0);
    return res;
  } else if(i == 2){
    res(0,0) = fullIntensity(beta.n_rows,beta.n_rows);
    return res;
  }
  return fullIntensity.submat(1,1 , beta.n_rows-2,beta.n_rows-2);
}


#endif