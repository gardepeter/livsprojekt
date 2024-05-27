#ifndef SemiMarkovIntensities
#define SemiMarkovIntensities

#include <cmath>
#include <filesystem>
#include "MarkovIntensities.hpp"

arma::cube beta;

void loadBeta(int microStates){
  if(microStates != 2 && microStates != 3 && microStates != 5 && microStates != 7 && microStates != 10){
    throw std::runtime_error("ERROR: Invalid microState amount (livsprojekt.logic.AggregateMarkovIntensities)");
  }
  
  std::string path0 = "../livsprojekt/data/resources/beta0microStates" + std::to_string(microStates) + ".csv";
  std::string path1 = "../livsprojekt/data/resources/beta1microStates" + std::to_string(microStates) + ".csv";
  
  beta.set_size(microStates, microStates, 2);
  beta.slice(0).load(path0, arma::csv_ascii);
  beta.slice(1).load(path1, arma::csv_ascii);
}

arma::mat intensityMatrix(double x){
  arma::mat M = exp(beta.slice(0) + beta.slice(1) * x);
  arma::mat diag = arma::diagmat(arma::sum(M, 1) + 1);
  return M - diag;
}


#endif