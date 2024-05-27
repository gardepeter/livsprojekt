#ifndef SemiMarkovIntensities
#define SemiMarkovIntensities

#include <cmath>
#include <filesystem>
#include "MarkovIntensities.hpp"

arma::cube beta;
arma::cube betaFull;

void loadBeta(int microStates){
  if(microStates != 2 && microStates != 3 && microStates != 5 && microStates != 7 && microStates != 10){
    throw std::runtime_error("ERROR: Invalid microState amount (livsprojekt.logic.AggregateMarkovIntensities)");
  }
  
  std::string path0 = "../livsprojekt/data/resources/beta0microStates" + std::to_string(microStates) + ".csv";
  std::string path1 = "../livsprojekt/data/resources/beta1microStates" + std::to_string(microStates) + ".csv";
  std::string pathFull0 = "../livsprojekt/data/resources/full/fullIntensitybeta0microStates" + std::to_string(microStates) + ".csv";
  std::string pathFull1 = "../livsprojekt/data/resources/full/fullIntensitybeta1microStates" + std::to_string(microStates) + ".csv";
  
  beta.set_size(microStates, microStates, 2);
  betaFull.set_size(microStates + 2, microStates + 2, 2);
  
  beta.slice(0).load(path0, arma::csv_ascii);
  beta.slice(1).load(path1, arma::csv_ascii);
  betaFull.slice(0).load(pathFull0, arma::csv_ascii);
  betaFull.slice(1).load(pathFull1, arma::csv_ascii);
}

arma::mat intensityMatrix(double age, double x, arma::cube& param){
  arma::mat M = exp(param.slice(0) + param.slice(1) * (age + x) );
  M -= arma::diagmat(M.diag());
  return M - arma::diagmat(arma::sum(M, 1));
}


#endif