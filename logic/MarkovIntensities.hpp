#ifndef MarkovIntensities
#define MarkovIntensities
#include "SemiMarkovIntensities.hpp"

const int states = 3;

double mu10(double x) {
  return exp(0.73724137-0.07162389*x);
}

double mu12(double x) {
  return exp(-7.14108933+0.06785069*x);
}


double mu(int i, int j, double x){
  if(i == 0 && j == 1){
    return mu01(x);
  }
  if(i == 0 && j == 2){
    return mu02(x);
  }
  if(i == 1 && j == 0){
    return mu10(x);
  }
  if(i == 1 && j == 2){
    return mu12(x);
  }
  if(i == 2 && ( j == 1 || j == 2)){
    return 0.;
  }
  return 0.;
}

#endif