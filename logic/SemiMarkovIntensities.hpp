#ifndef SemiMarkovIntensities
#define SemiMarkovIntensities

#include <cmath>
#include "MarkovIntensities.hpp"

double mu10(double x, double d) {
  if(d <= 0.2291667){
    return exp( -0.9148875 - 0.0309126 * x + 4.8715347 * d );
  }
  else if(d > 0.2291667 && d <= 2.){
    return exp( 0.3766531 - 0.0309126 * x - 0.7642786 * d );
  }
  else if(d > 2 && d <= 5){
    return exp(-0.4808001 - 0.0309126*x - 0.335552 * d );
  }
  else if(d > 5){
    return exp(-0.042168 - 0.092455*x);
  }
  throw std::runtime_error("ERROR: mu10(.) out of bounds (livsprojekt.logic.SemiMarkovIntensities)");
}

double mu12(double x, double d) {
  if(d <= 5){
    return exp( -6.1057464 + 0.0635736 * x - 0.2891195 * d );
  }
  return exp( - 11.9169277 + 0.1356766 * x );
}

double mu(int i, int j, double x, double d, double age){
  if(i == 0 && j == 1){
    return mu01(age + x);
  }
  else if(i == 0 && j == 2){
    return mu02(age + x, x);
  }
  else if(i == 1 && j == 0){
    return mu10(age + x, d);
  }
  else if(i == 1 && j == 2){
    return mu12(age + x, d);
  }
  else if(i == 2 && ( j == 0 || j == 1 )){
    return 0.;
  }
  throw std::runtime_error("ERROR: mu(.) out of bounds (livsprojekt.logic.SemiMarkovIntensities)");
}

#endif