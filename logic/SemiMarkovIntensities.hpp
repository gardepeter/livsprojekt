#ifndef SemiMarkovIntensities
#define SemiMarkovIntensities

#include <cmath>
#include "SubSemiMarkovIntensities.hpp"

const int states = 3;

double mu01(double x) {
  if(x > 67){
    return 0.0009687435;
  }
  
  return exp(72.53851 - 10.66927* x + 0.53371 * pow(x, 2) - 0.012798 * pow(x, 3)
             + 0.00014922 * pow(x, 4) - 0.00000068007 * pow(x, 6));
}

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
}

double mu12(double x, double d) {
  if(d <= 5){
    return exp( -6.1057464 + 0.0635736 * x - 0.2891195 * d );
  }
  else if(d > 5){
    return exp( - 11.9169277 + 0.1356766 * x );
  }
}

double mu(int i, int j, double x, double d){
  double age = 60 + x;
  
  if(i == 0 && j == 1){
    return mu01(age);
  }
  else if(i == 0 && j == 2){
    return mu02(age);
  }
  else if(i == 1 && j == 0){
    return mu10(age, d);
  }
  else if(i == 1 && j == 2){
    return mu12(age, d);
  }
  else if(i == 2 && ( j == 1 || j == 2)){
    return 0.;
  }
}

#endif