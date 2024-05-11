#ifndef SemiMarkovIntensities
#define SemiMarkovIntensities

const int states = 3;

double mu01(double x, double d) {
  return 0.1 * x + 0.5;
}

double mu02(double x, double d) {
  return 0.05 * x + 0.5;
}

double mu10(double x, double d) {
  return 0.15 * x + 1;
}

double mu12(double x, double d) {
  return 0.75 * x + 1;
}


double mu(int i, int j, double x, double d){
  if(i == 0 && j == 1){
    return mu01(x, d);
  }
  if(i == 0 && j == 2){
    return mu02(x, d);
  }
  if(i == 1 && j == 0){
    return mu10(x, d);
  }
  if(i == 1 && j == 2){
    return mu12(x, d);
  }
  if(i == 2 && ( j == 1 || j == 2)){
    return 0.;
  }
  return 0.;
}

#endif