#ifndef MarkovIntensities
#define MarkovIntensities

//#include "SemiMarkovIntensities.hpp"
const int states=3;

#include <iostream>

//used to input entries from a csv file to a matrix 
#include <fstream>
#include <sstream>
const int yearsFromToday=2;
// get info from csv-file
arma::mat improvementData(120,120);

void loadCsvFile(){
  // Alder,Kvinder,Mænd,Kvinder_levetids_forberinger,Mænd_levetids_forbedringer
  improvementData.load("../livsprojekt/data/deathIntensity.csv", arma::csv_ascii);
}

double mu10(double x) {
  return exp(0.73724137-0.07162389*x);
}

double mu12(double x) {
  return exp(-7.14108933+0.06785069*x);
}

double mu(int i, int j, double x, double age){
  if(i == 0 && j == 1){
    return mu01(age + x);
  }
  if(i == 0 && j == 2){
    return mu02(age+x, x);
  }
  if(i == 1 && j == 0){
    return mu10(age + x);
  }
  if(i == 1 && j == 2){
    return mu12(age + x);
  }
  if(i == 2 && ( j == 1 || j == 2)){
    return 0.;
  }
  return 0.;
}

#endif