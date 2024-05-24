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
  //std::cout << improvementData(0,1); 
}
double improvement(double age , double time ){

  //double uniSexImprovementFactor=(improvementData((int) floor(age)-1,3)+improvementData((int) floor(age)-1,4))/2;
  
  //improvement for Female
  double improvementFactorMale = improvementData((int) floor(age)-1,3);
  //improvement for male
  double improvementFactorFemale = improvementData((int) floor(age)-1,4);
  
  //the actual factor we multiply on the intensity
  improvementFactorMale  = pow(1-improvementFactorMale,time+yearsFromToday);
  improvementFactorFemale = pow(1-improvementFactorFemale, time + yearsFromToday);
  
  // we assume we are in 2024 and the data is from 2022 so we add 2 in the powerfactor
  return (improvementFactorMale+improvementFactorFemale);
  
  //return pow(1-uniSexImprovementFactor,time+yearsFromToday); 
}

double mu10(double x) {
  return exp(0.73724137-0.07162389*x);
}

double mu12(double x) {
  return exp(-7.14108933+0.06785069*x);
}

double mu02(double x) {
  //return (improvementData((int) floor(x)-1,1)+ improvementData((int) floor(x)-1,2));
  return 0.05 * x + 0.5;
}

double mu01(double x) {
  return 0.1 * x + 0.5;
}

double mu02Improvement(double age , double time){
  // Alder,Kvinder,Mænd,Kvinder_levetids_forberinger,Mænd_levetids_forbedringer
  //std::cout<< mu02(age);
  //std::cout << improvement(age,time);
  double improvementFactorMale = improvementData((int) floor(age),4);
  //improvement for male
  double improvementFactorFemale = improvementData((int) floor(age),3);
  
  //the actual factor we multiply on the intensity
  improvementFactorMale  = pow(1-improvementFactorMale,time+yearsFromToday);
  improvementFactorFemale = pow(1-improvementFactorFemale, time + yearsFromToday);
  
  return (improvementData((int) floor(age),2)*improvementFactorMale+improvementData((int) floor(age),1)*improvementFactorFemale)/2;
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

//overloading when we have improvements
double muImprovement(int i, int j, double x, double time){
  if(i == 0 && j == 1){
    return mu01(x);
  }
  if(i == 0 && j == 2){
    return mu02Improvement(x, time);
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
/*
//load the file
int countLines(const std::string &filename) {
  std::ifstream file(filename);
  std::string line;
  int line_count = 0;
  // Read the file line by line and count the lines
  while (std::getline(file, line)) {
    line_count++;
  }
  return line_count;
}

// Helper function to count the number of columns in the first line of a file
int countColumns(const std::string &filename) {
  std::ifstream file(filename);
  std::string line; // defines a string
  std::getline(file, line); // get the first line of the file
  std::stringstream ss(line); // makes it a string stream such that we can work with it as we want
  std::string cell;// defines a new string
  //counting and each col is seperated by ;
  int columns = 0; 
  while (std::getline(ss, cell, ',')) {
    columns++;
  }
  return columns;
}


// makes the csv file to a matrix we can actually work with
arma::mat readCsvToMatrix(const std::string &filename) { 
  int rows = countLines(filename);
  int cols = countColumns(filename);
  
  //makes a matrix of the right dimensions
  arma::mat matrix(rows, cols);
  
  std::ifstream file(filename);
  std::string line;
  int row = 0;
  
  while (std::getline(file, line)) { // for every line
    std::stringstream ss(line);
    std::string cell;
    int col = 0;
    while (std::getline(ss, cell, ';')) { //for every cell in the line
      matrix(row, col) = std::stod(cell);
      col++;
    }
    row++;
  }
  
  return matrix;
}
*/

#endif