#ifndef MarkovIntensities
#define MarkovIntensities

const int states = 3;
const int RETIREMENT_AGE = 67;
const double EPSILON = 0.00001;
const int yearsFromToday = 2;

arma::mat improvementData(111,5);

void progressBar(double percent){
  int barWidth = 70;
  
  std::cout << "[";
  int pos = barWidth * percent;
  for (int i = 0; i < barWidth; ++i) {
    if (i < pos) std::cout << "=";
    else if (i == pos) std::cout << ">";
    else std::cout << " ";
  }
  std::cout << "] " << int(percent * 100.0) << " %\r";
  std::cout.flush();
}

void loadCsvFile(){
  // Alder,Kvinder,Mænd,Kvinder_levetids_forberinger,Mænd_levetids_forbedringer
  improvementData.load("../livsprojekt/data/deathIntensity.csv", arma::csv_ascii);
}

double mu01(double x) {
  if(x > 67){
    return 0.0009687435;
  }
  
  return exp(72.53851 - 10.66927* x + 0.53371 * pow(x, 2.) - 0.012798 * pow(x, 3.)
               + 0.00014922 * pow(x, 4.) - 0.00000068007 * pow(x, 5.));
}

double mu02unisex(double age , double x){
  double improvementFactorMale = improvementData((int) floor(age),4);
  double improvementFactorFemale = improvementData((int) floor(age),3);

  improvementFactorMale  = pow(1-improvementFactorMale,floor(x)+yearsFromToday);
  improvementFactorFemale = pow(1-improvementFactorFemale, floor(x) + yearsFromToday);
  
  return (improvementData((int) floor(age),2)*improvementFactorMale+improvementData((int) floor(age),1)*improvementFactorFemale)/2;
}

double mu02(double age , double x){
  double improvementFactorMale = improvementData((int) floor(age),4);
  
  improvementFactorMale  = pow(1-improvementFactorMale,floor(x)+yearsFromToday);
  
  return improvementData((int) floor(age), 2) * improvementFactorMale;
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
    return mu02(age + x, x);
  }
  if(i == 1 && j == 0){
    return mu10(age + x);
  }
  if(i == 1 && j == 2){
    return mu12(age + x);
  }
  if(i == 2 && ( j == 0 || j == 1)){
    return 0.;
  }
  throw std::runtime_error("ERROR: mu(.) out of bounds (livsprojekt.logic.MarkovIntensities)");
}

arma::mat markovIntensityMatrix(double x, double age){
  arma::mat res(states, states);
  for(int i = 0; i < states - 1; i++){
    for(int j = 0; j < states; j++){
      if(i == j) continue;
      res(i, j) = mu(i, j, x, age);
    }
  }
  
  return res - arma::diagmat(arma::sum(res, 1));
}

#endif