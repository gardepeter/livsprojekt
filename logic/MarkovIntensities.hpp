#ifndef MarkovIntensities
#define MarkovIntensities

const int states = 3;
const int RETIREMENT_AGE = 67;
const double EPSILON = 0.00001;
const int yearsFromToday = 2;

arma::mat improvementData(111,5);

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

double mu02(double age , double x){
  // Alder,Kvinder,Mænd,Kvinder_levetids_forberinger,Mænd_levetids_forbedringer
  double improvementFactorMale = improvementData((int) floor(age),4);
  //improvement for male
  double improvementFactorFemale = improvementData((int) floor(age),3);
  
  //the actual factor we multiply on the intensity
  improvementFactorMale  = pow(1-improvementFactorMale,floor(x)+yearsFromToday);
  improvementFactorFemale = pow(1-improvementFactorFemale, floor(x) + yearsFromToday);
  
  return (improvementData((int) floor(age),2)*improvementFactorMale+improvementData((int) floor(age),1)*improvementFactorFemale)/2;
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

#endif