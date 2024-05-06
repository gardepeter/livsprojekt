#include "Rcpp.h"

int getOne() {
  return 1;
}

// [[Rcpp::export]]
int fibonacci(const int x) {
  if (x < 2) return(x);
  return (fibonacci(x - 1)) + fibonacci(x - 2);
}

// [[Rcpp::export]]
int prodOfOnes(const int n) {
  int prod = 1;
  for(int i = 0; i < n; i++){
    prod *= getOne();
  }
  return prod;
}