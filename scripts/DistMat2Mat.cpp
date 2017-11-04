#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix DistMat2Mat(NumericMatrix A, NumericMatrix B){
  
  NumericMatrix A2 = A; // don't overwrite inputs
  NumericMatrix B2 = B;
  
  int rows = A2.nrow();
  int cols = A2.ncol();
  
  int rows_b = B2.nrow();

  NumericMatrix answer(rows,rows_b);
  
  // Do the main calculations
  for(int j = 0; j < rows; j++){
    
    for(int k = 0; k < rows_b; k++){
      
      double result = 0.0;
      
      for(int i = 0; i < cols; i++){
        result = result + 
          (double(A2(j, i)) - double(B2(k, i))) *
          (double(A2(j, i)) - double(B2(k, i)));
      }
      
      answer(j , k) = std::sqrt(result);
      
    }
  }
  
  return(answer);
}