#include <Rcpp.h>
#include <math.h>

using namespace Rcpp;

//' Efficently simulate the toy model - model0
//' @export
// [[Rcpp::export]]
NumericMatrix model0(int N, int M, float theta) {
    // set seed
    RNGScope scope;

    // allocate output
    NumericMatrix y(N,M);


    for(int m = 0; m < M; m++) {
        NumericVector eps = rnorm(N,theta,1);
        for(int n = 1; n < N; n++) {
            y(n,m) = y(n-1,m) + eps(n);
        }
    }



    return(y);
}
