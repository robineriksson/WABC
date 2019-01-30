// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// model0
NumericMatrix model0(int N, int M, float theta);
RcppExport SEXP _wabc_model0(SEXP NSEXP, SEXP MSEXP, SEXP thetaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< int >::type M(MSEXP);
    Rcpp::traits::input_parameter< float >::type theta(thetaSEXP);
    rcpp_result_gen = Rcpp::wrap(model0(N, M, theta));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_wabc_model0", (DL_FUNC) &_wabc_model0, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_wabc(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}