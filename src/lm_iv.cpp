#include <RcppArmadillo.h>
using namespace Rcpp;

//[[Rcpp::export]]
List lm_iv_rcpp(const arma::mat& X, const arma::colvec& Y, const arma::mat& Z) {
int n = X.n_rows, k = X.n_cols;
  arma::mat ident = arma::eye<arma::mat>(n, n);
  arma::mat mz = ident - Z * inv(Z.t() * Z) * Z.t();
  arma::colvec coef = inv(X.t() * (ident - mz) * X) * X.t() * (ident - mz) * Y;
  arma::colvec res  = Y - X*coef;           // residuals
  double s2 = std::inner_product(res.begin(), res.end(), res.begin(), 0.0)/(n - k);
  arma::mat vcov = s2 * inv(X.t() * (ident - mz) * X);
  arma::colvec std_err = arma::sqrt(arma::diagvec(vcov)); 
  
  return List::create(Named("coefficients") = coef,
                      Named("stderr")       = std_err,
                      Named("df.residual")  = n - k,
                      Named("vcov") = vcov);
}