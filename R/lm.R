#' @import rlang
#' @importFrom stats model.frame model.matrix model.response

# The following function is copied from RcppArmadillo, and the modified

#' @export
tidymod_lm_matrix <- function(X, Y, ...) {
  
  X <- as.matrix(X)
  Y <- as.numeric(Y)
  stopifnot(is.matrix(X), is.vector(Y), nrow(Y) == nrow(X))
  
  res <- lm_rcpp(X, Y)
  
  res$coefficients <- as.vector(res$coefficients)
  names(res$coefficients) <- colnames(X)
  res$fitted_values <- as.vector(X %*% res$coefficients)
  res$residuals <- Y - res$fitted_values
  res$call <- match.call()
  res$intercept <- any(apply(X, 2, function(x) all(x == x[1])))
  
  class(res) <- "tidymod_lm"
  res
}

#' @export
tidymod_lm <- function(data, formula, ...) {
  
  if(!is.data.frame(data)) {
    abort("data has to be a data.frame")
  }
  
  if(!is_formula(formula)) {
    abort("formula has to be a model formula")
  }
  
  mf <- model.frame(formula = formula, data = data)
  X <- model.matrix(attr(mf, "terms"), data=mf)
  Y <- model.response(mf)
  
  res <- tidymod_lm_matrix(X, Y, ...)
  res$call <- match.call()
  res$formula <- formula
  res$intercept <- attr(attr(mf, "terms"), "intercept")
  res
}