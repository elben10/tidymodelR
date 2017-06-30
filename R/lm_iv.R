#' @importFrom stats delete.response terms
#' @importFrom Formula as.Formula
tidymod_iv_pure <- function(X, Y, Z, ...) {
  stopifnot(is.matrix(X), is.numeric(Y), is.matrix(Z))
  .Call("tidymodelR_lm_iv_rcpp", X, Y, Z, PACKAGE = "tidymodelR")
}

#' @export
tidymod_iv_matrix <- function(X, Y, Z, ...) {
  X <- as.matrix(X)
  Y <- as.numeric(Y)
  Z <- as.matrix(Z)
  
  res <- tidymod_iv_pure(X, Y, Z)
  
  res$coefficients <- as.vector(res$coefficients)
  names(res$coefficients) <- colnames(X)
  res$fitted.values <- as.vector(X %*% res$coefficients)
  res$residuals <- Y - res$fitted.values
  res$data <- cbind(Y, X, Z)
  res$call <- match.call()
  res$intercept <- any(apply(X, 2, function(x) all(x == x[1])))
  
  class(res) <- "tidymod_lm"
  res
}

#' @export
tidymod_iv <- function(data, formula, ...) {
  if(!is.data.frame(data)) {
    abort("data has to be a data.frame")
  }
  
  if(!is_formula(formula)) {
    abort("formula has to be a model formula")
  }
  
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data"), names(mf), 0)
  mf <- mf[c(1, m)]
  ## handle instruments for backward compatibility
  
  formula <- Formula::as.Formula(formula)
  
  stopifnot(length(formula)[1] == 1L, length(formula)[2] %in% 1:2)
  
  ## try to handle dots in formula
  has_dot <- function(formula) inherits(try(terms(formula), silent = TRUE), "try-error")
  if(has_dot(formula)) {
    f1 <- formula(formula, rhs = 1)
    f2 <- formula(formula, lhs = 0, rhs = 2)
    if(!has_dot(f1) & has_dot(f2)) formula <- Formula::as.Formula(f1,
                                                                  update(formula(formula, lhs = 0, rhs = 1), f2))
  }
  mf$drop.unused.levels <- TRUE
  mf$formula <- formula
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())

  X <- stats::model.matrix(terms(formula, data = data, rhs = 1), mf, contrasts)
  Y <- stats::model.response(mf)
  Z <- stats::model.matrix(stats::delete.response(stats::terms(formula, data = data, rhs = 2)), mf, contrasts)
  
  res <- tidymod_iv_matrix(X, Y, Z)
  res$call <- match.call()
  res$intercept <- attr(attr(mf, "terms"), "intercept")
  res$mf
  res$X
  res$Y
  res
}
