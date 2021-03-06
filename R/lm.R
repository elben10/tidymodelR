#' @import rlang
#' @importFrom stats model.frame model.matrix model.response coef fitted printCoefmat pt
#' @importFrom stats confint qt vcov

# The following function is copied from RcppArmadillo, and the modified

tidymod_lm_pure <- function(X, Y) {
  stopifnot(is.matrix(X), is.vector(Y), nrow(Y) == nrow(X))
  
  .Call("tidymodelR_lm_rcpp", X, Y, PACKAGE = "tidymodelR")
}

#' OLS estimation - Matrix
#' 
#' The function conduct the very common OLS estimation, which minimizes the sum of squared residuals. 
#' @param X is the covariate matrix, which contains all explanatory variables. Has to be a matrix or be convertible
#' with as.matrix() 
#' @param Y is the explained variable. Has to be numeric or be convertible with as.numeric()
#' @return A list is returned containing the following elements coefficients, standard errors, 
#' degrees of freedom, variance-covariance matrix, fitted values, residuals, data used in the regression,
#' the call, the intercept and the formula
#' @export
#' @examples 
#' tidymod_lm_matrix(mtcars[,2:3], mtcars[,1])
tidymod_lm_matrix <- function(X, Y, ...) {
  
  if(!(is.matrix(X) & is.numeric(Y))) {
    X <- as.matrix(X)
    Y <- as.numeric(Y)
  }
  
  res <- tidymod_lm_pure(X, Y)
  
  res$coefficients <- as.vector(res$coefficients)
  names(res$coefficients) <- colnames(X)
  res$fitted.values <- as.vector(X %*% res$coefficients)
  res$residuals <- Y - res$fitted.values
  res$data <- cbind(Y, X)
  res$call <- match.call()
  res$intercept <- any(apply(X, 2, function(x) all(x == x[1])))
  
  class(res) <- "tidymod_lm"
  res
}

#' OLS estimation - formula
#' 
#' The function conduct the very common OLS estimation, which minimizes the sum of squared residuals. The function
#' is implemented such that it works with piping, which creates more readable code.
#' @param data must be a dataframe containing the variables used in the estimation
#' @param formula must be a modelformula, which specifies the explained and explanatory variables
#' @return A list is returned containing the following elements coefficients, standard errors, 
#' degrees of freedom, variance-covariance matrix, fitted values, residuals, data used in the regression,
#' the call, the intercept and the formula
#' @export
#' @examples 
#' tidymod_lm(mtcars, mpg~cyl)
#' mtcars %>% tidymod_lm(mpg~cyl)
tidymod_lm <- function(data, formula) {
  
  if(!is.data.frame(data)) {
    abort("data has to be a data.frame")
  }
  
  if(!is_formula(formula)) {
    abort("formula has to be a model formula")
  }
  
  mf <- model.frame(formula = formula, data = data)
  X <- model.matrix(attr(mf, "terms"), data=mf)
  Y <- model.response(mf)
  
  res <- tidymod_lm_matrix(X, Y)
  res$call <- match.call()
  res$formula <- formula
  res$intercept <- attr(attr(mf, "terms"), "intercept")
  res$mf
  res$X
  res$Y
  res
}

#' @export
print.tidymod_lm <- function(x) {
  cat("\nCall:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$coefficients, digits=5)
}

#' @export
summary.tidymod_lm <- function(object) {
  se <- object$stderr
  tval <- coef(object) / se
  
  TAB <- cbind(Estimate = coef(object),
               StdErr = se,
               t.value = tval,
               p.value = 2*pt(-abs(tval), df=object$df))
  
  # why do I need this here?
  rownames(TAB) <- names(object$coefficients)
  colnames(TAB) <- c("Estimate", "StdErr", "t.value", "p.value")
  ## cf src/library/stats/R/lm.R and case with no weights and an intercept
  f <- object$fitted.values
  r <- object$residuals
  a <- object$data[, 1]
  #mss <- sum((f - mean(f))^2)
  mss <- if (object$intercept) sum((f - mean(f))^2) else sum(f^2)
  rss <- sum(r^2)
  tss <- sum((a - mean(a))^2)
  
  r.squared <- 1 - (rss / tss)
  df.int <- if (object$intercept) 1L else 0L
  
  n <- length(f)
  rdf <- object$df
  adj.r.squared <- 1 - (1 - r.squared) * ((n - df.int)/rdf)
  
  vcov <- object$vcov
  colnames(vcov) <- names(object$coefficients)
  rownames(vcov) <- names(object$coefficients)
  
  res <- list(call=object$call,
              coefficients=TAB,
              r.squared=r.squared,
              adj.r.squared=adj.r.squared,
              sigma=sqrt(sum((object$residuals)^2)/rdf),
              df=object$df,
              residSum=summary(object$residuals, digits=5)[-4],
              vcov = vcov)
  
  class(res) <- "summary.tidymod_lm"
  res
}

#' @export
print.summary.tidymod_lm <- function(x) {
  cat("\nCall:\n")
  print(x$call)
  cat("\nResiduals:\n")
  print(x$residSum)
  cat("\n")
  
  printCoefmat(x$coefficients, P.values=TRUE, has.Pvalue=TRUE)
  digits <- max(3, getOption("digits") - 3)
  cat("\nResidual standard error: ", formatC(x$sigma, digits=digits), " on ",
      formatC(x$df), " degrees of freedom\n", sep="")
  cat("Multiple R-squared: ", formatC(x$r.squared, digits=digits),
      ",\tAdjusted R-squared: ",formatC(x$adj.r.squared, digits=digits),
      "\n", sep="")
  invisible(x)
}

#' @export
predict.tidymod_lm <- function(object) {
  if (is.null(newdata)) {
    y <- fitted(object)
  } else {
    if (!is.null(object$formula)) {
      x <- model.matrix(object$formula, newdata)
    } else {
      x <- newdata
    }
    y <- as.vector(x %*% coef(object))
  }
  y
}
 
#' @export
vcov.tidymod_lm <- function(object) {
  res <- object$vcov
  rownames(res) <- names(object$coefficients)
  colnames(res) <- names(object$coefficients)
  res
}

#' @export
vcov.summary.tidymod_lm <- function(object) {
  res <- object$vcov
  rownames(res) <- rownames(object$coefficients)
  colnames(res) <- rownames(object$coefficients)
  res
}

#' @export 
confint.tidymod_lm <- function(object, parm, level = 0.95) {
  cf <- coef(object)
  pnames <- names(cf)
  if(missing(parm)) parm <- pnames
  else if(is.numeric(parm)) parm <- pnames[parm]
  a <- (1 - level) / 2
  a <- c(a, 1 - a)
  fac <- qt(a, object$df.residual)
  pct <- format.perc(a, 3)
  ci <- array(NA, dim = c(length(parm), 2L),
              dimnames = list(parm, pct))
  ses <- sqrt(diag(vcov(object)))[parm]
  ci[] <- cf[parm] + ses %o% fac
  ci
}


