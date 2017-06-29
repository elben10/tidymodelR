#' @import rlang
#' @importFrom stats model.frame model.matrix model.response coef fitted printCoefmat pt

# The following function is copied from RcppArmadillo, and the modified

tidymod_lm_pure <- function(X, Y, ...) {
  stopifnot(is.matrix(X), is.vector(Y), nrow(Y) == nrow(X))
  
  .Call("tidymodelR_lm_rcpp", X, Y, PACKAGE = "tidymodelR")
}

#' @export
tidymod_lm_matrix <- function(X, Y, ...) {
  
  X <- as.matrix(X)
  Y <- as.numeric(Y)
  
  res <- tidymod_lm_pure(X, Y)
  
  res$coefficients <- as.vector(res$coefficients)
  names(res$coefficients) <- colnames(X)
  res$fitted.values <- as.vector(X %*% res$coefficients)
  res$residuals <- Y - res$fitted.values
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

#' @export
print.tidymod_lm <- function(x, ...) {
  cat("\nCall:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$coefficients, digits=5)
}

#' @export
summary.tidymod_lm <- function(object, ...) {
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
  #mss <- sum((f - mean(f))^2)
  mss <- if (object$intercept) sum((f - mean(f))^2) else sum(f^2)
  rss <- sum(r^2)
  
  r.squared <- mss/(mss + rss)
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
print.summary.tidymod_lm <- function(x, ...) {
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
predict.tidymod_lm <- function(object, newdata=NULL, ...) {
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
tidymod <- function(object, newdata=NULL, ...) {
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

