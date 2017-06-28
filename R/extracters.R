#' @export
tidymod_coef <- function(object, ...) {
  UseMethod("tidymod_coef")
}

#' @export
tidymod_coef.tidymod_lm <- function(object, ...) {
  res <- cbind(object$coefficients)
  rownames(res) <- names(object$coefficients)
  colnames(res) <- "coefficients"
  res
}

#' @export
tidymod_coef.summary.tidymod_lm <- function(object, ...) {
  res <- cbind(object[["coefficients"]][, 1])
  colnames(res) <- "Estimate"
  res
}

#' @export
tidymod_stderr <- function(object, ...) {
  UseMethod("tidymod_stderr")
}

#' @export
tidymod_stderr.tidymod_lm <- function(object, ...) {
  res <- cbind(object$stderr)
  rownames(res) <- names(object$coefficients)
  colnames(res) <- "std.err"
  res
}

#' @export
tidymod_stderr.summary.tidymod_lm <- function(object, ...) {
  res <- cbind(object[["coefficients"]][, 2])
  colnames(res) <- "StdErr"
  res
}