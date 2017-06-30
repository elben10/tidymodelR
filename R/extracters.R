#' @export
tidymod_coef <- function(object, ...) {
  UseMethod("tidymod_coef")
}

#' @export
tidymod_coef.tidymod_lm <- function(object, ...) {
  res <- cbind(object$coefficients)
  rownames(res) <- names(object$coefficients)
  colnames(res) <- "Estimate"
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

#' @export
tidymod_confint <- function(object, parm, conf_level = 0.95, ...) {
  UseMethod("tidymod_confint")
}

#' @export
tidymod_confint.tidymod_lm <- function(object, parm, conf_level = 0.95, ...) {
  confint(object, parm, level = conf_level, ...)
}