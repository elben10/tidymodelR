# The following code is copied from broom, and modified afterwards

#' @importFrom broom fix_data_frame augment_columns
#' @importFrom plyr ldply
#' @importFrom stringr str_replace

#' @export
tidy.tidymod_lm <- function(x, conf.int = FALSE, conf.level = .95, ...) {
  s <- summary(x)
  ret <- tidy.summary.tidymod_lm(s)
  
#  process_tidymod_lm(ret, x, conf.int = conf.int, conf.level = conf.level)
}

#' @export
tidy.summary.tidymod_lm <- function(x, ...) {
  co <- coef(x)
  nn <- c("estimate", "std.error", "statistic", "p.value") 
  if(inherits(co, "listof")) {
    ret <- ldply(co, fix_data_frame, nn[1:ncol(co[[1]])],
                       .id = "response")
    ret$response <- str_replace(ret$response, "Response ", "")
  } else {
    ret <- fix_data_frame(co, nn[1:ncol(co)])
  }
  ret
}

process_tidymod_lm <- function(ret, x, conf.int = FALSE, conf.level = 0.95) {
  if(conf.int == TRUE) {
   # CI <- suppressMessages(confint(x, level = conf.level))
    #colnames(CI) = c("conf.low", "conf.high")
    #ret <- cbind(ret, identity(unrowname(CI)))
    print("works")
  }
  #ret$estimate <- identity(ret$estimate)
  #ret
  print("works")
}
