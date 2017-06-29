format.perc <- function(probs, digits)
  ## Not yet exported, maybe useful in other contexts:
  ## quantile.default() sometimes uses a version of it
  paste(format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits),
        "%")

# Copied from broom
unrowname <- function(x) {
  rownames(x) <- NULL
  x
}