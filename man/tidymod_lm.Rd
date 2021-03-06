% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lm.R
\name{tidymod_lm}
\alias{tidymod_lm}
\title{OLS estimation - formula}
\usage{
tidymod_lm(data, formula)
}
\arguments{
\item{data}{must be a dataframe containing the variables used in the estimation}

\item{formula}{must be a modelformula, which specifies the explained and explanatory variables}
}
\value{
A list is returned containing the following elements coefficients, standard errors,
degrees of freedom, variance-covariance matrix, fitted values, residuals, data used in the regression,
the call, the intercept and the formula
}
\description{
The function conduct the very common OLS estimation, which minimizes the sum of squared residuals. The function
is implemented such that it works with piping, which creates more readable code.
}
\examples{
tidymod_lm(mtcars, mpg~cyl)
mtcars \%>\% tidymod_lm(mpg~cyl)
}
