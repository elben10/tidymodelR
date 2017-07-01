
tidymodelR
==========

[![Build Status](https://travis-ci.org/elben10/tidymodelR.svg?branch=master)](https://travis-ci.org/elben10/tidymodelR) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/elben10/tidymodelR?branch=master&svg=true)](https://ci.appveyor.com/project/elben10/tidymodelR) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tidymodelR)](https://cran.r-project.org/package=tidymodelR) [![codecov](https://codecov.io/gh/elben10/tidymodelr/branch/master/graph/badge.svg)](https://codecov.io/gh/elben10/tidymodelr)

THE PACKAGE IS VERY MUCH UNDER CONSTRUCTION

Overview
========

tidymodelR is model package to R, which aim at making the statistical modelling and testing more handy in R. It implement the most common estimation methods, and will contain the most common statistical tests in the future.

Installation
------------

``` r
# At the moment, the only way to install tidymodelR is from Github, which can be installed by:
devtools::install_github("elben10/tidymodelR")
```

Usage
-----

``` r
library(tidymodelR)

mtcars %>%
  tidymod_lm(mpg~cyl) %>%
  summary()
#> 
#> Call:
#> tidymod_lm(data = ., formula = mpg ~ cyl)
#> 
#> Residuals:
#>     Min.  1st Qu.   Median  3rd Qu.     Max. 
#> -4.98140 -2.11850  0.22174  1.07170  7.51860 
#> 
#>             Estimate   StdErr t.value   p.value    
#> (Intercept) 37.88458  2.07384 18.2678 < 2.2e-16 ***
#> cyl         -2.87579  0.32241 -8.9197 6.113e-10 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 3.206 on 30 degrees of freedom
#> Multiple R-squared: 0.7262,  Adjusted R-squared: 0.7171

mtcars %>%
  tidymod_lm(mpg~cyl) %>%
  tidymod_confint()
#>                 2.5 %    97.5 %
#> (Intercept) 33.649223 42.119930
#> cyl         -3.534237 -2.217343

mtcars %>%
  tidymod_iv(mpg~cyl|am) %>%
  summary()
#> 
#> Call:
#> tidymod_iv(data = ., formula = mpg ~ cyl | am)
#> 
#> Residuals:
#>     Min.  1st Qu.   Median  3rd Qu.     Max. 
#> -7.16360 -2.67010  0.40649  2.20490  6.12990 
#> 
#>             Estimate   StdErr t.value   p.value    
#> (Intercept) 44.05714  4.43226  9.9401 5.261e-11 ***
#> cyl         -3.87338  0.70856 -5.4666 6.240e-06 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 3.682 on 30 degrees of freedom
#> Multiple R-squared: 0.6388,  Adjusted R-squared: 0.6268

mtcars %>%
  tidymod_iv(mpg~cyl|am) %>%
  confint()
#>                 2.5 %    97.5 %
#> (Intercept) 35.005266 53.109020
#> cyl         -5.320445 -2.426308
```
