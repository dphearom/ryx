# ryx

<!-- badges: start -->

<!-- badges: end -->

The goal of ryx is to create a correlation table between a dependent variable y and a vector of independent variables x which return a correlation table of class ryx. Additionally, there are specific print.ryx, plot.ryx, and summary.ryx for the class ryx table.

## Installation

You can install the development version of ryx like so:

``` r
# packages.install("ryx")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ryx)
library(MASS)
x <- ryx(Boston, y = "medv", x = c("age", "dis", "lstat"))

print(x)
summary(x)
plot(x)
```
