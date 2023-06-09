% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mars.R
\name{fwd_stepwise}
\alias{fwd_stepwise}
\title{Forward Stepwise}
\usage{
fwd_stepwise(y, x, control = mars.control())
}
\arguments{
\item{y}{vector of response values}

\item{x}{dataset of predictor variables}

\item{control}{mars.control object created by the mars.control method}
}
\value{
A list with elements: y, B, Bfuncs
\item{y}{the same vector of response values given as input}
\item{B}{final set of basis functions generated from the input}
\item{Bfuncs}{summary of the basis functions as a product of functions. Contains the information for the knot, variable, and sign}
}
\description{
Implements the Forward Stepwise algorithm (Algorithm 2 from page 17 of the
Friedman Paper) which  builds a linear prediction equation of linear basis functions which are the products of hinge functions.
}
\references{
\describe{
\item{Friedman, J. H. (1991). Multivariate Adaptive Regression Splines. The Annals of Statistics, 19(1), 1–67. https://doi.org/10.1214/aos/1176347963}{}
\item{Lin, W. (2023). Lecture Material.}{}}
}
