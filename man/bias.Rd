\name{bias}
\alias{bias}
\alias{mean_error}
\alias{rmse}
\title{ Calculate objective functions between two vectors }
\description{
The function calculates the bias between two time series
}
\usage{
bias(a, b, cond = rep(TRUE, NROW(a)))
rmse(a, b, cond = rep(TRUE, NROW(a)))
mean_error(a, b, cond = rep(TRUE, NROW(a)))
}
\arguments{
  \item{a}{ First vector }
  \item{b}{ Second vector }
  \item{cond}{ Vector of boolean, indicating which cases to exclude }
}
\value{
The method describes a scalar the bias between the vectors
}
\author{ Dominik Reusser }
\keyword{ utilities }
