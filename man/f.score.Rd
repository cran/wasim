\name{f.score}
\alias{f.score}
\title{ Calculate objective functions for sensitive parts of a time series  }
\description{
Calculate objective functions for sensitive parts of a time series. ToDo: write better description.
}
\usage{
f.score(data, sens, measured, parameter, criterion, limit)
}
\arguments{
  \item{data}{ a matrix of modeled time series }
  \item{sens}{ result from \code{\link[fast]{sensitivity}} }
  \item{measured}{ measured time series}
  \item{parameter}{ vector of indizes, indicating which parameters to include in the analysis }
  \item{criterion}{ 1: rmse, 2: Nash-Sutcliffe coeffiecient of efficiency, 3: Nash-Sutcliffe coeffiecient of efficiency with more weight on high flows }
  \item{limit}{ limit, above which the sensitivity must be in order for the data to be included in the analysis. }
}
\value{
 A vector of results of the objective function, one for each time series
}
\author{ Dominik Reusser }
\examples{
#ToDo: Write an example
}
\keyword{ utilities }
\keyword{ univar }% __ONLY ONE__ keyword per line
