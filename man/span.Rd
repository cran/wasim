\name{span}
\alias{span}
\title{ Calculate the span/range of a time series}
\description{
This function has been implemented for a simpler call of apply.
}
\usage{
span(x)
}
\arguments{
  \item{x}{ A vector}
}
\value{
\code{ diff(range(x,na.rm=TRUE)) }
}
\author{ Dominik Reusser}
\examples{
   data(models_dawson,package="wasim")
   span(models_dawson[,1])

}
\keyword{ utilities }
