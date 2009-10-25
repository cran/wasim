\name{wasim-package}
\alias{wasim-package}
\alias{wasim}
\docType{package}
\title{
Visualisation and analysis of output files of the hydrological model WASIM
}
\description{
Visualisation and analysis of output files of the hydrological model WASIM
}
\details{
\tabular{ll}{
Package: \tab wasim\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2008-12-16\cr
License: \tab GPL-2\cr
}

}
\author{
Dominik Reusser

Maintainer: Dominik Reusser <dreusser@uni-potsdam.de>
}
\keyword{ hplot }
\keyword{ utilities }
\keyword{package}
\examples{

d.datum <- read.dates(file= system.file("weisseritz/qgesww100.txt", package="wasim"))  # Datumsreihe heisst jetzt d.datum
d.wasim.out <- read.results("", system.file("weisseritz", package="wasim"),ts.length=21937,subcatchments=2, ending="ww100.txt")    # Datensatz heisst jetzt d.wasim.out 
d.meas.all<-read.table(system.file("weisseritz/ww100-rh.txt", package="wasim"),header=TRUE, na.strings ="999", skip=4)  # gemessene Daten einlesen
d.meas<-d.meas.all$Ammelsdorf[69716:91652]
p.storage(d.wasim.out, xdata=d.datum, measured=d.meas)

sd.grid <- read.grid( file= system.file("weisseritz/sd__ww100.grd", package="wasim"))  # Datumsreihe heisst jetzt d.datum
p.grid(sd.grid, 
        color.palette=topo.colors,
	      zlim=c(-20,20),
        main="Saturation deficit at the Weisseritz")

#for english x-lab in Germany/Switzerland:
Sys.setlocale(category = "LC_ALL", locale = "en")

}
