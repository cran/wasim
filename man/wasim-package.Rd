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

  unzip(system.file("weisseritz.zip", package="wasim"))

d.datum <- read.dates(file= "weisseritz/qgesww100.txt")  # Datumsreihe heisst jetzt d.datum
d.wasim.out <- read.results("", "weisseritz",ts.length=21937,subcatchments=2, ending="ww100.txt")    # Datensatz heisst jetzt d.wasim.out 
d.meas.all<-read.table("weisseritz/ww100-rh.txt",header=TRUE, na.strings ="999", skip=4)  # gemessene Daten einlesen
d.meas<-d.meas.all$Ammelsdorf[69716:91652]
p.storage(d.wasim.out, xdata=d.datum, measured=d.meas)

sd.grid <- read.grid( file= "weisseritz/sd__ww100.grd")  # Datumsreihe heisst jetzt d.datum
p.grid(sd.grid, 
        color.palette=topo.colors,
	      zlim=c(-20,20),
        main="Saturation deficit at the Weisseritz")

#for english x-lab in Germany/Switzerland:
Sys.setlocale(category = "LC_ALL", locale = "en")

}
