\name{read.results}
\alias{read.results}
\alias{read.dates}
\alias{read.observations}
\title{ read.results }
\description{
  Import WASIM simulation results
}
\usage{
read.results(record, path = "wasim/output", 
                 ts.length, subcatchments, data.types.prefix =
                 data.types$prefix, data.types.has_stat =
                 data.types$has_stat, ending, endings = rep(ending,
                 NROW(data.types.prefix)), na.values = c("999",
                 "999.00"), read.files.nr = 1:nrow(data.types),
                 remove.missing = FALSE)

read.dates(file=NULL,na.strings=c("999", "999.00","-9999.00"),sep="\t", skip=3)

read.observations(filelist, path = "wasim/input", gauge_names = NULL,
                 date_modelled = NULL, na.strings = c("999", "999.00",
                 "9999", "9999.00", "9999.000000", "-9999", "-9999.0",
                 "-9999.00"), sep = "")
}
\arguments{
  \item{file}{ WASIM output file the date information is read from. If not specified, the function tries to open the first available file from the globaldata.types\$filename.}
  \item{record}{ subdirectory/ string to be appended to path (eg. for multiple runs, etc.)}
  \item{path}{ path the output files reside in (no ending slash) }
  \item{ts.length}{ number of records to be read }
  \item{subcatchments}{ number of subcatchments (columns) in WASIM-output file (without statistic column) }
  \item{data.types.prefix}{ prefixes for filenames (better use default) }
  \item{data.types.has_stat}{ indicator whether files contain an extra column for summary statistics}
  \item{ending}{ suffix for filenames }
  \item{endings}{ suffixes for filenames. Use this if endings differ for various files.  }
  \item{read.files.nr}{numeric vector indicating which entries from data.types.prefix to process}
  \item{na.values}{ list of strings for indicating nodata values in the WASIM-files }
  \item{na.strings}{ Strings in input file to be interpreted as nodata (NA)}
  \item{sep}{ field separator in input file}
  \item{remove.missing}{ boolean indication whether to remove an entry
  from the default data.types if a file was not found}
  \item{skip}{ number of lines to skip from the header}
	\item{filelist}{list of input files to be considered}
	\item{gauge_names}{names of gauges that should be read (NULL:read all)}
	\item{date_modelled}{timespan that will be extracted}
}
\details{
  read.results causes error messages:
   "...line n hasn't m elements..." The global variable data.types\$has\_stat specifies an statistics column in one of the output files where there is none.

   "...has length  14 expecting length 15 - filling ..." The global variable data.types\$has\_stat specifies no statistics column in one of the output files where there seems to be one.
}
\value{
  \item{date_vec}{vector of POSIX-dates}
}
\author{ Dominik Reusser, Till Francke }
\seealso{  \code{\link{p.storage}} }
\examples{
  unzip(system.file("weisseritz.zip", package="wasim"))
d.datum <- read.dates(file= "weisseritz/qgesww100.txt")  # Datumsreihe heisst jetzt d.datum
str(d.datum)

d.wasim.out <- read.results("", "weisseritz",ts.length=21937,subcatchments=2, ending="ww100.txt")    # Datensatz heisst jetzt d.wasim.out 

str(d.wasim.out)
}
\keyword{ utilities }
