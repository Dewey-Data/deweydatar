% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download.R
\name{read_local}
\alias{read_local}
\title{Read local data into memory from a path}
\usage{
read_local(path, nrows = -1)
}
\arguments{
\item{nrows}{(integer) Number of rows to read. Default is -1 (all).}

\item{apikey}{(character) Path to a .csv.gz or csv file.}
}
\value{
A data.frame object contains data.
}
\description{
Read local data into memory from a path
}
\details{
This can be slow. Recommend to use \code{fread} function
at \code{data.table} package.
}
\examples{
# not run
local_data = read_local(path, nrows = 100)

}
\references{
None
}
\seealso{
None
}
\author{
Dewey Data Inc.
}
