% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download.R
\name{read_sample_data}
\alias{read_sample_data}
\title{Read sample data into memory from a URL}
\usage{
read_sample_data(url, nrows = 100)
}
\arguments{
\item{url}{(character) A file URL.}

\item{nrows}{(integer) Number of rows to read. Default is 100.}
}
\value{
A data.frame object contains data.
}
\description{
Read sample data into memory from a URL.
}
\details{
None
}
\examples{
# not run
sample_data = read_sample_data(files_df$link[1], nrows = 100)

}
\references{
None
}
\seealso{
\code{\link{get_file_list}}, \code{\link{read_sample_data0}}
}
\author{
Dewey Data Inc.
}
