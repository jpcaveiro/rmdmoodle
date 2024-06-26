% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stuff.R
\name{add_extension}
\alias{add_extension}
\title{Add an extension to a file name}
\usage{
add_extension(filename, extension)
}
\arguments{
\item{filename}{possibly with or without extension}

\item{extension}{Rmd, R, .Rmd, .R, or any other}
}
\value{
character file name with extension
}
\description{
If the file name has the extension then
nothing is added.
}
\examples{
add_extension("f",".Rmd")
add_extension("f",".R")
add_extension("f.Rmd","R")
}
