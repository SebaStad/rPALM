% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/palm_global.R
\docType{data}
\name{palm_global}
\alias{palm_global}
\title{Create global parameters for static driver}
\format{An object of class \code{R6ClassGenerator} of length 24.}
\usage{
palm_global
}
\arguments{
\item{title}{Title of the file}

\item{author}{Author, i.e. you}

\item{institute}{Your institute}

\item{location}{Site of the static driver}

\item{x0}{x-coordinate of the lower left point in your coordinate system}

\item{y0}{y-coordinate of the lower left point in your coordinate system}

\item{z0}{lowest z-coordinate in your domain}

\item{t0}{good question}

\item{lat}{Latitude of the lower left point of your domain}

\item{lon}{Longitude of the lower left point of your domain}

\item{dx}{Grid spacing}
}
\value{
Creates the PALM-Header Class
}
\description{
Create an R6 class that contains all global attributes needed
in the static driver.
}
\examples{
palm_global$new('Testglobal', 'Max Mustermann', 'Some Institute',
'Somewehere', 1, 1, 0, 0, 33,33, 10)
}
\keyword{datasets}