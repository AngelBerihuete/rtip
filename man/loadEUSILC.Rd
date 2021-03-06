% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loadEUSILC.R
\name{loadEUSILC}
\alias{loadEUSILC}
\title{Load the living conditions survey (EUSILC)}
\usage{
loadEUSILC(eusilc_d_file, eusilc_h_file)
}
\arguments{
\item{eusilc_d_file}{a string with the filename of \code{D}-file.}

\item{eusilc_h_file}{a string with the filename of \code{H}-file.}
}
\value{
A data frame containing the variables required to use the functions
in the package.
}
\description{
\code{loadEUSILC()} extracts some variables from the EUSILC
survey files and transforms them into a suitable data frame in order to do
the calculations.
}
\details{
Vector strings \code{varD} and \code{varH} contain the names of the
variables needed to do the calculations with rtip package. These variables are given
by Eurostat in two different files, namely basic household register
(\code{H}-file) and household data (\code{D}-file).
}
\note{
We do not give examples in this function because the EUSILC survey
datasets have a restricted licence for use.
}
\author{
A. Berihuete, C.D. Ramos and M.A. Sordo
}
