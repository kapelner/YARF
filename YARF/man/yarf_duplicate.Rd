% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/YARF_hypothesis_tests.R
\name{yarf_duplicate}
\alias{yarf_duplicate}
\title{Duplicates the settings of YARF model. Wait is \code{TRUE}, all verbose logging is turned
off and serializing is turned off as well.}
\usage{
yarf_duplicate(yarf_mod, X = NULL, y = NULL)
}
\arguments{
\item{yarf_mod}{The YARF model with settings you wish to duplicate}

\item{X}{The training data as a data frame. Default is \code{NULL} indicating the original training data.}

\item{y}{The training responses as numeric / integer / factor vector. Default is \code{NULL} indicating 
the original training responses.}
}
\value{

}
\description{
Duplicates the settings of YARF model. Wait is \code{TRUE}, all verbose logging is turned
off and serializing is turned off as well.
}
\author{
Adam Kapelner
}
