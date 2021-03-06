% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dictionary.R
\name{dictionary_reset}
\alias{dictionary_reset}
\title{Reset the added-variables dictionary file.}
\usage{
dictionary_reset(filename = "added-variables.yaml")
}
\arguments{
\item{filename}{The name of the yaml dictionary file}
}
\description{
Deletes the dictionary yaml file, so that it can be updated again. Really
should only be used for the \code{added-variables.yaml} dictionary.
}
