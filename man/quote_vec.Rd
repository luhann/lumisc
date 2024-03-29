% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.r
\name{quote_vec}
\alias{quote_vec}
\title{Return vector as quoted character vector}
\usage{
quote_vec(vec, collapse = ", ")
}
\arguments{
\item{vec}{A character or integer vector to be quoted}

\item{collapse}{An optional character string to separate the results. Not NA_character_.}
}
\value{
An quoted vector of length \code{vec}.
}
\description{
Take an existing R vector and add quotes to it for easy manipulation and copy/pasting.
}
