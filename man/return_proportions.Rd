% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proportions.r
\name{return_proportions}
\alias{return_proportions}
\title{Return group proportions}
\usage{
return_proportions(dataframe, variable, precision = 2)
}
\arguments{
\item{dataframe}{The dataframe containing all longitudinal measures.}

\item{variable}{The variable you want to return the category proportions for.}

\item{precision}{The number of decimal points to round to.}
}
\value{
A datafram with one entry per category of the variable and its proportion.
}
\description{
Give proportions and counts for each category of a given group in a long form dataset.
}
