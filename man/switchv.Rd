% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.r
\name{switchv}
\alias{switchv}
\title{switchv}
\usage{
switchv(EXPR, ...)
}
\arguments{
\item{EXPR}{An expression evaluating to a vector of numbers of strings}

\item{...}{List of alternatives}
}
\value{
Vector of returned values.
}
\description{
Vectorized version of switch
}
\details{
Vectorized version of \code{\link[base:switch]{base::switch()}}: just loops over
input and calls \code{\link[base:switch]{base::switch()}}.
}
\examples{
switchv(c("horse", "fish", "cat", "bug"),
  horse = "fast",
  cat = "cute",
  "what?"
)
}
\author{
Karl Broman
}
