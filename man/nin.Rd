% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/infix.r
\name{nin}
\alias{nin}
\alias{\%!in\%}
\title{Inverse Value Matching}
\usage{
x \%!in\% table
}
\arguments{
\item{x}{The values to be matched. Long vectors are supported.}

\item{table}{Vector or ‘NULL’: the values to be matched against. Long vectors are not supported.}
}
\description{
Complement of \code{\%in\%}. Returns the elements of \code{x} that are
not in \code{y}.
}
\examples{
1 \%!in\% 1:10
1 \%!in\% 2:10
11 \%!in\% 1:10
}
