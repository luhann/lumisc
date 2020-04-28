#  rmvn
#'
#' Simulate multivariate normal
#'
#' Simulate from a multivariate normal distribution.
#'
#' @param n Number of simulation replicates.
#'
#' @param mu Mean vector.
#'
#' @param cov Variance-covariance matrix.
#'
#' @details
#' Uses the Cholesky decomposition of the matrix `cov`, obtained by
#'   [base::chol()].
#'
#' @importFrom stats rnorm
#' @export
#' @return
#' A matrix of size n x `length(mu)`.  Each row corresponds to a
#'   separate replicate.
#'
#' @examples
#' x = rmvn(100, c(1, 2), matrix(c(1, 1, 1, 4), ncol = 2))
#' @seealso
#' [stats::rnorm()]
#'
#' @keywords
#' datagen
rmvn = function(n, mu = 0, cov = matrix(1)) {
  p = length(mu)
  if (any(is.na(match(dim(cov), p)))) {
    rlang::abort(glue::glue("cov matrix has incorrect number of dimensions"), class = "invalid_dim_error")
  }
  d = chol(cov)
  matrix(rnorm(n * p), ncol = p) %*% d + rep(mu, rep(n, p))
}