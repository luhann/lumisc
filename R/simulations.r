#' Simulate multivariate normal
#'
#' Simulate from a multivariate normal distribution.
#'
#' @param n Number of simulation replicates.
#'
#' @param mu Mean vector.
#'
#' @param cov Symmetric positive definite variance-covariance matrix.
#'
#' @details
#' Uses the Cholesky decomposition of the matrix `cov`, obtained by
#'   [base::chol()], so singular (positive semi-definite) matrices are not supported.
#'
#' @importFrom stats rnorm
#' @export
#' @return
#' A matrix of size n x `length(mu)`.  Each row corresponds to a
#'   separate replicate.
#'
#' @examples
#' rmvn(100, c(1, 2), matrix(c(1, 1, 1, 4), ncol = 2))
#' @seealso
#' [stats::rnorm()]
#'
#' @keywords
#' datagen
rmvn = function(n, mu = 0, cov = matrix(1)) {
  p = length(mu)
  cov = as.matrix(cov)
  if (!identical(dim(cov), c(p, p))) {
    rlang::abort("cov matrix has incorrect number of dimensions", class = "invalid_dim_error")
  }
  if (!isSymmetric(unname(cov))) {
    rlang::abort("cov matrix is not symmetric", class = "asymmetric_cov_error")
  }
  matrix(rnorm(n * p), ncol = p) %*% chol(cov) + rep(mu, rep(n, p))
}
