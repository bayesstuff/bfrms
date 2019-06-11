#' Orthonormal Contrast Matrices for Bayesian Estimation
#'
#' Returns a design or model matrix of orthonormal contrasts such that the
#' marginal prior on all effects is identical. Implementation follows the
#' description in Rouder, Morey, Speckman, & Province (2012, p. 363).
#'
#' @param n a vector of levels for a factor, or the number of levels.
#' @param logical indicating whether contrasts should be computed.
#'
#' @references Rouder, J. N., Morey, R. D., Speckman, P. L., & Province, J. M.
#'   (2012). Default Bayes factors for ANOVA designs. *Journal of Mathematical
#'   Psychology*, 56(5), 356-374. https://doi.org/10.1016/j.jmp.2012.08.001
#'
#' @return A `matrix` with n rows and k columns, with k=n-1 if contrasts is
#'   `TRUE`` and k=n if contrasts is `FALSE`.
#'
#' @example examples/examples.contr.bayes.R
#'
#' @export
contr.bayes <- function(n, contrasts = TRUE) {
  if (length(n) <= 1L) {
    if (is.numeric(n) && length(n) == 1L && n > 1L)
      TRUE
    else stop("not enough degrees of freedom to define contrasts")
  } else n <- length(n)
  cont <- diag(n)
  if (contrasts) {
    a <- n
    I_a <- diag(a)
    J_a <- matrix(1, nrow = a, ncol = a)
    Sigma_a <- I_a - J_a/a
    cont <- eigen(Sigma_a)$vectors[,seq_len(a-1), drop = FALSE]
  }
  cont
}
