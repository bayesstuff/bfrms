#' Fit Bayesian Linear Mixed Model with JZS Prior
#'
#' @param formula An object of class [`formula`].
#' @param data An object of class `data.frame` containing all observations and
#'   variables used in the model.
#' @param family Currently only [gaussian()] is supported.
#' @param prior_structure character string. Currently only `'jzs'` (for
#'   Jeffreys-Zellner-Siow) is supported.
#' @param prior_arg `list` of additional arguments specific to
#'   `prior_structure`. For `'jzs'`, these can be `r_fixed` and `r_random`.
#' @param ... Further arguments passed to [brms::brm()] such as `iter`,
#'   `warmup`, or `cores`.
#'
#' @export
bfrm <- function(formula, data,
                 family = gaussian(),
                 prior_structure = "jzs",
                 prior_arg = list(r_fixed = 0.5, r_random = 1),
                 sigma_scaling = 1,
                 ...) {
  dots <- list(...)
  if ("save_all_pars" %in% names(dots)) {
    dots[["save_all_pars"]] <- NULL
  }
  brm_args <- prep_brm(formula = formula,
                       data = data,
                       family = family,
                       prior_structure = prior_structure,
                       prior_arg = prior_arg,
                       sigma_scaling = sigma_scaling)
    do.call(what = "brm",
          args = c(
            brm_args,
            dots,
            save_all_pars = TRUE
          ))

}

