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
                 prior_structure = "jzs", prior_arg = list(r_fixed = 0.5,
                                                           r_random = 0.5),
                 ...) {
  dots <- list(...)
  if ("save_all_pars" %in% names(dots)) {
    dots[["save_all_pars"]] <- NULL
  }
  if (is.brmsformula(formula)) {
    stop("bfrm currently only supports non-brms formulas.", call. = FALSE)
  } else if (inherits(formula, "formula")) {
    formula_fixed <- lme4::nobars(formula)
    mm <- model.matrix(formula_fixed, data = data)
    if (ncol(mm) > 1) {
      intercept_only <- FALSE
      brm_family <- jzs_normal
      var_llk <- var_likelihood
      code_model_extra <- var_model
      bf_formula <- brmsformula(as.formula(paste(
        deparse(formula[[2]], width.cutoff = 500L),
        "~ 0 +",
        deparse(formula[[3]], width.cutoff = 500L))), cmc = FALSE)
      var_prior <- prior_string("", class = "sigmaSQ") +
        prior_string("", class = "sd") +
        prior_string("target +=  -log(sigmaSQ)", class = "sigmaSQ", check = FALSE)
    } else {
      intercept_only <- TRUE
      brm_family <- jzs0_normal
      var_llk <- var_likelihood0
      code_model_extra <- NULL
      bf_formula <- brmsformula(formula, cmc = FALSE)
      var_prior <- prior_string("", class = "sigmaSQ") +
        prior_string("", class = "sd") +
        prior_string("target +=  -log(sigmaSQ)", class = "sigmaSQ", check = FALSE) +
        prior_string("", class = "Intercept")
    }
  } else stop("formula needs to be a formula.", call. = FALSE)

  var_data <-
    stanvar(scode = paste0("real r_fixed =", prior_arg$r_fixed,";"),
            block = "tdata") +
    stanvar(scode = paste0("real r_random =", prior_arg$r_random,";"),
            block = "tdata")


  re_code <- random_effects_code(bf_formula, data)

  stanvars <- var_llk +
    stanvar(scode = paste(code_model_extra, re_code$prior, collapse = "\n"),
            block = "model") +
    stanvar(scode = paste(re_code$scale, collapse = "\n"),
            block = "tparameters") +
    var_data
  do.call(what = "brm",
          args = c(
            formula = list(bf_formula),
            data = list(data),
            family = list(brm_family),
            stanvars = list(stanvars),
            prior = list(var_prior),
            dots,
            save_all_pars = TRUE
          ))
}
