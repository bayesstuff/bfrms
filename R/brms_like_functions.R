#' brms-like Functions
#' @rdname brms_like
#' @aliases brms_like
#'
#' @description Generate code or data for `brms` models using JSZ-priors to be
#' passed to `Stan`. `make_stancode_bfrms` is the `bfrms` alias of
#' [brms::make_stancode()] and `make_standata_bfrms` is the `bfrms` alias of
#' [brms::make_standata()].
#'
#' @param ... further arguments passed to the corresponding `brms` function. See
#'   their help pages for details.
#' @inheritParams bfrm
#'
#' @return `make_stancode_bfrms` returns the same as [brms::make_stancode()] and
#'   `make_standata_bfrms` returns the same as [brms::make_standata()].
#'
#' @export
make_stancode_bfrms <- function(formula, data,
                                family = gaussian(),
                                prior_structure = "jzs",
                                prior_arg = list(r_fixed = 0.5, r_random = 1),
                                ...) {
  dots <- list(...)
  brm_args <- prep_brm(formula = formula,
                       data = data,
                       family = family,
                       prior_structure = prior_structure,
                       prior_arg = prior_arg)
    do.call(what = "make_stancode",
          args = c(
            brm_args,
            dots
          ))
}

#' @rdname brms_like
#' @export
make_standata_bfrms <- function(formula, data,
                                family = gaussian(),
                                prior_structure = "jzs",
                                prior_arg = list(r_fixed = 0.5, r_random = 1),
                                ...) {
  dots <- list(...)
  brm_args <- prep_brm(formula = formula,
                       data = data,
                       family = family,
                       prior_structure = prior_structure,
                       prior_arg = prior_arg)
    do.call(what = "make_standata",
          args = c(
            brm_args,
            dots
          ))
}
