check_coding <- function(formula, data) {
  if (options("contrasts")[[1]][1] != "contr.bayes") {
    model_vars <- all.vars(formula)
    factor_vars <- vapply(model_vars,
                          function(x)
                            is.factor(data[[x]]) | is.character(data[[x]]),
                          NA)
    vars_change <- model_vars[factor_vars]
    for (i in seq_along(vars_change)) {
      contrasts(data[[vars_change[i]]]) <- "contr.bayes"
    }
    if (length(vars_change) > 0) {
      message(paste0("Contrasts set to contr.bayes for following variables: ",
                     paste0(vars_change, collapse=", ")))
    }
  }
  data
}

check_prior_arg <- function(prior_structure,
                        prior_arg) {
  prior_structure <- match.arg(tolower(prior_structure), "jzs")
  if (prior_structure == "jzs") {
    ## replace default values with passed values
    default_prior_arg <- eval(formals(bfrm)$prior_arg)
    out_prior_arg <- default_prior_arg
    out_prior_arg[names(prior_arg)] <- prior_arg
  }
  out_prior_arg
}

prep_brm <- function(formula, data,
                     family,
                     prior_structure, prior_arg) {

  prior_arg <- check_prior_arg(prior_structure = prior_structure,
                           prior_arg = prior_arg)

  if (is.brmsformula(formula)) {
    stop("bfrm currently only supports non-brms formulas.", call. = FALSE)
  } else if (inherits(formula, "formula")) {
    formula <- update.formula(formula, ~.)
    formula_fixed <- lme4::nobars(formula)
    if (attr(terms(formula_fixed, data = data), "intercept") == 0) {
      stop("formula needs to have an intercept (i.e., no 0/-1)", call. = FALSE)
    }
    mm <- model.matrix(formula_fixed, data = data)
    if (ncol(mm) > 1) {
      intercept_only <- FALSE
      brm_family <- jzs_normal
      var_llk <- var_likelihood
      bf_formula <- brmsformula(update.formula(formula, ~ 0 + .), cmc = FALSE)
      var_prior <- prior_string("", class = "sigmaSQ") +
        prior_string("", class = "sd") +
        prior_string("target +=  -log(sigmaSQ)", class = "sigmaSQ", check = FALSE)
      var_data <-
        stanvar(scode = paste0("real r_fixed = ", prior_arg$r_fixed,";"),
                block = "tdata") +
        stanvar(scode = paste0("real r_random = ", prior_arg$r_random,";"),
                block = "tdata") +
        stanvar(scode = paste0("int TRMS = ", max(attr(mm, "assign")),";"),
                block = "tdata") +
        stanvar(x = attr(mm, "assign")[-1], name = "b_MAP")
      code_model_extra <- if (length(attr(mm, "assign")[-1]) > 1)
        var_model_m else var_model_1
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
      var_data <- NULL
    }
  } else stop("formula needs to be a formula.", call. = FALSE)

  data <- check_coding(formula, data)




  re_code <- random_effects_code(bf_formula, data)

  stanvars <- var_llk +
    stanvar(scode = paste(code_model_extra, re_code$prior, collapse = "\n"),
            block = "model") +
    stanvar(scode = re_code$scale,
            block = "tparameters") +
    var_data

  c(
    formula = list(bf_formula),
    data = list(data),
    family = list(brm_family),
    stanvars = list(stanvars),
    prior = list(var_prior)
  )
}
