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
