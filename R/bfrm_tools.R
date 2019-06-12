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
