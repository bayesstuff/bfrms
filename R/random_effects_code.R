var_likelihood <- stanvar(scode = "
  real jzs_normal_lpdf(real Y, real mu, real sigmaSQ, real interc, real g) {
    return normal_lpdf(Y | interc + mu, sqrt(sigmaSQ));
  }
", block = 'functions')
var_likelihood0 <- stanvar(scode = "
  real jzs0_normal_lpdf(real Y, real mu, real sigmaSQ) {
    return normal_lpdf(Y | mu, sqrt(sigmaSQ));
  }
", block = 'functions')

var_model_1 <- "
  target += normal_lpdf(b | 0, sqrt(sigmaSQ * g[1]) * sigma_scaling);
  target += inv_gamma_lpdf(g | 0.5, 0.5 * r_fixed^2);
"

var_model_m <- "
  for (k in 1:K) {
    target += normal_lpdf(b[k] | 0, sqrt(sigmaSQ * g[b_MAP[k]]) * sigma_scaling);
  }
  target += inv_gamma_lpdf(g | 0.5, 0.5 * r_fixed^2);
"

jzs_normal <- custom_family("jzs_normal",
                            dpars = c("mu", "sigmaSQ", "interc", "g[TRMS]"),
                            type = "real",
                            lb = c(NA, 0, NA, 0))
jzs0_normal <- custom_family("jzs0_normal",
                            dpars = c("mu", "sigmaSQ"),
                            type = "real",
                            lb = c(NA, 0))


#' @importFrom brms make_standata
random_effects_code <- function(formula, data) {
  tmpdat <- make_standata(formula, data,
                          family = jzs_normal)
  re_terms <- grep("^M_" , names(tmpdat), value = TRUE)
  re_length <- unlist(tmpdat[re_terms])

  out_prior <- vector("list", length(re_terms))
  out_scale <- vector("list", length(re_terms))

  prior_template <- "  target += log(2) + log(SDi) + inv_gamma_lpdf(SDi^2.0 | 0.5, 0.5 * r_random^2);"
  # scale_template <- "r_1_1 = r_1_1 * sqrt(sigmaSQ);"

  for (i in seq_along(re_terms)) {
    tmp_prior <- vector("character", re_length[i])
    tmp_scale <- vector("character", re_length[i])
    for (j in seq_len(re_length[i])) {
      tmp_prior[j] <- gsub("SDi", paste0("sd_", i, "[", j, "]"), prior_template)
      tmp_scale[j] <- paste0("  r_", i, "_", j, " = ",  "r_", i, "_", j,
                             " * sqrt(sigmaSQ) * sigma_scaling;")
    }
    out_prior[[i]] <- tmp_prior
    out_scale[[i]] <- tmp_scale
  }
  return(list(
    prior = paste(unlist(out_prior), collapse = "\n"),
    scale = paste(unlist(out_scale), collapse = "\n")
  ))
}


