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
      tmp_scale[j] <- paste0("  r_", i, "_", j, " = ",  "r_", i, "_", j, " * sqrt(sigmaSQ);")
    }
    out_prior[[i]] <- tmp_prior
    out_scale[[i]] <- tmp_scale
  }
  return(list(
    prior = paste(unlist(out_prior), collapse = "\n"),
    scale = paste(unlist(out_scale), collapse = "\n")
  ))
}


