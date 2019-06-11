
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

var_model <- "
target += normal_lpdf(b | 0, sqrt(sigmaSQ * g));
target += inv_gamma_lpdf(g | 0.5, 0.5 * r_fixed^2);
"

jzs_normal <- custom_family("jzs_normal",
                            dpars = c("mu", "sigmaSQ", "interc", "g"),
                            type = "real",
                            lb = c(NA, 0, NA, 0))
jzs0_normal <- custom_family("jzs0_normal",
                            dpars = c("mu", "sigmaSQ"),
                            type = "real",
                            lb = c(NA, 0))

