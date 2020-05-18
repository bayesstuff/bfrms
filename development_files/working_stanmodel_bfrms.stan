// generated with brms 2.10.0
functions {

  real jzs_normal_lpdf(real Y, real mu, real sigmaSQ, real interc, real g) {
    return normal_lpdf(Y | interc + mu, sqrt(sigmaSQ));
  }

}
data {
  int<lower=1> N;  // number of observations
  vector[N] Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  int<lower=1> J_1[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_1;
  vector[N] Z_1_2;
  int prior_only;  // should the likelihood be ignored?
  real r_fixed;
  real r_random;
  int TRMS;
  int b_MAP;
}
transformed data {
}
parameters {
  vector[K] b;  // population-level effects
  real<lower=0> sigmaSQ;
  real interc;
  real<lower=0> g[TRMS];
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  // standardized group-level effects
  vector[N_1] z_1[M_1];
}
transformed parameters {
  // actual group-level effects
  vector[N_1] r_1_1 = (sd_1[1] * (z_1[1]));
  vector[N_1] r_1_2 = (sd_1[2] * (z_1[2]));
  r_1_1 = r_1_1 * sqrt(sigmaSQ);
  r_1_2 = r_1_2 * sqrt(sigmaSQ);
}
model {
  // initialize linear predictor term
  vector[N] mu = X * b;

  target += normal_lpdf(b | 0, sqrt(sigmaSQ * g[1]));
  target += inv_gamma_lpdf(g | 0.5, 0.5 * r_fixed^2);
   target += log(2) + log(sd_1[1]) + inv_gamma_lpdf(sd_1[1]^2.0 | 0.5, 0.5 * r_random^2);
  target += log(2) + log(sd_1[2]) + inv_gamma_lpdf(sd_1[2]^2.0 | 0.5, 0.5 * r_random^2);
  for (n in 1:N) {
    // add more terms to the linear predictor
    mu[n] += r_1_1[J_1[n]] * Z_1_1[n] + r_1_2[J_1[n]] * Z_1_2[n];
  }
  // priors including all constants
  target += normal_lpdf(z_1[1] | 0, 1);
  target += normal_lpdf(z_1[2] | 0, 1);
  target +=  -log(sigmaSQ);
  // likelihood including all constants
  if (!prior_only) {
    for (n in 1:N) {
      target += jzs_normal_lpdf(Y[n] | mu[n], sigmaSQ, interc, g[TRMS]);
    }
  }
}
generated quantities {
}
