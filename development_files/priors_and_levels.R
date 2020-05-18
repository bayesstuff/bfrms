
warpbreaks$id <- 1:nrow(warpbreaks)

fit_warpbreaks <- bfrm(
  breaks ~ wool * tension +
    (1|id), data = warpbreaks,
  warmup = 1000, iter = 2000, chains = 2, cores = 1,
  sample_prior = "only")


code_warpbreaks <- make_stancode_bfrms(
  breaks ~ wool * tension +
    (1|id), data = warpbreaks,
  save_model = "development_files/model_warpbreaks.stan")

data_warpbreaks <- make_standata_bfrms(
  breaks ~ wool * tension +
    (1|id), data = warpbreaks)

data_warpbreaks$prior_only <- 1
data_warpbreaks$X

prior_warpbreaks <- rstan::stan(
  file = "development_files/model_warpbreaks.stan",
  data = data_warpbreaks
)
prior_warpbreaks

summary(prior_warpbreaks)
