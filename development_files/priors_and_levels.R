
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
  data = data_warpbreaks, warmup = 1000, iter = 11000,
)
prior_warpbreaks

summary(prior_warpbreaks)

samps <- rstan::As.mcmc.list(prior_warpbreaks)

bayesplot::mcmc_dens(prior_warpbreaks, regex_pars = "^b") +
  ggplot2::coord_cartesian(xlim = c(-10, 10))

bayesplot::mcmc_hist_by_chain(prior_warpbreaks, regex_pars = "^b", binwidth = 5) +
  ggplot2::xlim(c(-1000, 1000))


bayesplot::mcmc_trace(prior_warpbreaks, regex_pars = "^b")

### re-transform:
samps2 <- rstan::extract(prior_warpbreaks, pars = "b")
bmat <- cbind(0, samps2$b)
head(bmat)

## use generated data
NNN <- 1e6
bmat <- cbind(0, replicate(5, rcauchy(NNN)))

ef_2levels <- bmat[,1:2] %*% t(t(c(1, sqrt(2)/2)))
ef_3levels <- vector("list", 3)
for (i in 1:3) {
  ef_3levels[[i]] <-
    bmat[,1:3] %*% rbind(1, t(contr.bayes(3)[i,, drop = FALSE]))

}
ef_3levels <- data.frame(ef_3levels)
colnames(ef_3levels) <- c("L1_3", "L2_3", "L3_3")

options(contrasts=c('contr.bayes', 'contr.poly'))

newdf <- expand.grid(wool = levels(warpbreaks$wool),
            tension = levels(warpbreaks$tension))
newmm <- model.matrix(~wool * tension, newdf)

ef_inter <- vector("list", 6)
for (i in 1:6) {
   ef_inter[[i]] <-
    bmat %*% t(newmm[i, , drop = FALSE])
}
ef_inter <- data.frame(ef_inter)
colnames(ef_inter) <- c("C1_inter", "C2_inter", "C3_inter",
                        "C4_inter", "C5_inter", "C6_inter")
head(ef_inter)
library("tidyverse")
xlim <- c(-20, 20)

all_e <- cbind(levels2 = ef_2levels,
      ef_3levels, ef_inter)
all_e %>%
  pivot_longer(cols = everything()) %>%
  ggplot(aes(value)) +
  geom_density() +
  facet_wrap("name") +
  xlim(xlim)

xlim <- c(-20, 20)
## xlim <- c(-500, 500)
ef_2levels <- tibble(effect = ef_2levels)
ggplot(ef_2levels, aes(effect)) +
  geom_density() +
  xlim(xlim)
ggplot(ef_2levels, aes(effect)) +
  geom_histogram(binwidth = 1) +
  xlim(xlim)

ef_3levels %>%
  pivot_longer(cols = everything()) %>%
  ggplot(aes(value)) +
  geom_histogram(binwidth = 1) +
  xlim(c(-500, 500)) +
  facet_wrap("name")

ef_inter %>%
  pivot_longer(cols = everything()) %>%
  ggplot(aes(value)) +
  geom_histogram(binwidth = 1) +
  xlim(c(-500, 500)) +
  facet_wrap("name")
