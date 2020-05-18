
fit_warp <- bfrm(breaks ~ wool * tension, data = warpbreaks,
                 iter = 25000, warmup = 1000,
                 cores = 1)
summary(fit_warp)

fit_warp_prior <- stanova_lm(breaks ~ wool * tension, data = warpbreaks,
                    prior = rstanarm::student_t(3, 0, 3, autoscale = FALSE),
                    chains = 2, iter = 2000, prior_PD = TRUE)
summary(fit_warp_prior)
