
data("fhch2010", package = "afex")

library("tidyverse")

fhch <- as_tibble(fhch2010) %>%
  filter(correct)

fhch2 <- fhch %>%
  group_by(id, stimulus) %>%
  nest() %>%
  mutate(samp = map(data, ~.[1:5,])) %>%
  select(id, stimulus, samp) %>%
  unnest()

####
fit1 <- bfrm(rt ~ task + (1|id),
             fhch2,
             iter = 25000, warmup = 1000,
             cores = 4)

summary(fit1)
#  Family: jzs_normal
#   Links: mu = identity; sigmaSQ = identity; interc = identity; g = identity
# Formula: rt ~ 0 + task + (1 | id)
#    Data: structure(list(id = structure(c(1L, 1L, 1L, 1L, 1L (Number of observations: 450)
# Samples: 4 chains, each with iter = 25000; warmup = 1000; thin = 1;
#          total post-warmup samples = 96000
#
# Group-Level Effects:
# ~id (Number of levels: 45)
#               Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
# sd(Intercept)     0.59      0.09     0.43     0.77      31505 1.00
#
# Population-Level Effects:
#       Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
# task1     0.15      0.07     0.02     0.28      47630 1.00
#
# Family Specific Parameters:
#         Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
# sigmaSQ     0.22      0.02     0.19     0.25     114615 1.00
# interc      1.07      0.05     0.98     1.17      48186 1.00
# g           5.67    645.02     0.05     7.23      83792 1.00
#
# Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample
# is a crude measure of effective sample size, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).

####

library("afex")
afex_options("method_mixed" = "S")

m_task_1 <- mixed(rt ~ task + (1|id), fhch)
m_task_1

m_task_2 <- mixed(rt ~ task + (1|id) + (1|item), fhch)
m_task_2

m_task_3 <- mixed(rt ~ task + (1|id) + (task||item), fhch, expand_re = TRUE)
m_task_3
summary(m_task_3)

m_task_4 <- mixed(rt ~ task + (1|id) + (task|item), fhch, expand_re = TRUE)
m_task_4
summary(m_task_4)
