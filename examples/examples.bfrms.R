\dontrun{
library(bfrms)
data(Machines, package = "MEMSS")
Machines <- droplevels(Machines[Machines$Machine %in% c("A", "C"),])

fit1 <- bfrm(score ~ Machine + (Machine||Worker),
             Machines,
             iter = 25000, warmup = 1000,
             cores = 4)
fit0 <- bfrm(score ~ 1 + (Machine||Worker),
             Machines,
             iter = 25000, warmup = 1000,
             cores = 4)
library("bridgesampling")
bayes_factor(fit1, fit0, silent = TRUE)
}
