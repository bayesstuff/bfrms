rm(list = ls())

library(mvtnorm)
library(MASS)
library(fractional)


#-------------------------------------------------------------------------------
# naive sum-to-zero constraint implementation
#-------------------------------------------------------------------------------

n <- 1e5
amin1 <- 2
hfixed <- 1
g <- 1/rgamma(n, 1/2, hfixed^2/2)
alphamin1 <- sapply(seq_along(g), function(x) rmvnorm(1, sigma = g[x]*diag(amin1)))

i <- 2
hist(alphamin1[i,][abs(alphamin1[i,]) < 8], probability = TRUE, breaks = 50)
plot(function(x) dcauchy(x, scale = hfixed), add = TRUE, xlim = c(-8, 8))

alpha3 <- apply(alphamin1, 2, function(x) -sum(x))

round(apply(rbind(alphamin1, alpha3), 2, sum), 3)

hist(alpha3[abs(alpha3) < 8], probability = TRUE, breaks = 50)
plot(function(x) dcauchy(x, scale = hfixed), add = TRUE, xlim = c(-8, 8))
# shows that naive sum-to-zero constraint implementation yields more diffuse
# prior on eliminated effect

#-------------------------------------------------------------------------------
# better projection approach
#-------------------------------------------------------------------------------

a <- 3
I_a <- diag(a)
J_a <- matrix(1, nrow = a, ncol = a)
Sigma_a <- I_a - J_a/a
Q_a <- eigen(Sigma_a)$vectors[,seq_len(a-1)]
t(Q_a)
fractional(t(Q_a))

a <- 2
I_a <- diag(a)
J_a <- matrix(1, nrow = a, ncol = a)
Sigma_a <- I_a - J_a/a
Q_a <- eigen(Sigma_a)$vectors[,seq_len(a-1)]
t(Q_a)

a <- 5
I_a <- diag(a)
J_a <- matrix(1, nrow = a, ncol = a)
Sigma_a <- I_a - J_a/a
Q_a <- eigen(Sigma_a)$vectors[,seq_len(a-1)]
t(Q_a)

n <- 1e5
amin1 <- a - 1
hfixed <- 1
g <- 1/rgamma(n, 1/2, hfixed^2/2)
alphamin1 <- sapply(seq_along(g), function(x) rmvnorm(1, sigma = g[x]*diag(amin1)))

i <- 3
hist(alphamin1[i,][abs(alphamin1[i,]) < 8], probability = TRUE, breaks = 50)
plot(function(x) dcauchy(x, scale = hfixed), add = TRUE, xlim = c(-8, 8))


# convert back to alpha (solve alphamin1 = t(Q_a) %*% alpha for alpha using SVD)
svdtQ <- svd(t(Q_a))
Qdiag <- diag(1/svdtQ$d)
alpha <- svdtQ$v %*% Qdiag %*% t(svdtQ$u) %*% alphamin1 

for (i in seq_len(a)) {
  Sys.sleep(1)
  hist(alpha[i,][abs(alpha[i,]) < 10], probability = TRUE, breaks = 20)
  plot(function(x) dcauchy(x, scale = hfixed), add = TRUE, xlim = c(-8, 8))
}
# all marginal priors identical
