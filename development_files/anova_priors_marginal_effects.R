
library("mvtnorm")
library("tidyverse")
theme_set(theme_bw(base_size = 15) +
            theme(legend.position="bottom",
                  panel.grid.major.x = element_blank()))

NSAMPLES <- 1e5

### functions ###

contr.bayes <- function(n, contrasts = TRUE) {
  if (length(n) <= 1L) {
    if (is.numeric(n) && length(n) == 1L && n > 1L)
      TRUE
    else stop("not enough degrees of freedom to define contrasts")
  } else n <- length(n)
  cont <- diag(n)
  if (contrasts) {
    a <- n
    I_a <- diag(a)
    J_a <- matrix(1, nrow = a, ncol = a)
    Sigma_a <- I_a - J_a/a
    cont <- eigen(Sigma_a)$vectors[,seq_len(a-1), drop = FALSE]
  }
  cont
}

rand_b_anova <- function(levels, n, hfixed = 1) {
  amin1 <- levels - 1
  g <- 1/rgamma(n, 1/2, hfixed^2/2)
  alphamin1 <- lapply(
    X = seq_along(g),
    FUN = function(x) rmvnorm(1, sigma = g[x]*diag(amin1))
  )
  do.call("rbind", alphamin1)
}

transform_b_to_levels <- function(bmat) {
  nlevels <- ncol(bmat) + 1
  mmat <- contr.bayes(nlevels)
  #mmat <- mmat / max(mmat)
  mat_out <- matrix(NA_real_, nrow = nrow(bmat), ncol = nlevels)
  for (i in seq_len(nlevels)) {
    mat_out[,i] <- bmat %*% t(mmat[i, , drop = FALSE])
  }
  colnames(mat_out) <- paste0("L", seq_len(nlevels))
  as_tibble(mat_out)
}

### main code

ef_2levels <- transform_b_to_levels(rand_b_anova(2, NSAMPLES)) %>%
  mutate(levels = "2 Levels") %>%
   pivot_longer(cols = -levels)
ef_3levels <- transform_b_to_levels(rand_b_anova(3, NSAMPLES)) %>%
  mutate(levels = "3 Levels") %>%
   pivot_longer(cols = -levels)
ef_6levels <- transform_b_to_levels(rand_b_anova(6, NSAMPLES)) %>%
  mutate(levels = "6 Levels") %>%
   pivot_longer(cols = -levels)

xlim <- c(-10, 10) ## to focus on area where it matters
bind_rows(ef_2levels, ef_3levels, ef_6levels) %>%
  ggplot(aes(value)) +
  geom_density() +
  facet_grid(rows = vars(name), cols = vars(levels)) +
  xlim(xlim)
ggsave("prior_pred.pdf", width = 10, height = 12)
