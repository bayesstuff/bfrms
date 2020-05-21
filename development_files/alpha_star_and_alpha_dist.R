
library("mvtnorm")
library("tidyverse")
theme_set(theme_bw(base_size = 15) +
            theme(legend.position="bottom",
                  panel.grid.major.x = element_blank()))

NSAMPLES <- 1e5
FAC_LEVELS <- c(2, 3, 6)
xlim <- c(-10, 10)


#################################################################
##                    Step 1: Sample alpha*                    ##
#################################################################

## two different generation functions:
gen_alphas_g <- function(levels, n, hfixed = 1) {  ## hfixed is rfixed in BayesFactor
  amin1 <- levels - 1
  g <- 1/rgamma(n, 1/2, hfixed^2/2)
  alphamin1 <- lapply(
    X = seq_along(g),
    FUN = function(x) rmvnorm(1, sigma = g[x]*diag(amin1))
  )
  do.call("rbind", alphamin1)
}

gen_alphas_mvc <- function(levels, n, hfixed = 1) {  ## hfixed is rfixed in BayesFactor
  amin <- levels - 1
  sigma <- diag(amin)
  diag(sigma) <- hfixed^2
  mvtnorm::rmvt(n, sigma = sigma, df = 1)
}

alpha_star_g <- vector("list", length(FAC_LEVELS))
alpha_star_mvc <- vector("list", length(FAC_LEVELS))

for (i in seq_along(FAC_LEVELS)) {
  alpha_star_g[[i]] <- gen_alphas_g(FAC_LEVELS[i], n = NSAMPLES)
  # cat("\nalpha star (g) cor (with ", FAC_LEVELS[i], " levels):\n", sep = "")
  # print(cor(alpha_star_g[[i]]))
  alpha_star_mvc[[i]] <- gen_alphas_mvc(FAC_LEVELS[i], n = NSAMPLES)
  # cat("\nalpha star (mvc) cor (with ", FAC_LEVELS[i], " levels):\n", sep = "")
  # print(cor(alpha_star_mvc[[i]]))
}

### plot marginal priors

## create reference line
ref <- tibble(
  value = seq(xlim[1], xlim[2], length.out = 201)
) %>%
  mutate(cauchy_0_7 = dcauchy(value, 0, scale = sqrt(2)/2),
         cauchy_1_0 = dcauchy(value, 0, scale = 1)) %>%
  pivot_longer(cols = -value, names_to = "cauchy", values_to = "density")

## function for transforming parameters to longn format for printing
transform_par_long <- function(mat, alphas = TRUE) {
  if (!is.matrix(mat)) mat <- matrix(mat, ncol = 1)
  if (alphas) {
    nlev <- ncol(mat) + 1
    colnames(mat) <- paste0("P_", seq_len(ncol(mat)))
  }
  else {
    nlev <- ncol(mat)
    colnames(mat) <- paste0("L_", seq_len(ncol(mat)))
  }
  out <- as_tibble(mat)
  out <- mutate(out, levels = paste(nlev, "Levels"))
  pivot_longer(out, -levels)
}

## combine both variants
alpha_star <- bind_rows(
  mutate(map_dfr(alpha_star_g, transform_par_long), generated = "g"),
  mutate(map_dfr(alpha_star_mvc, transform_par_long), generated = "mvc")
)


### all priors of alpha star are marginally cauchy:
alpha_star %>%
  ggplot(aes(value)) +
  geom_density(aes(color = generated), size = 1) +
  geom_line(data = ref, aes(x = value, y = density, linetype = cauchy),
            color = "black") +
  facet_grid(rows = vars(name), cols = vars(levels)) +
  xlim(xlim)


### repeat with different prior scale:
alpha_star_g2 <- vector("list", length(FAC_LEVELS))
alpha_star_mvc2 <- vector("list", length(FAC_LEVELS))

for (i in seq_along(FAC_LEVELS)) {
  alpha_star_g2[[i]] <- gen_alphas_g(FAC_LEVELS[i], n = NSAMPLES, hfixed = sqrt(2)/2)
  alpha_star_mvc2[[i]] <- gen_alphas_mvc(FAC_LEVELS[i], n = NSAMPLES, hfixed = sqrt(2)/2)
}

## combine both variants
alpha_star2 <- bind_rows(
  mutate(map_dfr(alpha_star_g2, transform_par_long), generated = "g"),
  mutate(map_dfr(alpha_star_mvc2, transform_par_long), generated = "mvc")
)

### all priors of alpha star are marginally cauchy:
alpha_star2 %>%
  ggplot(aes(value)) +
  geom_density(aes(color = generated), size = 1) +
  geom_line(data = ref, aes(x = value, y = density, linetype = cauchy),
            color = "black") +
  facet_grid(rows = vars(name), cols = vars(levels)) +
  xlim(xlim)


##################################################################
##           Step 2: Transform alpha* to alpha with Q           ##
##################################################################


## creates Q matrix
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

transform_alphas_and_make_long <- function(mat) {
  if (!is.matrix(mat)) mat <- matrix(mat, ncol = 1)
  nlevels <- ncol(mat) + 1
  mmat <- contr.bayes(nlevels)
  mat_out <- t(mmat %*% t(mat))
  transform_par_long(mat_out, alphas = FALSE)
}

## alpha = Q * alpha*
alpha <- bind_rows(
  mutate(map_dfr(alpha_star_g, transform_alphas_and_make_long), generated = "g"),
  mutate(map_dfr(alpha_star_mvc, transform_alphas_and_make_long), generated = "mvc")
)

alpha %>%
  ggplot(aes(value)) +
  geom_density(aes(color = generated), size = 1) +
  geom_line(data = ref, aes(x = value, y = density, linetype = cauchy),
            color = "black") +
  facet_grid(rows = vars(name), cols = vars(levels)) +
  xlim(xlim)

## alpha = Q * alpha*
alpha2 <- bind_rows(
  mutate(map_dfr(alpha_star_g2, transform_alphas_and_make_long), generated = "g"),
  mutate(map_dfr(alpha_star_mvc2, transform_alphas_and_make_long), generated = "mvc")
)

alpha2 %>%
  ggplot(aes(value)) +
  geom_density(aes(color = generated), size = 1) +
  geom_line(data = ref, aes(x = value, y = density, linetype = cauchy),
            color = "black") +
  facet_grid(rows = vars(name), cols = vars(levels)) +
  xlim(xlim)

