
library("mvtnorm")
library("tidyverse")
theme_set(theme_bw(base_size = 15) +
            theme(legend.position="bottom",
                  panel.grid.major.x = element_blank()))

NSAMPLES <- 1e5
FAC_LEVELS <- c(2, 3, 4, 5, 6)
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
transform_par_long <- function(mat, alphas = TRUE,
                               prefix = if (alphas) "P" else "L",
                               nlev) {
  if (!is.matrix(mat)) mat <- matrix(mat, ncol = 1)
  if (missing(nlev) & alphas) {
    nlev <- ncol(mat) + 1
  }
  else if (missing(nlev) & !alphas) {
    nlev <- ncol(mat)
  }
  colnames(mat) <- paste0(prefix, "_", seq_len(ncol(mat)))
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

## function for transforming parameters to pairs and long
transform_pairs_long <- function(mat, alphas = TRUE) {
  if (!is.matrix(mat) | ncol(mat) == 1) {
    return(transform_par_long(mat, prefix = "D", nlev = 1))
  } else {
    k <- ncol(mat)
    outmat <- matrix(NA_real_, nrow = nrow(mat), ncol = (k*(k-1))/2)
    c <- 1
    for (i in seq_len(k)) {
      for (j in seq_len(k)) {
        if (i == j) next
        if (i > j) next
        #print(c)
        outmat[,c] <- mat[,i] - mat[,j]
        c <- c+1
      }
    }
    return(transform_par_long(outmat, prefix = "D", nlev = ncol(mat) + alphas))
  }
}

diff_star <- bind_rows(
  mutate(map_dfr(alpha_star_g, transform_pairs_long), generated = "g"),
  mutate(map_dfr(alpha_star_mvc, transform_pairs_long), generated = "mvc")
)

### prior of difference of alpha star
diff_star %>%
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

transform_alphas_and_make_long <- function(mat, fin_fun = transform_par_long) {
  if (!is.matrix(mat)) mat <- matrix(mat, ncol = 1)
  nlevels <- ncol(mat) + 1
  mmat <- contr.bayes(nlevels)
  mat_out <- t(mmat %*% t(mat))
  fin_fun(mat_out, alphas = FALSE)
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

## prior of differences

alpha_dff <- bind_rows(
  mutate(map_dfr(alpha_star_g,
                 ~transform_alphas_and_make_long(., fin_fun = transform_pairs_long)),
         generated = "g"),
  mutate(map_dfr(alpha_star_mvc,
                 ~transform_alphas_and_make_long(., fin_fun = transform_pairs_long)),
         generated = "mvc")
)

### prior of difference of alpha star
alpha_dff %>%
  filter(as.numeric(substr(name, 3, 4)) < 6) %>%
  ggplot(aes(value)) +
  geom_density(aes(color = generated), size = 1) +
  geom_line(data = ref, aes(x = value, y = density, linetype = cauchy),
            color = "black") +
  facet_grid(rows = vars(name), cols = vars(levels)) +
  xlim(xlim)


##################################################################
##                  5 Levels and Prior Scaling                  ##
##################################################################

#### check case of 5 levels:
alpha_star_mvc_5l <- gen_alphas_mvc(5, n = NSAMPLES)
alpha_star_mvc_5l_l <- transform_par_long(alpha_star_mvc_5l) %>%
  mutate(scale = "alpha* (n - 1)") %>%
  mutate(name = str_replace(name, "P", "L"))
alpha_mvc_5l_l <- transform_alphas_and_make_long(alpha_star_mvc_5l) %>%
  mutate(scale = "alpha (n)")

ref <- tibble(
  value = seq(xlim[1], xlim[2], length.out = 201)
) %>%
  mutate(cauchy_0_9 = dcauchy(value, 0, scale = max(contr.bayes(5))),
         cauchy_1_0 = dcauchy(value, 0, scale = 1)) %>%
  pivot_longer(cols = -value, names_to = "cauchy", values_to = "density")

bind_rows(alpha_star_mvc_5l_l, alpha_mvc_5l_l) %>%
  ggplot(aes(value)) +
  geom_density(size = 1, color = "red") +
  geom_line(data = ref, aes(x = value, y = density, linetype = cauchy),
            color = "black") +
  facet_grid(rows = vars(name), cols = vars(scale)) +
  xlim(xlim)
