## 4_scoring_utils.R

# Functions for standardization of indices, scoring and plotting


# raw standardization function between 0 and 1 using min and max values
# if xmin or xmax are set, a manual min and max range are used for standardizing between 0 and 1
# if use_95 = TRUE, the min and max are set based on 95% quantile ranges to exclude outliers
linear_std <- function(x, ..., xmin = NA, xmax = NA, use_95 = T) {
  if (is.na(xmax)) {
    xmax <- max(x, na.rm = T)
    if (use_95 == TRUE) xmax <- unlist(quantile(x, na.rm = T, probs = 0.975))
  }
  if (is.na(xmin)) {
    xmin <- min(x, na.rm = T)
    if (use_95 == TRUE) xmin <- unlist(quantile(x, na.rm = T, probs = 0.025))
  }

  y <- rep(NA, length(x))

  for (i in 1:length(x)) {
    if (is.na(x[i])) next
    z <- ifelse(x[i] < xmin, xmin,
      ifelse(x[i] > xmax, xmax, x[i]))
    y[i] <- (z - xmin) / (xmax - xmin)
  }

  return(y)
}

# inverse raw standardization function with 1 corresponding to lowest value
invlinear_std <- function(x, ..., xmin = NA, xmax = NA, use_95 = T) {
  if (is.na(xmax)) {
    xmax <- max(x, na.rm = T)
    if (use_95 == TRUE) xmax <- unlist(quantile(x, na.rm = T, probs = 0.975))
  }
  if (is.na(xmin)) {
    xmin <- min(x, na.rm = T)
    if (use_95 == TRUE) xmin <- unlist(quantile(x, na.rm = T, probs = 0.025))
  }

  y <- rep(NA, length(x))

  for (i in 1:length(x)) {
    if (is.na(x[i])) next
    z <- ifelse(x[i] < xmin, xmin, ifelse(x[i] > xmax, xmax, x[i]))
    y[i] <- 1 - (z - xmin) / (xmax - xmin)
  }
  return(y)
}

## asymptotic standardization function for indicators where higher values = greater risk
logarithmic_std <- function(x, ..., lambda = 0.6, xmin = NA, xmax = NA) {

  if (is.na(xmax)) xmax <- max(x, na.rm = T)
  if (is.na(xmin)) xmin <- min(x, na.rm = T)
  y <- rep(NA, length(x))

  for (i in 1:length(x)) {
    if (is.na(x[i])) next
    z <- ifelse(x[i] < xmin, xmin, ifelse(x[i] > xmax, xmax, x[i]))
    z_std <- (z - xmin) / (xmax - xmin)
    y[i] <- 1 - exp(-lambda * z_std) + exp(-lambda) * z_std
  }
  return(y)
}

# exponential increase function where higher values = greater risk
exponential_std <- function(x, ..., lambda = 1, xmin = NA, xmax = NA, use_95 = T) {

  if (is.na(xmax)) {
    xmax <- max(x, na.rm = T)
    if (use_95 == TRUE) xmax <- unlist(quantile(x, na.rm = T, probs = 0.975))
  }
  if (is.na(xmin)) {
    xmin <- min(x, na.rm = T)
    if (use_95 == TRUE) xmin <- unlist(quantile(x, na.rm = T, probs = 0.025))
  }

  y <- rep(NA, length(x))
  for (i in 1:length(x)) {
    if (is.na(x[i])) next
    z <- ifelse(x[i] < xmin, xmin, ifelse(x[i] > xmax, xmax, x[i]))
    z_std <- (z - xmin) / (xmax - xmin)
    y[i] <- exp(lambda * z_std) / exp(lambda) - exp(-lambda) * (1 - z_std)
  }

  return(y)
}

# exponential decay standardization function for indicators where higher values = lower risk
decay_std <- function(x, ..., lambda = 0.03, xmin = NA, xmax = NA, use_95 = T) {

  if (is.na(xmax)) {
    xmax <- max(x, na.rm = T)
    if (use_95 == TRUE) xmax <- unlist(quantile(x, na.rm = T, probs = 0.975))
  }
  if (is.na(xmin)) {
    xmin <- min(x, na.rm = T)
    if (use_95 == TRUE) xmin <- unlist(quantile(x, na.rm = T, probs = 0.025))
  }
  y <- rep(NA, length(x))

  for (i in 1:length(x)) {
    if (is.na(x[i])) next
    z <- ifelse(x[i] < xmin, xmin, ifelse(x[i] > xmax, xmax, x[i]))
    z_std <- (z - xmin) / (xmax - xmin)
    y[i] <- exp(-lambda * z_std) - exp(-lambda) * z_std
  }
  return(y)
}

step_std <- function(x, ..., x1 = 200, x2 = 300, x3 = NA, x4 = NA) {
  # Standardize a score into 3 categories
  y <- rep(NA, length(x))

  nstep <- sum(!is.na(c(x1, x2, x3, x4))) + 1

  for (i in 1:length(x)) {
    if (is.na(x[i])) next
    if (x[i] < x1) y[i] <- (1 / nstep)
    if (x[i] >= x1 && x[i] < x2) y[i] <- (2 / nstep)
    if (x[i] >= x2) y[i] <- (3 / nstep)
  }

  return(y)
}


cat_std <- function(x, ..., x1 = "G", x2 = "A/G", x3 = "A", x4 = "R/A", x5 = "R") {
  # Standardize a score between 0 and 1 based on category values
  y <- rep(NA, length(x))

  for (i in 1:length(x)) {
    if (is.na(x[i])) next
    if (x[i] == x1) y[i] <- 0
    if (x[i] == x2) y[i] <- 0.25
    if (x[i] == x3) y[i] <- 0.5
    if (x[i] == x4) y[i] <- 0.75
    if (x[i] == x5) y[i] <- 1
  }
  return(y)
}

enh_std <- function(x, ..., z) {
  # Standardize a score between 0 and 1 based on category values
  y <- rep(NA, length(x))

  for (i in 1:length(x)) {
    if (x[i] == 0) y[i] <- 0
    if (x[i] > 0 && !grepl("Harvest", z[i])) y[i] <- 0.5
    if (x[i] > 0 && grepl("Harvest", z[i])) y[i] <- 1
  }
  return(y)
}





simulate_range <- function(x, n = 100) {

  minx <- min(x, na.rm = T)
  maxx <- max(x, na.rm = T)

  y <- seq(from = minx, to = maxx, length.out = n)
  return(y)

}



# sim_std <- simulate_range(seq(-0.5,0.1, by = 0.01))

# ggplot() +
#   geom_histogram(data = CVIS_CU, aes(x = rateT_spn), bins = 20, fill = "darkgreen")
#

#
# ggplot() +
#   geom_line(data = CVIS_std, aes(x = augQ_spn, y = augQ_spn_rawstd)) +
#   geom_point(data = CVIS_std, aes(x = augQ_spn, y = augQ_spn_rawstd)) +
#   geom_line(data = CVIS_std, aes(x = augQ_spn, y = augQ_spn_decay), color = "red") +
#   geom_point(data = CVIS_std, aes(x = augQ_spn, y = augQ_spn_decay))
