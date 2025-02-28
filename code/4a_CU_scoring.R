################################################################################
#
# 4a_CU_scoring.R
#
# 1 - get indicator values for each CU
# 2 - apply lower thresholds and standardization function
# 3 - calculate standardized indicator values
# 4 - summarize and compare indicator values across CUs
# 5 - estimate vulnerability indices in different categories
#
###############################################################################

## standardization function for indicators where higher values = greater risk
exp_std_inv <- function(x, lambda = 0.6) {
  # Standardize a score
  # x = raw score
  # lambda = rate parameter
  # returns standardized score
  return(1 - exp(-lambda * x))
}


# standardization function for indicators where higher values = lower risk
exp_std <- function(x, lambda = 0.03) {
  # Standardize a score
  # x = raw score
  # lambda = rate parameter
  # returns standardized score
  return(exp(-lambda * x))
}