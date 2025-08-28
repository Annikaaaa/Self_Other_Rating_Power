 ### best version
library(mlmpower)  # e package that provides effect size caluclation
library(dplyr)
library(purrr)
library(tidyr)
   
 # Parameter grids
icc_vals          <- c(.5, .10, .15)
within_vals       <- c(0.3, .5, .7)
between_vals      <- c(0.5, .10)
product_vals      <- c(0.05, 0.10, .20)
random_slope_vals <- c(.01, 0.02)
   
n_between_vals <- c(100, 200, 300, 400, 500)
n_within_vals  <- c(10)
   
# Expand all combinations of effect size parameters + sample sizes
param_grid <- expand.grid(
icc          = icc_vals,
within       = within_vals,
between      = between_vals,
product      = product_vals,
random_slope = random_slope_vals,
n_between    = n_between_vals,
n_within     = n_within_vals
)
   
# Function to build model + run power analysis for one combination
run_power <- function(icc, within, between, product, random_slope, n_between, n_within) {
tryCatch({
model <- effect_size(
icc          = icc,
within       = within,
between      = between,
product      = product,
random_slope = random_slope
) +
outcome("y", mean = 2.58, sd = 0.69) +
within_predictor("x1", icc = 0, weight = 0.80) +
within_predictor("x2", weight = 0.80) +
between_predictor("z1", weight = 0.30) +
between_predictor("z2", weight = 0.30) +
product("x1", "z1", weight = 1) +
random_slope("x1", weight = 1)
       
pa <- power_analysis(
model= model,
replications = 2000,
n_between    = n_between,
n_within     = n_within
)
       
tibble(success = TRUE, n_between = n_between, n_within = n_within, result = list(pa))
}, error = function(e) {
tibble(success = FALSE, n_between = n_between, n_within = n_within, result = NA)
     })
   }
   
# Loop over all parameter + sample size combinations
results <- param_grid %>%
mutate(out = pmap(param_grid, run_power)) %>%
unnest_wider(out)
   
# Final results table
results
   
   
   
extract_power <- function(pa_obj) {
if (is.na(pa_obj)) return(tibble(power = NA, CI_low = NA, CI_high = NA))
     
s <- summary(pa_obj)  # adjust if your package uses different function/columns
tibble(
power   = s$power,    # replace with actual column name
CI_low  = s$CI_low,   # replace with actual column name
CI_high = s$CI_high   # replace with actual column name
)
}
   
results_summary <- results %>%
mutate(power_summary = map(result, extract_power)) %>%
unnest(cols = power_summary) %>%
select(-result)  # remove full objects to keep table clean
   
results_summary
   