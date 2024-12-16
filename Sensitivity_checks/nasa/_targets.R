#####
# Source packages
#
# Load necessary packages here
#####

library(targets)
library(tarchetypes)


#####
# Global options
#
# Set here global options and variables
#####

# Global variables
meta <- list()
meta$seed <- 5678
meta$sample_size <- 37
meta$min_iterations <- 200
meta$desired_power <- .8
meta$power_uncertainty <- .01
meta$step_size <- .005
meta$interv_eff_start <- .1
meta$interv_eff_end <- 2

# Global R Options
set.seed(meta$seed)
options(
  # Multicore features
  boot.parallel = "multicore",
  Ncpus = parallel::detectCores() - 1L,
  mc.cores = parallel::detectCores() - 1L,
  # Use cmdstanr backend with brms
  brms.backend = "cmdstanr",
  logger.threshold = logger::INFO
)

#####
# Details for target pipeline
#
# Define here all further details for the target pipeline.
#####

# Custom functions / scripts
#
# Load all custom functions / scripts that are necessary for the targets
# pipeline.
source("R/start_logger.R")
source("R/define_population.R")
source("R/optimize_sample_size.R")

# Custom options for pipeline
tar_option_set(
  # Provide certain packages to all pipeline items
  packages = c("data.table")
)


#####
# Details for target pipeline
#
# Define here all further details for the target pipeline.
#####

list(
  # Start logger
  tar_target(logger, start_logger(), cue = tar_cue("always")),

  # Define the population
  tar_target(
    population,
    define_population()
  ),

  # Find minimal effect size that can be found with our current sample.
  tar_target(
    nec_effect_size,
    optimize_effect_size(
      population,
      meta$interv_eff_start,
      meta$interv_eff_end,
      meta$step_size,
      meta$min_iterations
    )
  ),

  # Allow always a comma at the end of the list
  NULL
)
