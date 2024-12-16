set.seed(1234)
options(
  # Multicore features
  boot.parallel = "multicore",
  Ncpus = parallel::detectCores() - 1L,
  mc.cores = parallel::detectCores() - 1L,
)

myd <- readRDS("./NASA.rds")
myd <- data.table::as.data.table(myd)

get_sample <- function(orig_sample) {
  orig_sample[sample(.N, replace = TRUE), ]
}

fit_model <- function(mysample) {
  lmerTest::lmer(
    answer_r ~ 1 + warning + (1 | code) + (1 | question) + (1 | object),
    data = mysample
  )
}

# In this case, we only test one coefficient. The results are transferable to
# any other coefficient as they have the same frequency.
evaluate_model <- function(mymodel) {
  tmp <- summary(mymodel)
  c(
    estimate = tmp$coefficients["warningblur", "Estimate"],
    pvalue = tmp$coefficients["warningblur", "Pr(>|t|)"]
  )
}

ret <- replicate(
  5000,
  {
    mysample <- get_sample(myd)
    mymodel <- fit_model(mysample)
    evaluate_model(mymodel)
  }
) |>
  t() |>
  data.table::as.data.table()

# Make sure that we ignore the direction of the effect
ret[, estimate := abs(estimate)]

# Get results
ret[pvalue < .05, quantile(estimate, probs = c(0.025, 0.5, 0.975))]

# Compare against original model/sample
fit <- fit_model(myd)
summary(fit)

# Plot distributions
ggplot(ret, aes(x = estimate, y = pvalue)) +
  geom_point(alpha = .1) +
  ggExtra::ggMarginal(type = "density") +
  see::theme_modern()
