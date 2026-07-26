# A summary object built by hand, so the print and plot methods can be tested
# without paying for real EFA-Boosting fits.
fake_recovery <- function() {
  s <- expand.grid(n = c(200, 500), loading = c(0.50, 0.70),
                   items_per_factor = c(4, 6))
  s$n_reps           <- 10
  s$convergence_rate <- 1
  s$recovery_rate    <- seq(0.50, 1, length.out = nrow(s))
  s$sensitivity      <- 0.80
  s$specificity      <- 0.95
  s$mean_retained    <- 12
  s$mean_rmsea       <- 0.02
  structure(
    list(summary = s, replications = NULL, conditions = s[, 1:3],
         population = list(n_factors = 2,
                           role = c(IT1 = "good", IT2 = "good", IT3 = "cross")),
         n_reps = 10, call = NULL),
    class = "simulate_recovery")
}

test_that("failed replications are recorded instead of dropped", {
  # Regression test. `fits[[r]] <- NULL` deletes the slot rather than filling it,
  # and a failed replication returns exactly NULL, so the list shrank and the
  # scoring loop aborted with a subscript error on any hard condition.
  local_mocked_bindings(efa_boosting = function(...) stop("no fit"))

  sim <- simulate_recovery(n = 120, n_factors = 2, items_per_factor = 3,
                           n_cross = 0, n_low = 0, n_reps = 4, verbose = FALSE)

  expect_equal(nrow(sim$replications), 4L)
  expect_false(any(sim$replications$converged))
  expect_true(all(is.na(sim$replications$n_retained)))
  expect_equal(sim$summary$convergence_rate, 0)
  expect_true(is.na(sim$summary$recovery_rate))
})

test_that("a mixture of failed and successful replications is summarised correctly", {
  perfect <- data.frame(Items = paste0("IT", 1:6),
                        f1 = c(.7, .7, .7, 0, 0, 0),
                        f2 = c(0, 0, 0, .7, .7, .7))
  canned <- list(final_structure = perfect, final_rmsea = 0.01,
                 stop_reason = "canned")
  calls <- 0
  local_mocked_bindings(efa_boosting = function(...) {
    calls <<- calls + 1
    if (calls %% 2 == 0) stop("no fit") else canned
  })

  sim <- simulate_recovery(n = 120, n_factors = 2, items_per_factor = 3,
                           n_cross = 0, n_low = 0, n_reps = 4, verbose = FALSE)

  expect_equal(sum(sim$replications$converged), 2L)
  expect_equal(sim$summary$convergence_rate, 0.5)
  # The rates are computed over the replications that converged, not over all.
  expect_equal(sim$summary$recovery_rate, 1)
  expect_equal(sim$summary$mean_retained, 6)
})

test_that("the in-process worker really fits", {
  skip_on_cran()
  # Regression test. The worker called OptimalFactor::efa_boosting even when
  # running in process, which fails under devtools::load_all() because nothing
  # is exported yet. The error was swallowed by tryCatch and every replication
  # was silently recorded as non-convergent, a result that looked like evidence.
  pop <- .of_population_model(2, 4, 0.70, 0.30, 0, 0.40, 0, 0.20)
  set.seed(3)
  dat <- .of_simulate_ordinal(pop$sigma, 300, 5, "symmetric")

  fit <- .of_make_fit_worker(n_factors = 2, dots = list(), remote = FALSE)(dat)
  expect_false(is.null(fit))
  expect_false(is.null(fit$final_structure))
})

test_that("a crossed design returns one summary row per condition", {
  skip_on_cran()
  sim <- simulate_recovery(n = c(200, 400), loading = 0.70, items_per_factor = 3,
                           n_factors = 2, n_reps = 2, verbose = FALSE)

  expect_s3_class(sim, "simulate_recovery")
  expect_equal(nrow(sim$summary), 2L)
  expect_equal(nrow(sim$replications), 4L)
  expect_equal(sim$summary$n, c(200, 400))
  expect_true(all(sim$replications$converged))
  expect_true(all(sim$summary$recovery_rate >= 0 & sim$summary$recovery_rate <= 1))
})

test_that("the number of cores does not change the results", {
  skip_on_cran()
  # The workers load the installed OptimalFactor, so this only proves anything
  # when the installed version matches the sources under test. Opt in with
  # Sys.setenv(OPTIMALFACTOR_TEST_CLUSTER = 1).
  skip_if_not(nzchar(Sys.getenv("OPTIMALFACTOR_TEST_CLUSTER")),
              "set OPTIMALFACTOR_TEST_CLUSTER=1 to run the cluster comparison")

  args <- list(n = 200, loading = 0.70, items_per_factor = 3, n_factors = 2,
               n_reps = 3, verbose = FALSE)
  serial   <- do.call(simulate_recovery, c(args, list(n_cores = 1)))
  parallel <- do.call(simulate_recovery, c(args, list(n_cores = 2)))

  expect_equal(serial$summary, parallel$summary)
  expect_equal(serial$replications, parallel$replications)
})

test_that("the print method reports the design", {
  out <- capture.output(print(fake_recovery()))
  expect_true(any(grepl("8 condition", out)))
  expect_true(any(grepl("10 replications", out)))
  expect_true(any(grepl("recovery_rate", out)))
})

test_that("plot returns a drawable ggplot for every metric", {
  skip_if_not_installed("ggplot2")
  sim <- fake_recovery()

  for (m in c("recovery_rate", "sensitivity", "specificity",
              "convergence_rate", "mean_retained")) {
    p <- plot(sim, metric = m)
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$y, gsub("_", " ", m))
    # ggplot_build is what catches a column referred to in aes() but absent.
    expect_no_error(ggplot2::ggplot_build(p))
  }
})

test_that("plot facets by items per factor only when the design crosses it", {
  skip_if_not_installed("ggplot2")
  sim <- fake_recovery()
  expect_s3_class(plot(sim)$facet, "FacetWrap")

  single <- sim
  single$summary <- single$summary[single$summary$items_per_factor == 4, ]
  expect_s3_class(plot(single)$facet, "FacetNull")
})

test_that("plot rejects a metric that is not in the summary", {
  skip_if_not_installed("ggplot2")
  expect_error(plot(fake_recovery(), metric = "nonsense"), "should be one of")
})
