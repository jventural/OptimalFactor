test_that("a fixed structure is replicated on every split", {
  skip_on_cran()
  pop <- .of_population_model(n_factors = 2, items_per_factor = 4,
                              loading = 0.70, phi = 0.30,
                              n_cross = 0, cross_loading = 0.40,
                              n_low = 0, low_loading = 0.20)
  set.seed(3)
  dat <- .of_simulate_ordinal(pop$sigma, n = 400, n_categories = 5, skew = "symmetric")
  its <- colnames(dat)
  theory <- list(A = its[1:4], B = its[5:8])

  # The "algorithm" returns the theory unchanged: the decision cannot vary, so
  # the replication metrics must be perfect and every split must succeed.
  st <- algorithm_stability(dat, function(d) theory, n_splits = 4,
                            theory = theory, verbose = FALSE)
  expect_s3_class(st, "algorithm_stability")
  expect_equal(st$summary$success_rate, 1)
  expect_equal(st$summary$jaccard, 1)
  expect_equal(st$summary$recovery, 1)
  expect_true(all(st$item_retention$retention == 1))
  expect_true(st$summary$cfi > 0.95)
})

test_that("the splits are reproducible and a failing algorithm is counted, not fatal", {
  skip_on_cran()
  pop <- .of_population_model(2, 4, 0.70, 0.30, 0, 0.40, 0, 0.20)
  set.seed(4)
  dat <- .of_simulate_ordinal(pop$sigma, 300, 5, "symmetric")
  its <- colnames(dat)
  flaky <- function(d) if (nrow(d) > 0 && d[1, 1] >= 3) stop("boom") else list(G = its)

  a <- algorithm_stability(dat, flaky, n_splits = 6, reference = list(G = its), verbose = FALSE)
  b <- algorithm_stability(dat, flaky, n_splits = 6, reference = list(G = its), verbose = FALSE)
  expect_identical(a$splits$success, b$splits$success)
  expect_lt(a$summary$success_rate, 1)
})

test_that("objects named in a console function travel with it", {
  f <- local(function(d) model_syntax)
  environment(f) <- globalenv()
  assign("model_syntax", "F =~ x1 + x2 + x3", envir = globalenv())
  on.exit(rm("model_syntax", envir = globalenv()))
  g <- .of_portable_function(f)
  expect_true(exists("model_syntax", envir = environment(g), inherits = FALSE))
  expect_identical(g(NULL), "F =~ x1 + x2 + x3")
})

test_that("a split that exceeds the timeout is counted as failed, not waited for", {
  skip_on_cran()
  skip_if_not_installed("R.utils")
  pop <- .of_population_model(2, 4, 0.70, 0.30, 0, 0.40, 0, 0.20)
  set.seed(6)
  dat <- .of_simulate_ordinal(pop$sigma, 200, 5, "symmetric")
  its <- colnames(dat)
  lento <- function(d) { Sys.sleep(30); list(G = its) }
  t0 <- Sys.time()
  st <- algorithm_stability(dat, lento, n_splits = 2, reference = list(G = its),
                            timeout = 1, verbose = FALSE)
  expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 20)
  expect_equal(st$summary$success_rate, 0)
})
