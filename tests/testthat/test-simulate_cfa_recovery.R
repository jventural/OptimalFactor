test_that("the specified model puts every item on a factor, contaminated ones included", {
  pop <- .of_population_model(3, 4, 0.65, 0.30, 1, 0.40, 1, 0.20)
  syn <- .of_cfa_syntax(pop)

  lines <- strsplit(syn, "\n")[[1]]
  expect_length(lines, 3L)
  expect_match(lines[1], "^f1 =~ ")

  # Every one of the 14 items appears exactly once: the model a researcher
  # writes before knowing which items misbehave.
  mentioned <- unlist(regmatches(syn, gregexpr("IT[0-9]+", syn)))
  expect_setequal(mentioned, pop$items)
  expect_equal(length(mentioned), length(pop$items))

  # The cross-loading and weak items are specified on the first factor.
  bad <- names(pop$role)[pop$role != "good"]
  expect_true(all(bad %in% strsplit(lines[1], " \\+ |f1 =~ ")[[1]]))
})

test_that("scoring separates removing the right items from removing any items", {
  pop  <- .of_population_model(2, 3, 0.65, 0.30, 1, 0.40, 1, 0.20)
  good <- names(pop$role)[pop$role == "good"]   # IT1..IT6
  bad  <- names(pop$role)[pop$role != "good"]   # IT7 cross, IT8 low

  perfect <- .of_score_cfa_recovery(
    list(removed_items = bad, added_covariances = list(), fit_indices = list(rmsea = 0.03)),
    pop)
  expect_true(perfect$exact)
  expect_equal(perfect$sensitivity, 1)
  expect_equal(perfect$specificity, 1)
  expect_equal(perfect$n_retained, 6L)
  expect_equal(perfect$rmsea, 0.03)

  # Removing nothing: the good items all survive, but no contaminated one goes.
  nothing <- .of_score_cfa_recovery(
    list(removed_items = character(0), added_covariances = NULL, fit_indices = NULL),
    pop)
  expect_false(nothing$exact)
  expect_equal(nothing$sensitivity, 0)
  expect_equal(nothing$specificity, 1)
  expect_equal(nothing$covs_added, 0L)
  expect_true(is.na(nothing$rmsea))

  # Removing everything scores perfect sensitivity and terrible specificity,
  # which is why the two are never read apart.
  everything <- .of_score_cfa_recovery(
    list(removed_items = pop$items, added_covariances = list(), fit_indices = list()),
    pop)
  expect_equal(everything$sensitivity, 1)
  expect_equal(everything$specificity, 0)
  expect_false(everything$exact)

  # One right, one wrong: neither exact nor a clean sweep.
  partial <- .of_score_cfa_recovery(
    list(removed_items = c(bad[1], good[1]), added_covariances = list(1, 2),
         fit_indices = list(rmsea.scaled = 0.05)),
    pop)
  expect_false(partial$exact)
  expect_equal(partial$sensitivity, 0.5)
  expect_equal(partial$specificity, 5 / 6)
  expect_equal(partial$covs_added, 2L)
  expect_equal(partial$rmsea, 0.05)
})

test_that("a scaled rmsea is preferred over the plain one", {
  pop <- .of_population_model(2, 3, 0.65, 0.30, 0, 0.40, 0, 0.20)
  ev <- .of_score_cfa_recovery(
    list(removed_items = character(0), added_covariances = list(),
         fit_indices = list(rmsea = 0.09, rmsea.scaled = 0.04)),
    pop)
  expect_equal(ev$rmsea, 0.04)
})

test_that("the design is summarised one row per condition", {
  canned <- list(removed_items = c("IT7", "IT8"), added_covariances = list(),
                 fit_indices = list(rmsea.scaled = 0.02))
  local_mocked_bindings(cfa_boosting = function(...) canned)

  sim <- simulate_cfa_recovery(n = c(150, 300), items_per_factor = 3,
                               n_factors = 2, n_reps = 3, n_cross = 1, n_low = 1,
                               verbose = FALSE)

  expect_s3_class(sim, "simulate_cfa_recovery")
  expect_equal(nrow(sim$summary), 2L)
  expect_equal(nrow(sim$replications), 6L)
  expect_equal(sim$summary$exact_rate, c(1, 1))
  expect_equal(sim$summary$sensitivity, c(1, 1))
  expect_equal(sim$summary$mean_covs_added, c(0, 0))
  expect_match(sim$model, "f1 =~")
})

test_that("failed replications are recorded rather than dropped", {
  local_mocked_bindings(cfa_boosting = function(...) stop("no fit"))
  sim <- simulate_cfa_recovery(n = 150, items_per_factor = 3, n_factors = 2,
                               n_reps = 4, verbose = FALSE)

  expect_equal(nrow(sim$replications), 4L)
  expect_false(any(sim$replications$converged))
  expect_equal(sim$summary$convergence_rate, 0)
  expect_true(is.na(sim$summary$exact_rate))
})

test_that("plot draws every metric, counts included", {
  skip_if_not_installed("ggplot2")
  canned <- list(removed_items = "IT7", added_covariances = list(1),
                 fit_indices = list(rmsea = 0.03))
  local_mocked_bindings(cfa_boosting = function(...) canned)
  sim <- simulate_cfa_recovery(n = c(150, 300), items_per_factor = 3,
                               n_factors = 2, n_reps = 2, verbose = FALSE)

  for (m in c("exact_rate", "sensitivity", "specificity", "convergence_rate",
              "mean_retained", "mean_covs_added")) {
    p <- plot(sim, metric = m)
    expect_s3_class(p, "ggplot")
    expect_no_error(ggplot2::ggplot_build(p))
  }
  # A count metric gets no 0-1 axis: added covariances are not a proportion.
  expect_null(plot(sim, metric = "mean_covs_added")$scales$get_scales("y")$limits)
})

test_that("the print method explains what zero added covariances means", {
  canned <- list(removed_items = character(0), added_covariances = list(),
                 fit_indices = list(rmsea = 0.03))
  local_mocked_bindings(cfa_boosting = function(...) canned)
  sim <- simulate_cfa_recovery(n = 150, items_per_factor = 3, n_factors = 2,
                               n_reps = 2, verbose = FALSE)

  out <- capture.output(print(sim))
  expect_true(any(grepl("CFA-Boosting recovery", out)))
  expect_true(any(grepl("exact_rate", out)))
  expect_true(any(grepl("capitalization on chance", out)))
})
