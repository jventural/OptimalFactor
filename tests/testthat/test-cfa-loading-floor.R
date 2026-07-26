test_that("an item below the loading floor is removed even when the fit is fine", {
  skip_on_cran()
  # Regression test for the behaviour this floor exists to produce. Until 1.3.0
  # the loop broke as soon as the fit targets were met, before ever looking at
  # the loadings, so an item the user's own threshold declares inadmissible
  # survived untouched next to an excellent RMSEA.
  pop <- .of_population_model(n_factors = 3, items_per_factor = 4,
                              loading = 0.70, phi = 0.30,
                              n_cross = 0, cross_loading = 0.40,
                              n_low = 1, low_loading = 0.15)
  set.seed(11)
  dat <- .of_simulate_ordinal(pop$sigma, n = 500, n_categories = 5,
                              skew = "symmetric")
  syntax <- .of_cfa_syntax(pop)
  weak <- names(pop$role)[pop$role == "low"]

  con <- suppressWarnings(cfa_boosting(dat, syntax, verbose = FALSE))
  sin <- suppressWarnings(cfa_boosting(dat, syntax, verbose = FALSE,
                                       thresholds = list(enforce_loading = FALSE)))

  expect_true(weak %in% con$removed_items)
  expect_false(weak %in% sin$removed_items)

  # The floor must not become a licence to prune: the good items stay.
  good <- names(pop$role)[pop$role == "good"]
  expect_length(intersect(good, con$removed_items), 0L)

  # And nothing below the floor survives in the reported solution.
  L <- con$standardized_loadings
  expect_true(all(abs(L$est.std) >= 0.30))
})

test_that("the stop reason says why the loop ended", {
  skip_on_cran()
  pop <- .of_population_model(3, 4, 0.70, 0.30, 0, 0.40, 0, 0.20)
  set.seed(5)
  dat <- .of_simulate_ordinal(pop$sigma, 400, 5, "symmetric")

  fit <- suppressWarnings(cfa_boosting(dat, .of_cfa_syntax(pop), verbose = FALSE))
  expect_true(fit$stop_reason %in% c("all_targets_met", "no_improving_action",
                                     "targets_met_loading_protected",
                                     "max_iterations"))
  # A clean population with no contaminated item should simply meet its targets.
  expect_equal(fit$stop_reason, "all_targets_met")
  expect_length(fit$removed_items, 0L)
})

test_that("an item loading on a foreign factor is removed", {
  skip_on_cran()
  # The loading floor catches the item that measures nothing. It does not catch
  # the one that measures two things: a population cross-loading of .40 leaves
  # the item above the floor on its own factor, and global fit stays clean
  # because the omitted path is absorbed by the interfactor correlation.
  pop <- .of_population_model(n_factors = 3, items_per_factor = 4,
                              loading = 0.60, phi = 0.30,
                              n_cross = 1, cross_loading = 0.40,
                              n_low = 0, low_loading = 0.20)
  set.seed(2026)
  dat <- .of_simulate_ordinal(pop$sigma, n = 600, n_categories = 5,
                              skew = "symmetric")
  syntax <- .of_cfa_syntax(pop)
  cross <- names(pop$role)[pop$role == "cross"]

  con <- suppressWarnings(cfa_boosting(dat, syntax, verbose = FALSE))
  sin <- suppressWarnings(cfa_boosting(dat, syntax, verbose = FALSE,
                                       thresholds = list(enforce_simple_structure = FALSE)))

  expect_true(cross %in% con$removed_items)
  expect_false(cross %in% sin$removed_items)

  # And it does not become an excuse to prune the good items.
  good <- names(pop$role)[pop$role == "good"]
  expect_length(intersect(good, con$removed_items), 0L)
})

test_that("a clean population survives the simple structure check untouched", {
  skip_on_cran()
  # The detection is by modification index, which is exactly the machinery that
  # capitalizes on chance when used on significance alone. Requiring the
  # standardized EPC to reach the threshold is what keeps it quiet here.
  pop <- .of_population_model(3, 4, 0.65, 0.30, 0, 0.40, 0, 0.20)
  set.seed(21)
  dat <- .of_simulate_ordinal(pop$sigma, 500, 5, "symmetric")

  fit <- suppressWarnings(cfa_boosting(dat, .of_cfa_syntax(pop), verbose = FALSE))
  expect_length(fit$removed_items, 0L)
  expect_equal(fit$stop_reason, "all_targets_met")
})

test_that("the floor cannot shrink a factor below its minimum", {
  skip_on_cran()
  # Three items per factor is the floor of the floor: even if one of them loads
  # badly, removing it would leave two, so it has to stay.
  pop <- .of_population_model(2, 3, 0.70, 0.30, 0, 0.40, 0, 0.20)
  set.seed(7)
  dat <- .of_simulate_ordinal(pop$sigma, 400, 5, "symmetric")
  syntax <- "f1 =~ IT1 + IT2 + IT3\nf2 =~ IT4 + IT5 + IT6"

  fit <- suppressWarnings(cfa_boosting(dat, syntax, verbose = FALSE,
                                       thresholds = list(min_items_per_factor = 3)))
  expect_length(fit$removed_items, 0L)
})
