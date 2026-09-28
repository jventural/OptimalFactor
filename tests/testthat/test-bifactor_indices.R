bifactor_data <- function(seed) {
  pop <- .of_population_model(n_factors = 3, items_per_factor = 4,
                              loading = 0.65, phi = 0.60,
                              n_cross = 0, cross_loading = 0.40,
                              n_low = 0, low_loading = 0.20)
  set.seed(seed)
  as.data.frame(.of_simulate_ordinal(pop$sigma, n = 500, n_categories = 5, skew = "symmetric"))
}

test_that("items that load only on the general factor do not turn the indices into NA", {
  skip_on_cran()
  dat <- bifactor_data(21)
  its <- names(dat)
  # Bifactor S-1: the third dimension is the reference and has no specific factor.
  mod <- paste0("G =~ ", paste(its, collapse = " + "), "\n",
                "S1 =~ ", paste(its[1:4], collapse = " + "), "\n",
                "S2 =~ ", paste(its[5:8], collapse = " + "))
  fit <- lavaan::cfa(mod, data = dat, estimator = "MLR", std.lv = TRUE, orthogonal = TRUE)
  bi <- suppressWarnings(utils::capture.output(r <- bifactor_indices(fit, general = "G")))

  expect_false(anyNA(r$overall))
  expect_false(anyNA(r$by_factor))
  expect_true(all(is.na(r$by_item$Factor[9:12])))
  expect_true(r$overall$omega_H <= r$overall$omega && r$overall$omega <= 1)
  # Only the 8 items with a specific factor contaminate correlations.
  expect_equal(r$overall$PUC, 1 - 2 * choose(4, 2) / choose(12, 2))
})

test_that("an inadmissible solution is flagged instead of reported silently", {
  fake <- structure(list(), class = "fake_fit")
  std <- data.frame(lhs = c(rep("G", 4), "S1", "S1"), op = "=~",
                    rhs = c("a", "b", "c", "d", "a", "b"),
                    est.std = c(.6, .7, .6, .5, 1.3, .2))
  local_mocked_bindings(standardizedSolution = function(fit, ...) std, .package = "lavaan")
  expect_warning(utils::capture.output(bifactor_indices(fake, general = "G")), "Inadmissible")
})
