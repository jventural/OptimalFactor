test_that("the population model has the announced shape", {
  pop <- .of_population_model(n_factors = 3, items_per_factor = 4,
                              loading = 0.65, phi = 0.30,
                              n_cross = 1, cross_loading = 0.40,
                              n_low = 2, low_loading = 0.20)

  expect_equal(dim(pop$lambda), c(3 * 4 + 1 + 2, 3))
  expect_equal(unname(table(pop$role)[["good"]]), 12L)
  expect_equal(unname(table(pop$role)[["cross"]]), 1L)
  expect_equal(unname(table(pop$role)[["low"]]), 2L)

  # A good item is simple structure by construction; the cross item is not.
  good  <- names(pop$role)[pop$role == "good"]
  cross <- names(pop$role)[pop$role == "cross"]
  expect_true(all(rowSums(pop$lambda[good, , drop = FALSE] != 0) == 1))
  expect_equal(sum(pop$lambda[cross, ] != 0), 2L)

  # Several cross-loading items go on DIFFERENT pairs of factors. Sharing a
  # pair would make them clones of one another: they correlate strongly, form a
  # coherent cluster, and the rotation reports that cluster as a factor of its
  # own, each item showing a single clean loading near .80. A simulation built
  # that way measures the generator instead of the pipeline.
  many <- .of_population_model(n_factors = 3, items_per_factor = 4,
                               loading = 0.60, phi = 0.30,
                               n_cross = 3, cross_loading = 0.55,
                               n_low = 0, low_loading = 0.20)
  cr <- names(many$role)[many$role == "cross"]
  pares <- lapply(cr, function(it) sort(which(many$lambda[it, ] != 0)))
  expect_length(unique(pares), 3L)

  # Only the good items carry a true factor; the contaminated ones do not.
  expect_true(all(!is.na(pop$true_factor[good])))
  expect_true(all(is.na(pop$true_factor[names(pop$role)[pop$role != "good"]])))

  # The implied matrix must be a usable correlation matrix.
  expect_equal(unname(diag(pop$sigma)), rep(1, nrow(pop$lambda)))
  expect_true(all(eigen(pop$sigma, only.values = TRUE)$values > 0))
  expect_true(isSymmetric(pop$sigma))
})

test_that("an impossible population model is refused instead of returned", {
  # Communalities above 1 have no data-generating meaning, so the model must
  # fail loudly rather than hand back a non positive definite matrix.
  expect_error(
    .of_population_model(n_factors = 2, items_per_factor = 3, loading = 0.65,
                         phi = 0.80, n_cross = 1, cross_loading = 0.90,
                         n_low = 0, low_loading = 0.20),
    "communalities"
  )
})

test_that("the ordinal simulation respects size, names and category range", {
  pop <- .of_population_model(2, 3, 0.65, 0.30, 0, 0.40, 0, 0.20)
  set.seed(11)
  d <- .of_simulate_ordinal(pop$sigma, n = 250, n_categories = 5,
                            skew = "symmetric", colnames_prefix = "IT")

  expect_equal(dim(d), c(250L, 6L))
  expect_equal(names(d), paste0("IT", 1:6))
  expect_true(all(vapply(d, is.integer, logical(1))))
  expect_true(all(unlist(d) >= 1 & unlist(d) <= 5))
})

test_that("the simulated data reproduce the population correlations", {
  pop <- .of_population_model(2, 4, 0.70, 0.30, 0, 0.40, 0, 0.20)
  set.seed(4)
  d <- .of_simulate_ordinal(pop$sigma, n = 4000, n_categories = 5, skew = "symmetric")
  # Polychoric would recover sigma exactly; Pearson on categories attenuates it,
  # so the check is on the ordering: within-factor pairs above between-factor ones.
  r <- stats::cor(d)
  within  <- mean(r[1:4, 1:4][upper.tri(diag(4))])
  between <- mean(r[1:4, 5:8])
  expect_gt(within, between)
  expect_gt(within, 0.30)
})

test_that("'skewed' puts the mass in the low categories, not the high ones", {
  # Regression test. The thresholds were built without cumsum(), so the values
  # were not cumulative proportions: the last one reached 0.43 instead of 1 and
  # about 70 % of the responses landed in the *top* category, the exact reverse
  # of the documented behaviour.
  pop <- .of_population_model(2, 3, 0.65, 0.30, 0, 0.40, 0, 0.20)
  set.seed(11)
  sk <- .of_simulate_ordinal(pop$sigma, 3000, 5, "skewed")
  sy <- .of_simulate_ordinal(pop$sigma, 3000, 5, "symmetric")

  p_sk <- as.numeric(prop.table(table(factor(sk$IT1, levels = 1:5))))
  expect_equal(which.max(p_sk), 1L)
  expect_true(all(diff(p_sk) < 0))
  expect_lt(mean(sk$IT1), mean(sy$IT1))

  # The symmetric option stays flat across categories.
  p_sy <- as.numeric(prop.table(table(factor(sy$IT1, levels = 1:5))))
  expect_true(max(abs(p_sy - 0.2)) < 0.05)
})
