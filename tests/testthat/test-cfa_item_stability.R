test_that("a clean two-factor structure is stable and reproducible", {
  skip_on_cran()
  pop <- .of_population_model(n_factors = 2, items_per_factor = 4,
                              loading = 0.70, phi = 0.30,
                              n_cross = 0, cross_loading = 0.40,
                              n_low = 0, low_loading = 0.20)
  set.seed(5)
  dat <- .of_simulate_ordinal(pop$sigma, n = 300, n_categories = 5, skew = "symmetric")
  its <- colnames(dat)
  model <- list(A = its[1:4], B = its[5:8])

  a <- cfa_item_stability(dat, model, R = 6, seed = 2026, verbose = FALSE)
  b <- cfa_item_stability(dat, model, R = 6, seed = 2026, verbose = FALSE)
  expect_s3_class(a, "cfa_item_stability")
  expect_identical(a$items, b$items)
  expect_equal(a$summary$success_rate, 1)
  expect_true(all(a$items$stability == 1))
  expect_true(all(a$items$reading == "stable"))
  expect_true(all(a$fit$phi_max < 0.90))
  expect_output(print(a), "CFA item-assignment stability")
})

test_that("a misassigned item is flagged as preferring the other factor", {
  skip_on_cran()
  pop <- .of_population_model(2, 4, 0.70, 0.30, 0, 0.40, 0, 0.20)
  set.seed(6)
  dat <- .of_simulate_ordinal(pop$sigma, n = 300, n_categories = 5, skew = "symmetric")
  its <- colnames(dat)
  # The fourth item of factor A is placed on B on purpose.
  st <- cfa_item_stability(dat, list(A = its[1:3], B = c(its[4], its[5:8])),
                           R = 6, seed = 1, verbose = FALSE)
  wrong <- st$items[st$items$item == its[4], ]
  expect_equal(wrong$reading, "prefers other factor")
  expect_equal(wrong$other, "A")
})

test_that("lavaan syntax is accepted and bad models are rejected", {
  m <- .of_model_list("F1 =~ a + b + c\nF2 =~ d + e + f\nF1 ~~ F2")
  expect_equal(m, list(F1 = c("a", "b", "c"), F2 = c("d", "e", "f")))
  d <- data.frame(a = 1:5, b = 1:5, c = 1:5, d = 1:5)
  expect_error(cfa_item_stability(d, list(F1 = c("a", "b", "c", "d"))), "at least two")
  expect_error(cfa_item_stability(d, list(F1 = c("a", "b"), F2 = c("b", "c"))), "one factor only")
  expect_error(cfa_item_stability(d, list(F1 = c("a", "b"), F2 = c("c", "z"))), "not found")
})
