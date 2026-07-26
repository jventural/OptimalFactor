test_that("the failure counter reports the resamples that actually failed", {
  # Regression test. `fits[[r]] <- NULL` removes the slot instead of filling it,
  # so the NULLs vanished before they could be counted and the progress line
  # always claimed "(0 failed)" no matter how many resamples had died.
  perfect <- data.frame(Items = paste0("IT", 1:6),
                        f1 = c(.7, .7, .7, 0, 0, 0),
                        f2 = c(0, 0, 0, .7, .7, .7))
  canned <- list(final_structure = perfect, stop_reason = "canned",
                 removed_items = character(0))
  pop <- .of_population_model(2, 3, 0.70, 0.30, 0, 0.40, 0, 0.20)
  set.seed(2)
  dat <- .of_simulate_ordinal(pop$sigma, 150, 5, "symmetric")

  calls <- 0
  local_mocked_bindings(efa_boosting = function(...) {
    calls <<- calls + 1
    if (calls <= 2) canned else stop("no fit")  # reference + first resample only
  })

  out <- capture.output(
    st <- item_stability(dat, name_items = "IT", n_factors = 2, R = 3,
                         verbose = TRUE)
  )
  expect_true(any(grepl("2 failed", out)))
  expect_equal(st$n_valid, 1L)
  expect_equal(st$n_failed, 2L)
})

test_that("retention rates are computed over every candidate item", {
  perfect <- data.frame(Items = paste0("IT", 1:6),
                        f1 = c(.7, .7, .7, 0, 0, 0),
                        f2 = c(0, 0, 0, .7, .7, .7))
  # The fit keeps six of the eight candidates; the two contaminated ones are out.
  canned <- list(final_structure = perfect, stop_reason = "canned",
                 removed_items = c("IT7", "IT8"))
  pop <- .of_population_model(2, 3, 0.70, 0.30, 1, 0.40, 1, 0.20)
  set.seed(2)
  dat <- .of_simulate_ordinal(pop$sigma, 150, 5, "symmetric")

  local_mocked_bindings(efa_boosting = function(...) canned)
  st <- item_stability(dat, name_items = "IT", n_factors = 2, R = 3,
                       verbose = FALSE)

  # The universe is the eight items in the data, not the six survivors.
  expect_equal(nrow(st$retention), 8L)
  expect_equal(st$retention$retention_rate[st$retention$item %in% paste0("IT", 1:6)],
               rep(1, 6))
  expect_equal(st$retention$removal_rate[st$retention$item %in% c("IT7", "IT8")],
               rep(1, 2))
  expect_true(all(st$retention$factor_agreement[st$retention$item == "IT1"] == 1))
  expect_s3_class(st, "item_stability")
})

test_that("item_stability refuses to run when the reference fit fails", {
  local_mocked_bindings(efa_boosting = function(...) stop("no fit"))
  pop <- .of_population_model(2, 3, 0.70, 0.30, 0, 0.40, 0, 0.20)
  set.seed(2)
  dat <- .of_simulate_ordinal(pop$sigma, 150, 5, "symmetric")

  expect_error(
    item_stability(dat, name_items = "IT", n_factors = 2, R = 2, verbose = FALSE),
    "reference"
  )
})
