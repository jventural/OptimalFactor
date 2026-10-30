test_that("cross_loading_method only accepts saris or epc", {
  data(Data_Expectativas, package = "OptimalFactor", envir = environment())
  mod <- "F1 =~ EAF1 + EAF2 + EAF3 + EAF4 + EAF5
          F2 =~ EAF6 + EAF7 + EAF8 + EAF9 + EAF10"
  expect_error(
    cfa_boosting(Data_Expectativas, model = mod, verbose = FALSE,
                 thresholds = list(cross_loading_method = "otro")),
    "arg"
  )
})

test_that("cross-loadings are classified with the Saris table by default", {
  skip_on_cran()
  data(Data_Expectativas, package = "OptimalFactor", envir = environment())
  mod <- "F1 =~ EAF1 + EAF2 + EAF3 + EAF4 + EAF5
          F2 =~ EAF6 + EAF7 + EAF8 + EAF9 + EAF10"
  res <- suppressWarnings(cfa_boosting(Data_Expectativas, model = mod, verbose = FALSE))
  cruz <- res$steps_log[res$steps_log$action == "Remove Item (cross-loading)", ]
  expect_equal(nrow(cruz), 2)
  # each removal carries the power and a misspecification decision
  expect_true(all(grepl("Power=", cruz$detail)))
  expect_true(all(grepl("=>(m|EPC:m)\\)", cruz$detail)))

  old <- suppressWarnings(cfa_boosting(Data_Expectativas, model = mod, verbose = FALSE,
                                       thresholds = list(cross_loading_method = "epc")))
  expect_false(any(grepl("Power=", old$steps_log$detail[
    old$steps_log$action == "Remove Item (cross-loading)"])))
})
