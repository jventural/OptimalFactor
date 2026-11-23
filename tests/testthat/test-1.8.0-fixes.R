test_that("discriminant_boosting judges correlations by absolute value", {
  skip_on_cran()
  skip_if_not_installed("semTools")
  it <- paste0("PPTQ", 1:15)
  th <- list(A = it[1:5], B = it[6:10], C = it[11:15])
  res <- suppressWarnings(discriminant_boosting(Data_Personality[, it], th, verbose = FALSE))
  # Every level of this example has a correlation beyond -1.
  expect_false(any(res$refined$admissible))
  expect_match(res$reason, "none is admissible")
  out <- paste(capture.output(print(res)), collapse = "\n")
  expect_match(out, "[INADMISSIBLE]", fixed = TRUE)
  expect_no_match(out, "]  OK", fixed = TRUE)
})

test_that("a removed item never stays in the final structure of efa_boosting", {
  skip_on_cran()
  for (k in 2:5) {
    res <- suppressMessages(suppressWarnings(
      efa_boosting(Data_Personality, name_items = "PPTQ", n_factors = k, verbose = FALSE)))
    expect_length(intersect(res$removed_items, res$final_structure$Items), 0)
    expect_equal(nrow(res$final_structure) + length(res$removed_items), 15)
    expect_equal(nrow(res$steps_log), length(res$removed_items))
  }
  opt <- suppressMessages(suppressWarnings(
    efa_optimizer(Data_Personality, name_items = "PPTQ", n_factors = 5, verbose = FALSE)))
  expect_length(intersect(opt$removed_items, opt$final_structure$Items), 0)
})

test_that("cfa_boosting drops the covariances of the items it removes", {
  skip_on_cran()
  model <- "F1 =~ EAF1 + EAF2 + EAF3 + EAF4 + EAF5
            F2 =~ EAF6 + EAF7 + EAF8 + EAF9 + EAF10
            EAF6 ~~ EAF7"
  res <- suppressWarnings(cfa_boosting(Data_Expectativas, model = model, verbose = FALSE))
  expect_true("EAF6" %in% res$removed_items)
  expect_setequal(lavaan::lavNames(res$final_fit, "ov"), unlist(res$final_factors))
  expect_false(any(grepl("EAF6", res$final_covs)))
})

test_that("bifactor_indices counts the covariances among specific factors", {
  skip_on_cran()
  pop <- paste(c(
    paste("G =~", paste0("0.6*x", 1:9, collapse = " + ")),
    "S1 =~ 0.5*x4 + 0.5*x5 + 0.5*x6", "S2 =~ 0.5*x7 + 0.5*x8 + 0.5*x9",
    "G ~~ 0*S1", "G ~~ 0*S2", "S1 ~~ 0.5*S2",
    paste0("x", 1:3, " ~~ 0.64*x", 1:3), paste0("x", 4:9, " ~~ 0.39*x", 4:9)), collapse = "\n")
  set.seed(1)
  dat <- lavaan::simulateData(pop, sample.nobs = 20000, std.lv = TRUE)
  fit <- lavaan::cfa(paste(
    paste("G =~", paste0("x", 1:9, collapse = " + ")),
    "S1 =~ x4 + x5 + x6", "S2 =~ x7 + x8 + x9", "G ~~ 0*S1", "G ~~ 0*S2", sep = "\n"),
    data = dat, std.lv = TRUE)
  r <- bifactor_indices(fit, general = "G", verbose = FALSE)
  # Population: total variance 29.16 + 2 * 2.25 + 2 * 0.5 * 2.25 + 4.26
  v <- 5.4^2 + 2 * 1.5^2 + 2 * 0.5 * 1.5^2 + 3 * 0.64 + 6 * 0.39
  expect_equal(r$overall$omega_H, 5.4^2 / v, tolerance = 0.03)
  expect_equal(r$overall$omega, 1 - (3 * 0.64 + 6 * 0.39) / v, tolerance = 0.02)
})

test_that("the timeout of algorithm_stability stops a search that absorbs errors", {
  skip_on_cran()
  skip_if_not_installed("callr")
  its <- paste0("EAF", 1:10)
  theory <- list(F1 = its[1:5], F2 = its[6:10])
  stubborn <- function(d) {
    t0 <- Sys.time()
    while (as.numeric(difftime(Sys.time(), t0, units = "secs")) < 60)
      tryCatch(Sys.sleep(0.05), error = function(e) NULL)
    list(G = names(d))
  }
  t0 <- Sys.time()
  st <- suppressWarnings(
    algorithm_stability(Data_Expectativas[, its], stubborn, n_splits = 2, reference = theory,
                        estimator = "MLR", ordered = FALSE, timeout = 3, seed = 1,
                        verbose = FALSE))
  expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 30)
  expect_equal(st$summary$success_rate, 0)
})

test_that("the key of one provider is never sent to another", {
  withr_base <- Sys.getenv("OPENAI_BASE_URL", NA)
  Sys.unsetenv("OPENAI_BASE_URL")
  on.exit(if (!is.na(withr_base)) Sys.setenv(OPENAI_BASE_URL = withr_base))
  expect_true(.of_is_claude("claude-haiku-5-5"))
  expect_false(.of_is_claude("some-other-model"))
  expect_error(.of_check_ai_key("sk-abc", "claude-haiku-5-5"), "Anthropic API key")
  expect_error(.of_check_ai_key("sk-ant-abc", "some-other-model"), "Claude model")
  expect_true(.of_check_ai_key("sk-ant-abc", "claude-haiku-5-5"))
  expect_true(.of_check_ai_key(NULL, "claude-haiku-5-5"))
})

test_that("the text is read by block type in both response formats", {
  claude <- list(stop_reason = "end_turn",
                 content = list(list(type = "thinking", thinking = ""),
                                list(type = "text", text = "answer")))
  expect_identical(.of_chat_text(claude), "answer")
  expect_identical(.of_chat_text(list(choices = list(list(message = list(content = "answer"))))),
                   "answer")
  expect_match(.of_chat_text(list(stop_reason = "refusal", content = list())), "declined")
  expect_error(.of_chat_text(list(stop_reason = "max_tokens",
                                  content = list(list(type = "thinking", thinking = "")))))
})

test_that("a model written over several lines keeps all its items", {
  want <- list(F1 = c("x1", "x2", "x3", "x4"), F2 = c("x5", "x6"))
  for (m in c("F1 =~ x1 + x2 + x3 + x4\nF2 =~ x5 + x6",
              "F1 =~ x1 + x2 +\n  x3 + x4\nF2 =~ x5 + x6",
              "F1 =~ x1 + x2\n  + x3 + x4\nF2 =~\n x5 + x6",
              "F1 =~ x1 + x2 # note\nF1 =~ x3 + x4\nF2 =~ x5 + x6",
              "F1 =~ x1 + x2 + x3 + x4; F2 =~ x5 + x6 ! note"))
    expect_identical(.of_model_factors(.of_model_lines(m)), want, info = m)
  expect_identical(.of_drop_items(c("NA*x1", "x2", "x10"), "x1"), c("x2", "x10"))
})

test_that("cfa_boosting starts from every declared item", {
  skip_on_cran()
  m <- "F1 =~ EAF1 + EAF2 +\n  EAF3 + EAF4\nF2 =~ EAF5 + EAF6 + EAF7"
  res <- suppressWarnings(cfa_boosting(Data_Expectativas, model = m, verbose = FALSE))
  expect_setequal(c(unlist(res$final_factors), res$removed_items), paste0("EAF", 1:7))
  suppressWarnings(expect_warning(
    cfa_boosting(Data_Expectativas, model = paste0(m, "
F1 ~ F2"), verbose = FALSE),
    "not used"))
})
