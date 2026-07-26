test_that("a NULL result keeps its slot instead of collapsing the list", {
  # This is the invariant behind both resampling routines. `out[[i]] <- NULL`
  # deletes the slot, and a failed fit returns exactly NULL, so a run where any
  # task failed used to end with a shorter list than tasks dispatched.
  out <- .of_serial_lapply(as.list(1:4),
                           function(i) if (i %% 2 == 0) NULL else i,
                           verbose = FALSE)
  expect_length(out, 4L)
  expect_equal(out, list(1L, NULL, 3L, NULL))
})

test_that("the sequential runner preserves order and passes each element", {
  out <- .of_serial_lapply(list("a", "b", "c"), toupper, verbose = FALSE)
  expect_equal(out, list("A", "B", "C"))
  expect_length(.of_serial_lapply(list(), identity, verbose = FALSE), 0L)
})

test_that("durations are formatted at the right scale", {
  expect_equal(.of_hms(0), "0s")
  expect_equal(.of_hms(45), "45s")
  expect_equal(.of_hms(90), "1m30s")
  expect_equal(.of_hms(3661), "1h01m")
  expect_equal(.of_hms(NA_real_), "?")
  expect_equal(.of_hms(Inf), "?")
})

test_that("the progress line reports the fraction done and an estimate", {
  t0 <- Sys.time() - 10
  out <- capture.output(.of_progress(3, 12, t0), type = "output")
  txt <- paste(out, collapse = "")

  expect_match(txt, "25%")
  expect_match(txt, "\\(3/12\\)")
  expect_match(txt, "elapsed")
  expect_match(txt, "left")
})

test_that("progress is silent when the caller asked for silence", {
  expect_silent(.of_serial_lapply(as.list(1:3), identity, verbose = FALSE))
})

test_that("a timeout is translated into efa_boosting's performance options", {
  skip_if_not_installed("R.utils")
  a <- .of_fit_args(list(), timeout = 90)

  expect_true(a$performance$use_timeouts)
  expect_equal(a$performance$timeout_optimization, 90)
  expect_equal(a$performance$timeout_efa, 30)      # a third of the budget
  expect_false(a$performance$emit_progress)
})

test_that("no timeout leaves the caps untouched", {
  for (tm in list(NULL, 0, Inf)) {
    a <- .of_fit_args(list(), timeout = tm)
    expect_null(a$performance$use_timeouts)
    expect_false(a$performance$emit_progress)
  }
})

test_that("the caller's performance settings win over the defaults", {
  # Regression test: 'performance' used to be hard-coded next to ... in the
  # do.call, so passing it through ... raised "formal argument matched by
  # multiple actual arguments" instead of overriding anything.
  a <- .of_fit_args(list(performance = list(max_candidates_eval = 4,
                                            emit_progress = TRUE)),
                    timeout = 60)
  expect_equal(a$performance$max_candidates_eval, 4)
  expect_true(a$performance$emit_progress)
  expect_equal(a$performance$timeout_optimization, 60)   # el resto sobrevive
})

test_that("other arguments travel untouched next to performance", {
  a <- .of_fit_args(list(thresholds = list(loading = 0.4)), timeout = NULL)
  expect_equal(a$thresholds$loading, 0.4)
  expect_named(a, c("thresholds", "performance"))
})
