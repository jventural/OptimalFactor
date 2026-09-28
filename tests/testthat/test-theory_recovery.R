theory <- list(A = c("x1", "x2", "x3"), B = c("x4", "x5", "x6"), C = c("x7", "x8", "x9"))

test_that("the theoretical partition itself is recovered perfectly", {
  r <- theory_recovery(theory, theory)$summary
  expect_equal(r$retention, 1)
  expect_equal(r$accuracy, 1)
  expect_equal(r$recovery, 1)
  expect_equal(r$ari, 1)
})

test_that("recovery does not depend on how the factors are named or ordered", {
  sol <- list(Z = theory$C, Y = theory$A, X = theory$B)
  r <- theory_recovery(theory, sol)
  expect_equal(r$summary$recovery, 1)
  expect_equal(unname(r$matching[c("Y", "X", "Z")]), c("A", "B", "C"))
})

test_that("dropping items lowers retention but not accuracy", {
  # This is the confirmatory case: items can only disappear, never move, so
  # accuracy stays at 1 and the loss shows up in retention and recovery.
  sol <- list(A = c("x1", "x2"), B = c("x4", "x5"), C = c("x7", "x8", "x9"))
  r <- theory_recovery(theory, sol)$summary
  expect_equal(r$retention, 7 / 9)
  expect_equal(r$accuracy, 1)
  expect_equal(r$recovery, 7 / 9)
})

test_that("a moved item lowers accuracy and is listed as misplaced", {
  sol <- list(F1 = c("x1", "x2"), F2 = c("x3", "x4", "x5", "x6"), F3 = theory$C)
  r <- theory_recovery(theory, sol)
  expect_equal(r$summary$accuracy, 8 / 9)
  expect_equal(r$misplaced, "x3")
  expect_lt(r$summary$ari, 1)
})

test_that("the matching is optimal where a greedy one would fail", {
  # F1 overlaps A by 2 and B by 2; F2 only overlaps A. Greedy takes the first
  # maximum (F1 -> A) and leaves F2 with nothing; the optimum is F1 -> B, F2 -> A.
  th  <- list(A = c("a1", "a2", "a3", "a4"), B = c("b1", "b2"))
  sol <- list(F1 = c("a1", "a2", "b1", "b2"), F2 = c("a3", "a4"))
  r <- theory_recovery(th, sol)
  expect_equal(unname(r$matching), c("B", "A"))
  expect_equal(r$summary$recovery, 4 / 6)
})

test_that("merged dimensions are penalised and the unmatched one recovers nothing", {
  sol <- list(F1 = c(theory$A, theory$B), F2 = theory$C)
  r <- theory_recovery(theory, sol)
  expect_equal(r$summary$k_solution, 2)
  expect_equal(r$summary$recovery, 6 / 9)
  expect_equal(sum(r$per_dimension$recovery == 0), 1)
})

test_that("a loading matrix is read by largest absolute loading and gives Tucker", {
  L <- matrix(c(.7, .6, .5, .1, 0, .2,
                .1, 0, .2, .8, .7, -.6), ncol = 2,
              dimnames = list(paste0("x", 1:6), c("F1", "F2")))
  r <- theory_recovery(theory[1:2], L)
  expect_equal(r$summary$recovery, 1)
  expect_length(r$tucker, 2)
  expect_true(all(r$tucker > 0.5 & r$tucker <= 1))
})

test_that("the outputs of the package routines are understood", {
  cfa_like <- list(final_factors = theory, final_covs = character(0))
  efa_like <- list(final_structure = data.frame(Items = paste0("x", 1:6),
                                                f1 = c(.7, .6, .5, 0, 0, 0),
                                                f2 = c(0, 0, 0, .8, .7, .6)))
  expect_equal(theory_recovery(theory, cfa_like)$summary$recovery, 1)
  expect_equal(theory_recovery(theory[1:2], efa_like)$summary$recovery, 1)
  lfs_like <- list(best = list(items = paste0("x", 1:9), ec = "x1 ~~ x2"), config = list())
  p <- .of_as_partition(lfs_like)
  expect_equal(p$asig, list(F = paste0("x", 1:9)))
  expect_equal(p$covs, "x1 ~~ x2")
  expect_error(theory_recovery(theory, 42), "Cannot read")
})
