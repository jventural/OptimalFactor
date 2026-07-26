# The helpers shared by item_stability() and simulate_recovery(): both compare a
# fitted solution against a reference, and rotation names factors arbitrarily.

test_that("the factor map takes the largest absolute loading", {
  fs <- data.frame(Items = c("IT1", "IT2", "IT3"),
                   f1 = c(0.70, 0.10, -0.80),
                   f2 = c(0.20, 0.65,  0.30))
  fm <- .of_factor_map(fs)

  expect_equal(unname(fm), c(1L, 2L, 1L))   # IT3 is negative but still on f1
  expect_equal(names(fm), c("IT1", "IT2", "IT3"))
})

test_that("an empty or degenerate structure maps to nothing rather than failing", {
  expect_length(.of_factor_map(NULL), 0)
  fs <- data.frame(Items = "IT1", f1 = 0, f2 = 0)
  expect_true(is.na(.of_factor_map(fs)[["IT1"]]))
})

test_that("swapped factor labels are aligned back to the reference", {
  ref <- c(IT1 = 1L, IT2 = 1L, IT3 = 2L, IT4 = 2L)
  rep <- c(IT1 = 2L, IT2 = 2L, IT3 = 1L, IT4 = 1L)  # same partition, other names
  expect_equal(.of_align_factors(rep, ref), ref)
})

test_that("alignment matches the largest overlap, not the first pairing", {
  ref <- c(IT1 = 1L, IT2 = 1L, IT3 = 1L, IT4 = 2L, IT5 = 2L, IT6 = 2L)
  rep <- c(IT1 = 2L, IT2 = 2L, IT3 = 1L, IT4 = 1L, IT5 = 1L, IT6 = 1L)
  out <- .of_align_factors(rep, ref)
  # rep-factor 1 overlaps ref-factor 2 three times against ref-factor 1 once, so
  # that pair is taken first and rep-factor 2 gets what is left.
  expect_equal(unname(out), c(1L, 1L, 2L, 2L, 2L, 2L))
})

test_that("a partition with no shared items is returned untouched", {
  ref <- c(IT1 = 1L, IT2 = 2L)
  rep <- c(IT9 = 1L, IT8 = 2L)
  expect_equal(.of_align_factors(rep, ref), rep)
  expect_equal(.of_align_factors(integer(0), ref), integer(0))
})

test_that("recovery scoring separates dropping good items from keeping bad ones", {
  pop  <- .of_population_model(2, 3, 0.65, 0.30, 1, 0.40, 1, 0.20)
  good <- names(pop$role)[pop$role == "good"]        # IT1..IT6
  bad  <- names(pop$role)[pop$role != "good"]        # IT7 cross, IT8 low

  # A perfect solution, with the factor labels swapped on purpose.
  perfect <- data.frame(Items = good,
                        f1 = c(0, 0, 0, .7, .7, .7),
                        f2 = c(.7, .7, .7, 0, 0, 0))
  ev <- .of_score_recovery(list(final_structure = perfect), pop)
  expect_true(ev$recovered)
  expect_equal(ev$specificity, 1)
  expect_equal(ev$sensitivity, 1)
  expect_equal(ev$n_retained, 6L)

  # Keeping the cross-loading item costs sensitivity, not specificity.
  kept_bad <- rbind(perfect, data.frame(Items = bad[1], f1 = .4, f2 = .4))
  ev_bad <- .of_score_recovery(list(final_structure = kept_bad), pop)
  expect_equal(ev_bad$sensitivity, 0.5)
  expect_equal(ev_bad$specificity, 1)

  # Dropping a good item costs specificity. Recovery stays TRUE by design: it is
  # defined over the items that survived, so it must be read next to specificity.
  dropped_good <- perfect[-1, ]
  ev_drop <- .of_score_recovery(list(final_structure = dropped_good), pop)
  expect_equal(ev_drop$specificity, 5 / 6)
  expect_true(ev_drop$recovered)

  # An item on the wrong factor is a genuine failure to recover.
  wrong <- perfect
  wrong[1, c("f1", "f2")] <- c(.7, 0)
  expect_false(.of_score_recovery(list(final_structure = wrong), pop)$recovered)

  # Losing a whole factor is a failure even though every item left is coherent.
  collapsed <- data.frame(Items = good, f1 = rep(.7, 6), f2 = rep(0, 6))
  expect_false(.of_score_recovery(list(final_structure = collapsed), pop)$recovered)
})
