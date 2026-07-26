#' Monte Carlo Recovery of a Known Structure with CFA-Boosting
#'
#' @description
#' The confirmatory counterpart of \code{\link{simulate_recovery}}. Data are
#' simulated from a known population, a simple structure model over every item
#' is handed to \code{\link{cfa_boosting}}, and the question is how often the
#' pipeline removes the items that deserve removal and only those.
#'
#' @details
#' The question is not the same one the exploratory simulation asks, and the
#' difference is not cosmetic. In \code{\link{simulate_recovery}} the structure
#' is discovered, so recovery means every retained item landed on its true
#' factor. Here the structure is imposed by the researcher, so the assignment
#' cannot be wrong by construction and what is at stake is the item selection:
#' the specified model deliberately includes the contaminated items, and a
#' pipeline that works removes them without taking good items with them.
#'
#' Two further quantities are worth watching. \code{covs_added} counts the
#' residual covariances the pipeline introduced: the population has none, so
#' every one of them is a false positive, which is the modification index
#' objection of MacCallum, Roznowski and Necowitz (1992) turned into a number.
#' And \code{exact} is deliberately strict, requiring the removed set to equal
#' the contaminated set exactly, because sensitivity and specificity can both
#' look respectable while no single replication got the answer right.
#'
#' The cross loading item is specified on the first factor and the weak item as
#' well, which is what a researcher would do when writing the model without
#' knowing which items are faulty.
#'
#' @inheritParams simulate_recovery
#' @param ... Further arguments passed to \code{\link{cfa_boosting}}, e.g.
#'   \code{thresholds = list(rmsea_target = 0.06)}.
#'
#' @return An object of class \code{simulate_cfa_recovery}: a list with
#'   \code{summary} (one row per condition), \code{replications},
#'   \code{conditions}, \code{population}, \code{model} (the syntax handed to
#'   the pipeline) and \code{call}.
#'
#' @examples
#' \dontrun{
#'   sim <- simulate_cfa_recovery(n = c(200, 500), loading = c(0.50, 0.70),
#'                                n_reps = 50, n_cores = 6)
#'   sim
#'   plot(sim, metric = "sensitivity")
#' }
#' @seealso \code{\link{simulate_recovery}}, \code{\link{cfa_boosting}}
#' @export
simulate_cfa_recovery <- function(n = 500, loading = 0.65, items_per_factor = 5,
                                  n_factors = 3, phi = 0.30,
                                  n_cross = 1, cross_loading = 0.40,
                                  n_low = 1, low_loading = 0.20,
                                  n_categories = 5,
                                  skew = c("symmetric", "skewed"),
                                  n_reps = 100, seed = 2026, verbose = TRUE,
                                  n_cores = 1, timeout = 120, ...) {
  skew <- match.arg(skew)
  cl <- match.call()
  conditions <- expand.grid(n = n, loading = loading,
                            items_per_factor = items_per_factor,
                            stringsAsFactors = FALSE)
  set.seed(seed)

  use_par <- n_cores > 1 && requireNamespace("parallel", quietly = TRUE)
  clu <- NULL
  if (use_par) {
    clu <- .of_start_cluster(n_cores, n_tasks = n_reps, verbose = verbose)
    on.exit(parallel::stopCluster(clu), add = TRUE)
  }
  dots <- list(...)

  reps <- list(); pop_last <- NULL; syntax_last <- NULL
  for (ci in seq_len(nrow(conditions))) {
    cnd <- conditions[ci, ]
    pop <- .of_population_model(n_factors = n_factors,
                                items_per_factor = cnd$items_per_factor,
                                loading = cnd$loading, phi = phi,
                                n_cross = n_cross, cross_loading = cross_loading,
                                n_low = n_low, low_loading = low_loading)
    syntax <- .of_cfa_syntax(pop)
    pop_last <- pop; syntax_last <- syntax

    if (verbose)
      .of_say("Condition %d/%d: N=%d, lambda=%.2f, items/factor=%d (%d items total)",
              ci, nrow(conditions), cnd$n, cnd$loading, cnd$items_per_factor,
              nrow(pop$lambda))

    datasets <- lapply(seq_len(n_reps), function(r)
      .of_simulate_ordinal(pop$sigma, cnd$n, n_categories, skew,
                           colnames_prefix = "IT"))
    fit_one <- .of_make_cfa_worker(syntax, dots, remote = use_par,
                                   timeout = timeout)

    fits <- if (use_par) {
      .of_cluster_lapply(clu, datasets, fit_one, verbose = verbose)
    } else {
      .of_serial_lapply(datasets, fit_one, verbose = verbose)
    }

    for (r in seq_len(n_reps)) {
      fit <- fits[[r]]
      row <- data.frame(condition = ci, n = cnd$n, loading = cnd$loading,
                        items_per_factor = cnd$items_per_factor,
                        rep = r, converged = !is.null(fit),
                        exact = NA, sensitivity = NA_real_,
                        specificity = NA_real_, n_retained = NA_integer_,
                        covs_added = NA_integer_, rmsea = NA_real_,
                        stringsAsFactors = FALSE)
      if (!is.null(fit)) {
        ev <- .of_score_cfa_recovery(fit, pop)
        row$exact       <- ev$exact
        row$sensitivity <- ev$sensitivity
        row$specificity <- ev$specificity
        row$n_retained  <- ev$n_retained
        row$covs_added  <- ev$covs_added
        row$rmsea       <- ev$rmsea
      }
      reps[[length(reps) + 1]] <- row
    }
  }

  replications <- do.call(rbind, reps)
  summ <- do.call(rbind, lapply(split(replications, replications$condition), function(d) {
    ok <- d[d$converged, , drop = FALSE]
    m <- function(v, k = 3) if (nrow(ok)) round(mean(v, na.rm = TRUE), k) else NA_real_
    data.frame(
      n = d$n[1], loading = d$loading[1], items_per_factor = d$items_per_factor[1],
      n_reps = nrow(d),
      convergence_rate = round(mean(d$converged), 3),
      exact_rate       = m(ok$exact),
      sensitivity      = m(ok$sensitivity),
      specificity      = m(ok$specificity),
      mean_retained    = m(ok$n_retained, 2),
      mean_covs_added  = m(ok$covs_added, 2),
      mean_rmsea       = m(ok$rmsea),
      stringsAsFactors = FALSE, row.names = NULL)
  }))
  rownames(summ) <- NULL

  out <- list(summary = summ, replications = replications,
              conditions = conditions, population = pop_last,
              model = syntax_last, n_reps = n_reps, call = cl)
  class(out) <- "simulate_cfa_recovery"
  out
}

# Simple structure lavaan syntax over every item of the population, which is
# what a researcher writes before knowing which items misbehave. The
# contaminated ones have no true factor, so they are specified on the first,
# exactly where an unsuspecting analyst would put them.
.of_cfa_syntax <- function(pop) {
  assign <- pop$true_factor
  assign[is.na(assign)] <- 1L
  paste(vapply(seq_len(pop$n_factors), function(f) {
    items <- names(assign)[assign == f]
    paste0("f", f, " =~ ", paste(items, collapse = " + "))
  }, character(1)), collapse = "\n")
}

# See .of_make_fit_worker for the remote/local distinction. cfa_boosting caps
# itself through performance$timeout_cfa rather than the R.utils machinery
# efa_boosting uses, so the timeout is translated here.
.of_make_cfa_worker <- function(model, dots, remote = FALSE, timeout = NULL) {
  force(model); force(remote)
  perf <- list()
  if (!is.null(timeout) && is.finite(timeout) && timeout > 0)
    perf$timeout_cfa <- timeout
  if (!is.null(dots$performance)) perf <- utils::modifyList(perf, dots$performance)
  if (length(perf)) dots$performance <- perf

  function(dat) {
    FUN <- if (remote) OptimalFactor::cfa_boosting else cfa_boosting
    suppressWarnings(tryCatch(
      do.call(FUN, c(list(data = dat, model = model, verbose = FALSE), dots)),
      error = function(e) NULL))
  }
}

# Compare a CFA-Boosting solution against the population it came from.
.of_score_cfa_recovery <- function(fit, pop) {
  removed <- as.character(fit$removed_items)
  removed <- removed[!is.na(removed)]
  good <- names(pop$role)[pop$role == "good"]
  bad  <- names(pop$role)[pop$role != "good"]

  specificity <- if (length(good)) mean(!(good %in% removed)) else NA_real_
  sensitivity <- if (length(bad))  mean(bad %in% removed)     else NA_real_
  exact <- setequal(removed, bad)

  n_covs <- length(fit$added_covariances)
  if (is.null(n_covs)) n_covs <- 0L

  idx <- fit$fit_indices
  rmsea <- NA_real_
  if (!is.null(idx)) {
    for (nm in c("rmsea.scaled", "rmsea.robust", "rmsea")) {
      v <- suppressWarnings(as.numeric(idx[[nm]]))
      if (length(v) == 1L && is.finite(v)) { rmsea <- v; break }
    }
  }

  list(exact = exact, sensitivity = sensitivity, specificity = specificity,
       n_retained = length(pop$items) - length(removed),
       covs_added = as.integer(n_covs), rmsea = rmsea)
}

#' @param x A \code{simulate_cfa_recovery} object.
#' @param ... Ignored.
#' @rdname simulate_cfa_recovery
#' @export
print.simulate_cfa_recovery <- function(x, ...) {
  cat("\nCFA-Boosting recovery simulation\n")
  cat(strrep("-", 68), "\n", sep = "")
  cat(sprintf("%d condition(s) x %d replications\n", nrow(x$conditions), x$n_reps))
  cat(sprintf("Population: %d factors, roles: %s\n", x$population$n_factors,
              paste(names(table(x$population$role)), table(x$population$role),
                    sep = "=", collapse = ", ")))
  cat("\n"); print(x$summary, row.names = FALSE)
  cat("\nexact_rate = the removed set equals the contaminated set, item for item.\n")
  cat("sensitivity = contaminated items removed; specificity = good items kept.\n")
  cat("mean_covs_added = residual covariances added; the population has none,\n")
  cat("so anything above zero is capitalization on chance.\n")
  invisible(x)
}

#' Plot a CFA Recovery Simulation
#'
#' @param x A \code{simulate_cfa_recovery} object.
#' @param metric Which column of \code{x$summary} to draw. Default
#'   \code{"exact_rate"}.
#' @param reference Horizontal reference line for the proportion metrics.
#'   Default 0.90; \code{NULL} removes it.
#' @param ... Ignored.
#'
#' @return A \code{ggplot} object.
#' @seealso \code{\link{simulate_cfa_recovery}}
#' @export
plot.simulate_cfa_recovery <- function(x, metric = c("exact_rate", "sensitivity",
                                                     "specificity", "convergence_rate",
                                                     "mean_retained", "mean_covs_added"),
                                       reference = 0.90, ...) {
  metric <- match.arg(metric)
  .of_recovery_plot(x$summary, metric, reference, x$n_reps,
                    sprintf("CFA-Boosting recovery (%d replications per condition)",
                            x$n_reps))
}
