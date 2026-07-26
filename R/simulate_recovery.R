#' Monte Carlo Recovery of a Known Factor Structure
#'
#' @description
#' Simulates ordinal data from a known population model, runs the
#' \code{\link{efa_boosting}} pipeline on each replication, and reports how
#' often the algorithm recovers the true structure. Conditions can be crossed
#' over sample size, loading size and number of items per factor, which is what
#' turns this into evidence about \emph{when} the method works rather than a
#' single anecdote.
#'
#' @details
#' The population model has \code{n_factors} correlated factors (correlation
#' \code{phi}), \code{items_per_factor} good items with loading \code{loading},
#' plus optional contaminated items: \code{n_cross} items that load
#' \code{cross_loading} on two factors and \code{n_low} items whose loading is
#' \code{low_loading}. Continuous responses are drawn from the implied
#' correlation matrix and discretized into \code{n_categories} ordered
#' categories, either symmetrically or with a skew.
#'
#' Four quantities are recorded per replication:
#' \describe{
#'   \item{\code{recovered}}{TRUE when every retained good item sits on the
#'     factor it truly belongs to, after aligning labels (rotation names factors
#'     arbitrarily), and no factor was lost.}
#'   \item{\code{sensitivity}}{proportion of contaminated items correctly
#'     removed. \code{NA} when the condition has no contaminated items.}
#'   \item{\code{specificity}}{proportion of good items correctly retained.}
#'   \item{\code{n_retained}, \code{rmsea}, \code{stop_reason}}{final size, fit
#'     and why the pipeline stopped.}
#' }
#'
#' Sensitivity and specificity are the honest way to read the pipeline: an
#' algorithm that removes everything scores a perfect sensitivity and a terrible
#' specificity, so both must be read together.
#'
#' @param n Sample size(s). Vector values are crossed into conditions.
#'   Default 500.
#' @param loading Population loading(s) of the good items. Default 0.65.
#' @param items_per_factor Number of good items per factor. Default 5.
#' @param n_factors Number of factors in the population model. Default 3.
#' @param phi Interfactor correlation. Default 0.30.
#' @param n_cross Number of cross-loading items added to the pool (they load
#'   \code{cross_loading} on factors 1 and 2). Default 1.
#' @param cross_loading Loading of the cross-loading items on both factors.
#'   Default 0.40.
#' @param n_low Number of weak items added to the pool. Default 1.
#' @param low_loading Loading of the weak items. Default 0.20.
#' @param n_categories Number of ordered response categories. Default 5.
#' @param skew Category threshold shape: \code{"symmetric"} (default) or
#'   \code{"skewed"}.
#' @param n_reps Replications per condition. Default 100.
#' @param seed Random seed. Default 2026.
#' @param verbose Print progress per condition. Default \code{TRUE}.
#' @param n_cores Number of worker processes used to fit the replications of a
#'   condition. Default 1 (sequential). A crossed design is hundreds of
#'   \code{efa_boosting()} runs, so this is usually the difference between
#'   minutes and hours; \code{parallel::detectCores() - 1} is a practical
#'   choice. The datasets are always generated sequentially from \code{seed},
#'   so results do not depend on the number of cores.
#' @param ... Further arguments passed to \code{\link{efa_boosting}}, e.g.
#'   \code{thresholds = list(min_omega = 0.70)}.
#'
#' @return An object of class \code{simulate_recovery}: a list with
#'   \code{summary} (one row per condition: recovery rate, mean sensitivity and
#'   specificity, mean items retained, convergence rate), \code{replications}
#'   (the raw per-replication data frame), \code{conditions}, \code{population}
#'   (the loading matrix, factor correlations and item roles of the last
#'   condition) and \code{call}.
#'
#' @examples
#' \dontrun{
#'   # single condition, quick check
#'   simulate_recovery(n = 300, n_reps = 20)
#'
#'   # crossed design for a validation paper
#'   sim <- simulate_recovery(n = c(200, 500, 1000), loading = c(0.50, 0.70),
#'                            items_per_factor = 5, n_reps = 200)
#'   sim$summary
#' }
#' @seealso \code{\link{efa_boosting}}, \code{\link{item_stability}}
#' @export
simulate_recovery <- function(n = 500, loading = 0.65, items_per_factor = 5,
                              n_factors = 3, phi = 0.30,
                              n_cross = 1, cross_loading = 0.40,
                              n_low = 1, low_loading = 0.20,
                              n_categories = 5, skew = c("symmetric", "skewed"),
                              n_reps = 100, seed = 2026, verbose = TRUE,
                              n_cores = 1, ...) {
  skew <- match.arg(skew)
  cl <- match.call()
  conditions <- expand.grid(n = n, loading = loading,
                            items_per_factor = items_per_factor,
                            stringsAsFactors = FALSE)
  set.seed(seed)

  # One cluster for the whole design, not one per condition.
  use_par <- n_cores > 1 && requireNamespace("parallel", quietly = TRUE)
  clu <- NULL
  if (use_par) {
    clu <- parallel::makePSOCKcluster(min(n_cores, parallel::detectCores()))
    on.exit(parallel::stopCluster(clu), add = TRUE)
    parallel::clusterEvalQ(clu, requireNamespace("OptimalFactor", quietly = TRUE))
  }
  dots <- list(...)

  reps <- list(); pop_last <- NULL
  for (ci in seq_len(nrow(conditions))) {
    cnd <- conditions[ci, ]
    pop <- .of_population_model(n_factors = n_factors,
                                items_per_factor = cnd$items_per_factor,
                                loading = cnd$loading, phi = phi,
                                n_cross = n_cross, cross_loading = cross_loading,
                                n_low = n_low, low_loading = low_loading)
    pop_last <- pop
    if (verbose)
      cat(sprintf("Condition %d/%d: N=%d, lambda=%.2f, items/factor=%d (%d items total)\n",
                  ci, nrow(conditions), cnd$n, cnd$loading, cnd$items_per_factor,
                  nrow(pop$lambda)))

    # The datasets are drawn sequentially so the design is reproducible from
    # 'seed' regardless of how many cores fit them afterwards.
    datasets <- lapply(seq_len(n_reps), function(r)
      .of_simulate_ordinal(pop$sigma, cnd$n, n_categories, skew,
                           colnames_prefix = "IT"))
    fit_one <- .of_make_fit_worker(n_factors, dots, remote = use_par)

    if (use_par) {
      fits <- parallel::parLapply(clu, datasets, fit_one)
    } else {
      fits <- vector("list", n_reps)
      for (r in seq_len(n_reps)) {
        # fits[[r]] <- NULL would delete the slot instead of filling it, and a
        # failed replication returns exactly NULL.
        fits[r] <- list(fit_one(datasets[[r]]))
        if (verbose && r %% 10 == 0) cat(sprintf("\r  replication %d/%d", r, n_reps))
      }
    }

    for (r in seq_len(n_reps)) {
      fit <- fits[[r]]
      row <- data.frame(condition = ci, n = cnd$n, loading = cnd$loading,
                        items_per_factor = cnd$items_per_factor,
                        rep = r, converged = !is.null(fit),
                        recovered = NA, sensitivity = NA_real_,
                        specificity = NA_real_, n_retained = NA_integer_,
                        rmsea = NA_real_, stop_reason = NA_character_,
                        stringsAsFactors = FALSE)
      if (!is.null(fit)) {
        ev <- .of_score_recovery(fit, pop)
        row$recovered   <- ev$recovered
        row$sensitivity <- ev$sensitivity
        row$specificity <- ev$specificity
        row$n_retained  <- ev$n_retained
        row$rmsea       <- as.numeric(fit$final_rmsea)
        row$stop_reason <- as.character(fit$stop_reason)
      }
      reps[[length(reps) + 1]] <- row
    }
    if (verbose) cat("\r  done                    \n")
  }

  replications <- do.call(rbind, reps)
  summ <- do.call(rbind, lapply(split(replications, replications$condition), function(d) {
    ok <- d[d$converged, , drop = FALSE]
    data.frame(
      n = d$n[1], loading = d$loading[1], items_per_factor = d$items_per_factor[1],
      n_reps = nrow(d),
      convergence_rate = round(mean(d$converged), 3),
      recovery_rate    = if (nrow(ok)) round(mean(ok$recovered, na.rm = TRUE), 3) else NA_real_,
      sensitivity      = if (nrow(ok)) round(mean(ok$sensitivity, na.rm = TRUE), 3) else NA_real_,
      specificity      = if (nrow(ok)) round(mean(ok$specificity, na.rm = TRUE), 3) else NA_real_,
      mean_retained    = if (nrow(ok)) round(mean(ok$n_retained, na.rm = TRUE), 2) else NA_real_,
      mean_rmsea       = if (nrow(ok)) round(mean(ok$rmsea, na.rm = TRUE), 3) else NA_real_,
      stringsAsFactors = FALSE, row.names = NULL)
  }))
  rownames(summ) <- NULL

  out <- list(summary = summ, replications = replications,
              conditions = conditions, population = pop_last,
              n_reps = n_reps, call = cl)
  class(out) <- "simulate_recovery"
  out
}

# Fitting closure with a deliberately small environment: only the arguments the
# fit needs travel to the cluster, not the datasets held by the calling frame.
.of_make_fit_worker <- function(n_factors, dots, remote = FALSE) {
  force(n_factors); force(dots); force(remote)
  function(dat) {
    # A PSOCK worker has to address the package by name, but in-process the
    # namespace resolves lexically. The distinction matters: under
    # devtools::load_all() nothing is exported yet, so `::` would fail and every
    # replication would silently be recorded as non-convergent.
    FUN <- if (remote) OptimalFactor::efa_boosting else efa_boosting
    suppressWarnings(tryCatch(
      do.call(FUN,
              c(list(data = dat, name_items = "IT", n_factors = n_factors,
                     verbose = FALSE,
                     performance = list(emit_progress = FALSE)), dots)),
      error = function(e) NULL))
  }
}

# Population loading matrix, factor correlations and implied item correlations.
.of_population_model <- function(n_factors, items_per_factor, loading, phi,
                                 n_cross, cross_loading, n_low, low_loading) {
  p_good <- n_factors * items_per_factor
  p <- p_good + n_cross + n_low
  lambda <- matrix(0, nrow = p, ncol = n_factors)
  role <- character(p); true_factor <- rep(NA_integer_, p)

  k <- 0L
  for (f in seq_len(n_factors)) for (i in seq_len(items_per_factor)) {
    k <- k + 1L
    lambda[k, f] <- loading; role[k] <- "good"; true_factor[k] <- f
  }
  if (n_cross > 0) for (i in seq_len(n_cross)) {
    k <- k + 1L
    lambda[k, 1] <- cross_loading
    lambda[k, min(2, n_factors)] <- cross_loading
    role[k] <- "cross"
  }
  if (n_low > 0) for (i in seq_len(n_low)) {
    k <- k + 1L
    lambda[k, 1] <- low_loading; role[k] <- "low"
  }

  Phi <- matrix(phi, n_factors, n_factors); diag(Phi) <- 1
  comm <- rowSums((lambda %*% Phi) * lambda)
  if (any(comm >= 1))
    stop("The population model implies communalities >= 1; lower 'loading', ",
         "'cross_loading' or 'phi'.")
  sigma <- lambda %*% Phi %*% t(lambda)
  diag(sigma) <- 1
  items <- paste0("IT", seq_len(p))
  dimnames(sigma) <- list(items, items)
  rownames(lambda) <- items

  list(lambda = lambda, phi = Phi, sigma = sigma, role = stats::setNames(role, items),
       true_factor = stats::setNames(true_factor, items), items = items,
       n_factors = n_factors)
}

# Draw multivariate normal data and cut it into ordered categories.
.of_simulate_ordinal <- function(sigma, n, n_categories, skew, colnames_prefix = "IT") {
  p <- ncol(sigma)
  L <- tryCatch(chol(sigma), error = function(e)
    stop("The implied correlation matrix is not positive definite; adjust the population model."))
  Z <- matrix(stats::rnorm(n * p), nrow = n) %*% L
  probs <- if (identical(skew, "skewed")) {
    # Right-skewed categories: the category weights decrease, so most responses
    # land in the lower options. cumsum() is what turns those weights into the
    # cumulative proportions the thresholds are read from; without it the values
    # are not a distribution and the skew comes out reversed.
    w <- rev(seq_len(n_categories))^1.8
    cumsum(w) / sum(w)
  } else {
    seq_len(n_categories) / n_categories
  }
  tau <- stats::qnorm(utils::head(probs, -1))
  X <- apply(Z, 2, function(z) as.integer(cut(z, breaks = c(-Inf, tau, Inf), labels = FALSE)))
  X <- as.data.frame(X)
  names(X) <- paste0(colnames_prefix, seq_len(p))
  X
}

# Compare a fitted solution against the population model.
.of_score_recovery <- function(fit, pop) {
  fm <- .of_factor_map(fit$final_structure)
  retained <- names(fm)
  good <- names(pop$role)[pop$role == "good"]
  bad  <- names(pop$role)[pop$role != "good"]

  specificity <- if (length(good)) mean(good %in% retained) else NA_real_
  sensitivity <- if (length(bad))  mean(!(bad %in% retained)) else NA_real_

  # Align estimated factors to true factors by overlap of good items.
  true_map <- pop$true_factor[good]
  est_map  <- fm[intersect(retained, good)]
  recovered <- FALSE
  if (length(est_map) > 0) {
    aligned <- .of_align_factors(est_map, true_map[names(est_map)])
    all_factors_present <- length(unique(stats::na.omit(aligned))) == pop$n_factors
    recovered <- all_factors_present &&
      all(!is.na(aligned)) &&
      all(aligned == true_map[names(aligned)])
  }

  list(recovered = recovered, sensitivity = sensitivity,
       specificity = specificity, n_retained = length(retained))
}

#' @param x A \code{simulate_recovery} object.
#' @param ... Ignored.
#' @rdname simulate_recovery
#' @export
print.simulate_recovery <- function(x, ...) {
  cat("\nEFA-Boosting recovery simulation\n")
  cat(strrep("-", 68), "\n", sep = "")
  cat(sprintf("%d condition(s) x %d replications\n", nrow(x$conditions), x$n_reps))
  cat(sprintf("Population: %d factors, roles: %s\n", x$population$n_factors,
              paste(names(table(x$population$role)), table(x$population$role),
                    sep = "=", collapse = ", ")))
  cat("\n"); print(x$summary, row.names = FALSE)
  cat("\nrecovery_rate = every retained good item on its true factor.\n")
  cat("sensitivity = contaminated items removed; specificity = good items kept.\n")
  invisible(x)
}

#' Plot a Recovery Simulation
#'
#' @description
#' Draws the recovery curve of a \code{\link{simulate_recovery}} design: the
#' chosen metric against sample size, one line per population loading and one
#' panel per number of items per factor. This is the figure a validation paper
#' reports, so the crossed design is worth running with enough replications for
#' the curve to be stable.
#'
#' @param x A \code{simulate_recovery} object.
#' @param metric Which column of \code{x$summary} to draw: \code{"recovery_rate"}
#'   (default), \code{"sensitivity"}, \code{"specificity"},
#'   \code{"convergence_rate"} or \code{"mean_retained"}.
#' @param reference Horizontal reference line, drawn only for the proportion
#'   metrics. Default 0.90; \code{NULL} removes it.
#' @param ... Ignored.
#'
#' @return A \code{ggplot} object.
#' @examples
#' \dontrun{
#'   sim <- simulate_recovery(n = c(200, 500, 1000), loading = c(0.50, 0.70),
#'                            n_reps = 200, n_cores = 6)
#'   plot(sim)
#'   plot(sim, metric = "sensitivity")
#' }
#' @seealso \code{\link{simulate_recovery}}
#' @export
plot.simulate_recovery <- function(x, metric = c("recovery_rate", "sensitivity",
                                                 "specificity", "convergence_rate",
                                                 "mean_retained"),
                                   reference = 0.90, ...) {
  metric <- match.arg(metric)
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is needed to plot a recovery simulation.")

  d <- x$summary
  if (!metric %in% names(d)) stop("Metric '", metric, "' is not in the summary.")
  d$value   <- d[[metric]]
  d$loading <- factor(d$loading)
  is_rate   <- metric != "mean_retained"

  p <- ggplot2::ggplot(d, ggplot2::aes(x = n, y = value,
                                       colour = loading, group = loading))
  if (is_rate && !is.null(reference))
    p <- p + ggplot2::geom_hline(yintercept = reference, linetype = "dashed",
                                 colour = "grey55", linewidth = 0.4)
  p <- p +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::labs(x = "Sample size", y = gsub("_", " ", metric),
                  colour = "Loading",
                  title = sprintf("EFA-Boosting recovery (%d replications per condition)",
                                  x$n_reps)) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  if (is_rate) p <- p + ggplot2::scale_y_continuous(limits = c(0, 1))
  if (length(unique(d$items_per_factor)) > 1)
    p <- p + ggplot2::facet_wrap(~ items_per_factor,
                                 labeller = ggplot2::labeller(
                                   items_per_factor = function(v) paste(v, "items/factor")))
  p
}

# Columns referred to inside aes(); declared so R CMD check stays quiet.
utils::globalVariables(c("n", "value", "loading", "items_per_factor"))
