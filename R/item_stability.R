#' Resampling Stability of the EFA-Boosting Item Selection
#'
#' @description
#' Runs the whole \code{\link{efa_boosting}} pipeline on many resamples of the
#' data and records how often each item survives, how often it is dropped, and
#' how stable its factor assignment is. This is the empirical answer to the
#' classic objection against data-driven item purification: that it capitalizes
#' on chance (MacCallum, Roznowski & Necowitz, 1992). An item removed in 97 out
#' of 100 resamples is a defensible removal; one removed in 55 is a coin flip
#' dressed as a decision.
#'
#' @details
#' Two resampling schemes are available. \code{"bootstrap"} draws \code{n}
#' cases with replacement, which is the usual choice for assessing sampling
#' variability. \code{"subsample"} draws \code{subsample_frac * n} cases without
#' replacement, which is more conservative with ordinal data because it cannot
#' duplicate rare response patterns and therefore triggers fewer empty
#' categories under WLSMV.
#'
#' Because rotation labels factors arbitrarily, the factor assignment of each
#' replication is aligned to a reference solution (the pipeline run once on the
#' complete sample) before agreement is computed. Alignment is greedy: factors
#' are matched by the largest overlap of retained items, most overlapping pair
#' first. \code{factor_agreement} is then the proportion of replications, among
#' those where the item was retained, in which the item landed on its modal
#' factor.
#'
#' Replications that fail (non-convergence, empty response categories after
#' resampling) are counted in \code{n_failed} and excluded from the rates, so
#' every proportion is computed over successful replications only.
#'
#' @param data Data frame with the item responses.
#' @param name_items Item name prefix, as in \code{\link{efa_boosting}}.
#' @param n_factors Number of factors to extract. Default 3.
#' @param R Number of resamples. Default 100. Runtime is roughly \code{R} times
#'   the cost of a single \code{efa_boosting()} call, so start small.
#' @param method Resampling scheme: \code{"bootstrap"} (default) or
#'   \code{"subsample"}.
#' @param subsample_frac Fraction of rows drawn when \code{method =
#'   "subsample"}. Default 0.8.
#' @param reference Optional \code{efa_boosting()} result on the full sample,
#'   used as the alignment reference. If \code{NULL} (default) it is computed
#'   internally.
#' @param seed Random seed. Default 2026.
#' @param n_cores Number of parallel workers. Default 1 (sequential). Values
#'   above 1 use a PSOCK cluster and require the package to be installed, not
#'   merely loaded with \code{pkgload::load_all()}.
#' @param verbose Print a progress line per replication. Default \code{TRUE}.
#' @param ... Further arguments passed to \code{\link{efa_boosting}}, e.g.
#'   \code{thresholds = list(min_omega = 0.80)} or \code{use_global = TRUE}.
#'
#' @return An object of class \code{item_stability}: a list with
#'   \code{retention} (one row per item: times retained, retention and removal
#'   rates, modal factor and factor agreement), \code{stop_reasons} (table of
#'   the stop reason across replications), \code{n_removed} (distribution of how
#'   many items each replication dropped), \code{reference} (the full-sample
#'   result), \code{n_valid}, \code{n_failed} and \code{call}.
#'
#' @section References:
#' MacCallum, R. C., Roznowski, M., & Necowitz, L. B. (1992). Model
#'   modifications in covariance structure analysis: The problem of
#'   capitalization on chance. \emph{Psychological Bulletin, 111}(3), 490--504.
#'
#' @examples
#' \dontrun{
#'   data(Data_Personality)
#'   st <- item_stability(Data_Personality, "PPTQ", n_factors = 3, R = 100)
#'   st                      # printed summary
#'   st$retention            # per-item detail
#'
#'   # with the reliability floor active
#'   item_stability(Data_Personality, "PPTQ", n_factors = 3, R = 100,
#'                  thresholds = list(min_omega = 0.70))
#' }
#' @seealso \code{\link{efa_boosting}}, \code{\link{cross_validate_cfa}},
#'   \code{\link{simulate_recovery}}
#' @export
item_stability <- function(data, name_items, n_factors = 3, R = 100,
                           method = c("bootstrap", "subsample"),
                           subsample_frac = 0.8, reference = NULL,
                           seed = 2026, n_cores = 1, verbose = TRUE, ...) {
  method <- match.arg(method)
  cl <- match.call()
  N <- nrow(data)
  if (R < 2) stop("'R' must be at least 2.")

  # Only genuine errors kill a replication: lavaan routinely warns about
  # non-positive-definite matrices with ordinal data and those runs are usable.
  run_one <- function(idx) {
    suppressWarnings(tryCatch(
      efa_boosting(data = data[idx, , drop = FALSE], name_items = name_items,
                   n_factors = n_factors, verbose = FALSE,
                   performance = list(emit_progress = FALSE), ...),
      error = function(e) NULL))
  }

  # Reference solution on the complete sample: defines the factor labels every
  # replication is aligned to.
  if (is.null(reference)) {
    if (verbose) cat("Fitting the reference solution on the full sample...\n")
    reference <- run_one(seq_len(N))
  }
  if (is.null(reference)) stop("The reference EFA-Boosting run failed; check the data and arguments.")
  ref_map <- .of_factor_map(reference$final_structure)
  # The universe is every candidate item, not just the survivors of the
  # reference run: an item the full sample discards may well survive most
  # resamples, and that disagreement is precisely what this function measures.
  items <- grep(paste0("^", name_items, "\\d+$"), names(data), value = TRUE)
  if (!length(items)) items <- names(ref_map)

  set.seed(seed)
  n_draw <- if (method == "bootstrap") N else max(2L, floor(subsample_frac * N))
  draws  <- lapply(seq_len(R), function(r)
    sample.int(N, n_draw, replace = (method == "bootstrap")))

  if (n_cores > 1 && requireNamespace("parallel", quietly = TRUE)) {
    clu <- parallel::makePSOCKcluster(min(n_cores, parallel::detectCores()))
    on.exit(parallel::stopCluster(clu), add = TRUE)
    parallel::clusterEvalQ(clu, requireNamespace("OptimalFactor", quietly = TRUE))
    dots <- list(...)
    parallel::clusterExport(clu, varlist = c("data", "name_items", "n_factors", "dots"),
                            envir = environment())
    fits <- parallel::parLapply(clu, draws, function(idx) {
      suppressWarnings(tryCatch(
        do.call(OptimalFactor::efa_boosting,
                c(list(data = data[idx, , drop = FALSE], name_items = name_items,
                       n_factors = n_factors, verbose = FALSE,
                       performance = list(emit_progress = FALSE)), dots)),
        error = function(e) NULL))
    })
  } else {
    fits <- vector("list", R)
    for (r in seq_len(R)) {
      # fits[[r]] <- NULL would drop the slot rather than fill it, and a failed
      # replication returns exactly NULL, so the counter below never saw one.
      fits[r] <- list(run_one(draws[[r]]))
      if (verbose) cat(sprintf("\r  replication %d/%d (%d failed)", r, R,
                               sum(vapply(fits[seq_len(r)], is.null, logical(1)))))
    }
    if (verbose) cat("\n")
  }

  ok    <- !vapply(fits, is.null, logical(1))
  valid <- fits[ok]
  if (!length(valid)) stop("Every replication failed; try method = 'subsample' or a smaller n_factors.")

  # Per-item tallies over successful replications.
  retained_mat <- matrix(FALSE, nrow = length(valid), ncol = length(items),
                         dimnames = list(NULL, items))
  factor_mat   <- matrix(NA_integer_, nrow = length(valid), ncol = length(items),
                         dimnames = list(NULL, items))
  for (i in seq_along(valid)) {
    fm <- .of_factor_map(valid[[i]]$final_structure)
    fm <- .of_align_factors(fm, ref_map)
    common <- intersect(names(fm), items)
    retained_mat[i, common] <- TRUE
    factor_mat[i, common]   <- fm[common]
  }

  modal <- vapply(items, function(it) {
    v <- factor_mat[, it]; v <- v[!is.na(v)]
    if (!length(v)) return(NA_integer_)
    as.integer(names(sort(table(v), decreasing = TRUE))[1])
  }, integer(1))
  agree <- vapply(items, function(it) {
    v <- factor_mat[, it]; v <- v[!is.na(v)]
    if (!length(v) || is.na(modal[it])) return(NA_real_)
    mean(v == modal[it])
  }, numeric(1))

  n_ok <- length(valid)
  retention <- data.frame(
    item             = items,
    times_retained   = colSums(retained_mat),
    retention_rate   = round(colSums(retained_mat) / n_ok, 3),
    times_removed    = n_ok - colSums(retained_mat),
    removal_rate     = round(1 - colSums(retained_mat) / n_ok, 3),
    modal_factor     = unname(modal),
    factor_agreement = round(unname(agree), 3),
    in_reference     = items %in% names(ref_map)[!is.na(ref_map)],
    stringsAsFactors = FALSE, row.names = NULL
  )
  retention <- retention[order(retention$retention_rate, retention$item), ]
  rownames(retention) <- NULL

  out <- list(
    retention    = retention,
    stop_reasons = table(vapply(valid, function(x)
      if (is.null(x$stop_reason)) "unknown" else as.character(x$stop_reason), character(1))),
    n_removed    = table(vapply(valid, function(x) length(x$removed_items), integer(1))),
    reference    = reference,
    method       = method,
    R            = R,
    n_valid      = n_ok,
    n_failed     = R - n_ok,
    call         = cl
  )
  class(out) <- "item_stability"
  out
}

# Item -> primary factor map from a final_structure data frame.
.of_factor_map <- function(final_structure) {
  if (is.null(final_structure)) return(stats::setNames(integer(0), character(0)))
  df <- as.data.frame(final_structure, stringsAsFactors = FALSE)
  lc <- which(startsWith(names(df), "f"))
  L  <- abs(as.matrix(df[, lc, drop = FALSE]))
  pr <- vapply(seq_len(nrow(L)), function(i) {
    if (all(is.na(L[i, ])) || max(L[i, ], na.rm = TRUE) == 0) return(NA_integer_)
    as.integer(which.max(L[i, ]))
  }, integer(1))
  stats::setNames(pr, df$Items)
}

# Greedy relabelling of a replication's factors to match the reference, so that
# "factor 2" means the same thing across resamples. Pairs are matched by the
# largest overlap of shared items, most overlapping pair first.
.of_align_factors <- function(rep_map, ref_map) {
  if (!length(rep_map)) return(rep_map)
  common <- intersect(names(rep_map), names(ref_map))
  if (!length(common)) return(rep_map)
  rf <- sort(unique(stats::na.omit(rep_map)))
  ff <- sort(unique(stats::na.omit(ref_map)))
  if (!length(rf) || !length(ff)) return(rep_map)
  ov <- outer(rf, ff, Vectorize(function(a, b)
    sum(rep_map[common] == a & ref_map[common] == b, na.rm = TRUE)))
  dimnames(ov) <- list(as.character(rf), as.character(ff))
  key <- stats::setNames(rep(NA_integer_, length(rf)), as.character(rf))
  while (any(is.finite(ov)) && max(ov, na.rm = TRUE) > 0) {
    pos <- which(ov == max(ov, na.rm = TRUE), arr.ind = TRUE)[1, ]
    key[rownames(ov)[pos[1]]] <- as.integer(colnames(ov)[pos[2]])
    ov[pos[1], ] <- -Inf; ov[, pos[2]] <- -Inf
  }
  # Factors with no overlap keep their original label.
  key[is.na(key)] <- as.integer(names(key)[is.na(key)])
  out <- rep_map
  idx <- !is.na(rep_map)
  out[idx] <- key[as.character(rep_map[idx])]
  out
}

#' @param x An \code{item_stability} object.
#' @param digits Number of digits for the printed rates. Default 3.
#' @param ... Ignored.
#' @rdname item_stability
#' @export
print.item_stability <- function(x, digits = 3, ...) {
  cat("\nEFA-Boosting item stability\n")
  cat(strrep("-", 60), "\n", sep = "")
  cat(sprintf("Method: %s | replications: %d valid, %d failed\n",
              x$method, x$n_valid, x$n_failed))
  cat(sprintf("Reference solution removed: %s\n",
              if (length(x$reference$removed_items)) paste(x$reference$removed_items, collapse = ", ") else "no item"))
  cat("\nRetention rate per item (lowest first):\n")
  print(x$retention, digits = digits, row.names = FALSE)
  cat("\nItems removed per replication:\n"); print(x$n_removed)
  cat("\nStop reasons:\n"); print(x$stop_reasons)
  unstable <- x$retention$item[x$retention$retention_rate > 0.25 &
                                 x$retention$retention_rate < 0.75]
  if (length(unstable))
    cat("\nUnstable decisions (retained in 25-75% of resamples): ",
        paste(unstable, collapse = ", "), "\n", sep = "")
  invisible(x)
}
