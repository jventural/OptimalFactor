#' Split-Half Stability of Any Item-Selection Algorithm
#'
#' @description
#' Answers the question that decides between purification algorithms: does the
#' algorithm make the same decision on a different sample, and does that
#' decision hold on data it has never seen? The data are split at random into
#' two halves many times. On each split the algorithm runs on the derivation
#' half, and the structure it returns is fitted as a CFA on the validation
#' half. Unlike \code{\link{item_stability}} (tied to \code{efa_boosting()})
#' and \code{\link{cross_validate_cfa}} (one factor only), the algorithm here is
#' any function, so every routine in the package can be compared on the same
#' footing.
#'
#' @details
#' An algorithm that purifies items on the same data used to judge them
#' capitalises on chance (MacCallum, Roznowski & Necowitz, 1992): the fit it
#' reports is optimistic, and a different sample may have led it elsewhere. Two
#' kinds of evidence are therefore recorded on every split:
#'
#' \describe{
#'   \item{Replication of the decision}{\code{jaccard}, the overlap between the
#'     items retained on the derivation half and those retained by the
#'     reference solution (the algorithm run once on the complete sample), and
#'     \code{ari_reference}, the agreement of the two partitions on their common
#'     items. \code{item_retention} gives, for each item, the proportion of
#'     splits in which it survived: an item dropped in 29 of 30 splits is a
#'     defensible removal, one dropped in 16 is a coin flip.}
#'   \item{Out-of-sample adequacy}{CFI, TLI, RMSEA, SRMR and the largest
#'     interfactor correlation of the derived structure fitted on the
#'     validation half, and \code{meets}, whether all the targets are met
#'     there.}
#' }
#'
#' When \code{theory} is given, \code{\link{theory_recovery}} is applied to
#' every derived structure, so the stability of the recovery of the theory is
#' reported as well.
#'
#' The halves are drawn on the master, so results are reproducible with
#' \code{seed} and identical for any \code{n_cores}.
#'
#' @param data Data frame with the item responses.
#' @param algorithm A function of one argument, the data of the derivation
#'   half, returning the structure it selects: a named list factor -> items, a
#'   loading matrix, or the object returned by \code{efa_boosting()},
#'   \code{cfa_boosting()}, \code{discriminant_boosting()},
#'   \code{specification_search_theory()} or \code{local_fit_search()}. The
#'   residual covariances these routines free are kept in the validation CFA. Functions from other
#'   packages must be called with \code{pkg::fun} inside it when
#'   \code{n_cores > 1}.
#' @param n_splits Number of random splits. Default 30.
#' @param theory Optional named list with the theoretical key, as in
#'   \code{\link{theory_recovery}}.
#' @param reference Optional structure used as reference for \code{jaccard}.
#'   Default \code{NULL} runs \code{algorithm} once on the complete sample.
#' @param estimator,ordered Estimation of the validation CFA. Defaults
#'   \code{"WLSMV"} and \code{TRUE} (items treated as ordinal).
#' @param targets Named vector with the fit targets used for \code{meets}.
#'   Default \code{c(cfi = .95, rmsea = .08, srmr = .08)}.
#' @param phi_max Interfactor correlation at or above which two factors are
#'   considered indistinguishable; part of \code{meets}. Default .90.
#' @param seed Seed for the splits. Default 2026.
#' @param timeout Seconds allowed for the algorithm on one split; a split that
#'   exceeds it is counted as failed instead of stalling the whole run. Small
#'   derivation halves can send an iterative search into a loop that never
#'   ends. Requires the R.utils package; \code{NULL} or \code{Inf} disables
#'   it. Default 300.
#' @param n_cores Number of cores. Values above 1 run the splits on a PSOCK
#'   cluster (works on Windows). Default 1.
#' @param verbose Print progress. Default \code{TRUE}.
#'
#' @return An object of class \code{algorithm_stability}: a list with
#'   \code{summary} (means over successful splits, plus \code{meets_rate} and
#'   \code{success_rate}), \code{splits} (one row per split),
#'   \code{item_retention}, \code{reference} (the reference partition) and
#'   \code{call}.
#'
#' @references
#' MacCallum, R. C., Roznowski, M., & Necowitz, L. B. (1992). Model
#' modifications in covariance structure analysis: The problem of
#' capitalization on chance. \emph{Psychological Bulletin, 111}(3), 490-504.
#' \doi{10.1037/0033-2909.111.3.490}
#'
#' @seealso \code{\link{theory_recovery}}, \code{\link{item_stability}}.
#'
#' @examples
#' \donttest{
#' data(Data_Personality)
#' items  <- paste0("PPTQ", 1:15)
#' theory <- list(F1 = paste0("PPTQ", 1:5), F2 = paste0("PPTQ", 6:10),
#'                F3 = paste0("PPTQ", 11:15))
#' model  <- paste(sapply(names(theory), function(f)
#'             paste(f, "=~", paste(theory[[f]], collapse = " + "))), collapse = "\n")
#'
#' st <- algorithm_stability(
#'   Data_Personality[, items],
#'   algorithm = function(d) cfa_boosting(d, model = model, verbose = FALSE),
#'   n_splits = 5, theory = theory, estimator = "MLR", ordered = FALSE)
#' st
#' }
#' @export
algorithm_stability <- function(data, algorithm, n_splits = 30, theory = NULL,
                                reference = NULL, estimator = "WLSMV",
                                ordered = TRUE,
                                targets = c(cfi = 0.95, rmsea = 0.08, srmr = 0.08),
                                phi_max = 0.90, seed = 2026, n_cores = 1,
                                timeout = 300, verbose = TRUE) {
  if (!requireNamespace("lavaan", quietly = TRUE)) stop("Package 'lavaan' is required.")
  if (!is.function(algorithm)) stop("'algorithm' must be a function of the data.", call. = FALSE)
  stopifnot(is.data.frame(data), n_splits >= 1)
  cl <- match.call()
  algorithm <- .of_portable_function(algorithm)
  use_timeout <- !is.null(timeout) && is.finite(timeout) && timeout > 0
  if (use_timeout && !requireNamespace("R.utils", quietly = TRUE)) {
    warning("Package 'R.utils' is needed for 'timeout'; running without it.", call. = FALSE)
    use_timeout <- FALSE
  }
  targets <- utils::modifyList(list(cfi = 0.95, rmsea = 0.08, srmr = 0.08), as.list(targets))

  if (is.null(reference)) {
    if (verbose) .of_say("Reference: running the algorithm on the complete sample (N = %d)...", nrow(data))
    reference <- algorithm(data)
  }
  ref <- .of_as_partition(reference)$asig
  ref_items <- unlist(ref, use.names = FALSE)

  set.seed(seed)
  n_der  <- floor(nrow(data) / 2)
  splits <- lapply(seq_len(n_splits), function(i) sample.int(nrow(data), n_der))

  one_split <- function(idx) {
    der <- data[idx, , drop = FALSE]; val <- data[-idx, , drop = FALSE]
    run_alg <- function() {
      if (use_timeout)
        R.utils::withTimeout(suppressMessages(suppressWarnings(algorithm(der))),
                             timeout = timeout, onTimeout = "error")
      else suppressMessages(suppressWarnings(algorithm(der)))
    }
    res <- tryCatch(run_alg(),
                    error = function(e) NULL)
    if (is.null(res)) return(list(ok = FALSE, items = NULL))
    p <- tryCatch(.of_as_partition(res), error = function(e) NULL)
    if (is.null(p) || !length(p$asig)) return(list(ok = FALSE, items = NULL))
    asig <- p$asig[lengths(p$asig) > 0]
    if (is.null(names(asig)) || any(names(asig) == ""))
      names(asig) <- paste0("F", seq_along(asig))
    ev <- .of_eval_cfa(asig, p$covs, val, estimator, ordered)
    list(ok = !is.null(ev), asig = asig, ev = ev)
  }

  if (verbose) .of_say("Running %d splits (derivation n = %d, validation n = %d)...",
                       n_splits, n_der, nrow(data) - n_der)
  if (n_cores > 1) {
    clu <- .of_start_cluster(n_cores, n_splits, verbose)
    on.exit(parallel::stopCluster(clu), add = TRUE)
    out <- .of_cluster_lapply(clu, splits, one_split, verbose)
  } else {
    out <- .of_serial_lapply(splits, one_split, verbose)
  }

  rows <- lapply(seq_along(out), function(i) {
    o <- out[[i]]
    base <- data.frame(split = i, success = isTRUE(o$ok) && !is.null(o$ev))
    if (!base$success) return(base)
    got <- unlist(o$asig, use.names = FALSE)
    common <- intersect(got, ref_items)
    lab <- function(a, its) stats::setNames(rep(names(a), lengths(a)), unlist(a, use.names = FALSE))[its]
    ev <- o$ev
    base <- cbind(base, data.frame(
      k = length(o$asig), n_items = length(got),
      cfi = ev$cfi, tli = ev$tli, rmsea = ev$rmsea, srmr = ev$srmr, phi_max = ev$phi_max,
      jaccard = length(common) / length(union(got, ref_items)),
      ari_reference = .of_ari(lab(o$asig, common), lab(ref, common))))
    base$meets <- base$cfi >= targets$cfi && base$rmsea <= targets$rmsea &&
      base$srmr <= targets$srmr && (is.na(base$phi_max) || base$phi_max < phi_max)
    if (!is.null(theory)) {
      tr <- theory_recovery(theory, o$asig)$summary
      base$recovery <- tr$recovery; base$ari_theory <- tr$ari
    }
    base
  })
  cols <- unique(unlist(lapply(rows, names)))
  tab <- do.call(rbind, lapply(rows, function(r) { for (c in setdiff(cols, names(r))) r[[c]] <- NA; r[cols] }))

  all_items <- unique(c(names(data)[names(data) %in% c(ref_items, unlist(theory))], ref_items))
  kept <- lapply(out, function(o) if (isTRUE(o$ok)) unlist(o$asig, use.names = FALSE) else NULL)
  kept <- kept[!vapply(kept, is.null, logical(1))]
  item_retention <- data.frame(
    item = all_items,
    in_reference = all_items %in% ref_items,
    retention = if (length(kept)) vapply(all_items, function(it) mean(vapply(kept, function(k) it %in% k, logical(1))), numeric(1)) else NA_real_,
    row.names = NULL)

  ok <- tab[tab$success, , drop = FALSE]
  num <- setdiff(names(ok), c("split", "success", "meets"))
  # With no successful split there is nothing to average, but the summary must
  # still exist: a success rate of 0 is the finding, not an error.
  summary <- if (nrow(ok)) as.data.frame(lapply(ok[num], function(x) mean(x, na.rm = TRUE)))
             else data.frame(n_items = NA_real_)
  summary$meets_rate   <- if (nrow(ok)) mean(ok$meets) else NA_real_
  summary$success_rate <- mean(tab$success)

  out <- list(summary = summary, splits = tab, item_retention = item_retention,
              reference = ref, call = cl)
  class(out) <- c("algorithm_stability", "list")
  out
}

#' @export
print.algorithm_stability <- function(x, digits = 3, ...) {
  cat("Split-half stability of the item selection\n")
  cat(sprintf("Splits: %d (successful: %.0f%%)\n\n", nrow(x$splits),
              100 * x$summary$success_rate))
  cat("Means over successful splits (fit on the validation half):\n")
  print(round(x$summary, digits), row.names = FALSE)
  unstable <- x$item_retention[which(x$item_retention$retention > 0.2 &
                                     x$item_retention$retention < 0.8), ]
  if (nrow(unstable)) {
    cat("\nItems whose fate is not settled (retained in 20-80% of splits):\n")
    print(unstable[order(unstable$retention), ], row.names = FALSE, digits = 2)
  }
  invisible(x)
}

# CFA of a partition on new data, with the handful of numbers the stability
# summary needs. NULL when the model does not converge.
.of_eval_cfa <- function(asig, covs, data, estimator = "WLSMV", ordered = TRUE) {
  its <- unlist(asig, use.names = FALSE)
  if (length(asig) == 1L && length(its) < 3) return(NULL)
  syn <- paste(c(vapply(names(asig), function(f)
    paste(f, "=~", paste(asig[[f]], collapse = " + ")), character(1)), covs), collapse = "\n")
  fit <- tryCatch(suppressWarnings(lavaan::cfa(syn, data = data[, its, drop = FALSE],
                    ordered = if (isTRUE(ordered)) its else NULL,
                    estimator = estimator, std.lv = TRUE)),
                  error = function(e) NULL)
  if (is.null(fit) || !isTRUE(lavaan::lavInspect(fit, "converged"))) return(NULL)
  fm <- lavaan::fitMeasures(fit)
  pick <- function(a, b) if (!is.na(fm[a])) unname(fm[a]) else unname(fm[b])
  ss <- lavaan::standardizedSolution(fit)
  phi <- ss[ss$op == "~~" & ss$lhs != ss$rhs & ss$lhs %in% names(asig) & ss$rhs %in% names(asig), ]
  list(cfi = pick("cfi.scaled", "cfi"), tli = pick("tli.scaled", "tli"),
       rmsea = pick("rmsea.scaled", "rmsea"), srmr = unname(fm["srmr"]),
       phi_max = if (nrow(phi)) max(abs(phi$est.std)) else NA_real_)
}

# A function written at the console has the global environment as its
# enclosure, and a PSOCK worker does not receive the master's global
# environment: the objects it mentions (a model syntax, a theory list) would be
# missing on the workers. The objects it names are copied into a private
# environment whose parent is the package namespace, so the function travels
# with what it needs and still finds cfa_boosting() and friends unqualified.
.of_portable_function <- function(f) {
  if (!identical(environment(f), globalenv())) return(f)
  env <- new.env(parent = asNamespace("OptimalFactor"))
  for (v in intersect(all.names(body(f)), ls(globalenv(), all.names = TRUE)))
    assign(v, get(v, envir = globalenv()), envir = env)
  environment(f) <- env
  f
}
