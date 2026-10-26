#' Resampling Stability of the Item Assignment in a Confirmatory Model
#'
#' @description
#' Checks, without leaving the confirmatory framework, whether each item of a
#' CFA model would stay on its factor in another sample. The model is refitted
#' on many subsamples and, one item at a time, the item is moved to each of
#' the other factors. The item "lands" on the factor whose model fits best
#' (lowest chi-square). The proportion of subsamples in which it lands on the
#' factor the model assigns is its stability.
#'
#' Unlike \code{\link{item_stability}}, which reruns \code{efa_boosting()} and
#' therefore extracts factors on every resample, no exploratory step is
#' involved: the factors are always those of the model, and only the place of
#' one item changes at a time. This makes it suitable for checking a
#' respecified CFA model, such as one in which some items were moved between
#' factors, against the classic objection that the respecification
#' capitalizes on chance (MacCallum, Roznowski & Necowitz, 1992).
#'
#' @details
#' On each subsample the function fits:
#' \enumerate{
#'   \item the model as specified, recording its fit and the largest
#'     correlation between factors; and
#'   \item for each item, one model per alternative factor, identical to the
#'     specified one except that the item loads on that factor instead of its
#'     own.
#' }
#' The item is moved, not duplicated, so every alternative has the same
#' degrees of freedom as the specified model and their chi-squares (the
#' unscaled test statistic, from the same subsample) compare directly. The
#' item keeps its factor unless an alternative has a lower chi-square. Freeing
#' a cross-loading instead would be fragile: with highly correlated factors it
#' leaves the item's loadings nearly unidentified and yields inadmissible
#' estimates. An alternative that would leave a factor with fewer than two
#' items is not considered.
#' Subsampling is without replacement, because duplicated cases distort the
#' polychoric correlations that WLSMV relies on (see \code{\link{item_stability}}).
#' The subsamples are drawn before any fitting, so results are reproducible
#' with \code{seed} and identical for any \code{n_cores}.
#'
#' Each item is read as \code{"stable"} when its stability is at least
#' \code{threshold}, as \code{"prefers other factor"} when it is at most
#' \code{1 - threshold}, and as \code{"unstable"} in between.
#'
#' @param data Data frame with the item responses. Columns not used by the
#'   model are ignored.
#' @param model The model, either as a named list (factor name -> character
#'   vector of items) or as lavaan syntax with \code{=~} lines only. At least
#'   two factors are required, and every item must load on one factor only.
#' @param R Number of subsamples. Default 200.
#' @param prop Proportion of cases drawn in each subsample, without
#'   replacement. Default 0.8.
#' @param threshold Stability from which an item is read as stable. Default
#'   0.75.
#' @param estimator Estimator passed to \code{lavaan::cfa()}. Default
#'   \code{"WLSMV"}.
#' @param ordered Treat the items as ordinal. Default \code{TRUE}.
#' @param seed Optional integer seed for reproducibility; NULL (default)
#'   leaves the RNG untouched.
#' @param n_cores Number of parallel workers. Default 1 (sequential). Values
#'   above 1 use a PSOCK cluster and require the package to be installed.
#' @param verbose Print progress. Default \code{TRUE}.
#'
#' @return An object of class \code{cfa_item_stability}: a list with
#'   \describe{
#'     \item{\code{items}}{one row per item: \code{item}, \code{factor} (the
#'       assigned factor), \code{stability}, \code{other} (the factor it lands
#'       on most often when it leaves its own), \code{lambda_own} (median
#'       standardized loading on its factor), \code{lambda_moved} (median
#'       loading when moved to its best alternative), \code{delta_chisq}
#'       (median chi-square of the best alternative minus that of the model:
#'       positive values favour the assigned factor), \code{n_valid} and
#'       \code{reading}.}
#'     \item{\code{fit}}{one row per subsample with the fit of the model as
#'       specified (\code{cfi}, \code{tli}, \code{rmsea}, \code{srmr}) and
#'       \code{phi_max}, the largest correlation between factors.}
#'     \item{\code{summary}}{success rate, median \code{phi_max} and the
#'       proportion of subsamples with \code{phi_max} below .90.}
#'     \item{\code{model}, \code{settings}, \code{call}}{the model as a named
#'       list, the settings used and the call.}
#'   }
#'
#' @references
#' MacCallum, R. C., Roznowski, M., & Necowitz, L. B. (1992). Model
#' modifications in covariance structure analysis: The problem of
#' capitalization on chance. \emph{Psychological Bulletin, 111}(3), 490-504.
#' \doi{10.1037/0033-2909.111.3.490}
#'
#' @seealso \code{\link{item_stability}}, \code{\link{algorithm_stability}},
#'   \code{\link{cross_validate_cfa}}.
#'
#' @examples
#' \donttest{
#' data(Data_Expectativas)
#' model <- list(F1 = paste0("EAF", 1:5), F2 = paste0("EAF", 6:10))
#' st <- cfa_item_stability(Data_Expectativas, model, R = 5, seed = 1)
#' st
#' st$items
#' }
#' @export
cfa_item_stability <- function(data, model, R = 200, prop = 0.8,
                               threshold = 0.75, estimator = "WLSMV",
                               ordered = TRUE, seed = NULL, n_cores = 1,
                               verbose = TRUE) {
  if (!requireNamespace("lavaan", quietly = TRUE)) stop("Package 'lavaan' is required.")
  cl <- match.call()
  model <- .of_model_list(model)
  if (length(model) < 2)
    stop("'model' must have at least two factors.", call. = FALSE)
  items <- unlist(model, use.names = FALSE)
  if (anyDuplicated(items))
    stop("Every item must load on one factor only: ",
         paste(unique(items[duplicated(items)]), collapse = ", "), call. = FALSE)
  miss <- setdiff(items, names(data))
  if (length(miss))
    stop("Items not found in 'data': ", paste(miss, collapse = ", "), call. = FALSE)
  if (R < 2) stop("'R' must be at least 2.", call. = FALSE)
  if (!is.numeric(prop) || prop <= 0 || prop > 1)
    stop("'prop' must be in (0, 1].", call. = FALSE)
  if (!is.numeric(threshold) || threshold <= 0.5 || threshold > 1)
    stop("'threshold' must be in (0.5, 1].", call. = FALSE)

  dat <- as.data.frame(data)[, items, drop = FALSE]
  dat <- dat[stats::complete.cases(dat), , drop = FALSE]
  N <- nrow(dat)
  n_draw <- max(2L, floor(prop * N))
  if (!is.null(seed)) set.seed(seed)
  draws <- lapply(seq_len(R), function(r) sample.int(N, n_draw))

  worker <- .of_make_cfa_stab_worker(dat, model, estimator, ordered)
  if (n_cores > 1 && requireNamespace("parallel", quietly = TRUE)) {
    clu <- .of_start_cluster(n_cores, n_tasks = R, verbose = verbose)
    on.exit(parallel::stopCluster(clu), add = TRUE)
    if (verbose) .of_say("Fitting %d subsamples on %d cores...", R, length(clu))
    res <- .of_cluster_lapply(clu, draws, worker, verbose = verbose)
  } else {
    if (verbose) .of_say("Fitting %d subsamples sequentially...", R)
    res <- .of_serial_lapply(draws, worker, verbose = verbose)
  }

  fit <- do.call(rbind, lapply(res, function(r)
    if (is.null(r)) data.frame(cfi = NA, tli = NA, rmsea = NA, srmr = NA, phi_max = NA)
    else r$fit))
  rownames(fit) <- NULL
  grab <- function(what, na) matrix(sapply(res, function(r)
    if (is.null(r)) rep(na, length(items)) else unname(r[[what]])), nrow = length(items))
  own <- grab("own", NA_real_); moved <- grab("moved", NA_real_)
  delta <- grab("delta", NA_real_); land <- grab("land", NA_character_)
  assigned <- rep(names(model), lengths(model))

  stab <- vapply(seq_along(items), function(i) {
    v <- land[i, ]; v <- v[!is.na(v)]
    if (!length(v)) NA_real_ else mean(v == assigned[i])
  }, numeric(1))
  other <- vapply(seq_along(items), function(i) {
    v <- land[i, ]; v <- v[!is.na(v) & v != assigned[i]]
    if (!length(v)) NA_character_ else names(sort(table(v), decreasing = TRUE))[1]
  }, character(1))
  reading <- ifelse(is.na(stab), NA_character_,
                    ifelse(stab >= threshold, "stable",
                           ifelse(stab <= 1 - threshold, "prefers other factor", "unstable")))
  med <- function(m) apply(m, 1, function(x) if (all(is.na(x))) NA_real_ else stats::median(x, na.rm = TRUE))

  out <- list(
    items = data.frame(item = items, factor = assigned, stability = round(stab, 3),
                       other = other, lambda_own = round(med(own), 3),
                       lambda_moved = round(med(moved), 3),
                       delta_chisq = round(med(delta), 2),
                       n_valid = rowSums(!is.na(land)), reading = reading,
                       stringsAsFactors = FALSE, row.names = NULL),
    fit = fit,
    summary = data.frame(success_rate = mean(!vapply(res, is.null, logical(1))),
                         phi_max_median = stats::median(fit$phi_max, na.rm = TRUE),
                         phi_below_90 = mean(fit$phi_max < 0.90, na.rm = TRUE)),
    model = model,
    settings = list(R = R, prop = prop, n = N, n_subsample = n_draw,
                    threshold = threshold, estimator = estimator, ordered = ordered,
                    seed = seed),
    call = cl
  )
  class(out) <- "cfa_item_stability"
  out
}

# A model given as lavaan syntax is reduced to factor -> items. Only =~ lines
# are read; anything else (covariances, constraints) is ignored here.
.of_model_list <- function(model) {
  if (is.list(model)) {
    if (is.null(names(model)) || any(names(model) == ""))
      stop("'model' must be a named list (factor -> items).", call. = FALSE)
    return(lapply(model, as.character))
  }
  if (!is.character(model))
    stop("'model' must be a named list or lavaan syntax.", call. = FALSE)
  pt <- lavaan::lavaanify(paste(model, collapse = "\n"))
  pt <- pt[pt$op == "=~", ]
  if (!nrow(pt)) stop("No '=~' lines found in 'model'.", call. = FALSE)
  split(pt$rhs, factor(pt$lhs, levels = unique(pt$lhs)))
}

# Fitting closure for one subsample. The data travel inside the closure, as in
# item_stability(); only lavaan is called, through its namespace, so the same
# function runs locally and on PSOCK workers.
.of_make_cfa_stab_worker <- function(dat, model, estimator, ordered) {
  force(dat); force(model); force(estimator); force(ordered)
  items <- unlist(model, use.names = FALSE)
  assigned <- rep(names(model), lengths(model))
  base <- paste(sprintf("%s =~ %s", names(model),
                        vapply(model, paste, character(1), collapse = " + ")),
                collapse = "\n")
  syntax_of <- function(m) paste(sprintf("%s =~ %s", names(m),
                                         vapply(m, paste, character(1), collapse = " + ")),
                                 collapse = "\n")
  function(idx) {
    x <- dat[idx, , drop = FALSE]
    fit_one <- function(syntax) {
      f <- tryCatch(suppressWarnings(
        lavaan::cfa(syntax, data = x, estimator = estimator, std.lv = TRUE,
                    ordered = if (isTRUE(ordered)) items else NULL)),
        error = function(e) NULL)
      if (is.null(f) || !isTRUE(lavaan::lavInspect(f, "converged"))) NULL else f
    }
    load_of <- function(f, item) {
      s <- lavaan::standardizedSolution(f)
      s$est.std[s$op == "=~" & s$rhs == item][1]
    }
    fb <- fit_one(base)
    if (is.null(fb)) return(NULL)
    fm <- lavaan::fitMeasures(fb)
    pick <- function(a, b) if (!is.na(fm[a])) unname(fm[a]) else unname(fm[b])
    phi <- lavaan::lavInspect(fb, "cor.lv")
    fit <- data.frame(cfi = pick("cfi.scaled", "cfi"), tli = pick("tli.scaled", "tli"),
                      rmsea = pick("rmsea.scaled", "rmsea"), srmr = unname(fm["srmr"]),
                      phi_max = max(abs(phi[lower.tri(phi)])))
    chi_base <- unname(fm["chisq"])
    own <- vapply(items, function(it) load_of(fb, it), numeric(1))
    moved <- delta <- rep(NA_real_, length(items)); land <- rep(NA_character_, length(items))
    for (i in seq_along(items)) {
      # The item is moved, not duplicated: every alternative has the same
      # degrees of freedom as the model, so their chi-squares compare directly.
      alt <- lapply(setdiff(names(model), assigned[i]), function(fa) {
        m <- lapply(model, setdiff, items[i])
        m[[fa]] <- c(m[[fa]], items[i])
        if (any(lengths(m) < 2)) return(NULL)
        f <- fit_one(syntax_of(m))
        if (is.null(f)) NULL else list(factor = fa, chisq = unname(lavaan::fitMeasures(f, "chisq")),
                                       loading = load_of(f, items[i]))
      })
      alt <- Filter(Negate(is.null), alt)
      if (!length(alt)) next
      best <- alt[[which.min(vapply(alt, `[[`, numeric(1), "chisq"))]]
      land[i]  <- if (best$chisq < chi_base) best$factor else assigned[i]
      moved[i] <- best$loading
      delta[i] <- best$chisq - chi_base
    }
    list(fit = fit, own = own, moved = moved, delta = delta, land = land)
  }
}

#' @param x A \code{cfa_item_stability} object.
#' @param digits Number of digits for the printed values. Default 3.
#' @param ... Ignored.
#' @rdname cfa_item_stability
#' @export
print.cfa_item_stability <- function(x, digits = 3, ...) {
  s <- x$settings
  cat("\nCFA item-assignment stability\n")
  cat(strrep("-", 60), "\n", sep = "")
  cat(sprintf("Subsamples: %d of n = %d (%.0f%% of %d), %.0f%% converged\n",
              s$R, s$n_subsample, 100 * s$prop, s$n, 100 * x$summary$success_rate))
  cat(sprintf("Largest interfactor correlation: median %.3f; below .90 in %.1f%% of subsamples\n",
              x$summary$phi_max_median, 100 * x$summary$phi_below_90))
  cat(sprintf("\nProportion of subsamples in which each item fits best on its own factor (stable >= %.2f):\n",
              s$threshold))
  print(x$items, digits = digits, row.names = FALSE)
  flag <- x$items$item[!is.na(x$items$reading) & x$items$reading != "stable"]
  if (length(flag))
    cat("\nItems to review: ", paste(flag, collapse = ", "), "\n", sep = "")
  invisible(x)
}
