#' Split-Half Cross-Validation of a Factor Model
#'
#' @description
#' Assesses the out-of-sample stability of a one-factor scale by repeated
#' split-half cross-validation. The sample is randomly split many times; on each
#' split the one-factor model is fitted and its fit and reliability are recorded
#' on independent subsamples. Two modes are available: (a) confirm a
#' \strong{fixed} item set (default), and (b) test the \strong{procedure} by
#' re-deriving a short form with \code{\link{redundancy_short_form}} in the
#' calibration half and confirming it in the validation half (set \code{derive_k}).
#' Mode (b) also returns how often each item is selected, exposing whether a
#' data-driven short form capitalizes on chance.
#'
#' @param data Data frame with the item responses.
#' @param items Character vector with the item names of the scale to validate
#'   (fixed mode) or the candidate pool (derivation mode).
#' @param n_splits Number of random splits. Default 200.
#' @param derive_k If \code{NULL} (default), the fixed \code{items} set is
#'   confirmed across holdout subsamples. If an integer, a \code{derive_k}-item
#'   short form is re-derived in each calibration half and confirmed in the
#'   validation half.
#' @param groups,min_per_group Passed to \code{\link{redundancy_short_form}} when
#'   \code{derive_k} is set (to preserve content coverage). Default \code{NULL}, 3.
#' @param estimator Estimator passed to \code{lavaan::cfa}. Default \code{"WLSMV"}.
#' @param targets Named numeric vector of pass thresholds for the fraction of
#'   subsamples meeting fit. Default \code{c(cfi = 0.95, rmsea = 0.08)}.
#' @param seed Random seed for reproducibility. Default 2026.
#'
#' @return A list. In fixed mode: \code{summary} (P10/P50/P90 of cfi, rmsea, srmr,
#'   omega across holdout subsamples) and \code{pct_meeting} (fraction meeting all
#'   \code{targets}). In derivation mode it adds \code{selection_freq} (per-item
#'   selection frequency) and the confirmation summary of the derived forms.
#'
#' @section References:
#' MacCallum, R. C., Roznowski, M., & Necowitz, L. B. (1992). Model modifications
#'   in covariance structure analysis: The problem of capitalization on chance.
#'   \emph{Psychological Bulletin, 111}(3), 490--504.
#'
#' @examples
#' \dontrun{
#'   # confirm a fixed 7-item scale out of sample
#'   cross_validate_cfa(mydata, short_items, n_splits = 200)
#'   # test the derivation procedure itself
#'   cross_validate_cfa(mydata, all_items, derive_k = 7, n_splits = 100,
#'                      groups = list(A = a_items, B = b_items))
#' }
#' @seealso \code{\link{redundancy_short_form}}
#' @export
cross_validate_cfa <- function(data, items, n_splits = 200, derive_k = NULL,
                               groups = NULL, min_per_group = 3,
                               estimator = "WLSMV",
                               targets = c(cfi = 0.95, rmsea = 0.08), seed = 2026) {
  if (!requireNamespace("lavaan", quietly = TRUE)) stop("Package 'lavaan' is required.")
  D <- data[stats::complete.cases(data[, items]), , drop = FALSE]
  N <- nrow(D)
  fit1 <- function(dat, its) tryCatch(lavaan::cfa(paste0("G =~ ", paste(its, collapse = " + ")),
                data = dat[, its], ordered = its, estimator = estimator, std.lv = TRUE),
                error = function(e) NULL)
  getidx <- function(fit) { if (is.null(fit)) return(c(cfi=NA,rmsea=NA,srmr=NA,omega=NA))
    cv <- tryCatch(lavaan::lavInspect(fit, "converged"), error = function(e) FALSE)
    if (!isTRUE(cv)) return(c(cfi=NA,rmsea=NA,srmr=NA,omega=NA))
    fm <- lavaan::fitMeasures(fit, c("cfi.scaled","rmsea.scaled","srmr"))
    s  <- lavaan::standardizedSolution(fit); l <- s$est.std[s$op == "=~"]
    c(cfi = fm[[1]], rmsea = fm[[2]], srmr = fm[[3]], omega = sum(l)^2/(sum(l)^2 + sum(1 - l^2))) }
  qtab <- function(M) { M <- M[stats::complete.cases(M), , drop = FALSE]
    round(apply(M, 2, function(x) stats::quantile(x, c(.1,.5,.9))), 3) }

  set.seed(seed)
  if (is.null(derive_k)) {
    draws <- matrix(NA_real_, 2 * n_splits, 4, dimnames = list(NULL, c("cfi","rmsea","srmr","omega")))
    for (r in seq_len(n_splits)) {
      idA <- sample(N, floor(N/2)); sp <- list(idA, setdiff(seq_len(N), idA))
      for (h in 1:2) draws[(r-1)*2 + h, ] <- getidx(fit1(D[sp[[h]], ], items))
    }
    dd <- draws[stats::complete.cases(draws), , drop = FALSE]
    pct <- mean(dd[,"cfi"] >= targets["cfi"] & dd[,"rmsea"] <= targets["rmsea"])
    cat(sprintf("Cross-validacion split-half de %d items en %d submuestras (n~%d)\n",
                length(items), nrow(dd), floor(N/2)))
    print(qtab(dd))
    cat(sprintf("%% submuestras que cumplen CFI>=%.2f y RMSEA<=%.2f: %.1f%%\n",
                targets["cfi"], targets["rmsea"], 100*pct))
    return(invisible(list(summary = qtab(dd), pct_meeting = pct, draws = dd)))
  } else {
    conf <- matrix(NA_real_, n_splits, 4, dimnames = list(NULL, c("cfi","rmsea","srmr","omega")))
    sel <- stats::setNames(rep(0, length(items)), items); ok <- 0
    for (r in seq_len(n_splits)) {
      idA <- sample(N, floor(N/2)); idB <- setdiff(seq_len(N), idA)
      sf <- tryCatch(redundancy_short_form(D[idA, ], items, k = derive_k, groups = groups,
              min_per_group = min_per_group, estimator = estimator), error = function(e) NULL)
      if (is.null(sf) || length(sf$items) != derive_k) next
      ok <- ok + 1; sel[sf$items] <- sel[sf$items] + 1
      conf[r, ] <- getidx(fit1(D[idB, ], sf$items))
    }
    cc <- conf[stats::complete.cases(conf), , drop = FALSE]
    pct <- mean(cc[,"cfi"] >= targets["cfi"] & cc[,"rmsea"] <= targets["rmsea"])
    cat(sprintf("Estabilidad del procedimiento: derivar %d items en calibracion y confirmar en validacion (%d derivaciones)\n",
                derive_k, ok))
    cat("Frecuencia de seleccion por item:\n"); print(round(sort(sel/ok, decreasing = TRUE), 2))
    cat("Ajuste de las formas derivadas, confirmadas fuera de muestra:\n"); print(qtab(cc))
    cat(sprintf("%% que cumplen CFI>=%.2f y RMSEA<=%.2f: %.1f%%\n",
                targets["cfi"], targets["rmsea"], 100*pct))
    return(invisible(list(summary = qtab(cc), pct_meeting = pct,
                          selection_freq = round(sel/ok, 3), n_valid = ok, draws = cc)))
  }
}
