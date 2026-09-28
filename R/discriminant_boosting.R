#' Discriminant-Boosting: Rescue the Discriminant Validity of a Scale
#'
#' Reespecifies a multidimensional scale whose interfactor correlations are so
#' high that the factors cannot be told apart --- or so high that the solution is
#' inadmissible, with a correlation above 1 --- using the smallest possible
#' departure from the theoretical structure.
#'
#' A model can fit well and still be uninterpretable. A four-factor solution
#' whose factors correlate .97 reproduces the covariance matrix as well as a
#' one-factor solution does, so CFI and RMSEA say nothing about whether the
#' factors exist. This routine targets the correlation itself.
#'
#' Three findings shape the algorithm:
#'
#' \itemize{
#'   \item \strong{Pruning by fit does not lower the interfactor correlation.}
#'     Removing the item that most improves CFI can leave \eqn{\phi} untouched or
#'     even raise it. The greedy search must be guided by \eqn{\phi}.
#'   \item \strong{Letting the data assign items surfaces method variance.} When
#'     item-to-factor assignment is taken from an exploratory solution, what
#'     separates is often the wording polarity of the items, not their content,
#'     and the algorithm reports a method factor as if it were a dimension. The
#'     assignment therefore stays \emph{theoretical}; only the grouping of whole
#'     factors and the set of retained items are searched.
#'   \item \strong{Optimising fit and \eqn{\phi} at once lets fit dominate.} They
#'     are optimised in sequence: first \eqn{\phi}, then CFI.
#' }
#'
#' @param data Data frame with the item responses.
#' @param theory Named list encoding the theoretical structure, of the form
#'   \code{list(FactorA = c("it1","it2",...), FactorB = c(...))}.
#' @param reverse_items Character vector of reverse-worded items, used to test
#'   whether they form a method factor. Default \code{NULL}.
#' @param phi_max Maximum acceptable interfactor correlation. Default \code{0.90}.
#' @param cfi_target,rmsea_target Fit targets. Defaults \code{0.95} and \code{0.08}.
#' @param omega_min Reliability floor per factor. Default \code{0.70}.
#' @param min_loading Loading floor. Default \code{0.35}.
#' @param min_items_per_factor Structural protection rule. Default \code{4}.
#' @param exclude_reverse One of \code{"auto"}, \code{"always"} or \code{"never"}.
#'   With \code{"auto"} (default) a method factor over \code{reverse_items} is
#'   fitted and, if it raises CFI by more than .02, those items are dropped
#'   before the search. Reverse wording introduces shared variance that does not
#'   belong to the construct (Marsh, 1996; Podsakoff et al., 2003).
#' @param try_method_factor,try_bifactor Whether to include the method-factor and
#'   the bifactor specifications in the ladder. Default \code{TRUE}.
#' @param estimator,ordered Passed to \code{\link[lavaan]{cfa}}. Defaults
#'   \code{"WLSMV"} and \code{TRUE}.
#' @param n_cores Cores for the candidate evaluations within each greedy
#'   iteration. Default \code{1}. Values above 1 use a PSOCK cluster.
#' @param verbose Logical; print progress. Default \code{TRUE}.
#'
#' @details
#' The search proceeds in four phases.
#'
#' \strong{Phase 0 --- diagnosis.} The theoretical model is fitted and its
#' interfactor correlations inspected. A correlation above 1 is reported as an
#' inadmissible solution, not as a large correlation: the latent covariance
#' matrix is no longer positive definite and the model describes something that
#' cannot exist.
#'
#' \strong{Phase 1 --- ladder of structures.} Candidates are generated ordered by
#' distance from the original: the theoretical model, the model plus a method
#' factor, the bifactor model, and every partition of the factors into
#' \eqn{k-1, k-2, \ldots, 2} blocks. Item-to-factor assignment never changes;
#' only whole factors are merged.
#'
#' \strong{Phases 2--4 --- pruning.} For each level of the ladder the best
#' candidate is pruned by \eqn{\phi}, then by CFI while holding \eqn{\phi} below
#' the threshold, and finally cleaned of items below the loading floor.
#'
#' The winner is the model that meets every criterion \emph{with the largest
#' number of factors}, not the one with the best fit: the aim is to depart as
#' little as possible from the intended structure. If none qualifies, the
#' admissible model with the lowest \eqn{\phi} is returned and \code{reason}
#' says so.
#'
#' Reliability floors are \emph{adaptive}: a model already below the floor is not
#' frozen, its effective bar becomes its current value, so removals that do not
#' worsen reliability remain available. A floor set above the starting value
#' would reject every candidate and stall the search on the first iteration.
#'
#' \emph{Warning.} Like any specification search this is an EXPLORATORY device
#' that capitalises on chance (MacCallum, Roznowski & Necowitz, 1992).
#' Cross-validate the chosen model on an independent sample; see
#' \code{\link{cross_validate_cfa}}.
#'
#' @return An object of class \code{discriminant_boosting}, a list with
#'   \code{best} (the retained model and its indices), \code{best_model},
#'   \code{reason}, \code{baseline}, \code{ladder} (one row per candidate),
#'   \code{refined}, \code{logs}, \code{all}, \code{reverse_excluded} and
#'   \code{config}.
#'
#' @references
#' Marsh, H. W. (1996). Positive and negative global self-esteem: A substantively
#' meaningful distinction or artifactors? \emph{Journal of Personality and Social
#' Psychology, 70}(4), 810-819.
#'
#' MacCallum, R. C., Roznowski, M., & Necowitz, L. B. (1992). Model modifications
#' in covariance structure analysis: The problem of capitalization on chance.
#' \emph{Psychological Bulletin, 111}(3), 490-504.
#'
#' Podsakoff, P. M., MacKenzie, S. B., Lee, J.-Y., & Podsakoff, N. P. (2003).
#' Common method biases in behavioral research. \emph{Journal of Applied
#' Psychology, 88}(5), 879-903.
#'
#' @seealso \code{\link{local_fit_search}}, \code{\link{specification_search_theory}},
#'   \code{\link{cross_validate_cfa}}
#'
#' @examples
#' \dontrun{
#'   theory <- list(Cognitive  = paste0("IT", 1:8),
#'                  Affective  = paste0("IT", 9:16),
#'                  Behavioral = paste0("IT", 17:24))
#'
#'   res <- discriminant_boosting(mydata, theory,
#'            reverse_items = c("IT5", "IT6", "IT11"),
#'            n_cores = parallel::detectCores() - 1)
#'   res
#'   res$ladder
#' }
#'
#' @export
discriminant_boosting <- function(data,
                                  theory,
                                  reverse_items = NULL,
                                  phi_max = 0.90,
                                  cfi_target = 0.95,
                                  rmsea_target = 0.08,
                                  omega_min = 0.70,
                                  min_loading = 0.35,
                                  min_items_per_factor = 4,
                                  exclude_reverse = c("auto", "always", "never"),
                                  try_method_factor = TRUE,
                                  try_bifactor = TRUE,
                                  estimator = "WLSMV",
                                  ordered = TRUE,
                                  n_cores = 1,
                                  verbose = TRUE) {

  if (!requireNamespace("lavaan", quietly = TRUE)) stop("lavaan is required")
  if (!requireNamespace("semTools", quietly = TRUE))
    stop("semTools is required for the reliability floor")
  exclude_reverse <- match.arg(exclude_reverse)

  say <- function(...) if (verbose) cat(...)

  syntax_of <- function(asig, extra = NULL) {
    base <- paste(vapply(names(asig),
                         function(f) paste0(f, " =~ ", paste(asig[[f]], collapse = " + ")),
                         character(1)), collapse = "\n")
    if (is.null(extra)) base else paste(base, extra, sep = "\n")
  }

  evaluate <- function(asig, extra = NULL, dat = data) {
    if (any(lengths(asig) < 3)) return(NULL)
    # lavaan's warnings (e.g. a latent covariance matrix that is not positive
    # definite) are the very thing to diagnose here, not a reason to discard
    # the fit, so they are suppressed rather than trapped as failures.
    fit <- tryCatch(suppressWarnings(
                      lavaan::cfa(syntax_of(asig, extra), data = dat,
                                  estimator = estimator, ordered = ordered,
                                  std.lv = TRUE)),
                    error = function(e) NULL)
    if (is.null(fit) || !isTRUE(lavaan::lavInspect(fit, "converged"))) return(NULL)

    f <- lavaan::fitMeasures(fit, c("chisq.scaled", "df.scaled", "cfi.scaled",
                                    "tli.scaled", "rmsea.scaled", "srmr"))
    if (any(is.na(f))) return(NULL)

    std <- lavaan::standardizedSolution(fit)
    lv  <- names(asig)
    phi <- std[std$op == "~~" & std$lhs != std$rhs &
               std$lhs %in% lv & std$rhs %in% lv & std$est.std != 0, ]
    om <- tryCatch(vapply(semTools::compRelSEM(fit), as.numeric, numeric(1)),
                   error = function(e) NA_real_)
    lam <- std[std$op == "=~" & std$lhs %in% lv, ]

    i <- if (nrow(phi)) which.max(abs(phi$est.std)) else NA_integer_
    list(fit = fit, asig = asig, extra = extra,
         n_factors = length(asig), n_items = length(unlist(asig)),
         chisq = f[["chisq.scaled"]], df = f[["df.scaled"]],
         cfi = f[["cfi.scaled"]], tli = f[["tli.scaled"]],
         rmsea = f[["rmsea.scaled"]], srmr = f[["srmr"]],
         phi = if (nrow(phi)) phi$est.std[i] else NA_real_,
         phi_lo = if (nrow(phi)) phi$ci.lower[i] else NA_real_,
         phi_hi = if (nrow(phi)) phi$ci.upper[i] else NA_real_,
         n_phi_over = if (nrow(phi)) sum(abs(phi$est.std) >= phi_max) else NA_integer_,
         admissible = if (nrow(phi)) all(abs(phi$est.std) <= 1) else TRUE,
         omega = om, omega_min = suppressWarnings(min(om, na.rm = TRUE)),
         min_loading = min(abs(lam$est.std)), loadings = lam)
  }

  meets <- function(m) {
    if (is.null(m)) return(FALSE)
    ok_phi <- is.na(m$phi) || (m$phi < phi_max && m$admissible)
    ok_phi && m$cfi >= cfi_target && m$rmsea <= rmsea_target &&
      m$omega_min >= omega_min && m$min_loading >= min_loading
  }

  row_of <- function(m, label) {
    # A model that did not converge still needs every column: with only two,
    # rbind() against the converged rows fails and the whole run is lost.
    if (is.null(m)) return(data.frame(model = label, converged = FALSE, n_factors = NA_integer_,
                                      n_items = NA_integer_, cfi = NA_real_, tli = NA_real_,
                                      rmsea = NA_real_, srmr = NA_real_, phi = NA_real_,
                                      phi_hi = NA_real_, phi_over = NA_integer_,
                                      omega_min = NA_real_, min_loading = NA_real_,
                                      admissible = NA, meets = FALSE,
                                      stringsAsFactors = FALSE, row.names = NULL))
    data.frame(model = label, converged = TRUE, n_factors = m$n_factors,
               n_items = m$n_items, cfi = round(m$cfi, 3), tli = round(m$tli, 3),
               rmsea = round(m$rmsea, 3), srmr = round(m$srmr, 3),
               phi = round(m$phi, 3), phi_hi = round(m$phi_hi, 3),
               phi_over = m$n_phi_over, omega_min = round(m$omega_min, 3),
               min_loading = round(m$min_loading, 3),
               admissible = m$admissible, meets = meets(m),
               stringsAsFactors = FALSE, row.names = NULL)
  }

  # Set partitions of the factor names into m blocks
  partitions_of <- function(x, m) {
    if (m == 1L) return(list(list(x)))
    if (m == length(x)) return(list(as.list(x)))
    first <- x[1]; rest <- x[-1]
    out <- list()
    for (p in partitions_of(rest, m - 1L)) out <- c(out, list(c(list(first), p)))
    for (p in partitions_of(rest, m)) {
      for (i in seq_along(p)) { q <- p; q[[i]] <- c(first, q[[i]]); out <- c(out, list(q)) }
    }
    out
  }

  merge_theory <- function(theory, blocks) {
    asig <- lapply(blocks, function(b) unlist(theory[b], use.names = FALSE))
    names(asig) <- vapply(blocks, function(b)
      paste(substr(b, 1, 8), collapse = "_"), character(1))
    asig
  }

  # Greedy pruning. objective = "phi" minimises the interfactor correlation;
  # objective = "cfi" maximises fit while holding phi below the threshold.
  prune <- function(m, objective, cl = NULL) {
    log <- data.frame(step = 0L, dropped = NA_character_, n_items = m$n_items,
                      phi = round(m$phi, 3), cfi = round(m$cfi, 3),
                      omega_min = round(m$omega_min, 3), stringsAsFactors = FALSE)
    step <- 0L
    repeat {
      done <- if (objective == "phi") !is.na(m$phi) && m$phi < phi_max else m$cfi >= cfi_target
      if (done) break

      cand <- unlist(lapply(names(m$asig), function(f)
        if (length(m$asig[[f]]) > min_items_per_factor) m$asig[[f]] else character(0)))
      if (!length(cand)) break

      try_drop <- function(it) {
        mm <- evaluate(lapply(m$asig, function(x) setdiff(x, it)), m$extra)
        if (is.null(mm)) return(NULL)
        data.frame(item = it, phi = mm$phi, cfi = mm$cfi,
                   omega_min = mm$omega_min, stringsAsFactors = FALSE)
      }
      res <- if (is.null(cl)) lapply(cand, try_drop) else parallel::parLapply(cl, cand, try_drop)
      res <- do.call(rbind, Filter(Negate(is.null), res))
      if (is.null(res)) break

      ok <- res[res$omega_min >= omega_min, , drop = FALSE]
      if (objective == "phi") {
        # Demanding cfi_target at every step would stall the search whenever the
        # starting model sits below it; requiring that fit does not worsen is
        # enough.
        ok <- ok[ok$cfi >= m$cfi - 0.005, , drop = FALSE]
        ok <- ok[order(ok$phi), , drop = FALSE]
        improves <- nrow(ok) > 0 && ok$phi[1] < m$phi
      } else {
        ok <- ok[is.na(ok$phi) | ok$phi < phi_max, , drop = FALSE]
        ok <- ok[order(-ok$cfi), , drop = FALSE]
        improves <- nrow(ok) > 0 && ok$cfi[1] > m$cfi
      }
      if (!improves) break

      step <- step + 1L
      m <- evaluate(lapply(m$asig, function(x) setdiff(x, ok$item[1])), m$extra)
      log <- rbind(log, data.frame(step = step, dropped = ok$item[1],
                                   n_items = m$n_items, phi = round(m$phi, 3),
                                   cfi = round(m$cfi, 3),
                                   omega_min = round(m$omega_min, 3),
                                   stringsAsFactors = FALSE))
      say(sprintf("      step %2d: dropped %-8s n=%2d  phi=%.3f  CFI=%.3f\n",
                  step, ok$item[1], m$n_items, m$phi, m$cfi))
    }
    list(m = m, log = log)
  }

  clean_loadings <- function(m) {
    log <- NULL
    repeat {
      if (m$min_loading >= min_loading) break
      weak <- m$loadings$rhs[which.min(abs(m$loadings$est.std))]
      owner <- which(vapply(m$asig, function(x) weak %in% x, logical(1)))
      if (length(m$asig[[owner]]) <= min_items_per_factor) break
      mm <- evaluate(lapply(m$asig, function(x) setdiff(x, weak)), m$extra)
      if (is.null(mm) || mm$omega_min < omega_min) break
      log <- rbind(log, data.frame(dropped = weak,
                                   loading = round(min(abs(m$loadings$est.std)), 3),
                                   cfi_after = round(mm$cfi, 3),
                                   phi_after = round(mm$phi, 3),
                                   stringsAsFactors = FALSE))
      say(sprintf("      low loading: dropped %-8s (%.3f) -> CFI=%.3f  phi=%.3f\n",
                  weak, min(abs(m$loadings$est.std)), mm$cfi, mm$phi))
      m <- mm
    }
    list(m = m, log = log)
  }

  # ---- Phase 0: diagnosis ----------------------------------------
  say("\n== Phase 0: theoretical model ==\n")
  base <- evaluate(theory)
  if (is.null(base)) stop("The theoretical model does not converge; check data and assignment.")
  say(sprintf("   %d factors, %d items | max phi = %.3f | CFI = %.3f | admissible = %s\n",
              base$n_factors, base$n_items, base$phi, base$cfi, base$admissible))
  if (!is.na(base$phi) && base$phi > 1)
    say("   [!] Correlation above 1: inadmissible solution\n")

  drop_reverse <- FALSE
  if (!is.null(reverse_items)) {
    drop_reverse <- switch(exclude_reverse,
      always = TRUE, never = FALSE,
      auto = {
        met <- evaluate(theory,
                        extra = paste(c(paste0("METHOD =~ ", paste(reverse_items, collapse = " + ")),
                                        paste0("METHOD ~~ 0*", names(theory))), collapse = "\n"))
        wins <- !is.null(met) && (met$cfi - base$cfi) > 0.02
        if (wins) say(sprintf("   Method factor over reverse items: CFI %.3f -> %.3f, they are excluded\n",
                              base$cfi, met$cfi))
        wins
      })
  }
  if (drop_reverse) {
    theory <- lapply(theory, function(x) setdiff(x, reverse_items))
    base   <- evaluate(theory)
    say(sprintf("   Without reverse items: %d items | phi = %.3f | CFI = %.3f\n",
                base$n_items, base$phi, base$cfi))
  }

  # ---- Phase 1: ladder of structures -----------------------------
  say("\n== Phase 1: ladder of structures ==\n")
  k <- length(theory)
  candidates <- list("theoretical" = list(asig = theory, extra = NULL))

  if (try_method_factor && !is.null(reverse_items) && !drop_reverse)
    candidates[["theoretical + method"]] <- list(
      asig = theory,
      extra = paste(c(paste0("METHOD =~ ", paste(reverse_items, collapse = " + ")),
                      paste0("METHOD ~~ 0*", names(theory))), collapse = "\n"))

  if (try_bifactor && k >= 2)
    candidates[["bifactor"]] <- list(
      asig = theory,
      extra = paste(c(paste0("G =~ ", paste(unlist(theory), collapse = " + ")),
                      paste0("G ~~ 0*", names(theory)),
                      apply(utils::combn(names(theory), 2), 2,
                            function(p) paste0(p[1], " ~~ 0*", p[2]))), collapse = "\n"))

  if (k > 2) for (m_blocks in seq(k - 1, 2)) {
    for (blocks in partitions_of(names(theory), m_blocks)) {
      asig <- merge_theory(theory, blocks)
      candidates[[paste0(m_blocks, "F: ", paste(names(asig), collapse = " | "))]] <-
        list(asig = asig, extra = NULL)
    }
  }

  ladder <- do.call(rbind, lapply(names(candidates), function(nm) {
    m <- evaluate(candidates[[nm]]$asig, candidates[[nm]]$extra)
    row_of(m, nm)
  }))
  ladder <- ladder[order(-ladder$n_factors, ladder$phi), ]
  if (verbose) print(ladder, row.names = FALSE)

  # ---- Phases 2-4: pruning per level ------------------------------
  cl <- NULL
  if (n_cores > 1 && requireNamespace("parallel", quietly = TRUE)) {
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterEvalQ(cl, {library(lavaan); library(semTools)})
    parallel::clusterExport(cl, c("data", "evaluate", "syntax_of", "estimator",
                                  "ordered", "phi_max"), envir = environment())
  }

  say("\n== Phases 2-4: pruning per level ==\n")
  levels_k <- sort(unique(ladder$n_factors[ladder$converged]), decreasing = TRUE)
  refined <- list()

  for (nf in levels_k) {
    sub <- ladder[ladder$converged & ladder$n_factors == nf, , drop = FALSE]
    sub <- sub[order(sub$phi, na.last = TRUE), , drop = FALSE]
    nm  <- sub$model[1]
    say(sprintf("\n   [%d factors] candidate: %s\n", nf, nm))

    m <- evaluate(candidates[[nm]]$asig, candidates[[nm]]$extra)
    if (is.null(m)) next

    p1 <- if (!is.na(m$phi)) prune(m, "phi", cl) else list(m = m, log = NULL)
    p2 <- prune(p1$m, "cfi", cl)
    p3 <- clean_loadings(p2$m)

    refined[[nm]] <- list(m = p3$m, base_model = nm,
                          log_phi = p1$log, log_cfi = p2$log, log_load = p3$log)
    say(sprintf("      -> n=%d | phi=%.3f | CFI=%.3f | omega_min=%.3f | min_loading=%.3f | meets=%s\n",
                p3$m$n_items, p3$m$phi, p3$m$cfi, p3$m$omega_min,
                p3$m$min_loading, meets(p3$m)))
  }

  # ---- Selection: the qualifying model with MOST factors ----------
  table_ref <- do.call(rbind, lapply(names(refined),
                                     function(nm) row_of(refined[[nm]]$m, paste(nm, "[pruned]"))))
  eligible <- table_ref[table_ref$meets, , drop = FALSE]
  if (nrow(eligible)) {
    eligible <- eligible[order(-eligible$n_factors, eligible$phi), ]
    winner <- names(refined)[which(paste(names(refined), "[pruned]") == eligible$model[1])]
    reason <- "meets every criterion with the largest number of factors"
  } else {
    adm <- table_ref[table_ref$converged & table_ref$admissible, , drop = FALSE]
    adm <- adm[order(adm$phi), , drop = FALSE]
    winner <- if (nrow(adm))
      names(refined)[which(paste(names(refined), "[pruned]") == adm$model[1])] else names(refined)[1]
    reason <- "no model qualifies: the admissible one with the lowest phi is returned"
  }

  say(sprintf("\n== Result: %s (%s) ==\n", winner, reason))

  out <- list(best = refined[[winner]]$m,
              best_model = winner,
              reason = reason,
              baseline = base,
              ladder = ladder,
              refined = table_ref,
              logs = refined[[winner]][c("log_phi", "log_cfi", "log_load")],
              all = refined,
              reverse_excluded = drop_reverse,
              config = list(phi_max = phi_max, cfi_target = cfi_target,
                            rmsea_target = rmsea_target, omega_min = omega_min,
                            min_loading = min_loading,
                            min_items_per_factor = min_items_per_factor))
  class(out) <- c("discriminant_boosting", "list")
  out
}


#' Print method for discriminant_boosting
#'
#' @param x Output from \code{\link{discriminant_boosting}}.
#' @param ... Ignored.
#' @return Invisibly returns \code{x}; called for its side effects.
#' @export
print.discriminant_boosting <- function(x, ...) {
  m <- x$best
  cat("\nDiscriminant Boosting\n")
  cat("---------------------------------------------------------------\n")
  cat("Theoretical model:", x$baseline$n_factors, "factors,", x$baseline$n_items, "items |",
      sprintf("phi = %.3f | CFI = %.3f%s\n", x$baseline$phi, x$baseline$cfi,
              if (!x$baseline$admissible) "  [INADMISSIBLE]" else ""))
  if (isTRUE(x$reverse_excluded))
    cat("Reverse-worded items excluded (they formed a method factor)\n")
  cat("\nSolution:", x$best_model, "\n")
  cat(sprintf("  %d factors | %d items\n", m$n_factors, m$n_items))
  cat(sprintf("  chi2(%d) = %.1f | CFI = %.3f | TLI = %.3f | RMSEA = %.3f | SRMR = %.3f\n",
              m$df, m$chisq, m$cfi, m$tli, m$rmsea, m$srmr))
  if (!is.na(m$phi))
    cat(sprintf("  phi = %.3f [%.3f, %.3f]%s\n", m$phi, m$phi_lo, m$phi_hi,
                if (m$phi < x$config$phi_max) "  OK" else "  (above the threshold)"))
  cat("  omega:", paste(sprintf("%s = %.3f", names(m$omega), m$omega), collapse = " | "), "\n")
  cat(sprintf("  minimum loading = %.3f\n", m$min_loading))
  cat("\nComposition:\n")
  for (f in names(m$asig)) cat("  ", f, ":", paste(m$asig[[f]], collapse = ", "), "\n")
  cat("\nReason:", x$reason, "\n")
  cat("\nExploratory search: cross-validate on an independent sample.\n")
  invisible(x)
}
