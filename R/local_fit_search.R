#' Local-Fit Search for a Unidimensional Model
#'
#' Searches for a specification of a one-factor model that reaches the fit
#' targets, combining two actions --- dropping items and freeing residual
#' covariances --- and then cleaning up the result.
#'
#' Applies to the scale that is essentially unidimensional but whose one-factor
#' model misfits. Where \code{\link{redundancy_short_form}} attacks a single
#' cause (local dependence between near-duplicate items) by pruning, this routine
#' also considers keeping the pair and modelling its residual covariance, and
#' decides between the two by their effect on fit.
#'
#' @param data Data frame with the item responses.
#' @param items Character vector of candidate item names.
#' @param factor_name Name given to the latent variable. Default \code{"F"}.
#' @param item_text Optional data frame with columns \code{Item} and \code{Texto}
#'   (or \code{Text}); used only to print the wording of the items involved in
#'   each retained covariance, so that its substantive plausibility can be judged.
#' @param rmsea_target,cfi_target,srmr_target Fit targets. Defaults \code{0.08},
#'   \code{0.95} and \code{0.08}.
#' @param omega_min Reliability floor. Default \code{0.70}.
#' @param min_loading Loading floor. Default \code{0.35}.
#' @param min_items Minimum number of retained items. Default \code{5}.
#' @param max_ec Maximum number of residual covariances. Default \code{4}.
#' @param mi_min Minimum modification index for a covariance to be considered.
#'   Default \code{5}.
#' @param start_ec Character vector of residual covariances to start from, in
#'   lavaan syntax (e.g. \code{"IT1 ~~ IT2"}). Default none.
#' @param estimator,ordered Passed to \code{\link[lavaan]{cfa}}.
#' @param verbose Logical; print progress. Default \code{TRUE}.
#'
#' @details
#' Four phases, three of which encode decisions usually taken by hand.
#'
#' \strong{Phase 0 --- sanitation.} Items below the loading floor are removed
#' \emph{without} requiring that fit improve. An item that does not load cannot
#' stay, and dropping it often worsens RMSEA temporarily because the degrees of
#' freedom lost outweigh the chi-square gained. A rule that demanded immediate
#' improvement would reject the removal and stall the search before it starts.
#'
#' \strong{Phase 1 --- greedy search.} At each step both actions are evaluated
#' for every candidate --- drop an item, or free one residual covariance --- and
#' the one that most reduces RMSEA is taken.
#'
#' \strong{Phase 2 --- consolidation.} If an item takes part in two or more
#' residual covariances, the item is the problem and not the relations: dropping
#' it is attempted, which typically resolves both at once and buys back a degree
#' of freedom.
#'
#' \strong{Phase 3 --- parsimony.} Each retained covariance is removed in turn;
#' those that are not needed to keep meeting the targets are discarded. Every
#' freed parameter is a debt that has to be justified.
#'
#' What the function does \emph{not} do is judge whether a covariance makes
#' substantive sense. It returns its estimate, confidence interval, sign and ---
#' when \code{item_text} is supplied --- the wording of both items, so that
#' decision stays with whoever knows the construct. A \emph{negative} residual
#' covariance between items at opposite poles of a construct is often the trace
#' of a dimension that the unidimensional model absorbed, and is worth reading
#' before treating it as noise.
#'
#' \emph{Warning.} This is a data-driven search and capitalises on chance
#' (MacCallum, Roznowski & Necowitz, 1992). It establishes whether an admissible
#' specification exists; it does not replace validation on an independent sample.
#'
#' @return An object of class \code{local_fit_search}, a list with \code{best}
#'   (the retained model and its indices), \code{log} (one row per action, with
#'   its phase), \code{ec_detail} (the retained covariances with sign, interval
#'   and wording), \code{meets} and \code{config}.
#'
#' @references
#' MacCallum, R. C., Roznowski, M., & Necowitz, L. B. (1992). Model modifications
#' in covariance structure analysis: The problem of capitalization on chance.
#' \emph{Psychological Bulletin, 111}(3), 490-504.
#'
#' Saris, W. E., Satorra, A., & van der Veld, W. M. (2009). Testing structural
#' equation models or detection of misspecifications? \emph{Structural Equation
#' Modeling, 16}(4), 561-582.
#'
#' @seealso \code{\link{redundancy_short_form}}, \code{\link{discriminant_boosting}},
#'   \code{\link{cross_validate_cfa}}
#'
#' @examples
#' \donttest{
#' data(Data_Expectativas)
#' res <- local_fit_search(Data_Expectativas, paste0("EAF", 1:10),
#'          factor_name = "Expectations", estimator = "MLR", ordered = FALSE)
#' res
#' res$log
#' }
#'
#' @export
local_fit_search <- function(data,
                             items,
                             factor_name  = "F",
                             item_text    = NULL,
                             rmsea_target = 0.08,
                             cfi_target   = 0.95,
                             srmr_target  = 0.08,
                             omega_min    = 0.70,
                             min_loading  = 0.35,
                             min_items    = 5,
                             max_ec       = 4,
                             mi_min       = 5,
                             start_ec     = character(0),
                             estimator    = "WLSMV",
                             ordered      = TRUE,
                             verbose      = TRUE) {

  if (!requireNamespace("lavaan", quietly = TRUE)) stop("lavaan is required")
  if (!requireNamespace("semTools", quietly = TRUE))
    stop("semTools is required for the reliability floor")
  say <- function(...) if (verbose) cat(...)

  pair_of <- function(e) strsplit(gsub(" ", "", e), "~~")[[1]]

  evaluate <- function(its, ec) {
    if (length(its) < min_items) return(NULL)
    ec <- ec[vapply(ec, function(e) all(pair_of(e) %in% its), logical(1))]
    mod <- paste0(factor_name, " =~ ", paste(its, collapse = " + "))
    if (length(ec)) mod <- paste(mod, paste(ec, collapse = "\n"), sep = "\n")
    fit <- tryCatch(suppressWarnings(
             lavaan::cfa(mod, data = data, estimator = estimator,
                         ordered = ordered, std.lv = TRUE)),
             error = function(e) NULL)
    if (is.null(fit) || !isTRUE(lavaan::lavInspect(fit, "converged"))) return(NULL)
    f <- lavaan::fitMeasures(fit, c("chisq.scaled", "df.scaled", "cfi.scaled",
                                    "tli.scaled", "rmsea.scaled", "srmr"))
    if (any(is.na(f))) return(NULL)
    std <- lavaan::standardizedSolution(fit)
    lam <- std[std$op == "=~", ]
    om  <- tryCatch(vapply(semTools::compRelSEM(fit), as.numeric, numeric(1))[[1]],
                    error = function(e) NA_real_)
    list(fit = fit, items = its, ec = ec, n = length(its),
         chisq = f[["chisq.scaled"]], df = f[["df.scaled"]],
         cfi = f[["cfi.scaled"]], tli = f[["tli.scaled"]],
         rmsea = f[["rmsea.scaled"]], srmr = f[["srmr"]],
         omega = om, min_loading = min(abs(lam$est.std)),
         heywood = any(diag(lavaan::lavInspect(fit, "theta")) < 0))
  }

  # Adaptive floors: a model already below a threshold is not frozen, its
  # effective bar becomes the current value. A floor above the starting value
  # would reject every candidate and stall the search.
  admissible <- function(m, ref = NULL) {
    if (is.null(m) || m$heywood || is.na(m$omega)) return(FALSE)
    floor_om  <- if (is.null(ref)) omega_min else min(omega_min, ref$omega)
    floor_lam <- if (is.null(ref)) min_loading else min(min_loading, ref$min_loading)
    m$omega >= floor_om && m$min_loading >= floor_lam
  }

  qualifies <- function(m) admissible(m) && m$rmsea <= rmsea_target &&
    m$cfi >= cfi_target && m$srmr <= srmr_target

  candidate_ec <- function(m) {
    mi <- lavaan::modificationIndices(m$fit, sort. = TRUE)
    mi <- mi[mi$op == "~~" & mi$lhs != mi$rhs &
             mi$lhs %in% m$items & mi$rhs %in% m$items & mi$mi >= mi_min, ]
    if (!nrow(mi)) return(character(0))
    pairs <- paste(mi$lhs, "~~", mi$rhs)
    already <- c(m$ec, vapply(m$ec, function(e) paste(rev(pair_of(e)), collapse = " ~~ "),
                              character(1)))
    utils::head(setdiff(pairs, already), 8)
  }

  row_of <- function(m, action, phase) data.frame(
    phase = phase, action = action, n_items = m$n, n_ec = length(m$ec),
    cfi = round(m$cfi, 3), tli = round(m$tli, 3), rmsea = round(m$rmsea, 3),
    srmr = round(m$srmr, 3), omega = round(m$omega, 3),
    meets = qualifies(m), stringsAsFactors = FALSE)

  m <- evaluate(items, start_ec)
  if (is.null(m)) stop("The starting model does not converge.")
  log <- row_of(m, "start", "0. start")
  say(sprintf("\n[start] n=%d ec=%d | CFI=%.3f RMSEA=%.3f omega=%.3f min_loading=%.3f\n",
              m$n, length(m$ec), m$cfi, m$rmsea, m$omega, m$min_loading))

  # ---- Phase 0: sanitation ---------------------------------------
  say("\n== Phase 0: sanitation (items below the loading floor) ==\n")
  repeat {
    if (m$min_loading >= min_loading || m$n <= min_items) break
    std <- lavaan::standardizedSolution(m$fit)
    lam <- std[std$op == "=~", ]
    weak <- lam$rhs[which.min(abs(lam$est.std))]
    mm <- evaluate(setdiff(m$items, weak), m$ec)
    if (is.null(mm) || mm$heywood) break
    say(sprintf("   dropped %-8s (loading %.3f) | n=%2d | CFI=%.3f RMSEA=%.3f omega=%.3f\n",
                weak, m$min_loading, mm$n, mm$cfi, mm$rmsea, mm$omega))
    m <- mm
    log <- rbind(log, row_of(m, paste("sanitise: drop", weak), "0. sanitation"))
  }
  if (m$min_loading >= min_loading) say("   every loading reaches the floor\n")

  # ---- Phase 1: greedy search ------------------------------------
  say("\n== Phase 1: greedy search (drop item / add covariance) ==\n")
  repeat {
    if (qualifies(m)) { say("   targets reached\n"); break }
    acts <- list()
    for (it in m$items) {
      mm <- evaluate(setdiff(m$items, it), m$ec)
      if (admissible(mm, m)) acts[[paste("drop", it)]] <- mm
    }
    if (length(m$ec) < max_ec) for (p in candidate_ec(m)) {
      mm <- evaluate(m$items, c(m$ec, p))
      if (admissible(mm, m)) acts[[paste("add", p)]] <- mm
    }
    if (!length(acts)) { say("   no valid actions\n"); break }

    r <- vapply(acts, function(a) a$rmsea, numeric(1))
    if (min(r) >= m$rmsea) { say("   no action reduces RMSEA\n"); break }
    best <- names(acts)[which.min(r)]
    m <- acts[[best]]
    log <- rbind(log, row_of(m, best, "1. search"))
    say(sprintf("   %-24s n=%2d ec=%d | CFI=%.3f RMSEA=%.3f\n",
                best, m$n, length(m$ec), m$cfi, m$rmsea))
  }

  # ---- Phase 2: consolidation ------------------------------------
  say("\n== Phase 2: consolidation (item in 2+ covariances) ==\n")
  repeat {
    if (length(m$ec) < 2) { say("   nothing to consolidate\n"); break }
    counts <- table(unlist(lapply(m$ec, pair_of)))
    repeated <- names(counts)[counts >= 2]
    if (!length(repeated)) { say("   no repeated item\n"); break }

    improved <- FALSE
    for (it in repeated) {
      mm <- evaluate(setdiff(m$items, it), m$ec)   # evaluate() drops orphan covariances
      if (admissible(mm, m) && length(mm$ec) < length(m$ec) &&
          (qualifies(mm) || mm$rmsea <= m$rmsea)) {
        say(sprintf("   %s appears in %d covariances: dropped | n=%d ec=%d | CFI=%.3f RMSEA=%.3f\n",
                    it, counts[[it]], mm$n, length(mm$ec), mm$cfi, mm$rmsea))
        m <- mm
        log <- rbind(log, row_of(m, paste("consolidate: drop", it), "2. consolidation"))
        improved <- TRUE
        break
      }
    }
    if (!improved) { say("   dropping does not help: covariances are kept\n"); break }
  }

  # ---- Phase 3: parsimony ----------------------------------------
  say("\n== Phase 3: parsimony (remove dispensable covariances) ==\n")
  if (length(m$ec)) {
    for (e in m$ec) {
      mm <- evaluate(m$items, setdiff(m$ec, e))
      if (qualifies(mm)) {
        say(sprintf("   %s is dispensable: removed | CFI=%.3f RMSEA=%.3f\n",
                    e, mm$cfi, mm$rmsea))
        m <- mm
        log <- rbind(log, row_of(m, paste("remove", e), "3. parsimony"))
      }
    }
    if (length(m$ec)) say(sprintf("   %d covariance(s) kept\n", length(m$ec)))
  } else say("   no covariances\n")

  # ---- Retained covariances, for substantive judgement -----------
  ec_detail <- NULL
  if (length(m$ec)) {
    std <- lavaan::standardizedSolution(m$fit)
    cr  <- std[std$op == "~~" & std$lhs != std$rhs, ]
    ec_detail <- data.frame(
      pair = paste(cr$lhs, "~~", cr$rhs),
      estimate = round(cr$est.std, 3),
      ci_lower = round(cr$ci.lower, 3), ci_upper = round(cr$ci.upper, 3),
      sign = ifelse(cr$est.std < 0, "negative", "positive"),
      stringsAsFactors = FALSE)
    if (!is.null(item_text)) {
      txt <- if ("Texto" %in% names(item_text)) item_text$Texto else item_text$Text
      ec_detail$text_1 <- txt[match(cr$lhs, item_text$Item)]
      ec_detail$text_2 <- txt[match(cr$rhs, item_text$Item)]
    }
  }

  out <- list(best = m, log = log, ec_detail = ec_detail,
              meets = qualifies(m),
              config = list(rmsea_target = rmsea_target, cfi_target = cfi_target,
                            srmr_target = srmr_target, omega_min = omega_min,
                            min_loading = min_loading, min_items = min_items,
                            max_ec = max_ec, mi_min = mi_min))
  class(out) <- c("local_fit_search", "list")
  out
}


#' Print method for local_fit_search
#'
#' @param x Output from \code{\link{local_fit_search}}.
#' @param ... Ignored.
#' @return Invisibly returns \code{x}; called for its side effects.
#' @export
print.local_fit_search <- function(x, ...) {
  m <- x$best
  cat("\nLocal-Fit Search\n----------------------------------------------------------\n")
  cat(sprintf("Solution: %d items, %d covariance(s) | %s\n", m$n, length(m$ec),
              ifelse(x$meets, "MEETS the targets", "does NOT meet the targets")))
  cat(sprintf("  chi2(%d) = %.1f | CFI = %.3f | TLI = %.3f | RMSEA = %.3f | SRMR = %.3f\n",
              m$df, m$chisq, m$cfi, m$tli, m$rmsea, m$srmr))
  cat(sprintf("  omega = %.3f | minimum loading = %.3f\n", m$omega, m$min_loading))
  cat("  Items:", paste(m$items, collapse = ", "), "\n")
  if (!is.null(x$ec_detail)) {
    cat("\nResidual covariances (check sign and content):\n")
    for (i in seq_len(nrow(x$ec_detail))) {
      d <- x$ec_detail[i, ]
      cat(sprintf("  %s = %.3f [%.3f, %.3f]  (%s)\n", d$pair, d$estimate,
                  d$ci_lower, d$ci_upper, d$sign))
      if (!is.null(d$text_1)) {
        cat("     ", d$text_1, "\n")
        cat("     ", d$text_2, "\n")
      }
    }
    cat("\n  A NEGATIVE covariance between items at opposite poles is often the\n")
    cat("  trace of a dimension the unidimensional model absorbed.\n")
  }
  cat("\nData-driven search: cross-validate on an independent sample.\n")
  invisible(x)
}
