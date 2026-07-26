#' Redundancy-Guided Short Form of a Unidimensional Scale
#'
#' @description
#' Builds a short form of an (essentially) unidimensional scale by iteratively
#' removing the most locally-dependent item. At each step a one-factor model is
#' fitted, the pair of items with the largest residual correlation is located,
#' and the item of that pair with the \emph{lower} loading is dropped (keeping the
#' stronger indicator). Pruning stops when the target length \code{k} is reached
#' or when no residual correlation exceeds \code{threshold}. This addresses the
#' common situation where a scale is unidimensional but the one-factor model
#' misfits because of near-duplicate items (local dependence).
#'
#' @details
#' Removing redundant items is preferred over piling up residual covariances,
#' which capitalizes on chance (MacCallum, Roznowski & Necowitz, 1992). The
#' resulting short form should be cross-validated on an independent sample (see
#' \code{\link{cross_validate_cfa}}).
#'
#' @param data Data frame with the item responses.
#' @param items Character vector with the candidate item names.
#' @param k Target number of items. If \code{NULL}, prunes until the largest
#'   residual correlation drops below \code{threshold}. Default \code{NULL}.
#' @param groups Optional named list mapping content groups to items (e.g.
#'   theoretical dimensions), used only to preserve at least \code{min_per_group}
#'   items per group during pruning. Default \code{NULL} (no constraint).
#' @param min_per_group Minimum items kept per group in \code{groups}. Default 3.
#' @param threshold Residual-correlation stopping threshold when \code{k = NULL}.
#'   Default 0.15.
#' @param min_omega Reliability floor. When set (e.g. \code{0.80}), an item is
#'   only dropped if the resulting form keeps McDonald's omega at or above this
#'   value, so the short form cannot buy fit at the cost of internal
#'   consistency. A scale that already sits below the floor is not frozen: for
#'   it the effective bar is its current omega, so removals that do not reduce
#'   reliability are still allowed. \code{NULL} (default) disables the check.
#' @param estimator Estimator passed to \code{lavaan::cfa}. Default \code{"WLSMV"}.
#' @param ordered Logical; treat items as ordered. Default \code{TRUE}.
#'
#' @return A list with \code{items} (retained items), \code{trajectory} (data
#'   frame with n_items, cfi, tli, rmsea, srmr, omega and the item dropped at
#'   each step), \code{fit} (final one-factor \code{lavaan} object),
#'   \code{loadings} (standardized), \code{omega} (McDonald's omega of the final
#'   form) and \code{stop_reason} (\code{"target_k"}, \code{"threshold"},
#'   \code{"min_items"}, \code{"min_per_group"} or \code{"min_omega"}).
#'
#' @section References:
#' MacCallum, R. C., Roznowski, M., & Necowitz, L. B. (1992). Model modifications
#'   in covariance structure analysis: The problem of capitalization on chance.
#'   \emph{Psychological Bulletin, 111}(3), 490--504.
#'
#' @examples
#' \dontrun{
#'   sf <- redundancy_short_form(mydata, paste0("IT", 1:16), k = 7,
#'           groups = list(A = paste0("IT",1:8), B = paste0("IT",9:16)))
#'   sf$trajectory; sf$items; sf$omega
#' }
#' @seealso \code{\link{cross_validate_cfa}}
#' @export
redundancy_short_form <- function(data, items, k = NULL, groups = NULL,
                                  min_per_group = 3, threshold = 0.15,
                                  min_omega = NULL,
                                  estimator = "WLSMV", ordered = TRUE) {
  if (!requireNamespace("lavaan", quietly = TRUE)) stop("Package 'lavaan' is required.")
  grp_of <- function(it) { if (is.null(groups)) return(NA_character_)
    for (gname in names(groups)) if (it %in% groups[[gname]]) return(gname); NA_character_ }
  omega_of <- function(l) { l <- abs(as.numeric(l[is.finite(l)]))
    if (length(l) < 2) return(NA_real_); sum(l)^2 / (sum(l)^2 + sum(1 - l^2)) }
  fit1 <- function(its) lavaan::cfa(paste0("G =~ ", paste(its, collapse = " + ")),
                data = data[, its], ordered = if (ordered) its else FALSE,
                estimator = estimator, std.lv = TRUE)
  idx <- function(fit) { fm <- lavaan::fitMeasures(fit,
                c("cfi.scaled","tli.scaled","rmsea.scaled","srmr"))
    stats::setNames(as.numeric(fm), c("cfi","tli","rmsea","srmr")) }
  loads <- function(fit) { s <- lavaan::standardizedSolution(fit); s <- s[s$op == "=~", ]
    stats::setNames(s$est.std, s$rhs) }

  its <- items; traj <- list(); last_drop <- "-"; stop_reason <- "min_items"
  blocked_by_omega <- character(0)
  repeat {
    fit <- fit1(its); f <- idx(fit); L <- loads(fit)
    om_now <- omega_of(L[its])
    traj[[length(traj) + 1]] <- data.frame(n_items = length(its), cfi = round(f["cfi"],3),
      tli = round(f["tli"],3), rmsea = round(f["rmsea"],3), srmr = round(f["srmr"],3),
      omega = round(om_now, 3), dropped = last_drop, row.names = NULL)
    rc <- lavaan::residuals(fit, type = "cor")$cov; diag(rc) <- 0
    if (!is.null(k) && length(its) <= k) { stop_reason <- "target_k"; break }
    if (is.null(k) && max(abs(rc)) < threshold) { stop_reason <- "threshold"; break }
    if (length(its) <= 3) { stop_reason <- "min_items"; break }
    ord <- order(-abs(rc[upper.tri(rc)])); pr <- which(upper.tri(rc), arr.ind = TRUE)[ord, , drop = FALSE]
    drop <- NA_character_; blocked_reason <- "min_per_group"
    for (r in seq_len(nrow(pr))) {
      a <- rownames(rc)[pr[r,1]]; b <- colnames(rc)[pr[r,2]]
      cand <- if (abs(L[a]) <= abs(L[b])) a else b
      if (!is.null(groups)) {
        gc <- grp_of(cand); keep_n <- sum(vapply(setdiff(its, cand), function(x) identical(grp_of(x), gc), logical(1)))
        if (!is.na(gc) && keep_n < min_per_group) next
      }
      # Reliability floor, projected on the current loadings. A form already
      # below the floor is judged against its own omega, not against the floor,
      # so pruning is never frozen outright.
      if (!is.null(min_omega)) {
        om_after <- omega_of(L[setdiff(its, cand)])
        bar <- min(min_omega, if (is.na(om_now)) min_omega else om_now)
        if (!is.na(om_after) && om_after < bar - 1e-8) {
          blocked_by_omega <- unique(c(blocked_by_omega, cand))
          blocked_reason <- "min_omega"
          next
        }
      }
      drop <- cand; break
    }
    if (is.na(drop)) { stop_reason <- blocked_reason; break }
    last_drop <- drop; its <- setdiff(its, drop)
  }
  fit <- fit1(its); L <- loads(fit)
  om  <- omega_of(L[its])
  list(items = its, trajectory = do.call(rbind, traj), fit = fit,
       loadings = round(L[its], 3), omega = round(om, 3),
       stop_reason = stop_reason, omega_blocked = blocked_by_omega)
}
