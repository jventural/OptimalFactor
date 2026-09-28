#' How Much of the Theoretical Structure a Solution Recovers
#'
#' @description
#' Compares the item-to-factor partition of a solution with the theoretical key
#' of the instrument and reports how much of the theory survives. It works on
#' the output of any routine in the package (\code{\link{efa_boosting}},
#' \code{\link{cfa_boosting}}, \code{\link{discriminant_boosting}},
#' \code{\link{specification_search_theory}}), on a plain partition, or on a
#' loading matrix, so different algorithms can be compared on the same scale.
#'
#' @details
#' No single number answers the question, because a solution can depart from
#' the theory in two ways that one index confounds: it can \emph{drop} items or
#' \emph{move} them to another factor. The metrics are therefore meant to be
#' read together:
#'
#' \describe{
#'   \item{retention}{Proportion of the theoretical items that survive.}
#'   \item{accuracy}{Among the retained items, proportion that sit on the factor
#'     matched to their own dimension. Factors are matched to dimensions by the
#'     assignment that maximises the number of hits, searched exhaustively, so
#'     the result does not depend on the order of the factors.}
#'   \item{recovery}{\code{retention * accuracy}: proportion of the theoretical
#'     items that end up in their place. This is the headline figure.}
#'   \item{ari}{Adjusted Rand Index (Hubert & Arabie, 1985) between the
#'     empirical and theoretical partitions of the retained items. It is 0 when
#'     the agreement is what chance alone would produce and 1 when it is
#'     perfect, and it does not require the numbers of factors to coincide.}
#'   \item{tucker_mean}{Only when loadings are available: mean Tucker congruence
#'     between each factor's loadings and the binary key of its matched
#'     dimension. Values of .85-.94 indicate fair similarity and .95 or above
#'     equality (Lorenzo-Seva & ten Berge, 2006). Absolute loadings are used,
#'     so a reverse-keyed item is not penalised for its sign: polarity is a
#'     matter of scoring, not of structure.}
#' }
#'
#' Accuracy must never be read alone. A confirmatory routine such as
#' \code{\link{cfa_boosting}} cannot move items between factors, only drop
#' them, so its accuracy is 1 by construction and its departure from the theory
#' shows up in retention.
#'
#' @param theory Named list encoding the theoretical key, of the form
#'   \code{list(Dimension = c("item1", "item2"), ...)}.
#' @param solution The solution to evaluate. One of: a named list factor ->
#'   items; a loading matrix or data frame (items in rows, factors in columns;
#'   each item is assigned to its largest absolute loading); or the object
#'   returned by \code{efa_boosting()}, \code{cfa_boosting()},
#'   \code{discriminant_boosting()}, \code{specification_search_theory()} or
#'   \code{local_fit_search()}.
#' @param loadings Optional loading matrix (items in rows) used for the Tucker
#'   congruence. Taken from \code{solution} automatically when it is a loading
#'   matrix or an \code{efa_boosting()} result.
#'
#' @return A list with \code{summary} (one-row data frame with \code{k_theory},
#'   \code{k_solution}, \code{retention}, \code{accuracy}, \code{recovery},
#'   \code{ari} and \code{tucker_mean}), \code{per_dimension} (retention and
#'   recovery of each theoretical dimension and the empirical factor matched to
#'   it), \code{matching}, \code{tucker} and \code{misplaced} (retained items
#'   that sit outside their dimension's factor).
#'
#' @references
#' Hubert, L., & Arabie, P. (1985). Comparing partitions. \emph{Journal of
#' Classification, 2}(1), 193-218. \doi{10.1007/BF01908075}
#'
#' Lorenzo-Seva, U., & ten Berge, J. M. F. (2006). Tucker's congruence
#' coefficient as a meaningful index of factor similarity. \emph{Methodology,
#' 2}(2), 57-64. \doi{10.1027/1614-2241.2.2.57}
#'
#' @seealso \code{\link{algorithm_stability}}, which applies these metrics to
#'   every split of a cross-validation.
#'
#' @examples
#' theory <- list(A = c("x1", "x2", "x3"), B = c("x4", "x5", "x6"))
#'
#' # x3 moved to the other factor and x6 dropped
#' sol <- list(F1 = c("x1", "x2"), F2 = c("x3", "x4", "x5"))
#' theory_recovery(theory, sol)$summary
#'
#' # A loading matrix works too
#' L <- matrix(c(.7, .6, .5, .1, .0, .2,
#'               .1, .0, .2, .8, .7, .6), ncol = 2,
#'             dimnames = list(paste0("x", 1:6), c("F1", "F2")))
#' theory_recovery(theory, L)$summary
#' @export
theory_recovery <- function(theory, solution, loadings = NULL) {
  if (!is.list(theory) || is.null(names(theory)) || any(names(theory) == ""))
    stop("'theory' must be a named list of item vectors.", call. = FALSE)

  sol <- .of_as_partition(solution)
  if (is.null(loadings)) loadings <- sol$loadings
  asig <- sol$asig[lengths(sol$asig) > 0]
  if (!length(asig)) stop("The solution has no items.", call. = FALSE)
  if (is.null(names(asig)) || any(names(asig) == ""))
    names(asig) <- paste0("F", seq_along(asig))

  all_items <- unique(unlist(theory, use.names = FALSE))
  t_lab <- stats::setNames(rep(names(theory), lengths(theory)),
                           unlist(theory, use.names = FALSE))
  e_lab <- stats::setNames(rep(names(asig), lengths(asig)),
                           unlist(asig, use.names = FALSE))
  kept  <- intersect(names(e_lab), all_items)
  e_lab <- e_lab[kept]
  t_k   <- t_lab[kept]

  match <- .of_best_matching(e_lab, t_k, names(asig), names(theory))
  hit   <- !is.na(match[e_lab]) & match[e_lab] == t_k

  per_dim <- data.frame(
    dimension     = names(theory),
    n_theory      = as.integer(lengths(theory)),
    retained      = vapply(theory, function(x) length(intersect(x, kept)), integer(1)),
    in_its_factor = vapply(names(theory), function(dm) sum(hit & t_k == dm), integer(1)),
    empirical_factor = vapply(names(theory), function(dm) {
      f <- names(match)[!is.na(match) & match == dm]
      if (length(f)) f[1] else NA_character_
    }, character(1)),
    row.names = NULL, stringsAsFactors = FALSE)
  per_dim$recovery <- per_dim$in_its_factor / per_dim$n_theory

  tucker <- NULL
  if (!is.null(loadings)) {
    L <- as.matrix(loadings)
    L <- L[intersect(rownames(L), all_items), , drop = FALSE]
    matched <- names(match)[!is.na(match) & names(match) %in% colnames(L)]
    if (nrow(L) && length(matched)) {
      tucker <- vapply(matched, function(f) {
        key <- as.numeric(rownames(L) %in% theory[[match[[f]]]])
        l   <- abs(L[, f])
        den <- sqrt(sum(l^2) * sum(key^2))
        if (den == 0) NA_real_ else sum(l * key) / den
      }, numeric(1))
      names(tucker) <- paste0(matched, "->", match[matched])
    }
  }

  n_kept <- length(kept)
  list(
    summary = data.frame(
      k_theory    = length(theory),
      k_solution  = length(asig),
      retention   = n_kept / length(all_items),
      accuracy    = if (n_kept) mean(hit) else NA_real_,
      recovery    = sum(hit) / length(all_items),
      ari         = .of_ari(e_lab, t_k),
      tucker_mean = if (is.null(tucker)) NA_real_ else mean(tucker, na.rm = TRUE),
      row.names = NULL),
    per_dimension = per_dim,
    matching      = match,
    tucker        = tucker,
    misplaced     = kept[!hit])
}

# Any solution the package produces, reduced to a named list factor -> items
# (plus residual covariances and loadings when the source carries them). Every
# routine returns its structure under a different name, and comparing
# algorithms is only possible once they speak the same language.
.of_as_partition <- function(x) {
  out <- list(asig = NULL, covs = character(0), loadings = NULL)
  from_loadings <- function(L) {
    L <- as.matrix(L)
    if (is.null(colnames(L))) colnames(L) <- paste0("F", seq_len(ncol(L)))
    keep <- apply(abs(L), 1, function(r) any(r > 0, na.rm = TRUE))
    L <- L[keep, , drop = FALSE]
    prim <- colnames(L)[apply(abs(L), 1, which.max)]
    s <- split(rownames(L), factor(prim, levels = colnames(L)))
    list(asig = s[lengths(s) > 0], loadings = L)
  }

  if (is.matrix(x)) {
    r <- from_loadings(x); out$asig <- r$asig; out$loadings <- r$loadings
  } else if (is.data.frame(x)) {
    if ("Items" %in% names(x)) {
      L <- as.matrix(x[, setdiff(names(x), "Items"), drop = FALSE])
      rownames(L) <- x$Items
    } else L <- as.matrix(x)
    r <- from_loadings(L); out$asig <- r$asig; out$loadings <- r$loadings
  } else if (is.list(x) && !is.null(x$final_structure)) {          # efa_boosting
    return(.of_as_partition(x$final_structure))
  } else if (is.list(x) && !is.null(x$final_factors)) {            # cfa_boosting
    out$asig <- x$final_factors
    out$covs <- as.character(x$final_covs)
  } else if (is.list(x) && is.list(x$best) && !is.null(x$best$asig)) {     # discriminant_boosting
    out$asig <- x$best$asig
  } else if (is.list(x) && is.list(x$best) && !is.null(x$best$factors)) {  # specification_search(_theory)
    out$asig <- x$best$factors
    out$covs <- as.character(x$best$covs)
  } else if (is.list(x) && is.list(x$best) && !is.null(x$best$items)) {    # local_fit_search
    fname <- if (!is.null(x$config$factor_name)) x$config$factor_name else "F"
    out$asig <- stats::setNames(list(x$best$items), fname)
    out$covs <- as.character(x$best$ec)
  } else if (is.list(x) && !is.null(x$asig)) {
    out$asig <- x$asig
    if (!is.null(x$covs)) out$covs <- as.character(x$covs)
  } else if (is.list(x) && all(vapply(x, is.character, logical(1)))) {
    out$asig <- x
  } else {
    stop("Cannot read an item partition from this object. Pass a named list ",
         "factor -> items, a loading matrix, or the result of one of the ",
         "package's routines.", call. = FALSE)
  }
  out
}

# Factor -> dimension matching that maximises the number of items in place.
# Greedy matching (largest overlap first) can lock in a pair that blocks a
# better global assignment; with the handful of factors a scale has, the
# exhaustive search costs nothing. Past 7 factors it falls back to greedy.
.of_best_matching <- function(e_lab, t_lab, e_names, t_names) {
  ov <- table(factor(e_lab, levels = e_names), factor(t_lab, levels = t_names))
  ov <- matrix(ov, nrow = length(e_names), dimnames = list(e_names, t_names))
  match <- stats::setNames(rep(NA_character_, length(e_names)), e_names)
  m <- min(nrow(ov), ncol(ov))
  if (m == 0) return(match)

  if (max(nrow(ov), ncol(ov)) <= 7) {
    perms <- function(v, k) {
      if (k == 0) return(list(integer(0)))
      out <- list()
      for (i in seq_along(v)) for (p in perms(v[-i], k - 1)) out[[length(out) + 1]] <- c(v[i], p)
      out
    }
    best <- NULL; best_s <- -1
    if (nrow(ov) <= ncol(ov)) {
      for (p in perms(seq_len(ncol(ov)), m)) {
        s <- sum(ov[cbind(seq_len(m), p)])
        if (s > best_s) { best_s <- s; best <- cbind(seq_len(m), p) }
      }
    } else {
      for (p in perms(seq_len(nrow(ov)), m)) {
        s <- sum(ov[cbind(p, seq_len(m))])
        if (s > best_s) { best_s <- s; best <- cbind(p, seq_len(m)) }
      }
    }
    match[rownames(ov)[best[, 1]]] <- colnames(ov)[best[, 2]]
  } else {
    ov2 <- ov
    for (step in seq_len(m)) {
      if (all(ov2 < 0)) break
      pos <- which(ov2 == max(ov2), arr.ind = TRUE)[1, ]
      match[rownames(ov2)[pos[1]]] <- colnames(ov2)[pos[2]]
      ov2[pos[1], ] <- -1; ov2[, pos[2]] <- -1
    }
  }
  match
}

# Adjusted Rand Index of two labelings of the same items.
.of_ari <- function(a, b) {
  if (length(a) < 2) return(NA_real_)
  tab <- table(a, b)
  c2  <- function(x) sum(choose(x, 2))
  idx <- c2(tab)
  exp <- c2(rowSums(tab)) * c2(colSums(tab)) / choose(sum(tab), 2)
  mx  <- (c2(rowSums(tab)) + c2(colSums(tab))) / 2
  if (mx == exp) return(if (idx == mx) 1 else NA_real_)
  (idx - exp) / (mx - exp)
}
