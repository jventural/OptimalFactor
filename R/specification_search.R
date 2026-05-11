#' Heuristic Specification Search for CFA Models
#'
#' @description
#' Performs a heuristic specification search over CFA models in the spirit of
#' MacCallum (1986). For each seed configuration (with a fixed number of
#' factors), the algorithm runs a hill-climbing loop that evaluates, at each
#' step, three local operations:
#' \itemize{
#'   \item \strong{move}: reassign an item from its current factor to another.
#'   \item \strong{drop}: remove an item with low loading or causing strain.
#'   \item \strong{cov}: add a residual covariance suggested by modification
#'         indices (within-factor by default).
#' }
#' Optionally, a bifactor variant (one orthogonal general factor plus the
#' configured group factors) is fitted for every k >= 2 seed. The procedure
#' returns every configuration evaluated, the subset that meets the
#' user-supplied fit targets, and the best model under a composite loss.
#'
#' @details
#' \strong{Warning (MacCallum, 1986).} Specification search capitalizes on
#' chance: the more configurations explored, the more likely an apparently
#' good model is sample-specific. If you use this function in a publication
#' you should (a) report the procedure transparently as exploratory,
#' (b) cross-validate the chosen model with an independent sample or via
#' bootstrap, and (c) justify each accepted modification on substantive
#' theoretical grounds. The function prints this warning on every run
#' unless \code{verbose = FALSE}.
#'
#' The composite loss is
#' \deqn{L = w_{rmsea}\cdot\max(0,(RMSEA - t_{rmsea})/0.03) +
#'       w_{cfi}\cdot\max(0,(t_{cfi} - CFI)/0.03) +
#'       w_{srmr}\cdot\max(0,(SRMR - t_{srmr})/0.03)}
#' with default weights 0.5, 0.3, 0.2. A configuration is flagged as
#' \emph{successful} if CFI >= \code{cfi_target} and RMSEA <= \code{rmsea_target}.
#'
#' @param data Data frame with the observed item responses.
#' @param items Character vector with the names of the items to be searched
#'   over (must all exist in \code{data}).
#' @param seeds A named list of initial seed configurations. Names must be
#'   the number of factors as a character string (e.g. \code{"3"}) and each
#'   element is itself a list of seeds, where each seed is a named list of
#'   the form \code{list(F1 = c("it1","it2"), F2 = c(...))}. If \code{NULL},
#'   default block-partition seeds are generated for k = 1 to \code{max_factors}.
#' @param max_factors Maximum number of factors to consider (used when
#'   \code{seeds = NULL}). Default 4.
#' @param min_items_factor Minimum number of items per factor preserved
#'   during move/drop operations. Default 2.
#' @param cfi_target,rmsea_target,srmr_target Fit thresholds. Successful
#'   models must satisfy CFI >= \code{cfi_target} and RMSEA <= \code{rmsea_target}.
#'   \code{srmr_target} is used only in the loss. Defaults 0.95 / 0.08 / 0.08.
#' @param max_iter_per_config Maximum hill-climbing iterations per seed.
#'   Default 40.
#' @param max_covs Maximum residual covariances that can be added in a single
#'   configuration. Default 5.
#' @param max_items_removed Maximum items that can be removed (across all
#'   factors) in a single configuration. Default 6.
#' @param try_bifactor Logical. If \code{TRUE} (default), each seed with
#'   k >= 2 is also evaluated as a bifactor (one orthogonal G plus the
#'   configured group factors).
#' @param operations Character vector with the local operations to enable.
#'   Subset of \code{c("move","drop","cov")}. Default all three.
#' @param estimator Estimator passed to \code{lavaan::cfa}. Default
#'   \code{"WLSMV"}.
#' @param ordered Logical. If \code{TRUE} (default), the items are treated as
#'   ordered (recommended for Likert data with WLSMV).
#' @param std.lv Logical. If \code{TRUE} (default), latent variances are fixed
#'   to one.
#' @param mi_min Minimum modification-index value considered when proposing
#'   residual covariances. Default 10.
#' @param mi_top Maximum number of top-MI covariances examined at each step.
#'   Default 3.
#' @param loss_weights Named numeric vector with the weights of the composite
#'   loss for \code{rmsea}, \code{cfi} and \code{srmr}. Default
#'   \code{c(rmsea = 0.5, cfi = 0.3, srmr = 0.2)}.
#' @param patience Number of consecutive iterations without improvement that
#'   stops the hill climb. Default 8.
#' @param early_stop_after_meet Once a configuration meets the targets, the
#'   loop runs at most this many additional non-improving iterations. Default 3.
#' @param verbose Logical. Print progress and the MacCallum warning. Default
#'   \code{TRUE}.
#'
#' @return An object of class \code{specification_search}, which is a list
#'   with components:
#'   \describe{
#'     \item{\code{table}}{Data frame with one row per evaluated configuration,
#'           ordered by composite loss.}
#'     \item{\code{successful}}{Subset of \code{table} that meets the targets.}
#'     \item{\code{best}}{Best result (lowest loss) including the fitted
#'           \code{lavaan} object, the final factor assignment and the
#'           residual covariances added.}
#'     \item{\code{results}}{List with every configuration evaluated.}
#'     \item{\code{call}}{The matched call.}
#'   }
#'
#' @section References:
#' MacCallum, R. C. (1986). Specification searches in covariance structure
#'   modeling. \emph{Psychological Bulletin, 100}(1), 107--120.
#'
#' Saris, W. E., Satorra, A., & van der Veld, W. M. (2009). Testing structural
#'   equation models or detection of misspecifications? \emph{Structural
#'   Equation Modeling, 16}(4), 561--582.
#'
#' @examples
#' \dontrun{
#'   data(Data_Personality)
#'   items <- paste0("PPTQ", 1:15)
#'
#'   res <- specification_search(
#'     data        = Data_Personality,
#'     items       = items,
#'     max_factors = 3,
#'     estimator   = "MLR",
#'     ordered     = FALSE,
#'     try_bifactor = TRUE,
#'     verbose     = TRUE
#'   )
#'
#'   head(res$table)
#'   res$successful
#'   summary(res$best$fit)
#' }
#'
#' @seealso \code{\link{cfa_boosting}}
#' @export
specification_search <- function(data,
                                 items,
                                 seeds = NULL,
                                 max_factors = 4,
                                 min_items_factor = 2,
                                 cfi_target = 0.95,
                                 rmsea_target = 0.08,
                                 srmr_target = 0.08,
                                 max_iter_per_config = 40,
                                 max_covs = 5,
                                 max_items_removed = 6,
                                 try_bifactor = TRUE,
                                 operations = c("move", "drop", "cov"),
                                 estimator = "WLSMV",
                                 ordered = TRUE,
                                 std.lv = TRUE,
                                 mi_min = 10,
                                 mi_top = 3,
                                 loss_weights = c(rmsea = 0.5, cfi = 0.3, srmr = 0.2),
                                 patience = 8,
                                 early_stop_after_meet = 3,
                                 verbose = TRUE) {

  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package 'lavaan' is required.")
  }
  call_ <- match.call()

  # ---- Validations ----
  if (!is.data.frame(data)) stop("'data' must be a data frame.")
  if (!is.character(items) || length(items) < 3) {
    stop("'items' must be a character vector with at least 3 item names.")
  }
  missing_items <- setdiff(items, colnames(data))
  if (length(missing_items) > 0) {
    stop("These items are not present in 'data': ",
         paste(missing_items, collapse = ", "))
  }
  operations <- match.arg(operations, c("move", "drop", "cov"), several.ok = TRUE)
  wts <- c(rmsea = 0.5, cfi = 0.3, srmr = 0.2)
  if (!is.null(loss_weights)) {
    for (nm in names(loss_weights)) if (nm %in% names(wts)) wts[nm] <- loss_weights[[nm]]
  }

  # ---- Warning (MacCallum, 1986) ----
  if (isTRUE(verbose)) {
    cat("\n", strrep("=", 72), "\n", sep = "")
    cat(" Specification Search (MacCallum, 1986)\n")
    cat(strrep("=", 72), "\n", sep = "")
    cat(" WARNING: specification search capitalizes on chance. Use this\n")
    cat(" function only as an EXPLORATORY device. Recommended practice:\n")
    cat("   (1) Report the procedure transparently as exploratory.\n")
    cat("   (2) Cross-validate the chosen model with an independent\n")
    cat("       sample or via bootstrap.\n")
    cat("   (3) Justify each accepted modification on theoretical grounds.\n")
    cat(strrep("=", 72), "\n\n", sep = "")
    cat(" Items:", length(items),
        "| Max factors:", max_factors,
        "| Operations:", paste(operations, collapse = "/"),
        "| Bifactor:", try_bifactor, "\n")
    cat(" Targets: CFI >=", cfi_target,
        " | RMSEA <=", rmsea_target,
        " | SRMR <=", srmr_target, "\n\n")
  }

  # ============================================================
  # Internal helpers
  # ============================================================

  fit_modelo <- function(syntax, items_used) {
    fit <- try(
      lavaan::cfa(syntax, data = data,
                  estimator = estimator,
                  ordered   = if (isTRUE(ordered)) items_used else FALSE,
                  std.lv    = std.lv),
      silent = TRUE
    )
    if (inherits(fit, "try-error")) return(NULL)
    conv <- try(lavaan::lavInspect(fit, "converged"), silent = TRUE)
    if (inherits(conv, "try-error") || !isTRUE(conv)) return(NULL)
    fit
  }

  get_indices <- function(fit) {
    if (is.null(fit)) {
      return(list(cfi = NA_real_, tli = NA_real_, rmsea = NA_real_,
                  srmr = NA_real_, chisq = NA_real_, df = NA_real_))
    }
    fm <- suppressWarnings(lavaan::fitMeasures(fit))
    pick <- function(s, n) {
      if (s %in% names(fm)) as.numeric(fm[s])
      else if (n %in% names(fm)) as.numeric(fm[n])
      else NA_real_
    }
    list(
      cfi   = pick("cfi.scaled",   "cfi"),
      tli   = pick("tli.scaled",   "tli"),
      rmsea = pick("rmsea.scaled", "rmsea"),
      srmr  = as.numeric(fm["srmr"]),
      chisq = pick("chisq.scaled", "chisq"),
      df    = pick("df.scaled",    "df")
    )
  }

  es_admisible <- function(fit) {
    if (is.null(fit)) return(FALSE)
    lat <- unique(lavaan::lavNames(fit, type = "lv"))
    pe  <- lavaan::parameterEstimates(fit, standardized = TRUE)
    neg <- any(pe$op == "~~" & pe$lhs == pe$rhs &
                 pe$lhs %in% lat & pe$est < 0)
    std <- try(lavaan::standardizedSolution(fit), silent = TRUE)
    big <- if (inherits(std, "try-error")) FALSE else
      any(std$op == "=~" & std$lhs %in% lat & abs(std$est.std) > 1)
    !neg && !big
  }

  cumple <- function(idx) {
    !is.na(idx$cfi) && !is.na(idx$rmsea) &&
      idx$cfi >= cfi_target && idx$rmsea <= rmsea_target
  }

  loss <- function(idx) {
    if (is.na(idx$cfi) || is.na(idx$rmsea) || is.na(idx$srmr)) return(Inf)
    rmsea_l <- max(0, (idx$rmsea - rmsea_target) / 0.03)
    cfi_l   <- max(0, (cfi_target - idx$cfi)     / 0.03)
    srmr_l  <- max(0, (idx$srmr  - srmr_target)  / 0.03)
    as.numeric(wts["rmsea"]) * rmsea_l +
      as.numeric(wts["cfi"])   * cfi_l +
      as.numeric(wts["srmr"])  * srmr_l
  }

  asig_to_syntax <- function(asig, covs = character(0)) {
    lines <- vapply(names(asig), function(f) {
      paste0(f, " =~ ", paste(asig[[f]], collapse = " + "))
    }, character(1))
    syn <- paste(lines, collapse = "\n")
    if (length(covs) > 0) syn <- paste(syn, paste(covs, collapse = "\n"), sep = "\n")
    syn
  }

  asig_to_bifactor <- function(asig, covs = character(0)) {
    all_items <- sort(unique(unlist(asig, use.names = FALSE)))
    fnames <- names(asig)
    lines <- c(paste0("G =~ ", paste(all_items, collapse = " + ")))
    for (f in fnames) {
      lines <- c(lines, paste0(f, " =~ ", paste(asig[[f]], collapse = " + ")))
    }
    for (f in fnames) lines <- c(lines, paste0("G ~~ 0*", f))
    if (length(fnames) > 1) {
      combs <- utils::combn(fnames, 2)
      for (i in seq_len(ncol(combs))) {
        lines <- c(lines, paste0(combs[1, i], " ~~ 0*", combs[2, i]))
      }
    }
    syn <- paste(lines, collapse = "\n")
    if (length(covs) > 0) syn <- paste(syn, paste(covs, collapse = "\n"), sep = "\n")
    syn
  }

  items_en_asig <- function(asig) sort(unique(unlist(asig, use.names = FALSE)))

  default_seeds <- function() {
    out <- list()
    out[["1"]] <- list(list(G = items))
    if (max_factors < 2) return(out)
    for (k in 2:max_factors) {
      # block partition: as balanced as possible
      sizes <- rep(floor(length(items) / k), k)
      remainder <- length(items) - sum(sizes)
      if (remainder > 0) sizes[seq_len(remainder)] <- sizes[seq_len(remainder)] + 1
      idx <- 1L
      asig <- list()
      for (j in seq_len(k)) {
        asig[[paste0("F", j)]] <- items[idx:(idx + sizes[j] - 1L)]
        idx <- idx + sizes[j]
      }
      out[[as.character(k)]] <- list(asig)
    }
    out
  }

  if (is.null(seeds)) seeds <- default_seeds()

  # ============================================================
  # Hill climb for a single configuration
  # ============================================================
  optimizar_config <- function(asig_inicial, config_id, es_bifactor = FALSE) {

    items_used <- items_en_asig(asig_inicial)
    covs <- character(0)

    syn <- if (es_bifactor) asig_to_bifactor(asig_inicial, covs)
           else             asig_to_syntax(asig_inicial, covs)

    fit <- fit_modelo(syn, items_used)
    if (is.null(fit) || !es_admisible(fit)) return(NULL)

    idx <- get_indices(fit)
    current_loss <- loss(idx)
    current_asig <- asig_inicial
    current_fit  <- fit

    mejoras_sin_avance <- 0L
    iters_tras_cumplir <- 0L

    for (iter in seq_len(max_iter_per_config)) {
      if (mejoras_sin_avance >= patience) break

      mejor_accion <- NULL
      mejor_loss <- current_loss
      mejor_fit_new  <- NULL
      mejor_asig_new <- NULL
      mejor_covs_new <- NULL

      fnames <- names(current_asig)
      items_actuales <- items_en_asig(current_asig)

      # ---- Op 1: MOVE ----
      if ("move" %in% operations && length(fnames) >= 2) {
        for (fi in seq_along(fnames)) {
          f_o <- fnames[fi]
          its_f <- current_asig[[f_o]]
          if (length(its_f) <= min_items_factor) next
          for (item in its_f) {
            for (fj in seq_along(fnames)) {
              if (fi == fj) next
              f_d <- fnames[fj]
              nueva <- current_asig
              nueva[[f_o]] <- setdiff(nueva[[f_o]], item)
              nueva[[f_d]] <- c(nueva[[f_d]], item)

              syn_new <- if (es_bifactor) asig_to_bifactor(nueva, covs)
                         else             asig_to_syntax(nueva, covs)
              fit_new <- fit_modelo(syn_new, items_en_asig(nueva))
              if (!is.null(fit_new) && es_admisible(fit_new)) {
                l <- loss(get_indices(fit_new))
                if (l < mejor_loss) {
                  mejor_loss <- l
                  mejor_accion <- paste0("MOVE ", item, ": ", f_o, " -> ", f_d)
                  mejor_fit_new  <- fit_new
                  mejor_asig_new <- nueva
                  mejor_covs_new <- covs
                }
              }
            }
          }
        }
      }

      # ---- Op 2: DROP ----
      if ("drop" %in% operations) {
        n_removidos <- length(items) - length(items_actuales)
        if (n_removidos < max_items_removed) {
          for (fi in seq_along(fnames)) {
            f_n <- fnames[fi]
            its_f <- current_asig[[f_n]]
            if (length(its_f) <= min_items_factor) next
            for (item in its_f) {
              nueva <- current_asig
              nueva[[f_n]] <- setdiff(nueva[[f_n]], item)
              syn_new <- if (es_bifactor) asig_to_bifactor(nueva, covs)
                         else             asig_to_syntax(nueva, covs)
              fit_new <- fit_modelo(syn_new, items_en_asig(nueva))
              if (!is.null(fit_new) && es_admisible(fit_new)) {
                l <- loss(get_indices(fit_new))
                if (l < mejor_loss) {
                  mejor_loss <- l
                  mejor_accion <- paste0("DROP ", item, " from ", f_n)
                  mejor_fit_new  <- fit_new
                  mejor_asig_new <- nueva
                  mejor_covs_new <- covs
                }
              }
            }
          }
        }
      }

      # ---- Op 3: COV (residual) ----
      if ("cov" %in% operations && length(covs) < max_covs) {
        mi <- tryCatch(
          lavaan::modificationIndices(current_fit, sort. = TRUE,
                                      minimum.value = mi_min),
          error = function(e) data.frame()
        )
        if (NROW(mi) > 0) {
          mi_cov <- mi[mi$op == "~~" &
                         mi$lhs %in% items_actuales &
                         mi$rhs %in% items_actuales &
                         mi$lhs != mi$rhs, , drop = FALSE]
          if (NROW(mi_cov) > 0) {
            mi_cov <- utils::head(mi_cov, mi_top)
            for (r in seq_len(nrow(mi_cov))) {
              new_cov <- paste0(mi_cov$lhs[r], " ~~ ", mi_cov$rhs[r])
              rev_cov <- paste0(mi_cov$rhs[r], " ~~ ", mi_cov$lhs[r])
              if (new_cov %in% covs || rev_cov %in% covs) next
              covs_new <- c(covs, new_cov)
              syn_new <- if (es_bifactor) asig_to_bifactor(current_asig, covs_new)
                         else             asig_to_syntax(current_asig, covs_new)
              fit_new <- fit_modelo(syn_new, items_actuales)
              if (!is.null(fit_new) && es_admisible(fit_new)) {
                l <- loss(get_indices(fit_new))
                if (l < mejor_loss) {
                  mejor_loss <- l
                  mejor_accion <- paste0("COV ", new_cov,
                                         " (MI=", round(mi_cov$mi[r], 1), ")")
                  mejor_fit_new  <- fit_new
                  mejor_asig_new <- current_asig
                  mejor_covs_new <- covs_new
                }
              }
            }
          }
        }
      }

      # ---- apply best ----
      if (!is.null(mejor_accion)) {
        current_loss <- mejor_loss
        current_fit  <- mejor_fit_new
        current_asig <- mejor_asig_new
        covs         <- mejor_covs_new
        mejoras_sin_avance <- 0L
        if (verbose) {
          inew <- get_indices(current_fit)
          cat(sprintf("    [%s] iter %d: %s -> CFI=%.4f RMSEA=%.4f SRMR=%.4f loss=%.4f\n",
                      config_id, iter, mejor_accion,
                      inew$cfi, inew$rmsea, inew$srmr, current_loss))
        }
      } else {
        mejoras_sin_avance <- mejoras_sin_avance + 1L
      }

      # Early stop after meeting targets
      ich <- get_indices(current_fit)
      if (cumple(ich)) {
        iters_tras_cumplir <- iters_tras_cumplir + 1L
        if (iters_tras_cumplir >= early_stop_after_meet) break
      }
    }

    idx_fin <- get_indices(current_fit)
    list(
      fit       = current_fit,
      asig      = current_asig,
      covs      = covs,
      indices   = idx_fin,
      cumple    = cumple(idx_fin),
      loss      = loss(idx_fin),
      n_items   = length(items_en_asig(current_asig)),
      n_factors = length(current_asig),
      bifactor  = es_bifactor,
      config_id = config_id
    )
  }

  # ============================================================
  # Run search
  # ============================================================
  todos <- list()
  contador <- 0L

  ks <- as.character(sort(as.integer(names(seeds))))
  for (k in ks) {
    if (is.null(seeds[[k]])) next
    if (verbose) cat("--- k =", k, "factor(s) ---\n")
    sl <- seeds[[k]]
    for (s in seq_along(sl)) {
      asig <- sl[[s]]
      cid <- paste0("k", k, "_s", s)
      if (verbose) cat("  Config:", cid, "(standard)\n")
      res <- tryCatch(
        optimizar_config(asig, cid, es_bifactor = FALSE),
        error = function(e) { if (verbose) cat("    Error:", conditionMessage(e), "\n"); NULL }
      )
      if (!is.null(res)) {
        contador <- contador + 1L
        todos[[contador]] <- res
      }
      if (try_bifactor && as.integer(k) >= 2L) {
        cid_bi <- paste0("k", k, "_s", s, "_bi")
        if (verbose) cat("  Config:", cid_bi, "(bifactor)\n")
        res_bi <- tryCatch(
          optimizar_config(asig, cid_bi, es_bifactor = TRUE),
          error = function(e) { if (verbose) cat("    Error:", conditionMessage(e), "\n"); NULL }
        )
        if (!is.null(res_bi)) {
          contador <- contador + 1L
          todos[[contador]] <- res_bi
        }
      }
    }
  }

  if (verbose) cat("\nModels evaluated:", contador, "\n")

  if (contador == 0L) {
    warning("No model could be fitted successfully. Returning empty result.")
    out <- list(
      table = data.frame(), successful = data.frame(),
      best = NULL, results = list(), call = call_
    )
    class(out) <- c("specification_search", "list")
    return(out)
  }

  # ---- Compile results table ----
  rows <- lapply(todos, function(r) {
    data.frame(
      config     = r$config_id,
      n_factors  = r$n_factors,
      bifactor   = r$bifactor,
      n_items    = r$n_items,
      n_covs     = length(r$covs),
      cfi        = round(r$indices$cfi,   4),
      tli        = round(r$indices$tli,   4),
      rmsea      = round(r$indices$rmsea, 4),
      srmr       = round(r$indices$srmr,  4),
      chisq      = round(r$indices$chisq, 2),
      df         = r$indices$df,
      loss       = round(r$loss,          4),
      meets      = r$cumple,
      stringsAsFactors = FALSE
    )
  })
  tabla <- do.call(rbind, rows)
  tabla <- tabla[order(tabla$loss, na.last = TRUE), , drop = FALSE]
  rownames(tabla) <- NULL

  exitosos <- tabla[isTRUE_vec(tabla$meets), , drop = FALSE]

  # ---- Best ----
  ord <- order(vapply(todos, function(r) r$loss, numeric(1)),
               na.last = TRUE)
  best_res <- todos[[ord[1]]]
  syn_best <- if (best_res$bifactor)
                asig_to_bifactor(best_res$asig, best_res$covs)
              else
                asig_to_syntax(best_res$asig,  best_res$covs)
  best <- list(
    config_id  = best_res$config_id,
    fit        = best_res$fit,
    syntax     = syn_best,
    factors    = best_res$asig,
    covs       = best_res$covs,
    indices    = best_res$indices,
    bifactor   = best_res$bifactor,
    meets      = best_res$cumple,
    loss       = best_res$loss
  )

  out <- list(
    table      = tabla,
    successful = exitosos,
    best       = best,
    results    = todos,
    call       = call_
  )
  class(out) <- c("specification_search", "list")
  out
}

# vector-safe isTRUE for logical column
isTRUE_vec <- function(x) {
  out <- logical(length(x))
  out[is.na(x)] <- FALSE
  out[!is.na(x)] <- as.logical(x[!is.na(x)])
  out
}

#' Print method for specification_search
#'
#' @param x An object returned by \code{\link{specification_search}}.
#' @param top Number of rows to print. Default 10.
#' @param ... Ignored.
#' @export
print.specification_search <- function(x, top = 10, ...) {
  if (is.null(x$table) || nrow(x$table) == 0) {
    cat("Specification search: no evaluated configurations.\n")
    return(invisible(x))
  }
  cat("Specification search:", nrow(x$table), "configurations evaluated\n")
  cat("Successful (meets CFI/RMSEA targets):", nrow(x$successful), "\n\n")
  cat("Top", min(top, nrow(x$table)), "by composite loss:\n")
  print(utils::head(x$table, top), row.names = FALSE)
  if (!is.null(x$best)) {
    cat("\nBest model (", x$best$config_id, "):\n", sep = "")
    cat(sprintf("  CFI=%.4f | RMSEA=%.4f | SRMR=%.4f | loss=%.4f | meets=%s\n",
                x$best$indices$cfi, x$best$indices$rmsea,
                x$best$indices$srmr, x$best$loss, x$best$meets))
    cat("  Factor assignment:\n")
    for (f in names(x$best$factors)) {
      cat(sprintf("    %s: %s\n", f,
                  paste(x$best$factors[[f]], collapse = ", ")))
    }
    if (length(x$best$covs) > 0) {
      cat("  Residual covariances:\n")
      for (cv in x$best$covs) cat("    ", cv, "\n")
    }
  }
  invisible(x)
}
