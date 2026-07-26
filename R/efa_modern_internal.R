# ─────────────────────────────────────────────────────────────────────────────
# Internal EFA engine
#
# Until version 1.2.2 the package delegated every exploratory factor analysis
# to PsyMetricTools::EFA_modern(). That made OptimalFactor depend on a package
# that is not on CRAN, which blocked its own submission. The four helpers below
# reproduce that pipeline (model syntax -> lavaan fit -> fit measures ->
# standardized pattern matrix) with the same numerical behaviour but using only
# the dependencies OptimalFactor already declares.
#
# The lavaan call is deliberately identical to the original one (same
# estimator, rotation, mimic = "Mplus" and ordered = TRUE defaults), so results
# obtained with previous versions are reproduced exactly.
# ─────────────────────────────────────────────────────────────────────────────

# Build lavaan EFA syntax for the 1..n_factors solutions.
.of_generate_models <- function(n_factors, specific_items = NULL,
                                name_items = NULL, n_items = NULL,
                                exclude_items = NULL) {
  var_names <- if (!is.null(specific_items)) specific_items else paste0(name_items, 1:n_items)
  if (!is.null(exclude_items)) var_names <- setdiff(var_names, exclude_items)
  if (length(var_names) == 0L) stop("No items left to fit the EFA model.")
  var_sum <- paste(var_names, collapse = "+")
  lapply(seq_len(n_factors), function(i) {
    factors <- paste0("efa(\"efa\")*f", seq_len(i), collapse = " +\n")
    paste0(factors, " =~\n", var_sum)
  })
}

# Fit every candidate solution with lavaan.
.of_specification_models <- function(modelos, data, estimator, rotation = "oblimin",
                                     ordered = TRUE, verbose = FALSE) {
  if (!requireNamespace("lavaan", quietly = TRUE))
    stop("Package 'lavaan' is required but not installed.")
  out <- vector("list", length(modelos))
  for (i in seq_along(modelos)) {
    out[[i]] <- lavaan::cfa(modelos[[i]],
                            data      = data,
                            estimator = estimator,
                            rotation  = rotation,
                            mimic     = "Mplus",
                            ordered   = ordered,
                            verbose   = verbose)
    if (verbose) print(out[[i]])
  }
  out
}

# Fit measures for every solution, one row per number of factors.
.of_extract_fit_measures <- function(Specifications) {
  keep <- c("chisq.scaled", "df.scaled", "srmr", "wrmr",
            "cfi.scaled", "tli.scaled", "rmsea.scaled")
  rows <- lapply(Specifications, function(fit) {
    fm <- tryCatch(lavaan::fitMeasures(fit, keep), error = function(e) NULL)
    if (is.null(fm)) return(stats::setNames(rep(NA_real_, length(keep)), keep))
    # Some estimators do not return every scaled index; keep the layout stable.
    out <- stats::setNames(rep(NA_real_, length(keep)), keep)
    nm  <- intersect(names(fm), keep)
    out[nm] <- as.numeric(fm[nm])
    out
  })
  mat <- do.call(rbind, lapply(rows, function(x) as.data.frame(as.list(x),
                                                               stringsAsFactors = FALSE)))
  mat <- round(mat, 3)
  data.frame(Factores = paste0("f", seq_along(Specifications)), mat,
             stringsAsFactors = FALSE, check.names = FALSE)
}

# Standardized pattern matrix, items in rows and f1..fk in columns.
.of_standardized_solutions <- function(specification, name_items = NULL,
                                       apply_threshold = TRUE) {
  if (!requireNamespace("tidyr", quietly = TRUE))
    stop("Package 'tidyr' is required but not installed.")
  s <- lavaan::standardizedsolution(specification)
  s <- s[s$op == "=~", c("lhs", "rhs", "est.std"), drop = FALSE]
  result <- tidyr::pivot_wider(s, names_from = "lhs", values_from = "est.std")
  result <- as.data.frame(result, stringsAsFactors = FALSE)

  fcols <- which(startsWith(names(result), "f"))
  if (apply_threshold)
    result[fcols] <- lapply(result[fcols], function(x) ifelse(abs(x) > 0.30, x, 0))

  # Order items by their primary factor and, within it, by loading size.
  L <- abs(as.matrix(result[, fcols, drop = FALSE]))
  primary  <- vapply(seq_len(nrow(L)), function(i)
    if (all(is.na(L[i, ]))) NA_integer_ else as.integer(which.max(L[i, ])), integer(1))
  strength <- vapply(seq_len(nrow(L)), function(i)
    if (all(is.na(L[i, ]))) NA_real_ else max(L[i, ], na.rm = TRUE), numeric(1))
  result <- result[order(primary, -strength), , drop = FALSE]
  rownames(result) <- NULL

  names(result)[names(result) == "rhs"] <- "Items"
  result
}

# Drop-in replacement for PsyMetricTools::EFA_modern().
.of_efa_modern <- function(n_factors, n_items, name_items, data,
                           apply_threshold = FALSE, estimator = "WLSMV",
                           rotation = "oblimin", exclude_items = NULL,
                           only_target = FALSE, ...) {
  modelos <- .of_generate_models(n_factors = n_factors, n_items = n_items,
                                 name_items = name_items,
                                 exclude_items = exclude_items)

  if (only_target) {
    # Candidate evaluation reads the n_factors solution and nothing else, so the
    # 1..n_factors-1 models are fitted and thrown away. Skipping them leaves the
    # returned object with the same shape: the unused slots stay empty and the
    # fit table keeps one row per number of factors, with NA where no model was
    # fitted, so Bondades_Original[[index]][n_factors] still resolves.
    fitted_target <- .of_specification_models(modelos[n_factors], data = data,
                                              estimator = estimator,
                                              rotation = rotation, ...)
    Specifications <- vector("list", n_factors)
    Specifications[[n_factors]] <- fitted_target[[1L]]
  } else {
    Specifications <- .of_specification_models(modelos, data = data,
                                               estimator = estimator,
                                               rotation = rotation, ...)
  }
  list(
    Bondades_Original = .of_extract_fit_measures(Specifications),
    Specifications    = Specifications,
    InterFactor       = lavaan::inspect(Specifications[[n_factors]], what = "std")$psi,
    result_df         = .of_standardized_solutions(Specifications[[n_factors]],
                                                   name_items = name_items,
                                                   apply_threshold = apply_threshold)
  )
}
