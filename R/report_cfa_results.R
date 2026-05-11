#' Report the CFA boosting results
#'
#' Counterpart of \code{\link{report_efa_results}} for objects returned by
#' \code{\link{cfa_boosting}}. Side-effect: writes a human-readable report
#' to the console. Return value: an invisible \code{list} (class
#' \code{"cfa_boost_report"}) with the same information structured for
#' programmatic use, including a \code{text} field with the formatted
#' lines so any frontend can re-render exactly what the console showed.
#'
#' @param res A list produced by \code{cfa_boosting()}. Expected fields
#'   include \code{removed_items}, \code{added_covariances},
#'   \code{fit_indices}, \code{targets_met}, \code{iterations},
#'   \code{standardized_loadings}, \code{factor_correlations},
#'   \code{reliability}, \code{steps_log}, \code{final_syntax}.
#' @param show_plot Logical. If \code{TRUE} (default), include ASCII charts
#'   of RMSEA / CFI / SRMR evolution.
#' @param print Logical. If \code{TRUE} (default), write the formatted
#'   report to the console. Set to \code{FALSE} when only the structured
#'   list is needed.
#'
#' @return An invisible \code{list} with class \code{"cfa_boost_report"} —
#'   see Details below for fields.
#'
#' @details Fields of the returned list:
#'   \describe{
#'     \item{type}{\code{"cfa_boosting"}}
#'     \item{summary}{Single-row list with iterations, n_removed_items,
#'       n_added_covariances, targets_all_met}
#'     \item{fit_indices}{Final-model rmsea, cfi, tli, srmr, chisq, df}
#'     \item{targets_met}{Logical flags by index from \code{res$targets_met}}
#'     \item{removed_items}{Character vector}
#'     \item{added_covariances}{Character vector with "x ~~ y" strings}
#'     \item{standardized_loadings}{Loadings data.frame}
#'     \item{factor_correlations}{Phi matrix}
#'     \item{reliability}{Reliability table (composite/AVE/etc.)}
#'     \item{steps_log}{Per-iteration log}
#'     \item{final_syntax}{The final lavaan syntax used}
#'     \item{text}{Character vector — same lines that were printed}
#'   }
#'
#' @export
report_cfa_results <- function(res, show_plot = TRUE, print = TRUE) {
  rpt   <- .build_cfa_report(res)
  lines <- .format_cfa_report(rpt, show_plot = show_plot)
  rpt$text <- lines
  class(rpt) <- c("cfa_boost_report", "list")
  if (isTRUE(print)) cat(lines, sep = "\n")
  invisible(rpt)
}


#' @keywords internal
.build_cfa_report <- function(res) {
  fi <- res$fit_indices %||% list()
  tg <- res$targets_met %||% list()
  list(
    type = "cfa_boosting",
    summary = list(
      iterations          = res$iterations %||% NA_integer_,
      n_removed_items     = length(res$removed_items %||% character(0)),
      n_added_covariances = length(res$added_covariances %||% character(0)),
      targets_all_met     = isTRUE(tg$all_met)
    ),
    fit_indices = list(
      chisq = fi$chisq %||% NA_real_,
      df    = fi$df    %||% NA_real_,
      rmsea = fi$rmsea %||% NA_real_,
      cfi   = fi$cfi   %||% NA_real_,
      tli   = fi$tli   %||% NA_real_,
      srmr  = fi$srmr  %||% NA_real_
    ),
    targets_met            = tg,
    removed_items          = res$removed_items %||% character(0),
    added_covariances      = res$added_covariances %||% character(0),
    standardized_loadings  = res$standardized_loadings,
    factor_correlations    = res$factor_correlations,
    reliability            = res$reliability,
    steps_log              = res$steps_log,
    final_syntax           = res$final_syntax
  )
}


#' @keywords internal
.format_cfa_report <- function(rpt, show_plot = TRUE) {
  max_bars <- 20L
  out <- character(0)
  hr  <- paste(rep("─", 65), collapse = "")

  out <- c(out, "", hr,
           "ANÁLISIS FACTORIAL CONFIRMATORIO - REPORTE DE OPTIMIZACIÓN",
           hr, "")

  s  <- rpt$summary
  fi <- rpt$fit_indices
  tg <- rpt$targets_met

  out <- c(out, "RESUMEN DEL PROCESO", "")
  summary_df <- data.frame(
    Metrica = c("Iteraciones", "Items eliminados", "Covarianzas agregadas",
                 "Cumple todos los targets"),
    Valor   = c(s$iterations,
                 s$n_removed_items,
                 s$n_added_covariances,
                 if (isTRUE(s$targets_all_met)) "Si" else "No")
  )
  out <- c(out, utils::capture.output(
    print(summary_df, row.names = FALSE, right = FALSE)))

  if (length(rpt$removed_items) > 0L) {
    out <- c(out, "", paste("Items eliminados:",
                              paste(rpt$removed_items, collapse = ", ")))
  } else {
    out <- c(out, "", "Items eliminados: Ninguno")
  }
  if (length(rpt$added_covariances) > 0L) {
    out <- c(out, paste("Covarianzas agregadas:",
                          paste(rpt$added_covariances, collapse = "; ")))
  } else {
    out <- c(out, "Covarianzas agregadas: Ninguna")
  }

  if (isTRUE(show_plot) && !is.null(rpt$steps_log) &&
      is.data.frame(rpt$steps_log) && nrow(rpt$steps_log) > 0L) {

    if ("rmsea" %in% names(rpt$steps_log)) {
      out <- c(out, "", "", "EVOLUCIÓN DEL RMSEA", "")
      vals <- as.numeric(rpt$steps_log$rmsea)
      lo <- 0.05; hi <- 0.13
      for (i in seq_along(vals)) {
        v <- vals[i]
        bars <- round(max(0, min((v - lo) / (hi - lo) * max_bars, max_bars)))
        out <- c(out, sprintf("  Paso %2d [%.3f] %s",
                               i - 1, v,
                               if (bars > 0) paste(rep("█", bars), collapse = "") else ""))
      }
      out <- c(out, "                    └─────┴─────┴─────┴─────┘",
                    "                     0.05  0.08  0.10  0.13")
    }
    if ("cfi" %in% names(rpt$steps_log)) {
      out <- c(out, "", "", "EVOLUCIÓN DEL CFI", "")
      vals <- as.numeric(rpt$steps_log$cfi)
      lo <- 0.70; hi <- 1.00
      for (i in seq_along(vals)) {
        v <- vals[i]
        bars <- round(max(0, min((v - lo) / (hi - lo) * max_bars, max_bars)))
        out <- c(out, sprintf("  Paso %2d [%.3f] %s",
                               i - 1, v,
                               if (bars > 0) paste(rep("█", bars), collapse = "") else ""))
      }
      out <- c(out, "                    └─────┴─────┴─────┴─────┴─────┘",
                    "                     0.70  0.80  0.90  0.95  1.00")
    }
    if ("srmr" %in% names(rpt$steps_log)) {
      out <- c(out, "", "", "EVOLUCIÓN DEL SRMR", "")
      vals <- as.numeric(rpt$steps_log$srmr)
      lo <- 0.03; hi <- 0.10
      for (i in seq_along(vals)) {
        v <- vals[i]
        bars <- round(max(0, min((v - lo) / (hi - lo) * max_bars, max_bars)))
        out <- c(out, sprintf("  Paso %2d [%.3f] %s",
                               i - 1, v,
                               if (bars > 0) paste(rep("█", bars), collapse = "") else ""))
      }
      out <- c(out, "                    └─────┴─────┴─────┴─────┘",
                    "                     0.03  0.05  0.08  0.10")
    }
  }

  out <- c(out, "", "", "ÍNDICES DE AJUSTE DEL MODELO FINAL", "")
  fit_df <- data.frame(
    Indice = c("Chi-cuadrado", "gl", "RMSEA", "CFI", "TLI", "SRMR"),
    Valor  = c(sprintf("%.2f", fi$chisq), fi$df,
                sprintf("%.3f", fi$rmsea), sprintf("%.3f", fi$cfi),
                sprintf("%.3f", fi$tli),  sprintf("%.3f", fi$srmr)),
    Estado = c("", "",
                if (isTRUE(tg$rmsea_ok)) "OK" else "NO CUMPLE",
                if (isTRUE(tg$cfi_ok))   "OK" else "NO CUMPLE",
                "",
                if (isTRUE(tg$srmr_ok))  "OK" else "NO CUMPLE")
  )
  out <- c(out, utils::capture.output(print(fit_df, row.names = FALSE)))

  if (!is.null(rpt$standardized_loadings)) {
    out <- c(out, "", "", "CARGAS ESTANDARIZADAS (modelo final)", "")
    out <- c(out, utils::capture.output(print(rpt$standardized_loadings)))
  }
  if (!is.null(rpt$factor_correlations) &&
      is.matrix(rpt$factor_correlations) &&
      nrow(rpt$factor_correlations) > 1L) {
    out <- c(out, "", "", "CORRELACIONES INTER-FACTORIALES", "")
    out <- c(out, utils::capture.output(
      print(round(rpt$factor_correlations, 3))))
  }
  if (!is.null(rpt$reliability)) {
    out <- c(out, "", "", "FIABILIDAD COMPUESTA", "")
    out <- c(out, utils::capture.output(print(rpt$reliability)))
  }

  out <- c(out, "", "", "MODELO FINAL (sintaxis lavaan)", "")
  out <- c(out, strsplit(rpt$final_syntax %||% "", "\n")[[1]])

  out <- c(out, "", hr)
  out
}
