#' Report the EFA boosting results
#'
#' Prints a human-readable optimisation report for the object returned by
#' \code{\link{efa_boosting}} (also called by \code{\link{run_efa_boosting}}).
#' Side-effect: writes a formatted block to the console. Return value: an
#' invisible \code{list} with the same information in a structured form, so
#' callers that need the data (e.g. a Shiny renderer) can capture it without
#' parsing console output.
#'
#' @param res A list produced by \code{efa_boosting()}. Must contain
#'   \code{removed_items}, \code{final_structure}, \code{steps_log},
#'   \code{final_rmsea}, \code{iterations}, \code{bondades_original},
#'   optionally \code{inter_factor_correlation} and \code{interfactor_check}.
#' @param show_plot Logical. If \code{TRUE} (default), print the ASCII
#'   evolution charts for RMSEA / SRMR / CFI.
#' @param print Logical. If \code{TRUE} (default), write the formatted report
#'   to the console via \code{cat()}. Set to \code{FALSE} to suppress the
#'   side-effect — useful when only the structured list is needed.
#'
#' @return An invisible \code{list} with class \code{"efa_boost_report"} and
#'   the following fields:
#'   \describe{
#'     \item{type}{\code{"efa_boosting"}}
#'     \item{summary}{Single-row list with n_initial, n_final, n_removed,
#'       pct_retained, rmsea_init, rmsea_final, rmsea_reduction_pct,
#'       iterations}
#'     \item{steps_log}{The data.frame with the per-iteration trail}
#'     \item{final_structure}{The loadings table that survived boosting}
#'     \item{fit_indices}{Final-model chisq, df, rmsea, cfi, tli, srmr}
#'     \item{inter_factor_correlation}{Phi matrix (if available)}
#'     \item{interfactor_check}{Inter-factor minimum-correlation check}
#'     \item{removed_items}{Character vector of dropped items}
#'     \item{text}{Character vector — the same lines that were printed,
#'       suitable for re-rendering in any frontend}
#'   }
#'
#' @export
report_efa_results <- function(res, show_plot = TRUE, print = TRUE) {
  rpt   <- .build_efa_report(res)
  lines <- .format_efa_report(rpt, show_plot = show_plot)
  rpt$text <- lines
  class(rpt) <- c("efa_boost_report", "list")
  if (isTRUE(print)) cat(lines, sep = "\n")
  invisible(rpt)
}


#' @keywords internal
.build_efa_report <- function(res) {
  n_initial    <- length(res$removed_items %||% character(0)) +
                    nrow(res$final_structure)
  n_final      <- nrow(res$final_structure)
  pct_retained <- round(100 * n_final / max(n_initial, 1), 1)

  rmsea_init  <- res$steps_log$rmsea[1]
  rmsea_final <- res$final_rmsea
  rmsea_reduction <- if (!is.null(rmsea_init) && rmsea_init > 0)
    round(100 * (rmsea_init - rmsea_final) / rmsea_init, 1)
  else NA_real_

  bf_last <- if (!is.null(res$bondades_original) &&
                  nrow(res$bondades_original) > 0L)
    res$bondades_original[nrow(res$bondades_original), ]
  else NULL

  list(
    type = "efa_boosting",
    summary = list(
      n_initial            = n_initial,
      n_final              = n_final,
      n_removed            = length(res$removed_items %||% character(0)),
      pct_retained         = pct_retained,
      rmsea_init           = rmsea_init,
      rmsea_final          = rmsea_final,
      rmsea_reduction_pct  = rmsea_reduction,
      iterations           = res$iterations %||% NA_integer_,
      stop_reason          = res$stop_reason %||% NA_character_
    ),
    steps_log         = res$steps_log,
    final_structure   = res$final_structure,
    fit_indices = if (!is.null(bf_last)) list(
      chisq = bf_last$chisq.scaled %||% NA_real_,
      df    = bf_last$df.scaled    %||% NA_real_,
      rmsea = bf_last$rmsea.scaled %||% NA_real_,
      cfi   = bf_last$cfi.scaled   %||% NA_real_,
      tli   = bf_last$tli.scaled   %||% NA_real_,
      srmr  = bf_last$srmr         %||% NA_real_
    ) else NULL,
    inter_factor_correlation = res$inter_factor_correlation,
    interfactor_check        = res$interfactor_check,
    removed_items            = res$removed_items %||% character(0)
  )
}


#' @keywords internal
.format_efa_report <- function(rpt, show_plot = TRUE) {
  max_bars <- 20L
  out <- character(0)
  hr  <- paste(rep("─", 65), collapse = "")

  out <- c(out, "", hr,
           "ANÁLISIS FACTORIAL EXPLORATORIO - REPORTE DE OPTIMIZACIÓN",
           hr, "")

  out <- c(out, "RESUMEN DEL PROCESO", "")
  s   <- rpt$summary
  summary_df <- data.frame(
    Metrica = c("Items iniciales", "Items finales", "Items eliminados",
                 "% Retenido", "RMSEA inicial", "RMSEA final",
                 "Reduccion RMSEA", "Iteraciones"),
    Valor   = c(s$n_initial, s$n_final, s$n_removed,
                 paste0(s$pct_retained, "%"),
                 sprintf("%.3f", s$rmsea_init),
                 sprintf("%.3f", s$rmsea_final),
                 paste0(s$rmsea_reduction_pct, "%"),
                 s$iterations)
  )
  out <- c(out, utils::capture.output(
    print(summary_df, row.names = FALSE, right = FALSE)))

  if (isTRUE(show_plot)) {
    out <- c(out, "", "", "EVOLUCIÓN DEL RMSEA", "")
    rmsea_all <- c(s$rmsea_init, rpt$steps_log$rmsea)
    rmsea_min_ref <- 0.05; rmsea_max_ref <- 0.13
    for (i in seq_along(rmsea_all)) {
      val  <- rmsea_all[i]
      bars <- (val - rmsea_min_ref) / (rmsea_max_ref - rmsea_min_ref) * max_bars
      bars <- round(max(0, min(bars, max_bars)))
      out <- c(out, sprintf("  Paso %2d [%.3f] %s",
                             i - 1, val,
                             if (bars > 0) paste(rep("█", bars), collapse = "") else ""))
    }
    out <- c(out, "                    └─────┴─────┴─────┴─────┘",
                  "                     0.05  0.08  0.10  0.13")

    if ("srmr" %in% names(rpt$steps_log)) {
      out <- c(out, "", "", "EVOLUCIÓN DEL SRMR", "")
      srmr_init <- rpt$steps_log$srmr[1]
      srmr_all  <- as.numeric(c(srmr_init, rpt$steps_log$srmr))
      srmr_min_ref <- 0.03; srmr_max_ref <- 0.10
      for (i in seq_along(srmr_all)) {
        val  <- srmr_all[i]
        bars <- (val - srmr_min_ref) / (srmr_max_ref - srmr_min_ref) * max_bars
        bars <- round(max(0, min(bars, max_bars)))
        out <- c(out, sprintf("  Paso %2d [%.3f] %s",
                               i - 1, val,
                               if (bars > 0) paste(rep("█", bars), collapse = "") else ""))
      }
      out <- c(out, "                    └─────┴─────┴─────┴─────┘",
                    "                     0.03  0.05  0.08  0.10")
    }

    if ("cfi" %in% names(rpt$steps_log)) {
      out <- c(out, "", "", "EVOLUCIÓN DEL CFI", "")
      cfi_init <- rpt$steps_log$cfi[1]
      cfi_all  <- as.numeric(c(cfi_init, rpt$steps_log$cfi))
      cfi_min_ref <- 0.70; cfi_max_ref <- 1.00
      for (i in seq_along(cfi_all)) {
        val  <- cfi_all[i]
        bars <- (val - cfi_min_ref) / (cfi_max_ref - cfi_min_ref) * max_bars
        bars <- round(max(0, min(bars, max_bars)))
        out <- c(out, sprintf("  Paso %2d [%.3f] %s",
                               i - 1, val,
                               if (bars > 0) paste(rep("█", bars), collapse = "") else ""))
      }
      out <- c(out, "                    └─────┴─────┴─────┴─────┴─────┘",
                    "                     0.70  0.80  0.90  0.95  1.00")
    }
  }

  out <- c(out, "", "", "DETALLE DE ELIMINACIÓN DE ÍTEMS", "")
  el_df <- rpt$steps_log
  num_cols <- intersect(c("rmsea", "srmr", "cfi"), names(el_df))
  if (length(num_cols) > 0) {
    el_df[num_cols] <- lapply(el_df[num_cols],
      function(x) sprintf("%.3f", as.numeric(x)))
  }
  base_cols <- c("step", "removed_item", "reason", "rmsea", "srmr", "cfi")
  ordered   <- c(intersect(base_cols, names(el_df)),
                  setdiff(names(el_df), base_cols))
  out <- c(out, utils::capture.output(
    print(el_df[, ordered, drop = FALSE], row.names = FALSE)))

  out <- c(out, "", "", "ESTRUCTURA FACTORIAL FINAL (cargas > 0.30)", "")
  out <- c(out, utils::capture.output(print(rpt$final_structure, n = Inf)))

  if (!is.null(rpt$fit_indices)) {
    out <- c(out, "", "", "ÍNDICES DE AJUSTE DEL MODELO FINAL", "")
    fi <- rpt$fit_indices
    fit_df <- data.frame(
      Indice = c("Chi-cuadrado escalado", "gl", "RMSEA", "CFI", "TLI", "SRMR"),
      Valor  = c(sprintf("%.2f", fi$chisq), fi$df,
                  sprintf("%.3f", fi$rmsea), sprintf("%.3f", fi$cfi),
                  sprintf("%.3f", fi$tli),  sprintf("%.3f", fi$srmr)),
      Interpretacion = c("", "",
        ifelse(fi$rmsea <= 0.05, "Excelente",
        ifelse(fi$rmsea <= 0.08, "Bueno",
        ifelse(fi$rmsea <= 0.10, "Mediocre", "Pobre"))),
        ifelse(fi$cfi >= 0.95, "Excelente",
        ifelse(fi$cfi >= 0.90, "Bueno", "Pobre")),
        ifelse(fi$tli >= 0.95, "Excelente",
        ifelse(fi$tli >= 0.90, "Bueno", "Pobre")),
        ifelse(fi$srmr <= 0.08, "Bueno", "Pobre"))
    )
    out <- c(out, utils::capture.output(print(fit_df, row.names = FALSE)))
  }

  if (!is.null(rpt$inter_factor_correlation) &&
      nrow(rpt$inter_factor_correlation) > 1) {
    out <- c(out, "", "", "CORRELACIONES INTER-FACTORIALES (PHI)", "")
    out <- c(out, utils::capture.output(
      print(round(rpt$inter_factor_correlation, 3))))
    if (!is.null(rpt$interfactor_check)) {
      out <- c(out, "")
      if (isTRUE(rpt$interfactor_check$criteria_met)) {
        out <- c(out, sprintf(
          "[OK] Criterio de correlacion minima cumplido (todas >= %.2f)",
          rpt$interfactor_check$threshold))
        out <- c(out, sprintf("     Minima encontrada: %.3f",
                               rpt$interfactor_check$min_value))
      } else {
        out <- c(out, sprintf(
          "[!!] No se cumplio el criterio de correlacion minima (>= %.2f)",
          rpt$interfactor_check$threshold))
        for (viol in rpt$interfactor_check$violations %||% character(0))
          out <- c(out, paste0("     - ", viol))
        out <- c(out, sprintf("     Minima encontrada: %.3f",
                               rpt$interfactor_check$min_value))
      }
    }
  }
  out <- c(out, "", hr)
  out
}

`%||%` <- function(a, b) if (is.null(a)) b else a
