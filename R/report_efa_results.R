report_efa_results <- function(res, show_plot = TRUE) {
  # Parámetro global de ancho máximo de barra
  max_bars <- 20

  # Header
  cat("\n", paste(rep("─", 65), collapse = ""), "\n", sep = "")
  cat("ANÁLISIS FACTORIAL EXPLORATORIO - REPORTE DE OPTIMIZACIÓN\n")
  cat(paste(rep("─", 65), collapse = ""), "\n\n", sep = "")

  # Calcular métricas globales
  n_initial <- length(res$removed_items) + nrow(res$final_structure)
  n_final   <- nrow(res$final_structure)
  pct_retained <- round(100 * n_final / n_initial, 1)

  rmsea_init      <- res$steps_log$rmsea[1]
  rmsea_reduction <- round(100 * (rmsea_init - res$final_rmsea) / rmsea_init, 1)

  # Resumen en tabla
  cat("RESUMEN DEL PROCESO\n\n")
  summary_df <- data.frame(
    Métrica = c("Ítems iniciales", "Ítems finales", "Ítems eliminados",
                "% Retenido", "RMSEA inicial", "RMSEA final",
                "Reducción RMSEA", "Iteraciones"),
    Valor = c(n_initial, n_final, length(res$removed_items),
              paste0(pct_retained, "%"),
              sprintf("%.3f", rmsea_init),
              sprintf("%.3f", res$final_rmsea),
              paste0(rmsea_reduction, "%"),
              res$iterations)
  )
  print(summary_df, row.names = FALSE, right = FALSE)

  # Evolución: RMSEA, SRMR y CFI por separado
  if (show_plot) {
    ## ----------------- RMSEA -----------------
    cat("\n\nEVOLUCIÓN DEL RMSEA\n\n")
    rmsea_all <- c(rmsea_init, res$steps_log$rmsea)

    # Rango de referencia para escalar
    rmsea_min_ref <- 0.05
    rmsea_max_ref <- 0.13

    for (i in seq_along(rmsea_all)) {
      val  <- rmsea_all[i]
      bars <- (val - rmsea_min_ref) / (rmsea_max_ref - rmsea_min_ref) * max_bars
      bars <- round(max(0, min(bars, max_bars)))
      cat(sprintf("  Paso %2d [%.3f] %s\n",
                  i - 1, val,
                  if (bars > 0) paste(rep("█", bars), collapse = "") else ""))
    }
    cat("                    └─────┴─────┴─────┴─────┘\n")
    cat("                     0.05  0.08  0.10  0.13\n")

    ## ----------------- SRMR ------------------
    if ("srmr" %in% names(res$steps_log)) {
      cat("\n\nEVOLUCIÓN DEL SRMR\n\n")
      srmr_init <- res$steps_log$srmr[1]
      srmr_all  <- c(srmr_init, res$steps_log$srmr)
      srmr_all  <- as.numeric(srmr_all)

      srmr_min_ref <- 0.03
      srmr_max_ref <- 0.10

      for (i in seq_along(srmr_all)) {
        val  <- srmr_all[i]
        bars <- (val - srmr_min_ref) / (srmr_max_ref - srmr_min_ref) * max_bars
        bars <- round(max(0, min(bars, max_bars)))
        cat(sprintf("  Paso %2d [%.3f] %s\n",
                    i - 1, val,
                    if (bars > 0) paste(rep("█", bars), collapse = "") else ""))
      }
      cat("                    └─────┴─────┴─────┴─────┘\n")
      cat("                     0.03  0.05  0.08  0.10\n")
    } else {
      cat("\n\nEVOLUCIÓN DEL SRMR\n\n")
      cat("  (SRMR no disponible en steps_log)\n")
    }

    ## ------------------ CFI ------------------
    if ("cfi" %in% names(res$steps_log)) {
      cat("\n\nEVOLUCIÓN DEL CFI\n\n")
      cfi_init <- res$steps_log$cfi[1]
      cfi_all  <- c(cfi_init, res$steps_log$cfi)
      cfi_all  <- as.numeric(cfi_all)

      # Escala lineal: 0.70 -> 0 barras, 1.00 -> max_bars
      cfi_min_ref <- 0.70
      cfi_max_ref <- 1.00

      for (i in seq_along(cfi_all)) {
        val  <- cfi_all[i]
        bars <- (val - cfi_min_ref) / (cfi_max_ref - cfi_min_ref) * max_bars
        bars <- round(max(0, min(bars, max_bars)))
        cat(sprintf("  Paso %2d [%.3f] %s\n",
                    i - 1, val,
                    if (bars > 0) paste(rep("█", bars), collapse = "") else ""))
      }
      cat("                    └─────┴─────┴─────┴─────┴─────┘\n")
      cat("                     0.70  0.80  0.90  0.95  1.00\n")
    } else {
      cat("\n\nEVOLUCIÓN DEL CFI\n\n")
      cat("  (CFI no disponible en steps_log)\n")
    }
  }

  # Detalle de eliminación de ítems
  cat("\n\nDETALLE DE ELIMINACIÓN DE ÍTEMS\n\n")
  elimination_df <- res$steps_log

  # Formatear columnas de ajuste si existen
  num_cols <- intersect(c("rmsea", "srmr", "cfi"), names(elimination_df))
  if (length(num_cols) > 0) {
    elimination_df[num_cols] <- lapply(
      elimination_df[num_cols],
      function(x) sprintf("%.3f", as.numeric(x))
    )
  }

  # Orden de columnas: step, removed_item, reason, rmsea, srmr, cfi, luego el resto
  base_cols      <- c("step", "removed_item", "reason", "rmsea", "srmr", "cfi")
  remaining_cols <- setdiff(names(elimination_df), base_cols)
  ordered_cols   <- c(intersect(base_cols, names(elimination_df)), remaining_cols)

  print(elimination_df[, ordered_cols, drop = FALSE], row.names = FALSE)

  # Estructura factorial final
  cat("\n\nESTRUCTURA FACTORIAL FINAL (cargas > 0.30)\n\n")
  print(res$final_structure, n = Inf)

  # Métricas de ajuste finales
  cat("\n\nÍNDICES DE AJUSTE DEL MODELO FINAL\n\n")
  bf <- res$bondades_original[nrow(res$bondades_original), ]
  fit_df <- data.frame(
    Índice = c("χ² escalado", "gl", "RMSEA", "CFI", "TLI", "SRMR"),
    Valor = c(sprintf("%.2f", bf$chisq.scaled),
              bf$df.scaled,
              sprintf("%.3f", bf$rmsea.scaled),
              sprintf("%.3f", bf$cfi.scaled),
              sprintf("%.3f", bf$tli.scaled),
              sprintf("%.3f", bf$srmr)),
    Interpretación = c(
      "",
      "",
      ifelse(bf$rmsea.scaled <= 0.05, "Excelente",
             ifelse(bf$rmsea.scaled <= 0.08, "Bueno",
                    ifelse(bf$rmsea.scaled <= 0.10, "Mediocre", "Pobre"))),
      ifelse(bf$cfi.scaled >= 0.95, "Excelente",
             ifelse(bf$cfi.scaled >= 0.90, "Bueno", "Pobre")),
      ifelse(bf$tli.scaled >= 0.95, "Excelente",
             ifelse(bf$tli.scaled >= 0.90, "Bueno", "Pobre")),
      ifelse(bf$srmr <= 0.08, "Bueno", "Pobre")
    )
  )
  print(fit_df, row.names = FALSE)

  # Correlaciones inter-factoriales
  if (!is.null(res$inter_factor_correlation) && nrow(res$inter_factor_correlation) > 1) {
    cat("\n\nCORRELACIONES INTER-FACTORIALES (PHI)\n\n")
    print(round(res$inter_factor_correlation, 3))

    # Verificación del criterio de correlación mínima
    if (!is.null(res$interfactor_check)) {
      cat("\n")
      if (res$interfactor_check$criteria_met) {
        cat("✓ Criterio de correlación mínima cumplido (todas >= ",
            res$interfactor_check$threshold, ")\n", sep = "")
        cat("  Correlación mínima encontrada: ",
            sprintf("%.3f", res$interfactor_check$min_value), "\n", sep = "")
      } else {
        cat("⚠️  ADVERTENCIA: No se cumplió el criterio de correlación mínima (>= ",
            res$interfactor_check$threshold, ")\n", sep = "")
        cat("  Correlaciones que no cumplen:\n")
        for (viol in res$interfactor_check$violations) {
          cat("    - ", viol, "\n", sep = "")
        }
        cat("  Correlación mínima encontrada: ",
            sprintf("%.3f", res$interfactor_check$min_value), "\n", sep = "")
      }
    }
  }

  cat("\n", paste(rep("─", 65), collapse = ""), "\n", sep = "")
}
