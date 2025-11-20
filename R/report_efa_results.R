report_efa_results <- function(res, show_plot = TRUE) {
  # Header
  cat("\n", paste(rep("─", 65), collapse = ""), "\n", sep = "")
  cat("ANÁLISIS FACTORIAL EXPLORATORIO - REPORTE DE OPTIMIZACIÓN\n")
  cat(paste(rep("─", 65), collapse = ""), "\n\n", sep = "")

  # Calcular métricas
  n_initial <- length(res$removed_items) + nrow(res$final_structure)
  n_final <- nrow(res$final_structure)
  pct_retained <- round(100 * n_final / n_initial, 1)
  rmsea_init <- res$steps_log$rmsea[1]
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

  # Evolución simplificada
  if (show_plot) {
    cat("\n\nEVOLUCIÓN DEL RMSEA\n\n")
    rmsea_all <- c(rmsea_init, res$steps_log$rmsea)
    for (i in seq_along(rmsea_all)) {
      bars <- round((rmsea_all[i] - 0.05) * 100)
      bars <- max(0, min(bars, 20))
      cat(sprintf("  Paso %2d [%.3f] %s\n",
                  i-1, rmsea_all[i],
                  paste(rep("█", bars), collapse = "")))
    }
    cat("                    └─────┴─────┴─────┴─────┘\n")
    cat("                     0.05  0.08  0.10  0.13\n")
  }

  # Detalles en secciones separadas
  cat("\n\nDETALLE DE ELIMINACIÓN DE ÍTEMS\n\n")
  elimination_df <- res$steps_log
  elimination_df$rmsea <- sprintf("%.3f", elimination_df$rmsea)
  print(elimination_df, row.names = FALSE)

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
    Interpretación = c("", "",
                       ifelse(bf$rmsea.scaled <= 0.05, "Excelente",
                              ifelse(bf$rmsea.scaled <= 0.08, "Bueno",
                                     ifelse(bf$rmsea.scaled <= 0.10, "Mediocre", "Pobre"))),
                       ifelse(bf$cfi.scaled >= 0.95, "Excelente",
                              ifelse(bf$cfi.scaled >= 0.90, "Bueno", "Pobre")),
                       ifelse(bf$tli.scaled >= 0.95, "Excelente",
                              ifelse(bf$tli.scaled >= 0.90, "Bueno", "Pobre")),
                       ifelse(bf$srmr <= 0.08, "Bueno", "Pobre"))
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
            res$interfactor_check$threshold, ")\n", sep="")
        cat("  Correlación mínima encontrada: ",
            sprintf("%.3f", res$interfactor_check$min_value), "\n", sep="")
      } else {
        cat("⚠️  ADVERTENCIA: No se cumplió el criterio de correlación mínima (>= ",
            res$interfactor_check$threshold, ")\n", sep="")
        cat("  Correlaciones que no cumplen:\n")
        for (viol in res$interfactor_check$violations) {
          cat("    - ", viol, "\n", sep="")
        }
        cat("  Correlación mínima encontrada: ",
            sprintf("%.3f", res$interfactor_check$min_value), "\n", sep="")
      }
    }
  }

  cat("\n", paste(rep("─", 65), collapse = ""), "\n", sep = "")
}
