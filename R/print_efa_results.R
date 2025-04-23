print_efa_results <- function(res) {

  # Helper para imprimir ítems eliminados
  print_removed_items <- function(removed_items) {
    msg <- if (length(removed_items) > 0) {
      paste(removed_items, collapse = ", ")
    } else {
      "Ninguno"
    }
    cat("Ítems eliminados:", msg, "\n")
  }

  cat("\n===== Resumen =====\n")
  print_removed_items(res$removed_items)
  cat("RMSEA final: ", res$final_rmsea, "\n", sep = "")
  cat("Iteraciones:   ", res$iterations, "\n", sep = "")

  cat("===== Estructura factorial (EFA UD) =====\n")
  print(res$final_structure)

  cat("\n===== Log de pasos =====\n")
  print(res$steps_log)

  cat("\n===== Bondades de ajuste finales =====\n")
  print(res$bondades_original)
}
