print_cfa_results <- function(res) {
  # Helper para imprimir ítems eliminados
  print_removed_items <- function(removed_items) {
    msg <- if (length(removed_items) > 0) {
      paste(removed_items, collapse = ", ")
    } else {
      "Ninguno"
    }
    cat("Ítems eliminados:", msg, "\n")
  }
  cat("===== Componentes disponibles =====\n")
  print(names(res))

  cat("\n===== Log de modificaciones =====\n")
  print(res$log)

  cat("\n===== Modelo final =====\n")
  cat(res$final_model, "\n")

  cat("\n===== Ítems eliminados =====\n")
  print_removed_items(res$removed_items)

  cat("\n===== Medidas finales =====\n")
  cat("  • RMSEA:", res$final_rmsea, "\n")
  cat("  • CFI:  ", res$final_cfi, "\n")

  cat("\n===== Ajuste final =====\n")
  print(lavaan::fitMeasures(res$final_fit, c("chisq.scaled", "df.scaled", "srmr","wrmr", "cfi.scaled", "tli.scaled","rmsea.scaled")))

  cat("\n===== Carga factoriales finales =====\n")
  print(lavaan::standardizedsolution(res$final_fit) %>% filter(op == "=~"))
  cat("\n===== Fiabilidad =====\n")
  semTools::compRelSEM(res$final_fit, tau.eq = FALSE, ord.scale = TRUE)
}
