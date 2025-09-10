export_conceptual_analysis <- function(resultado, file = "conceptual_analysis.txt") {
  # Asegurar encoding UTF-8
  sink(file, encoding = "UTF-8")
  print_conceptual_analysis(resultado, width = 100, show_stats = TRUE)
  sink()
  cat("Análisis exportado a:", file, "\n")
  invisible(file)
}
