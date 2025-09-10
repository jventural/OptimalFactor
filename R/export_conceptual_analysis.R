export_conceptual_analysis <- function(resultado, file = "conceptual_analysis.txt") {
  con <- file(file, open = "w", encoding = "UTF-8")
  # Asegura que se cierren sink/connection aunque falle algo
  open_sinks <- sink.number(type = "output")
  on.exit({
    while (sink.number(type = "output") > open_sinks) sink(type = "output")
    close(con)
  }, add = TRUE)

  sink(con, type = "output")
  print_conceptual_analysis(resultado, width = 100, show_stats = TRUE)
  sink(type = "output")  # cierra el desvío
  cat("Análisis exportado a:", normalizePath(file, winslash = "/"), "\n")
  invisible(file)
}
