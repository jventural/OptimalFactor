export_conceptual_analysis <- function(resultado, file) {
  if (missing(file) || is.null(file) || !nzchar(file))
    stop("'file' is required, e.g. file = file.path(tempdir(), \"conceptual_analysis.txt\").",
         call. = FALSE)
  con <- file(file, open = "w", encoding = "UTF-8")
  # Asegura que se cierren sink/connection aunque falle algo
  open_sinks <- sink.number(type = "output")
  on.exit({
    while (sink.number(type = "output") > open_sinks) sink(type = "output")
    close(con)
  }, add = TRUE)

  sink(con, type = "output")
  print_conceptual_analysis(resultado, width = 100, show_stats = TRUE)
  sink(type = "output")  # cierra el desv\u00EDo
  message("An\u00E1lisis exportado a: ", normalizePath(file, winslash = "/"))
  invisible(file)
}
