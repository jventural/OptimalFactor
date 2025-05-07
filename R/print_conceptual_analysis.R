print_conceptual_analysis <- function(resultado) {
  ca <- resultado$conceptual_analysis

  if (is.null(ca)) {
    cat("No hay análisis conceptual disponible.\n")
    return(invisible(NULL))
  }

  # Ítems eliminados
  if (!is.null(ca$removed) && length(ca$removed) > 0) {
    cat("\n===== Análisis conceptual de ítems eliminados =====\n")
    for (it in names(ca$removed)) {
      cat(paste0("\n", it, ":\n"))
      cat(ca$removed[[it]], "\n")
    }
  } else {
    cat("\nNo hay análisis conceptual de ítems eliminados.\n")
  }

  # Ítems conservados
  if (!is.null(ca$kept) && length(ca$kept) > 0) {
    cat("\n===== Análisis conceptual de ítems conservados =====\n")
    for (it in names(ca$kept)) {
      cat(paste0("\n", it, ":\n"))
      cat(ca$kept[[it]], "\n")
    }
  } else {
    cat("\nNo hay análisis conceptual de ítems conservados.\n")
  }

  invisible(NULL)
}
