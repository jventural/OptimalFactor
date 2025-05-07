print_conceptual_analysis <- function(resultado) {
  ca <- resultado$conceptual_analysis

  if (is.null(ca)) {
    cat("No hay análisis conceptual disponible.\n")
    return(invisible(NULL))
  }

  # Ítems eliminados
  if (!is.null(ca$excluded) && length(ca$excluded) > 0) {
    cat("\n===== Análisis conceptual de ítems eliminados =====\n")
    for (it in names(ca$excluded)) {
      cat(paste0("\n", it, ":\n"))
      cat(ca$excluded[[it]], "\n")
    }
  } else {
    cat("\nNo hay análisis conceptual de ítems eliminados.\n")
  }

  # Ítems conservados
  if (!is.null(ca$conserved) && length(ca$conserved) > 0) {
    cat("\n===== Análisis conceptual de ítems conservados =====\n")
    for (it in names(ca$conserved)) {
      cat(paste0("\n", it, ":\n"))
      cat(ca$conserved[[it]], "\n")
    }
  } else {
    cat("\nNo hay análisis conceptual de ítems conservados.\n")
  }

  invisible(NULL)
}
