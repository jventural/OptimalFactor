imprimir_items_eliminados <- function(removed_items) {
  mensaje <- if (length(removed_items) > 0) {
    paste(removed_items, collapse = ", ")
  } else {
    "Ninguno"
  }
  cat("\u00CDtems eliminados: ", mensaje, "\n", sep = "")
}
