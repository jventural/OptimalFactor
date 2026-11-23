# Internal helper: the statements of a lavaan model, one per element. Comments
# (# and !) are dropped, ';' separates statements, and a statement may span
# several lines, as lavaan allows. Reading the text line by line used to lose,
# without a word, the items written on a continuation line.
.of_model_lines <- function(model) {
  txt <- paste(model, collapse = "\n")
  txt <- gsub("\r\n?", "\n", txt)
  txt <- gsub("[#!][^\n]*", "", txt)
  txt <- gsub(";", "\n", txt, fixed = TRUE)
  txt <- gsub("([+~*=])[[:space:]]*\n[[:space:]]*", "\\1 ", txt)
  txt <- gsub("[[:space:]]*\n[[:space:]]*([+*])", " \\1", txt)
  txt <- gsub("[[:space:]]*\n[[:space:]]*(=~|~~)", " \\1", txt)
  lines <- trimws(strsplit(txt, "\n", fixed = TRUE)[[1]])
  lines[nzchar(lines)]
}

# Internal helper: factor -> items, joining the lines of a factor declared twice.
# Modifiers are kept ("NA*x1").
.of_model_factors <- function(lines) {
  out <- list()
  for (l in lines[grepl("=~", lines, fixed = TRUE)]) {
    parts <- strsplit(l, "=~", fixed = TRUE)[[1]]
    f <- trimws(parts[1])
    out[[f]] <- unique(c(out[[f]], trimws(strsplit(parts[2], "+", fixed = TRUE)[[1]])))
  }
  out
}

# Internal helper: the variable of a term, without its modifier ("NA*x1" -> "x1").
.of_item_name <- function(x) {
  if (!length(x)) return(character(0))
  trimws(sub("^.*[*]", "", x))
}

# Internal helper: the terms left after dropping some variables.
.of_drop_items <- function(terms, items) terms[!(.of_item_name(terms) %in% items)]
