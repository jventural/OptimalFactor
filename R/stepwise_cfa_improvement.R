stepwise_cfa_improvement <- function(initial_model,
                                     data,
                                     rmsea_threshold = 0.08,
                                     mi_threshold = 3.84,
                                     max_steps = 10,
                                     verbose = TRUE,
                                     debug = FALSE,
                                     filter_expr = NULL,
                                     ...) {

  get_item_factor_mapping <- function(model_str) {
    # Convierte el modelo en un vector de líneas utilizando ; o \n como separadores
    lines <- unlist(strsplit(model_str, "[;\n]"))
    mapping <- list()

    for (line in lines) {
      line <- trimws(line)
      # Verifica si la línea contiene una especificación factorial con '=~'
      if (grepl("=~", line)) {
        parts <- unlist(strsplit(line, "=~"))
        factor_name <- trimws(parts[1])
        # Separa los ítems (asumiendo que están unidos por '+')
        items <- unlist(strsplit(parts[2], "\\+"))
        items <- trimws(items)
        # Asigna cada ítem a su factor correspondiente
        for (item in items) {
          mapping[[item]] <- factor_name
        }
      }
    }

    return(mapping)
  }
  # Función interna para extraer la correspondencia ítem-factor
  get_item_factor_mapping <- function(model_str) {
    lines <- unlist(strsplit(model_str, "[;\n]"))
    mapping <- list()

    for (line in lines) {
      line <- trimws(line)
      if (grepl("=~", line)) {
        parts <- unlist(strsplit(line, "=~"))
        factor_name <- trimws(parts[1])
        items <- unlist(strsplit(parts[2], "\\+"))
        items <- trimws(items)
        for (item in items) {
          mapping[[item]] <- factor_name
        }
      }
    }
    return(mapping)
  }

  # Función interna para eliminar ítems de la especificación del modelo,
  # tanto en las líneas de medición (operador "=~") como en las correlaciones de error ("~~")
  remove_items_from_model <- function(model_str, items_to_remove) {
    model_lines <- unlist(strsplit(model_str, "\n"))
    new_model_lines <- c()
    for (line in model_lines) {
      if (grepl("=~", line)) {
        # Línea de medición factorial
        parts <- unlist(strsplit(line, "=~"))
        factor_name <- trimws(parts[1])
        items <- unlist(strsplit(parts[2], "\\+"))
        items <- trimws(items)
        items_filtered <- items[!(items %in% items_to_remove)]
        if (length(items_filtered) > 0) {
          new_line <- paste(factor_name, "=~", paste(items_filtered, collapse = " + "))
          new_model_lines <- c(new_model_lines, new_line)
        } else {
          if (verbose) {
            cat("Se eliminó la especificación del factor", factor_name,
                "por remover todos sus ítems offending.\n")
          }
        }
      } else if (grepl("~~", line)) {
        # Línea de correlación de error: se elimina si contiene alguno de los ítems offending
        parts <- unlist(strsplit(line, "~~"))
        parts <- trimws(parts)
        if (any(parts %in% items_to_remove)) {
          if (verbose) {
            cat("Se eliminó la correlación de error por contener ítem offending:", line, "\n")
          }
          # No se agrega esta línea
        } else {
          new_model_lines <- c(new_model_lines, line)
        }
      } else {
        new_model_lines <- c(new_model_lines, line)
      }
    }
    paste(new_model_lines, collapse = "\n")
  }

  # 1. Filtrar la data si filter_expr no es NULL
  if (!is.null(filter_expr)) {
    data <- dplyr::filter(data, !!rlang::parse_expr(filter_expr))
  }

  # 2. Preservar saltos de línea y eliminar retornos de carro (\r)
  clean_model <- gsub("\\r", "", initial_model)
  clean_model <- trimws(clean_model)

  # 3. Ajuste inicial para obtener los loadings
  fit_initial <- lavaan::cfa(clean_model, data = data, ...)
  loadings_initial <- standardizedsolution(fit_initial) %>%
    dplyr::filter(op == "=~") %>%
    dplyr::select(rhs, est.std)

  # 4. Verificar que cada ítem tenga un loading absoluto >= 0.30
  removed_items <- c()  # ítems removidos por loadings bajos
  for (i in 1:nrow(loadings_initial)) {
    item <- loadings_initial$rhs[i]
    loading <- abs(loadings_initial$est.std[i])
    if (loading < 0.30) {
      removed_items <- c(removed_items, item)
    }
  }

  # Eliminar ítems con loadings bajos de la especificación
  if (length(removed_items) > 0) {
    if (verbose) {
      cat("Se han removido los siguientes ítems por tener loading < 0.30:",
          paste(removed_items, collapse = ", "), "\n")
    }
    model_lines <- unlist(strsplit(clean_model, "\n"))
    new_model_lines <- c()
    for (line in model_lines) {
      if (grepl("=~", line)) {
        parts <- unlist(strsplit(line, "=~"))
        factor_name <- trimws(parts[1])
        items <- unlist(strsplit(parts[2], "\\+"))
        items <- trimws(items)
        items_filtered <- items[!(items %in% removed_items)]
        if (length(items_filtered) > 0) {
          new_line <- paste(factor_name, "=~", paste(items_filtered, collapse = " + "))
          new_model_lines <- c(new_model_lines, new_line)
        } else {
          if (verbose) {
            cat("Se eliminó la especificación del factor", factor_name,
                "por remover todos sus ítems.\n")
          }
        }
      } else {
        new_model_lines <- c(new_model_lines, line)
      }
    }
    clean_model <- paste(new_model_lines, collapse = "\n")
  }

  # 5. Extraer la correspondencia ítem-factor a partir del modelo limpio
  mapping <- get_item_factor_mapping(clean_model)

  # 6. Inicializar variables de control
  current_model <- clean_model
  step <- 0
  log_steps <- data.frame(step = integer(),
                          modification = character(),
                          mi_value = numeric(),
                          rmsea = numeric(),
                          stringsAsFactors = FALSE)
  fit <- NULL

  repeat {
    step <- step + 1
    # Ajuste del modelo actual
    fit <- lavaan::cfa(current_model, data = data, ...)
    rmsea_val <- lavaan::fitMeasures(fit, "rmsea.scaled")

    if (verbose) {
      cat("Paso", step, "- rmsea.scaled:", round(rmsea_val, 3), "\n")
    }

    if (rmsea_val < rmsea_threshold) {
      if (verbose) {
        cat("Ajuste aceptable logrado (rmsea.scaled <", rmsea_threshold, ").\n")
      }
      break
    }
    if (step >= max_steps) {
      if (verbose) {
        cat("Se alcanzó el máximo de pasos sin lograr un ajuste aceptable.\n")
      }
      break
    }

    # 7. Obtener índices de modificación para correlaciones de error (op = "~~")
    mi_table <- lavaan::modificationIndices(
      fit,
      alpha = 0.05,
      minimum.value = mi_threshold,
      power = TRUE,
      delta = 0.1,
      high.power = 0.75,
      sort. = TRUE,
      op = "~~"
    )
    mi_candidates <- subset(mi_table, mi > mi_threshold)

    # Excluir correlaciones entre ítems de distintos factores
    if (nrow(mi_candidates) > 0) {
      mi_candidates <- mi_candidates[!(
        mi_candidates$op == "~~" &
          sapply(mi_candidates$lhs, function(x) {
            if (is.null(mapping[[x]])) NA else mapping[[x]]
          }) != sapply(mi_candidates$rhs, function(x) {
            if (is.null(mapping[[x]])) NA else mapping[[x]]
          })
      ), ]
    }

    if (nrow(mi_candidates) == 0) {
      if (verbose) {
        cat("No se encontraron índices de modificación significativos y lógicamente aceptables. Deteniendo el proceso.\n")
      }
      break
    }

    # 8. Seleccionar la modificación con mayor MI
    best_candidate <- mi_candidates[which.max(mi_candidates$mi), , drop = FALSE]
    if (debug) {
      cat("DEBUG: Estructura de best_candidate:\n")
      str(best_candidate)
    }

    mod_spec <- paste(as.character(best_candidate$lhs),
                      as.character(best_candidate$op),
                      as.character(best_candidate$rhs))
    if (length(mod_spec) == 0 || mod_spec == "") {
      if (verbose) {
        cat("La modificación generada es vacía. Deteniendo el proceso.\n")
      }
      break
    }
    if (debug) {
      cat("DEBUG: mod_spec generado:\n")
      print(mod_spec)
      cat("DEBUG: current_model:\n")
      print(current_model)
    }

    # Evitar duplicados en el modelo
    if (grepl(mod_spec, current_model, fixed = TRUE)) {
      if (verbose) {
        cat("La modificación ya existe en el modelo. Se busca otra modificación.\n")
      }
      mi_candidates <- mi_candidates[!(mi_candidates$lhs == best_candidate$lhs &
                                         mi_candidates$op == best_candidate$op &
                                         mi_candidates$rhs == best_candidate$rhs), ]
      if (nrow(mi_candidates) == 0) {
        if (verbose) {
          cat("No hay modificaciones nuevas disponibles. Deteniendo el proceso.\n")
        }
        break
      }
      best_candidate <- mi_candidates[which.max(mi_candidates$mi), , drop = FALSE]
      mod_spec <- paste(as.character(best_candidate$lhs),
                        as.character(best_candidate$op),
                        as.character(best_candidate$rhs))
    }

    # 9. Agregar la modificación al modelo
    current_model <- paste(current_model, mod_spec, sep = "\n")
    if (verbose) {
      cat("Paso", step, "- Se añade la modificación:", mod_spec,
          "con MI =", round(best_candidate$mi, 3), "\n")
    }
    log_steps <- rbind(
      log_steps,
      data.frame(step = step,
                 modification = mod_spec,
                 mi_value = best_candidate$mi,
                 rmsea = rmsea_val,
                 stringsAsFactors = FALSE)
    )

    # 10. Verificar loadings tras la modificación
    fit_temp <- lavaan::cfa(current_model, data = data, ...)
    temp_loadings <- standardizedsolution(fit_temp) %>%
      dplyr::filter(op == "=~") %>%
      dplyr::select(rhs, est.std)
    temp_removed <- c()
    if (nrow(temp_loadings) > 0) {
      for (i in 1:nrow(temp_loadings)) {
        item <- temp_loadings$rhs[i]
        loading <- abs(temp_loadings$est.std[i])
        if (loading < 0.30) {
          temp_removed <- c(temp_removed, item)
        }
      }
    }
    if (length(temp_removed) > 0) {
      if (verbose) {
        cat("Se han removido los siguientes ítems por tener loading < 0.30 tras modificación:",
            paste(temp_removed, collapse = ", "), "\n")
      }
      removed_items <- unique(c(removed_items, temp_removed))
      current_model <- remove_items_from_model(current_model, temp_removed)
      mapping <- get_item_factor_mapping(current_model)
    }
  }  # Fin del repeat

  # 11. Verificar si algún ítem aparece en correlaciones de error más de 2 veces
  mods <- log_steps$modification
  items_list <- unlist(lapply(mods, function(mod) {
    trimws(unlist(strsplit(mod, "~~")))
  }))
  items_freq <- table(items_list)
  offending_items <- names(items_freq)[items_freq > 2]

  if (length(offending_items) > 0) {
    if (verbose) {
      cat("Se detectó que los siguientes ítems aparecen más de 2 veces en correlaciones de error:",
          paste(offending_items, collapse = ", "), "\n")
      cat("Se procederá a remover dichos ítems y re-analizar el modelo.\n")
    }
    alternative_model <- remove_items_from_model(current_model, offending_items)
    alternative_fit <- lavaan::cfa(alternative_model, data = data, ...)
    alternative_rmsea <- lavaan::fitMeasures(alternative_fit, "rmsea.scaled")
  } else {
    alternative_model <- NA
    alternative_fit <- NA
    alternative_rmsea <- NA
  }

  # 12. Retornar resultados: modelo final y modelo alternativo (sin ítems offending)
  list(
    final_model = current_model,
    final_fit   = fit,
    log         = log_steps,
    removed_items = removed_items,
    alternative_model = alternative_model,
    alternative_fit = alternative_fit,
    alternative_rmsea = alternative_rmsea
  )
}
