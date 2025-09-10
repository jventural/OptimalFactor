print_conceptual_analysis <- function(resultado, width = 80, show_stats = FALSE) {
  ca <- resultado$conceptual_analysis
  lang <- resultado$config_used$ai_config$language
  is_spanish <- tolower(lang) == "spanish" || tolower(lang) == "español"

  if (is.null(ca)) {
    msg <- if (is_spanish) "No hay análisis conceptual disponible.\n"
    else "No conceptual analysis available.\n"
    cat(msg)
    return(invisible(NULL))
  }

  # Función auxiliar para formatear el análisis narrativo con secciones
  format_narrative_analysis <- function(item_name, item_def, analysis_text, item_stats = NULL, is_removed = TRUE) {
    cat("\n")
    cat("📊 ", item_name, " - \"", item_def, "\"\n", sep = "")
    cat(paste0(rep("─", 70), collapse = ""), "\n\n")

    # Verificar si el análisis parece estar truncado
    is_truncated <- FALSE
    truncation_indicators <- c(
      !grepl("[.!?]$", trimws(analysis_text)),  # No termina con puntuación
      nchar(analysis_text) < 300,  # Muy corto para análisis detallado
      grepl("(HTTP \\d{3}|error|Error|failed)", analysis_text),  # Mensajes de error
      grepl("\\b\\w+$", analysis_text) && !grepl("\\.$", analysis_text)  # Palabra cortada al final
    )

    if (any(truncation_indicators)) {
      is_truncated <- TRUE
    }

    # Mostrar advertencia si está truncado
    if (is_truncated) {
      if (is_spanish) {
        cat("⚠️  NOTA: Este análisis parece estar incompleto o truncado.\n\n")
      } else {
        cat("⚠️  NOTE: This analysis appears to be incomplete or truncated.\n\n")
      }
    }

    # Mostrar estadísticas si están disponibles y se solicitan
    if (show_stats && !is.null(item_stats)) {
      if (is_spanish) {
        cat("INFORMACIÓN TÉCNICA: ")
      } else {
        cat("TECHNICAL INFORMATION: ")
      }

      stats_shown <- FALSE
      if (!is.null(item_stats$loading) && !is.na(item_stats$loading)) {
        cat(sprintf("Carga=%.3f", item_stats$loading))
        stats_shown <- TRUE
      }
      if (!is.null(item_stats$h2) && !is.na(item_stats$h2)) {
        if (stats_shown) cat(", ")
        cat(sprintf("h²=%.3f", item_stats$h2))
        stats_shown <- TRUE
      }
      if (!is.null(item_stats$rmsea_at_removal) && !is.na(item_stats$rmsea_at_removal)) {
        if (stats_shown) cat(", ")
        cat(sprintf("RMSEA=%.3f", item_stats$rmsea_at_removal))
        stats_shown <- TRUE
      }
      if (!is.null(item_stats$reason)) {
        if (stats_shown) cat(", ")
        cat(item_stats$reason)
      }
      cat("\n\n")
    }

    # Si hay un error de API, mostrar mensaje especial
    if (grepl("(HTTP \\d{3}|GPT error|Server error|Rate limit|Connection error)", analysis_text)) {
      if (is_spanish) {
        cat("❌ ERROR EN ANÁLISIS:\n")
        cat(analysis_text, "\n\n")
        cat("Sugerencia: Intente ejecutar nuevamente el análisis para este ítem.\n")
      } else {
        cat("❌ ANALYSIS ERROR:\n")
        cat(analysis_text, "\n\n")
        cat("Suggestion: Try running the analysis again for this item.\n")
      }
      return(invisible(NULL))
    }

    # Intentar identificar y separar las tres secciones del análisis
    sections <- list()

    # Patrones para identificar secciones en español e inglés
    psych_patterns <- c(
      "problemas psicométricos", "psychometric problems",
      "fortalezas psicométricas", "psychometric strengths",
      "desde el punto de vista psicométrico", "from a psychometric",
      "el análisis psicométrico", "psychometric analysis",
      "en primer lugar", "first"
    )

    concept_patterns <- c(
      "desalineación conceptual", "conceptual misalignment",
      "alineación conceptual", "conceptual alignment",
      "desde una perspectiva teórica", "from a theoretical",
      "esta problemática teórica", "this theoretical",
      "conceptualmente", "conceptually",
      "en segundo lugar", "second"
    )

    benefit_patterns <- c(
      "beneficio", "benefit", "impacto", "impact",
      "la eliminación", "the removal", "la exclusión", "the exclusion",
      "la retención", "retention", "contribución", "contribution",
      "resulta en", "results in", "contribuye", "contributes",
      "en consecuencia", "consequently", "por consiguiente", "therefore",
      "finalmente", "finally"
    )

    # Dividir el texto en oraciones para procesamiento
    text_lower <- tolower(analysis_text)

    # Intentar encontrar las secciones
    psych_start <- -1
    concept_start <- -1
    benefit_start <- -1

    # Buscar inicio de cada sección
    for (pattern in psych_patterns) {
      pos <- regexpr(pattern, text_lower, ignore.case = TRUE)
      if (pos[1] > 0) {
        psych_start <- pos[1]
        break
      }
    }

    for (pattern in concept_patterns) {
      pos <- regexpr(pattern, text_lower, ignore.case = TRUE)
      if (pos[1] > 0 && pos[1] > psych_start) {
        concept_start <- pos[1]
        break
      }
    }

    for (pattern in benefit_patterns) {
      pos <- regexpr(pattern, text_lower, ignore.case = TRUE)
      if (pos[1] > 0 && pos[1] > concept_start) {
        benefit_start <- pos[1]
        break
      }
    }

    # Si encontramos las secciones, dividir el texto
    if (psych_start > 0 && concept_start > 0 && benefit_start > 0) {
      sections$psych <- substr(analysis_text, psych_start, concept_start - 1)
      sections$concept <- substr(analysis_text, concept_start, benefit_start - 1)
      sections$benefit <- substr(analysis_text, benefit_start, nchar(analysis_text))
    } else {
      # Si no podemos identificar las secciones, dividir el texto en tres partes aproximadamente iguales
      sentences <- unlist(strsplit(analysis_text, "(?<=[.!?])\\s+", perl = TRUE))
      n_sent <- length(sentences)

      if (n_sent >= 3) {
        third <- n_sent %/% 3
        sections$psych <- paste(sentences[1:third], collapse = " ")
        sections$concept <- paste(sentences[(third+1):min((2*third), n_sent)], collapse = " ")
        if ((2*third+1) <= n_sent) {
          sections$benefit <- paste(sentences[(2*third+1):n_sent], collapse = " ")
        } else {
          sections$benefit <- ""
        }
      } else {
        # Si hay muy pocas oraciones, usar todo el texto en la primera sección
        sections$psych <- analysis_text
        sections$concept <- ""
        sections$benefit <- ""
      }
    }

    # Función para limpiar y completar texto truncado
    clean_section <- function(text) {
      text <- trimws(text)

      # NUEVO: Limpiar marcadores de formato markdown de GPT
      # Eliminar ** para negritas
      text <- gsub("\\*\\*", "", text)
      # Eliminar numeración con paréntesis al inicio de líneas
      text <- gsub("^\\d+\\)\\s*", "", text)
      # Eliminar numeración con asteriscos **1), **2), etc.
      text <- gsub("\\*\\*\\d+\\)\\s*", "", text)
      # Limpiar espacios múltiples
      text <- gsub("\\s+", " ", text)

      # Si el texto no termina con puntuación, agregar puntos suspensivos
      if (nchar(text) > 0 && !grepl("[.!?]$", text)) {
        text <- paste0(text, "...")
      }
      return(text)
    }

    # Limpiar el análisis completo antes de procesarlo
    analysis_text <- gsub("\\*\\*\\d+\\)\\s*", "", analysis_text)  # Eliminar **1), **2), etc.
    analysis_text <- gsub("\\*\\*", "", analysis_text)  # Eliminar marcadores de negrita

    sections$psych <- clean_section(sections$psych)
    sections$concept <- clean_section(sections$concept)
    sections$benefit <- clean_section(sections$benefit)

    # Imprimir con los subtítulos apropiados según si es eliminado o conservado
    if (is_spanish) {
      if (is_removed) {
        # Para ítems ELIMINADOS
        cat("PROBLEMAS PSICOMÉTRICOS:\n")
        if (nchar(sections$psych) > 0) {
          wrapped_text <- strwrap(trimws(sections$psych), width = width)
          cat(wrapped_text, sep = "\n")
          cat("\n\n")
        }

        cat("DESALINEACIÓN CONCEPTUAL:\n")
        if (nchar(sections$concept) > 0) {
          wrapped_text <- strwrap(trimws(sections$concept), width = width)
          cat(wrapped_text, sep = "\n")
          cat("\n\n")
        }

        cat("BENEFICIO DE ELIMINACIÓN:\n")
        if (nchar(sections$benefit) > 0) {
          wrapped_text <- strwrap(trimws(sections$benefit), width = width)
          cat(wrapped_text, sep = "\n")
          cat("\n")
        }
      } else {
        # Para ítems CONSERVADOS
        cat("FORTALEZAS PSICOMÉTRICAS:\n")
        if (nchar(sections$psych) > 0) {
          wrapped_text <- strwrap(trimws(sections$psych), width = width)
          cat(wrapped_text, sep = "\n")
          cat("\n\n")
        }

        cat("ALINEACIÓN CONCEPTUAL:\n")
        if (nchar(sections$concept) > 0) {
          wrapped_text <- strwrap(trimws(sections$concept), width = width)
          cat(wrapped_text, sep = "\n")
          cat("\n\n")
        }

        cat("BENEFICIO DE RETENCIÓN:\n")
        if (nchar(sections$benefit) > 0) {
          wrapped_text <- strwrap(trimws(sections$benefit), width = width)
          cat(wrapped_text, sep = "\n")
          cat("\n")
        }
      }
    } else {
      # En inglés
      if (is_removed) {
        # Para ítems ELIMINADOS
        cat("PSYCHOMETRIC PROBLEMS:\n")
        if (nchar(sections$psych) > 0) {
          wrapped_text <- strwrap(trimws(sections$psych), width = width)
          cat(wrapped_text, sep = "\n")
          cat("\n\n")
        }

        cat("CONCEPTUAL MISALIGNMENT:\n")
        if (nchar(sections$concept) > 0) {
          wrapped_text <- strwrap(trimws(sections$concept), width = width)
          cat(wrapped_text, sep = "\n")
          cat("\n\n")
        }

        cat("REMOVAL BENEFIT:\n")
        if (nchar(sections$benefit) > 0) {
          wrapped_text <- strwrap(trimws(sections$benefit), width = width)
          cat(wrapped_text, sep = "\n")
          cat("\n")
        }
      } else {
        # Para ítems CONSERVADOS
        cat("PSYCHOMETRIC STRENGTHS:\n")
        if (nchar(sections$psych) > 0) {
          wrapped_text <- strwrap(trimws(sections$psych), width = width)
          cat(wrapped_text, sep = "\n")
          cat("\n\n")
        }

        cat("CONCEPTUAL ALIGNMENT:\n")
        if (nchar(sections$concept) > 0) {
          wrapped_text <- strwrap(trimws(sections$concept), width = width)
          cat(wrapped_text, sep = "\n")
          cat("\n\n")
        }

        cat("RETENTION BENEFIT:\n")
        if (nchar(sections$benefit) > 0) {
          wrapped_text <- strwrap(trimws(sections$benefit), width = width)
          cat(wrapped_text, sep = "\n")
          cat("\n")
        }
      }
    }
  }

  # Ítems eliminados
  if (!is.null(ca$removed) && length(ca$removed) > 0) {
    header <- "\n═════════════════════════════════════════════\n"
    cat(header)

    title <- if (is_spanish) "ANÁLISIS CONCEPTUAL DE ÍTEMS ELIMINADOS"
    else "CONCEPTUAL ANALYSIS OF REMOVED ITEMS"
    cat(title, "\n")
    cat(header)

    for (it in names(ca$removed)) {
      # Obtener definición del ítem
      item_def <- resultado$config_used$ai_config$item_definitions[[it]]
      if (is.null(item_def)) item_def <- "Sin definición"

      # Obtener estadísticas si están disponibles
      item_stats <- NULL
      if (!is.null(ca$item_stats) && it %in% names(ca$item_stats)) {
        item_stats <- ca$item_stats[[it]]
      }

      format_narrative_analysis(it, item_def, ca$removed[[it]], item_stats, is_removed = TRUE)
    }
  } else {
    msg <- if (is_spanish) "\nNo hay análisis conceptual de ítems eliminados.\n"
    else "\nNo conceptual analysis of removed items.\n"
    cat(msg)
  }

  # Ítems conservados
  if (!is.null(ca$kept) && length(ca$kept) > 0) {
    header <- "\n═════════════════════════════════════════════\n"
    cat(header)

    title <- if (is_spanish) "ANÁLISIS CONCEPTUAL DE ÍTEMS CONSERVADOS"
    else "CONCEPTUAL ANALYSIS OF RETAINED ITEMS"
    cat(title, "\n")
    cat(header)

    for (it in names(ca$kept)) {
      # Obtener definición del ítem
      item_def <- resultado$config_used$ai_config$item_definitions[[it]]
      if (is.null(item_def)) item_def <- "Sin definición"

      # Nota: Para ítems conservados, las estadísticas vienen del modelo final
      # y no están en item_stats
      format_narrative_analysis(it, item_def, ca$kept[[it]], NULL, is_removed = FALSE)
    }
  } else {
    msg <- if (is_spanish) "\nNo hay análisis conceptual de ítems conservados.\n"
    else "\nNo conceptual analysis of retained items.\n"
    cat(msg)
  }

  invisible(NULL)
}
