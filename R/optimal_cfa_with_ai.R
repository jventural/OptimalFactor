optimal_cfa_with_ai <- function(initial_model,
                                data,
                                rmsea_threshold = 0.08,
                                mi_threshold = 3.84,
                                max_steps = 10,
                                verbose = TRUE,
                                debug = FALSE,
                                filter_expr = NULL,
                                exclude_items = character(0),
                                analyze_removed = FALSE,
                                api_key = NULL,
                                item_definitions = NULL,
                                domain_name = "Dominio por Defecto",
                                scale_title = "Título de la Escala por Defecto",
                                construct_definition = "",
                                model_name = "Modelo CFA",
                                gpt_model = "gpt-3.5-turbo",
                                ...) {

  # Función interna para llamar a la API con reintentos
  call_openai_api <- function(prompt, api_key, gpt_model, timeout_seconds = 160, max_attempts = 3) {
    attempt <- 1
    repeat {
      response <- tryCatch({
        httr::POST(
          url = "https://api.openai.com/v1/chat/completions",
          httr::add_headers(
            Authorization = paste("Bearer", api_key),
            `Content-Type` = "application/json"
          ),
          config = httr::timeout(timeout_seconds),
          body = jsonlite::toJSON(list(
            model = gpt_model,
            messages = list(
              list(role = "system", content = "Eres un experto en psicometría y análisis factorial."),
              list(role = "user", content = prompt)
            ),
            temperature = 0.5
          ), auto_unbox = TRUE)
        )
      }, error = function(e) NULL)
      if (!is.null(response) && httr::status_code(response) == 200) {
        return(response)
      } else if (attempt >= max_attempts) {
        break
      } else {
        if (verbose) cat("Intento", attempt, "fallido. Reintentando...\n")
        attempt <- attempt + 1
        Sys.sleep(1)
      }
    }
    return(NULL)
  }

  # Función interna para el análisis conceptual con GPT
  analyze_item_with_gpt <- function(item, definition, api_key, domain_name,
                                    scale_title, construct_definition,
                                    model_name, gpt_model, action = c("exclusion", "conservación")) {
    action <- match.arg(action)
    if (is.null(definition) || is.na(definition) || definition == "") {
      return("No se proporcionó contenido del ítem para análisis.")
    }
    desc <- if (action == "exclusion")
      "Justifica concisamente la exclusión"
    else
      "Explica brevemente por qué se conserva"
    prompt <- paste0(
      "Eres un experto en psicometría y análisis factorial. ",
      desc, " del ítem '", item, "' cuyo contenido es: \"", definition, "\". ",
      "Constructo: '", construct_definition, "'. Escala: '", scale_title, "'. ",
      "Dominio: '", domain_name, "'. Modelo CFA: '", model_name, "'."
    )
    response <- call_openai_api(prompt, api_key, gpt_model)
    if (is.null(response)) {
      warning("Error en la llamada a la API tras varios intentos.")
      return("Error en la llamada a la API.")
    }
    res <- httr::content(response)
    out <- tryCatch(
      res$choices[[1]]$message$content,
      error = function(e) "Error al generar el análisis conceptual del ítem."
    )
    return(out)
  }

  current_model <- initial_model
  removed_items <- character(0)
  log_steps <- data.frame(
    step = integer(),
    modification = character(),
    mi_value = numeric(),
    rmsea = numeric(),
    stringsAsFactors = FALSE
  )
  alternative_fit <- NULL

  # Iteraciones para mejora paso a paso
  for (step in seq_len(max_steps)) {
    if (verbose) cat("Paso", step, "\n")
    fit <- tryCatch(
      lavaan::cfa(current_model, data = data, ...),
      error = function(e) stop("Error en el ajuste del modelo: ", e$message)
    )
    current_rmsea <- lavaan::fitMeasures(fit, "rmsea.scaled")

    if (current_rmsea < rmsea_threshold) {
      log_steps <- rbind(log_steps, data.frame(
        step = step,
        modification = NA_character_,
        mi_value = NA_real_,
        rmsea = current_rmsea,
        stringsAsFactors = FALSE
      ))
      if (verbose) cat("RMSEA (", current_rmsea, ") está por debajo del umbral (", rmsea_threshold, "). Deteniendo iteración.\n")
      alternative_fit <- fit
      break
    }

    mi <- lavaan::modificationIndices(fit)
    mi_filtered <- subset(mi, mi > mi_threshold)
    if (nrow(mi_filtered) == 0) {
      if (verbose) cat("No se encontraron MI mayores a", mi_threshold, ". Deteniendo iteración.\n")
      alternative_fit <- fit
      break
    }
    mi_ordered <- mi_filtered[order(-mi_filtered$mi), ]
    best_mod <- mi_ordered[1, ]
    modification_str <- paste(best_mod$lhs, best_mod$op, best_mod$rhs)
    std_sol <- lavaan::standardizedSolution(fit)
    loading_lhs <- as.numeric(
      std_sol$est.std[std_sol$op == "=~" & std_sol$rhs == best_mod$lhs]
    )
    loading_rhs <- as.numeric(
      std_sol$est.std[std_sol$op == "=~" & std_sol$rhs == best_mod$rhs]
    )
    if (length(loading_lhs) == 0 || is.na(loading_lhs)) loading_lhs <- 0
    if (length(loading_rhs) == 0 || is.na(loading_rhs)) loading_rhs <- 0
    if (loading_lhs <= loading_rhs) {
      item_to_remove <- best_mod$lhs
      removal_reason <- paste(
        "Eliminando", best_mod$lhs, "por tener una carga factorial menor (",
        round(loading_lhs, 3), ") que", best_mod$rhs, "(",
        round(loading_rhs, 3), ")."
      )
    } else {
      item_to_remove <- best_mod$rhs
      removal_reason <- paste(
        "Eliminando", best_mod$rhs, "por tener una carga factorial menor (",
        round(loading_rhs, 3), ") que", best_mod$lhs, "(",
        round(loading_lhs, 3), ")."
      )
    }
    removed_items <- c(removed_items, item_to_remove)
    log_steps <- rbind(log_steps, data.frame(
      step = step,
      modification = modification_str,
      mi_value = best_mod$mi,
      rmsea = current_rmsea,
      stringsAsFactors = FALSE
    ))
    if (verbose) cat(removal_reason, "\n")
    current_model <- remove_item_from_model(current_model, item_to_remove)
    alternative_fit <- fit
  }

  # Obtener ítems conservados
  kept_items <- unlist(strsplit(strsplit(current_model, "=~")[[1]][2], "\\+"))
  kept_items <- trimws(kept_items)

  # Análisis conceptual de ítems
  conceptual_analysis <- NULL
  if (analyze_removed && !is.null(api_key) && !is.null(item_definitions)) {
    analysis_removed <- setNames(vector("list", length(removed_items)), removed_items)
    for (item in removed_items) {
      if (verbose) cat("Analizando ítem eliminado:", item, "\n")
      analysis_removed[[item]] <- analyze_item_with_gpt(
        item, item_definitions[[item]], api_key,
        domain_name, scale_title, construct_definition,
        model_name, gpt_model, action = "exclusion"
      )
    }
    analysis_kept <- setNames(vector("list", length(kept_items)), kept_items)
    for (item in kept_items) {
      if (verbose) cat("Analizando ítem conservado:", item, "\n")
      analysis_kept[[item]] <- analyze_item_with_gpt(
        item, item_definitions[[item]], api_key,
        domain_name, scale_title, construct_definition,
        model_name, gpt_model, action = "conservación"
      )
    }
    conceptual_analysis <- list(removed = analysis_removed, kept = analysis_kept)
  }

  # Medidas finales: RMSEA y CFI
  final_measures <- lavaan::fitMeasures(alternative_fit, c("rmsea.scaled", "cfi.scaled"))

  return(list(
    final_model = current_model,
    final_fit = alternative_fit,
    log = log_steps,
    removed_items = removed_items,
    alternative_fit = alternative_fit,
    alternative_rmsea = final_measures["rmsea.scaled"],
    final_cfi = final_measures["cfi.scaled"],
    conceptual_analysis = conceptual_analysis
  ))
}
