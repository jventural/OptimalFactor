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
  # Cargar librerías
  library(lavaan)
  library(semTools)
  library(httr)
  library(jsonlite)

  # ---------------------------------------------------------------------
  # Helpers para modelo multidimensional
  # ---------------------------------------------------------------------
  split_model <- function(model_str) {
    lines <- unlist(strsplit(model_str, "[\r\n]"))
    lines <- trimws(lines)
    lines[nzchar(lines)]
  }
  get_factor_names <- function(lines) {
    sapply(lines, function(l) trimws(strsplit(l, "=~")[[1]][1]))
  }
  get_factor_items <- function(lines) {
    lapply(lines, function(l) {
      rhs <- strsplit(l, "=~")[[1]][2]
      trimws(unlist(strsplit(rhs, "\\+")))
    })
  }
  remove_item_from_model <- function(lines, item) {
    lapply(lines, function(l) {
      parts <- strsplit(l, "=~")[[1]]
      lhs <- trimws(parts[1])
      rhs_items <- trimws(unlist(strsplit(parts[2], "\\+")))
      rhs_items <- setdiff(rhs_items, item)
      paste0(lhs, " =~ ", paste(rhs_items, collapse = " + "))
    })
  }

  # ---------------------------------------------------------------------
  # Helpers para API OpenAI
  # ---------------------------------------------------------------------
  call_openai_api <- function(prompt) {
    attempt <- 1; resp <- NULL
    while(attempt <= 3) {
      resp <- tryCatch({
        httr::POST(
          url = "https://api.openai.com/v1/chat/completions",
          httr::add_headers(
            Authorization = paste("Bearer", api_key),
            `Content-Type` = "application/json"
          ),
          httr::timeout(160),
          body = jsonlite::toJSON(list(
            model = gpt_model,
            messages = list(
              list(role = "system", content = "Eres un experto en psicometría y análisis factorial."),
              list(role = "user",   content = prompt)
            ),
            temperature = 0.5
          ), auto_unbox = TRUE)
        )
      }, error = function(e) NULL)
      if (!is.null(resp) && httr::status_code(resp) == 200) break
      if (verbose) cat("Intento", attempt, "fallido. Reintentando...\n")
      attempt <- attempt + 1
      Sys.sleep(1)
    }
    resp
  }
  analyze_item_with_gpt <- function(item, definition, domain, action) {
    if (is.null(definition) || definition == "")
      return("No se proporcionó definición del ítem.")
    desc <- if (action == "exclusion")
      "Justifica concisamente la exclusión"
    else
      "Explica brevemente por qué se conserva"
    prompt <- paste0(
      "Eres un experto en psicometría y análisis factorial. ",
      desc, " del ítem '", item, "' cuyo contenido es: \"", definition, "\". ",
      "Constructo: '", construct_definition, "'. Escala: '", scale_title, "'. ",
      "Dominio: '", domain, "'. Modelo CFA: '", model_name, "'."
    )
    resp <- call_openai_api(prompt)
    if (is.null(resp)) {
      warning("Error en la llamada a la API para ítem ", item)
      return("Error en análisis GPT.")
    }
    content <- httr::content(resp)
    tryCatch(
      content$choices[[1]]$message$content,
      error = function(e) "Error al extraer respuesta de GPT."
    )
  }

  # ---------------------------------------------------------------------
  # Inicialización
  # ---------------------------------------------------------------------
  # Permitir filtrar datos si se provee filter_expr
  if (!is.null(filter_expr)) data <- subset(data, eval(filter_expr, data))

  model_lines       <- split_model(initial_model)
  current_lines     <- model_lines
  current_model_str <- paste(current_lines, collapse = "\n")
  removed_items     <- character(0)
  log_steps         <- data.frame(
    step         = integer(),
    modification = character(),
    mi_value     = numeric(),
    rmsea        = numeric(),
    stringsAsFactors = FALSE
  )
  alternative_fit <- NULL

  # ---------------------------------------------------------------------
  # Bucle iterativo de mejora
  # ---------------------------------------------------------------------
  for (step in seq_len(max_steps)) {
    if (verbose) cat("Paso", step, "\n")
    fit <- tryCatch(
      lavaan::cfa(current_model_str, data = data, ...),
      error = function(e) stop("Error en ajuste CFA: ", e$message)
    )
    curr_rmsea <- lavaan::fitMeasures(fit, "rmsea.scaled")

    # Criterio de parada por RMSEA
    if (!is.na(curr_rmsea) && curr_rmsea < rmsea_threshold) {
      log_steps <- rbind(log_steps, data.frame(
        step         = step,
        modification = NA_character_,
        mi_value     = NA_real_,
        rmsea        = curr_rmsea,
        stringsAsFactors = FALSE
      ))
      if (verbose) cat("RMSEA (", round(curr_rmsea,4),
                       ") por debajo de ", rmsea_threshold, ".\n")
      alternative_fit <- fit
      break
    }

    # Índices de modificación
    mi <- lavaan::modificationIndices(fit)
    mi_filtered <- subset(mi, mi$mi > mi_threshold)
    if (nrow(mi_filtered) == 0) {
      if (verbose) cat("No hay MI > ", mi_threshold, ".\n")
      alternative_fit <- fit
      break
    }
    best_mod <- mi_filtered[order(-mi_filtered$mi), ][1, ]
    modification_str <- paste(best_mod$lhs, best_mod$op, best_mod$rhs)

    # Comparar cargas para decidir ítem
    std_sol <- lavaan::standardizedSolution(fit)
    lh <- as.numeric(std_sol$est.std[std_sol$op=="=~" & std_sol$rhs==best_mod$lhs])
    rh <- as.numeric(std_sol$est.std[std_sol$op=="=~" & std_sol$rhs==best_mod$rhs])
    lh <- ifelse(length(lh)==0||is.na(lh), 0, lh)
    rh <- ifelse(length(rh)==0||is.na(rh), 0, rh)

    if (lh <= rh) {
      item_to_remove   <- best_mod$lhs
      removal_reason   <- paste0("Eliminando ", best_mod$lhs,
                                 " (carga ", round(lh,3),
                                 " < ", round(rh,3), ").")
    } else {
      item_to_remove   <- best_mod$rhs
      removal_reason   <- paste0("Eliminando ", best_mod$rhs,
                                 " (carga ", round(rh,3),
                                 " < ", round(lh,3), ").")
    }

    # Registrar y remover
    removed_items <- c(removed_items, item_to_remove)
    log_steps <- rbind(log_steps, data.frame(
      step         = step,
      modification = modification_str,
      mi_value     = best_mod$mi,
      rmsea        = curr_rmsea,
      stringsAsFactors = FALSE
    ))
    if (verbose) cat(removal_reason, "\n")

    # Actualizar modelo
    current_lines     <- remove_item_from_model(current_lines, item_to_remove)
    current_model_str <- paste(current_lines, collapse = "\n")
    alternative_fit   <- fit
  }

  # Ajuste final
  final_measures <- lavaan::fitMeasures(alternative_fit,
                                        c("rmsea.scaled","cfi.scaled"))

  # ---------------------------------------------------------------------
  # Análisis conceptual con IA (opcional)
  # ---------------------------------------------------------------------
  conceptual_analysis <- NULL
  if (analyze_removed && !is.null(api_key) && !is.null(item_definitions)) {
    # Ítems eliminados
    analysis_removed <- setNames(vector("list", length(removed_items)), removed_items)
    for (it in removed_items) {
      if (verbose) cat("Analizando ítem eliminado:", it, "\n")
      analysis_removed[[it]] <-
        analyze_item_with_gpt(it,
                              item_definitions[[it]],
                              domain_name,
                              "exclusion")
    }
    # Ítems conservados por factor
    factor_names      <- get_factor_names(current_lines)
    factor_items_list <- get_factor_items(current_lines)
    analysis_kept     <- list()
    for (i in seq_along(factor_items_list)) {
      dom_i <- if (length(domain_name) >= i) domain_name[i] else domain_name
      for (it in factor_items_list[[i]]) {
        if (verbose)
          cat("Analizando ítem conservado de", factor_names[i],
              "(", dom_i, "):", it, "\n")
        analysis_kept[[it]] <-
          analyze_item_with_gpt(it,
                                item_definitions[[it]],
                                dom_i,
                                "conservación")
      }
    }
    conceptual_analysis <- list(removed = analysis_removed,
                                kept    = analysis_kept)
  }

  # ---------------------------------------------------------------------
  # Retornar resultados
  # ---------------------------------------------------------------------
  return(list(
    final_model         = current_model_str,
    final_fit           = alternative_fit,
    log                 = log_steps,
    removed_items       = removed_items,
    alternative_fit     = alternative_fit,
    alternative_rmsea   = final_measures["rmsea.scaled"],
    final_cfi           = final_measures["cfi.scaled"],
    conceptual_analysis = conceptual_analysis
  ))
}
