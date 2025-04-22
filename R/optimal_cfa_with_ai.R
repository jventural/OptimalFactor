optimal_cfa_with_ai <- function(initial_model,
                                 data,
                                 rmsea_threshold      = 0.08,
                                 mi_threshold         = 3.84,
                                 max_steps            = 10,
                                 verbose              = TRUE,
                                 debug                = FALSE,
                                 filter_expr          = NULL,
                                 exclude_items        = character(0),
                                 analyze_removed      = FALSE,
                                 api_key              = NULL,
                                 item_definitions     = NULL,
                                 domain_name          = "Dominio por Defecto",
                                 scale_title          = "Título de la Escala por Defecto",
                                 construct_definition = "",
                                 model_name           = "Modelo CFA",
                                 gpt_model            = "gpt-3.5-turbo",
                                 factor_definitions   = NULL,
                                 ...) {
  library(lavaan)
  library(semTools)
  library(httr)
  library(jsonlite)

  # — Helpers para modelo multidimensional —
  split_model <- function(model_str) {
    lines <- trimws(unlist(strsplit(model_str, "[\r\n]")))
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
    vapply(lines, function(l) {
      parts     <- strsplit(l, "=~")[[1]]
      lhs       <- trimws(parts[1])
      rhs_items <- setdiff(trimws(unlist(strsplit(parts[2], "\\+"))), item)
      paste0(lhs, " =~ ", paste(rhs_items, collapse = " + "))
    }, character(1))
  }

  # — Helpers para API OpenAI —
  call_openai_api <- function(prompt) {
    attempt <- 1; resp <- NULL
    while(attempt <= 3) {
      resp <- tryCatch({
        httr::POST(
          "https://api.openai.com/v1/chat/completions",
          add_headers(Authorization = paste("Bearer", api_key),
                      `Content-Type` = "application/json"),
          timeout(160),
          body = toJSON(list(
            model       = gpt_model,
            messages    = list(
              list(role="system", content="Eres un experto en psicometría y análisis factorial."),
              list(role="user",   content=prompt)
            ),
            temperature = 0.5
          ), auto_unbox = TRUE)
        )
      }, error = function(e) NULL)
      if (!is.null(resp) && status_code(resp)==200) break
      if (verbose) cat("Intento", attempt, "fallido; reintentando...\n")
      attempt <- attempt + 1; Sys.sleep(1)
    }
    resp
  }
  analyze_item_with_gpt <- function(item, item_def, factor_def, action) {
    if (is.null(item_def) || item_def=="") return("No se proporcionó definición del ítem.")
    desc <- if (action=="exclusion") "Justifica concisamente la exclusión" else "Explica brevemente por qué se conserva"
    prompt <- paste0(
      "Eres un experto en psicometría y análisis factorial. ", desc,
      " del ítem '", item, "' cuyo contenido es: \"", item_def, "\". ",
      "Constructo: '", construct_definition, "'. Escala: '", scale_title, "'. ",
      "Modelo CFA: '", model_name, "'. ",
      if (!is.null(factor_def)) paste0("Definición del factor: \"", factor_def, "\".") else ""
    )
    resp <- call_openai_api(prompt)
    if (is.null(resp)) return("Error en análisis GPT.")
    content <- content(resp)
    tryCatch(content$choices[[1]]$message$content,
             error = function(e) "Error al extraer respuesta de GPT.")
  }

  # — Preparación inicial —
  init_lines   <- split_model(initial_model)
  init_factors <- get_factor_names(init_lines)
  init_items   <- get_factor_items(init_lines)

  if (!is.null(filter_expr)) data <- subset(data, eval(filter_expr, data))
  current_lines     <- init_lines
  current_model_str <- paste(current_lines, collapse = "\n")
  removed_items     <- character()
  log_steps <- data.frame(
    step         = integer(),
    modification = character(),
    mi_value     = numeric(),
    rmsea        = numeric(),
    stringsAsFactors = FALSE
  )
  alternative_fit <- NULL

  # — Bucle de mejora iterativa —
  for (step in seq_len(max_steps)) {
    if (verbose) cat("Paso", step, "\n")
    fit <- tryCatch(
      lavaan::cfa(current_model_str, data = data, ...),
      error = function(e) stop("Error en ajuste CFA: ", e$message)
    )
    curr_rmsea <- fitMeasures(fit, "rmsea.scaled")
    # parada por RMSEA
    if (!is.na(curr_rmsea) && curr_rmsea < rmsea_threshold) {
      if (verbose) cat("RMSEA(", round(curr_rmsea,4), ") <", rmsea_threshold, ": deteniendo.\n")
      alternative_fit <- fit
      log_steps <- rbind(log_steps, data.frame(step=step, modification=NA, mi_value=NA, rmsea=curr_rmsea))
      break
    }
    # obtener índices de modificación
    mi_raw <- lavaan::modificationIndices(fit)
    if (!is.data.frame(mi_raw)) {
      if (verbose) cat("No se obtuvieron índices de modificación válidos; deteniendo refinamiento.\n")
      alternative_fit <- fit
      break
    }
    mi_filt <- subset(mi_raw, mi > mi_threshold)
    if (nrow(mi_filt) == 0) {
      if (verbose) cat("No hay índices MI >", mi_threshold, "; deteniendo.\n")
      alternative_fit <- fit
      break
    }
    best_mod <- mi_filt[order(-mi_filt$mi),][1,]
    mod_str  <- paste(best_mod$lhs, best_mod$op, best_mod$rhs)
    # comparación de cargas estandarizadas
    stdsol <- standardizedSolution(fit)
    lh <- stdsol$est.std[stdsol$op=="=~" & stdsol$rhs==best_mod$lhs]
    rh <- stdsol$est.std[stdsol$op=="=~" & stdsol$rhs==best_mod$rhs]
    lh <- ifelse(length(lh)==0||is.na(lh), 0, lh)
    rh <- ifelse(length(rh)==0||is.na(rh), 0, rh)
    if (lh <= rh) {
      item_to_remove <- best_mod$lhs
      reason <- paste0("Eliminando ", best_mod$lhs, " (carga ", round(lh,3),
                       " < ", round(rh,3), ").")
    } else {
      item_to_remove <- best_mod$rhs
      reason <- paste0("Eliminando ", best_mod$rhs, " (carga ", round(rh,3),
                       " < ", round(lh,3), ").")
    }
    removed_items   <- c(removed_items, item_to_remove)
    log_steps       <- rbind(log_steps, data.frame(
      step         = step,
      modification = mod_str,
      mi_value     = best_mod$mi,
      rmsea        = curr_rmsea,
      stringsAsFactors = FALSE
    ))
    if (verbose) cat(reason, "\n")
    current_lines     <- remove_item_from_model(current_lines, item_to_remove)
    current_model_str <- paste(current_lines, collapse = "\n")
    alternative_fit   <- fit
  }

  # medidas finales
  final_meas <- fitMeasures(alternative_fit, c("rmsea.scaled","cfi.scaled"))

  # — Análisis conceptual con IA —
  conceptual_analysis <- NULL
  if (analyze_removed && !is.null(api_key) && !is.null(item_definitions)) {
    if (verbose) cat("Iniciando análisis conceptual con IA...\n")
    # ítems eliminados
    analysis_removed <- setNames(vector("list", length(removed_items)), removed_items)
    for (it in removed_items) {
      if (verbose) cat("  Analizando eliminado:", it, "\n")
      idx     <- which(sapply(init_items, function(x) it %in% x))
      fac_nm  <- if (length(idx)) init_factors[idx] else NA
      fac_def <- if (!is.null(factor_definitions) &&
                     !is.na(fac_nm) &&
                     fac_nm %in% names(factor_definitions))
        factor_definitions[[fac_nm]] else NULL
      analysis_removed[[it]] <- analyze_item_with_gpt(it,
                                                      item_definitions[[it]],
                                                      fac_def,
                                                      "exclusion")
    }
    # ítems conservados
    kept_factors <- get_factor_items(current_lines)
    factor_names <- get_factor_names(current_lines)
    analysis_kept <- list()
    for (i in seq_along(kept_factors)) {
      fac_nm  <- factor_names[i]
      fac_def <- if (!is.null(factor_definitions) &&
                     fac_nm %in% names(factor_definitions))
        factor_definitions[[fac_nm]] else NULL
      for (it in kept_factors[[i]]) {
        if (verbose) cat("  Analizando conservado:", it, "\n")
        analysis_kept[[it]] <- analyze_item_with_gpt(it,
                                                     item_definitions[[it]],
                                                     fac_def,
                                                     "conservación")
      }
    }
    conceptual_analysis <- list(removed = analysis_removed, kept = analysis_kept)
  }

  # — Retornar —
  list(
    final_model         = current_model_str,
    final_fit           = alternative_fit,
    log                 = log_steps,
    removed_items       = removed_items,
    final_rmsea         = final_meas["rmsea.scaled"],
    final_cfi           = final_meas["cfi.scaled"],
    conceptual_analysis = conceptual_analysis
  )
}
