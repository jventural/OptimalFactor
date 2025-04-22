optimal_efa_with_ai <- function(data,
                                items = NULL,
                                n_factors = 5,
                                n_items = NULL,
                                name_items = "PPTQ",
                                estimator = "WLSMV",
                                rotation = "oblimin",
                                threshold_rmsea = 0.08,
                                threshold_loading = 0.30,
                                min_items_per_factor = 2,
                                apply_threshold = TRUE,
                                max_steps = NULL,
                                verbose = TRUE,
                                exclude_items = character(0),
                                analyze_removed = FALSE,
                                api_key = NULL,
                                item_definitions = NULL,
                                domain_name = "Dominio por Defecto",
                                scale_title = "Título de la Escala por Defecto",
                                construct_definition = "",
                                model_name = "Modelo EFA",
                                gpt_model = "gpt-3.5-turbo",
                                ...) {
  # 0. Instalar/cargar PsyMetricTools si no existe
  if (!requireNamespace("PsyMetricTools", quietly = TRUE)) {
    if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
    devtools::install_github("jventural/PsyMetricTools", force = TRUE)
  }

  # 1. Helper para llamadas a la API de OpenAI
  call_openai_api <- function(prompt) {
    resp <- NULL
    attempt <- 1
    while (attempt <= 3 && (is.null(resp) || httr::status_code(resp) != 200)) {
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

  # 2. Helper para análisis conceptual de ítems
  analyze_item_with_gpt <- function(item, definition, action) {
    if (is.null(definition) || definition == "") return("No se proporcionó definición del ítem.")
    desc <- if (action == "exclusion") "Justifica concisamente la exclusión" else "Explica brevemente por qué se conserva"
    prompt <- paste0(
      "Eres un experto en psicometría y análisis factorial. ",
      desc, " del ítem '", item, "' con contenido: \"", definition, "\". ",
      "Constructo: '", construct_definition, "'. Escala: '", scale_title, "'. ",
      "Dominio: '", domain_name, "'. Modelo EFA: '", model_name, "'."
    )
    resp <- call_openai_api(prompt)
    if (is.null(resp)) {
      warning("Error en la llamada a la API para el ítem ", item)
      return("Error en análisis GPT.")
    }
    content <- httr::content(resp)
    tryCatch(
      content$choices[[1]]$message$content,
      error = function(e) "Error al extraer respuesta de GPT."
    )
  }

  # 3. Determinar ítems iniciales
  if (is.null(items)) {
    if (is.null(n_items)) {
      items <- grep(paste0("^", name_items, "\\d+$"), names(data), value = TRUE)
    } else {
      items <- paste0(name_items, 1:n_items)
    }
  }
  if (is.null(n_items))   n_items   <- length(items)
  if (is.null(max_steps)) max_steps <- length(items) - 1

  removed_items <- character(0)
  steps_log     <- data.frame(
    step         = integer(),
    removed_item = character(),
    reason       = character(),
    rmsea        = numeric(),
    stringsAsFactors = FALSE
  )
  step_counter <- 0

  # 4. Función para evaluar estructura factorial
  evaluate_structure <- function(loadings_df) {
    mat   <- as.matrix(loadings_df[, grep("^f", names(loadings_df))])
    names <- loadings_df$Items
    n     <- nrow(mat)
    ok    <- logical(n); scores <- numeric(n); reasons <- character(n)
    for (i in seq_len(n)) {
      li  <- abs(mat[i, ])
      cnt <- sum(li > threshold_loading, na.rm = TRUE)
      if (cnt == 1) {
        ok[i] <- TRUE; scores[i] <- max(li); reasons[i] <- "OK"
      } else if (cnt == 0) {
        ok[i] <- FALSE; scores[i] <- max(li); reasons[i] <- "No loading"
      } else {
        srt <- sort(li, decreasing = TRUE)
        ok[i]      <- FALSE
        scores[i]  <- srt[1] - srt[2]
        reasons[i] <- "Cross-loading"
      }
    }
    factor_counts <- colSums(mat > threshold_loading, na.rm = TRUE)
    structure_ok  <- all(ok) && all(factor_counts >= min_items_per_factor)
    list(
      structure_ok = structure_ok,
      ok_items      = ok,
      scores        = scores,
      reasons       = reasons,
      factor_counts = factor_counts,
      item_names    = names
    )
  }

  # 5. Bucle de refinamiento
  repeat {
    candidates <- setdiff(items, c(exclude_items, removed_items))
    if (length(candidates) < n_factors * min_items_per_factor) {
      if (verbose) cat("No quedan suficientes ítems para continuar.\n")
      break
    }
    if (step_counter >= max_steps) {
      if (verbose) cat("Se alcanzó el máximo de pasos:", max_steps, "\n")
      break
    }

    # 5.1 Ajustar modelo EFA
    model <- tryCatch({
      PsyMetricTools::EFA_modern(
        data            = data,
        n_factors       = n_factors,
        n_items         = n_items,
        name_items      = name_items,
        estimator       = estimator,
        rotation        = rotation,
        apply_threshold = apply_threshold,
        exclude_items   = c(exclude_items, removed_items),
        ...
      )
    }, error = function(e) stop("Error en EFA_modern: ", e$message))

    # 5.2 Obtener RMSEA actual (único valor)
    rmsea_now <- model$Bondades_Original$rmsea.scaled
    if (verbose) cat("Iteración", step_counter, "- RMSEA:", round(rmsea_now, 4), "\n")

    # 5.3 Evaluar estructura
    ev <- evaluate_structure(model$result_df)
    if (verbose) {
      cat("Items por factor:", paste(ev$factor_counts, collapse = " | "), "\n")
      prob <- which(!ev$ok_items)
      if (length(prob) > 0) {
        print(data.frame(
          Item   = ev$item_names[prob],
          Score  = ev$scores[prob],
          Reason = ev$reasons[prob],
          stringsAsFactors = FALSE
        ))
      }
    }

    # 5.4 Criterios de detención
    if (!is.na(rmsea_now) && rmsea_now <= threshold_rmsea && ev$structure_ok) {
      if (verbose) cat("Modelo aceptable encontrado. Deteniendo.\n")
      break
    }
    decision <- if (!is.na(rmsea_now) && rmsea_now > threshold_rmsea) "rmsea" else "structure"

    # 5.5 Eliminación basada en RMSEA
    if (decision == "rmsea") {
      cand_rmsea <- sapply(candidates, function(it) {
        tryCatch({
          m2 <- PsyMetricTools::EFA_modern(
            data            = data,
            n_factors       = n_factors,
            n_items         = n_items,
            name_items      = name_items,
            estimator       = estimator,
            rotation        = rotation,
            apply_threshold = apply_threshold,
            exclude_items   = c(exclude_items, removed_items, it),
            ...
          )
          m2$Bondades_Original$rmsea.scaled
        }, error = function(e) NA_real_)
      }, USE.NAMES = TRUE)

      if (all(is.na(cand_rmsea))) {
        if (verbose) cat("Ninguna remoción mejora RMSEA. Cambiando a estructura.\n")
        decision <- "structure"
      } else {
        best     <- names(which.min(cand_rmsea))
        best_val <- min(cand_rmsea, na.rm = TRUE)
        if (best_val < rmsea_now) {
          removed_items <- c(removed_items, best)
          step_counter  <- step_counter + 1
          steps_log     <- rbind(steps_log, data.frame(
            step         = step_counter,
            removed_item = best,
            reason       = "Mejora RMSEA",
            rmsea        = best_val,
            stringsAsFactors = FALSE
          ))
          if (verbose) {
            cat("Paso", step_counter, "- Se remueve", best,
                "RMSEA:", round(rmsea_now, 4), "→", round(best_val, 4), "\n")
          }
          next
        } else {
          decision <- "structure"
        }
      }
    }

    # 5.6 Eliminación basada en estructura
    if (decision == "structure") {
      prob_idx <- which(!ev$ok_items)
      if (length(prob_idx) == 0) {
        if (verbose) cat("No hay ítems problemáticos. Deteniendo.\n")
        break
      }
      worst     <- prob_idx[which.min(ev$scores[prob_idx])]
      item_rm   <- ev$item_names[worst]
      removed_items <- c(removed_items, item_rm)
      step_counter  <- step_counter + 1
      steps_log     <- rbind(steps_log, data.frame(
        step         = step_counter,
        removed_item = item_rm,
        reason       = ev$reasons[worst],
        rmsea        = rmsea_now,
        stringsAsFactors = FALSE
      ))
      if (verbose) {
        cat("Paso", step_counter, "- Se remueve", item_rm,
            "por estructura:", ev$reasons[worst], "\n")
      }
    }
  }

  # 6. Análisis conceptual con GPT (opcional)
  conceptual_analysis <- NULL
  if (analyze_removed && !is.null(api_key) && !is.null(item_definitions)) {
    kept_items    <- setdiff(items, c(exclude_items, removed_items))
    analysis_rem  <- list()
    for (it in removed_items) {
      if (verbose) cat("Analizando ítem eliminado con IA:", it, "\n")
      analysis_rem[[it]] <- analyze_item_with_gpt(it, item_definitions[[it]], "exclusion")
    }
    analysis_keep <- list()
    for (it in kept_items) {
      if (verbose) cat("Analizando ítem conservado con IA:", it, "\n")
      analysis_keep[[it]] <- analyze_item_with_gpt(it, item_definitions[[it]], "conservación")
    }
    conceptual_analysis <- list(removed = analysis_rem, kept = analysis_keep)
  }

  # 7. Retornar resultados
  list(
    final_structure     = model$result_df,
    removed_items       = removed_items,
    steps_log           = steps_log,
    iterations          = step_counter,
    final_rmsea         = rmsea_now,
    conceptual_analysis = conceptual_analysis
  )
}
