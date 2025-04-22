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
                                 item_factor_map = NULL,
                                 factor_definitions = NULL,
                                 ...) {
  # Si es unidimensional, limpiar mapeos y definiciones de factores
  if (n_factors == 1) {
    factor_definitions <- NULL
    domain_name        <- NULL
    item_factor_map    <- NULL
  }

  # 0. Instalar/cargar PsyMetricTools si no existe
  if (!requireNamespace("PsyMetricTools", quietly = TRUE)) {
    if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
    devtools::install_github("jventural/PsyMetricTools", force = TRUE)
  }

  # 1. Helpers para la API OpenAI
  call_openai_api <- function(prompt) {
    resp <- NULL; attempt <- 1
    while(attempt <= 3) {
      resp <- tryCatch({
        httr::POST(
          "https://api.openai.com/v1/chat/completions",
          httr::add_headers(
            Authorization = paste("Bearer", api_key),
            `Content-Type` = "application/json"
          ),
          httr::timeout(160),
          body = jsonlite::toJSON(list(
            model = gpt_model,
            messages = list(
              list(role="system", content="Eres un experto en psicometría y análisis factorial."),
              list(role="user",   content=prompt)
            ),
            temperature = 0.5
          ), auto_unbox = TRUE)
        )
      }, error = function(e) NULL)
      if (!is.null(resp) && httr::status_code(resp) == 200) break
      if (verbose) cat("Intento", attempt, "fallido. Reintentando...\n")
      attempt <- attempt + 1; Sys.sleep(1)
    }
    resp
  }
  analyze_item_with_gpt <- function(item, definition, context, action) {
    if (is.null(definition) || definition == "") return("No se proporcionó definición del ítem.")
    desc <- if (action == "exclusion") "Justifica concisamente la exclusión" else "Explica brevemente por qué se conserva"
    prompt <- paste0(
      "Eres un experto en psicometría y análisis factorial. ", desc,
      " del ítem '", item, "' cuyo contenido es: \"", definition, "\". ",
      "Constructo: '", construct_definition, "'. Escala: '", scale_title, "'. ",
      context,
      " Modelo EFA: '", model_name, "'."
    )
    resp <- call_openai_api(prompt)
    if (is.null(resp)) return("Error en análisis GPT.")
    content <- httr::content(resp)
    tryCatch(content$choices[[1]]$message$content,
             error = function(e) "Error al extraer respuesta de GPT.")
  }

  # 2. Determinar ítems iniciales
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
  mod <- NULL

  # 3. Evaluación de estructura factorial
  evaluate_structure <- function(df) {
    mat <- as.matrix(df[, grep("^f", names(df))])
    item_names <- df$Items
    n <- nrow(mat)
    ok <- logical(n); scores <- numeric(n); reasons <- character(n)
    for (i in seq_len(n)) {
      li <- abs(mat[i, ])
      cnt <- sum(li > threshold_loading, na.rm = TRUE)
      if (cnt == 1) {
        ok[i] <- TRUE; scores[i] <- max(li); reasons[i] <- "OK"
      } else if (cnt == 0) {
        ok[i] <- FALSE; scores[i] <- max(li); reasons[i] <- "No loading"
      } else {
        s <- sort(li, decreasing = TRUE)
        ok[i] <- FALSE; scores[i] <- s[1] - s[2]; reasons[i] <- "Cross-loading"
      }
    }
    factor_counts <- colSums(abs(mat) > threshold_loading, na.rm = TRUE)
    structure_ok <- all(factor_counts >= min_items_per_factor)
    list(mat = mat,
         items = item_names,
         ok = ok,
         scores = scores,
         reasons = reasons,
         counts = factor_counts,
         structure_ok = structure_ok)
  }

  # 4. Bucle de refinamiento
  repeat {
    candidates <- setdiff(items, c(exclude_items, removed_items))
    if (length(candidates) < n_factors * min_items_per_factor) {
      if (verbose) cat("No quedan suficientes ítems para continuar.\n")
      break
    }
    if (step_counter >= max_steps) {
      if (verbose) cat("Se alcanzó el máximo de pasos.\n")
      break
    }

    # Ajuste EFA
    converged <- TRUE
    tmp <- tryCatch({
      PsyMetricTools::EFA_modern(
        data            = data,
        n_factors       = n_factors,
        n_items         = n_items,
        name_items      = name_items,
        estimator       = estimator,
        rotation        = rotation,
        apply_threshold = FALSE,
        exclude_items   = c(exclude_items, removed_items),
        ...
      )
    }, error = function(e) {
      if (verbose) cat("Modelo no convergió:", e$message, "\n")
      converged <<- FALSE
      NULL
    })
    if (!converged) break
    mod <- tmp

    # RMSEA y estructura
    curr_rmsea <- mod$Bondades_Original$rmsea.scaled[n_factors]
    if (verbose) cat("Iteración", step_counter, "- RMSEA:", round(curr_rmsea, 4), "\n")
    ev <- evaluate_structure(mod$result_df)
    if (verbose) cat("Ítems/factor:", paste(ev$counts, collapse = " | "), "\n")

    if (!is.na(curr_rmsea) && curr_rmsea <= threshold_rmsea && ev$structure_ok) {
      if (verbose) cat("RMSEA y estructura cumplen criterios. Deteniendo.\n")
      break
    }

    decision <- if (!is.na(curr_rmsea) && curr_rmsea > threshold_rmsea) "rmsea" else "structure"

    # Eliminación por RMSEA
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
            apply_threshold = FALSE,
            exclude_items   = c(exclude_items, removed_items, it),
            ...
          )
          m2$Bondades_Original$rmsea.scaled[n_factors]
        }, error = function(e) NA_real_)
      }, USE.NAMES = TRUE)
      if (!all(is.na(cand_rmsea))) {
        best <- names(which.min(cand_rmsea))
        best_val <- min(cand_rmsea, na.rm = TRUE)
        if (best_val < curr_rmsea) {
          removed_items <- c(removed_items, best)
          step_counter <- step_counter + 1
          steps_log <- rbind(steps_log, data.frame(
            step         = step_counter,
            removed_item = best,
            reason       = "Mejora RMSEA",
            rmsea        = best_val,
            stringsAsFactors = FALSE
          ))
          if (verbose) cat("Se remueve", best, "→ RMSEA:", round(best_val, 4), "\n")
          next
        }
      }
      decision <- "structure"
    }

    # Eliminación por estructura
    if (decision == "structure") {
      prob_idx <- which(!ev$ok)
      worst <- ev$items[prob_idx[which.min(ev$scores[prob_idx])]]
      removed_items <- c(removed_items, worst)
      step_counter <- step_counter + 1
      steps_log <- rbind(steps_log, data.frame(
        step         = step_counter,
        removed_item = worst,
        reason       = ev$reasons[which(ev$items == worst)],
        rmsea        = curr_rmsea,
        stringsAsFactors = FALSE
      ))
      if (verbose) cat("Se remueve", worst, "por", ev$reasons[which(ev$items == worst)], "\n")
    }
  }

  # 5. Post‑procesar cargas
  df_final <- mod$result_df
  load_cols <- grep("^f", names(df_final))
  if (apply_threshold) {
    df_final[load_cols] <- lapply(df_final[load_cols], function(x)
      ifelse(abs(x) < threshold_loading, 0, x)
    )
  }

  # 6. Mapear ítem→factor
  if (is.null(item_factor_map)) {
    item_factor_map <- setNames(
      apply(as.matrix(df_final[, load_cols]), 1, function(x) which.max(abs(x))),
      df_final$Items
    )
  }

  # 7. Análisis conceptual (opcional)
  conceptual_analysis <- NULL
  if (analyze_removed && !is.null(api_key) && !is.null(item_definitions)) {
    analysis_removed <- setNames(vector("list", length(removed_items)), removed_items)
    for (it in removed_items) {
      if (verbose) cat("Analizando conceptualmente con IA ítem", it, "(eliminado)...\n")
      context <- if (n_factors == 1) construct_definition else {
        fac_i <- item_factor_map[[it]]
        if (!is.null(factor_definitions[[as.character(fac_i)]])) {
          paste0("Factor (definición): '", factor_definitions[[as.character(fac_i)]], "'.")
        } else {
          paste0("Factor ", fac_i, ".")
        }
      }
      analysis_removed[[it]] <- analyze_item_with_gpt(
        item       = it,
        definition = item_definitions[[it]],
        context    = context,
        action     = "exclusion"
      )
    }
    kept <- setdiff(items, removed_items)
    analysis_kept <- list()
    for (it in kept) {
      if (verbose) cat("Analizando conceptualmente con IA ítem", it, "(conservado)...\n")
      context <- if (n_factors == 1) construct_definition else {
        fac_i <- item_factor_map[[it]]
        if (!is.null(factor_definitions[[as.character(fac_i)]])) {
          paste0("Factor (definición): '", factor_definitions[[as.character(fac_i)]], "'.")
        } else {
          paste0("Factor ", fac_i, ".")
        }
      }
      analysis_kept[[it]] <- analyze_item_with_gpt(
        item       = it,
        definition = item_definitions[[it]],
        context    = context,
        action     = "conservación"
      )
    }
    conceptual_analysis <- list(removed = analysis_removed, kept = analysis_kept)
  }

  # 8. Devolver resultados
  list(
    final_structure     = df_final,
    removed_items       = removed_items,
    steps_log           = steps_log,
    iterations          = step_counter,
    final_rmsea         = curr_rmsea,
    item_factor_map     = item_factor_map,
    factor_definitions  = factor_definitions,
    conceptual_analysis = conceptual_analysis
  )
}
