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
  # 1. Limpiar factor maps si unidimensional
  if (n_factors == 1) {
    factor_definitions <- NULL
    domain_name        <- NULL
    item_factor_map    <- NULL
  }

  # 2. Instalar/cargar PsyMetricTools si falta
  if (!requireNamespace("PsyMetricTools", quietly = TRUE)) {
    if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
    devtools::install_github("jventural/PsyMetricTools", force = TRUE)
  }

  # 3. Helpers OpenAI
  call_openai_api <- function(prompt) {
    resp <- NULL; at <- 1
    while (at <= 3) {
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
          ), auto_unbox=TRUE)
        )
      }, error = function(e) NULL)
      if (!is.null(resp) && httr::status_code(resp)==200) break
      if (verbose) cat("Intento", at, "fallido; reintentando...\n")
      at <- at + 1; Sys.sleep(1)
    }
    resp
  }
  analyze_item_with_gpt <- function(item, definition, context, action) {
    if (is.null(definition) || definition=="") return("No se proporcionó definición.")
    desc <- if (action=="exclusion") "Justifica concisamente la exclusión" else "Explica brevemente por qué se conserva"
    prompt <- paste0(
      "Eres un experto en psicometría. ", desc,
      " del ítem '", item, "' (\"", definition, "\"). ",
      "Constructo: '", construct_definition, "'. Escala: '", scale_title, "'. ",
      context, " Modelo EFA: '", model_name, "'."
    )
    resp <- call_openai_api(prompt)
    if (is.null(resp)) return("Error en GPT.")
    content <- httr::content(resp)
    tryCatch(content$choices[[1]]$message$content,
             error = function(e) "Error extrayendo GPT.")
  }

  # 4. Ítems iniciales
  if (is.null(items)) {
    if (is.null(n_items)) {
      items <- grep(paste0("^", name_items, "\\d+$"), names(data), value=TRUE)
    } else {
      items <- paste0(name_items, 1:n_items)
    }
  }
  if (is.null(n_items))   n_items   <- length(items)
  if (is.null(max_steps)) max_steps <- length(items) - 1

  removed_items <- character()
  steps_log     <- data.frame(step=integer(), removed_item=character(),
                              reason=character(), rmsea=numeric(),
                              stringsAsFactors=FALSE)
  step_counter  <- 0
  mod           <- NULL

  # 5. Evaluación de estructura
  evaluate_structure <- function(df) {
    mat <- as.matrix(df[, grep("^f", names(df))])
    its <- df$Items; n <- nrow(mat)
    ok <- logical(n); scores <- numeric(n); reasons <- character(n)
    for (i in seq_len(n)) {
      li <- abs(mat[i, ])
      cnt <- sum(li > threshold_loading, na.rm=TRUE)
      if (cnt==1) {
        ok[i] <- TRUE; scores[i] <- max(li); reasons[i] <- "OK"
      } else if (cnt==0) {
        ok[i] <- FALSE; scores[i] <- max(li); reasons[i] <- "No loading"
      } else {
        s <- sort(li, decreasing=TRUE)
        ok[i] <- FALSE; scores[i] <- s[1]-s[2]; reasons[i] <- "Cross-loading"
      }
    }
    counts <- colSums(abs(mat) > threshold_loading, na.rm=TRUE)
    structure_ok <- all(counts >= min_items_per_factor)
    list(mat=mat, items=its, ok=ok, scores=scores,
         reasons=reasons, counts=counts, structure_ok=structure_ok)
  }

  # 6. Bucle de refinamiento
  repeat {
    candidates <- setdiff(items, c(exclude_items, removed_items))
    if (length(candidates) < n_factors * min_items_per_factor) {
      if (verbose) cat("No quedan suficientes ítems.\n"); break
    }
    if (step_counter >= max_steps) {
      if (verbose) cat("Máximo de pasos alcanzado.\n"); break
    }

    # Ajuste EFA
    conv <- TRUE
    tmp  <- tryCatch({
      PsyMetricTools::EFA_modern(
        data=data, n_factors=n_factors, n_items=n_items,
        name_items=name_items, estimator=estimator,
        rotation=rotation, apply_threshold=FALSE,
        exclude_items=c(exclude_items, removed_items), ...
      )
    }, error=function(e) {
      if (verbose) cat("No convergió:", e$message, "\n")
      conv <<- FALSE; NULL
    })
    if (!conv) break
    mod <- tmp

    curr_rmsea <- mod$Bondades_Original$rmsea.scaled[n_factors]
    if (verbose) cat("Iteración", step_counter, "- RMSEA:", round(curr_rmsea,4), "\n")
    ev <- evaluate_structure(mod$result_df)
    if (verbose) cat("Ítems/factor:", paste(ev$counts, collapse=" | "), "\n")

    # Parada por ajuste
    if (!is.na(curr_rmsea) && curr_rmsea <= threshold_rmsea && ev$structure_ok) {
      if (verbose) cat("Criterios cumplidos; deteniendo refinamiento.\n"); break
    }

    decision <- if (!is.na(curr_rmsea) && curr_rmsea > threshold_rmsea) "rmsea" else "structure"

    # 6.1 Eliminación por RMSEA
    if (decision=="rmsea") {
      cand_rmsea <- sapply(candidates, function(it) {
        tryCatch({
          m2 <- PsyMetricTools::EFA_modern(
            data=data, n_factors=n_factors, n_items=n_items,
            name_items=name_items, estimator=estimator,
            rotation=rotation, apply_threshold=FALSE,
            exclude_items=c(exclude_items, removed_items, it), ...
          )
          m2$Bondades_Original$rmsea.scaled[n_factors]
        }, error=function(e) NA_real_)
      }, USE.NAMES=TRUE)
      if (!all(is.na(cand_rmsea))) {
        best_val <- min(cand_rmsea, na.rm=TRUE)
        if (best_val < curr_rmsea) {
          best <- names(which.min(cand_rmsea))
          removed_items <- c(removed_items, best)
          step_counter <- step_counter + 1
          steps_log <- rbind(steps_log, data.frame(
            step=step_counter, removed_item=best,
            reason="Mejora RMSEA", rmsea=best_val,
            stringsAsFactors=FALSE
          ))
          if (verbose) cat("Removido", best, "→ RMSEA:", round(best_val,4), "\n")
          next
        }
      }
      decision <- "structure"
    }

    # 6.2 Eliminación por estructura
    ev <- evaluate_structure(mod$result_df)
    if (all(ev$ok)) {
      if (verbose) {
        cat(
          "No hay cross‐loading ni no‐loading, pero la\n",
          "estructura global sigue sin cumplir min_items_per_factor.\n",
          "Deteniendo refinamiento y procediendo a análisis conceptual.\n"
        )
      }
      break
    }
    # remueve el peor
    prob_idx <- which(!ev$ok)
    worst    <- ev$items[prob_idx[which.min(ev$scores[prob_idx])]]
    reason   <- ev$reasons[which(ev$items==worst)]
    removed_items <- c(removed_items, worst)
    step_counter  <- step_counter + 1
    steps_log <- rbind(steps_log, data.frame(
      step=step_counter, removed_item=worst,
      reason=reason, rmsea=curr_rmsea,
      stringsAsFactors=FALSE
    ))
    if (verbose) cat("Removido", worst, "por", reason, "\n")
  }

  # 7. Post‑procesar cargas y mapear factor
  df_final <- mod$result_df
  load_cols <- grep("^f", names(df_final))
  if (apply_threshold) {
    df_final[load_cols] <- lapply(df_final[load_cols], function(x)
      ifelse(abs(x) < threshold_loading, 0, x)
    )
  }
  if (is.null(item_factor_map)) {
    item_factor_map <- setNames(
      apply(as.matrix(df_final[, load_cols]), 1, function(x) which.max(abs(x))),
      df_final$Items
    )
  }

  # 8. Análisis conceptual con IA (opcional)
  conceptual_analysis <- NULL
  if (analyze_removed && !is.null(api_key) && !is.null(item_definitions)) {
    if (verbose) cat("Iniciando análisis conceptual con IA...\n")
    # ítems eliminados
    analysis_removed <- setNames(vector("list", length(removed_items)), removed_items)
    for (it in removed_items) {
      if (verbose) cat(" Analizando eliminado:", it, "...\n")
      context <- if (n_factors==1) {
        construct_definition
      } else {
        fac_i <- item_factor_map[[it]]
        if (!is.null(factor_definitions[[as.character(fac_i)]])) {
          paste0("Factor: '", factor_definitions[[as.character(fac_i)]], "'.")
        } else {
          paste0("Factor ", fac_i, ".")
        }
      }
      analysis_removed[[it]] <- analyze_item_with_gpt(
        item=it, definition=item_definitions[[it]],
        context=context, action="exclusion"
      )
    }
    # ítems conservados
    kept <- setdiff(items, removed_items)
    analysis_kept <- setNames(vector("list", length(kept)), kept)
    for (it in kept) {
      if (verbose) cat(" Analizando conservado:", it, "...\n")
      context <- if (n_factors==1) {
        construct_definition
      } else {
        fac_i <- item_factor_map[[it]]
        if (!is.null(factor_definitions[[as.character(fac_i)]])) {
          paste0("Factor: '", factor_definitions[[as.character(fac_i)]], "'.")
        } else {
          paste0("Factor ", fac_i, ".")
        }
      }
      analysis_kept[[it]] <- analyze_item_with_gpt(
        item=it, definition=item_definitions[[it]],
        context=context, action="conservación"
      )
    }
    conceptual_analysis <- list(removed=analysis_removed, kept=analysis_kept)
  }

  # 9. Devolver resultados
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
