stepwise_efa_removal_structure <- function(data,
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
                                           model_index = NULL,
                                           verbose = TRUE,
                                           ...) {
  # Función interna para comprobar e instalar los paquetes necesarios
  check_install_packages <- function() {
    # Verificar e instalar 'devtools' si no está disponible (necesario para instalar paquetes desde GitHub)
    if (!require("devtools", quietly = TRUE)) {
      install.packages("devtools")
      library(devtools)
    }

    # Instalar condicionalmente PsyMetricTools desde GitHub (forzando la instalación)
    if (!require("PsyMetricTools", quietly = TRUE)) {
      devtools::install_github("jventural/PsyMetricTools", force = TRUE)
    }

    # Si se requiere otro paquete usado en el flujo, se puede agregar condicionalmente aquí.
    # Por ejemplo, si se necesita BayesPsyMetrics, se puede usar:
    # if (!require("BayesPsyMetrics", quietly = TRUE)) {
    #   devtools::install_github("jventural/BayesPsyMetrics")
    # }
  }

  # Llamar a la función interna para asegurar que los paquetes necesarios estén instalados
  check_install_packages()

  # 1. Determinar los ítems a usar
  if (is.null(items)) {
    if (is.null(n_items)) {
      all_items <- grep(paste0("^", name_items, "\\d+$"), names(data), value = TRUE)
      items <- all_items
    } else {
      items <- paste0(name_items, 1:n_items)
    }
  }
  if (is.null(n_items)) {
    n_items <- length(items)
  }
  if (is.null(max_steps)) {
    max_steps <- length(items) - 1
  }
  if (is.null(model_index)) {
    model_index <- if (n_factors > 1) n_factors else 1
  }

  # 2. Inicialización de variables
  removed_items <- character(0)
  steps_log <- data.frame(step = integer(), removed_item = character(),
                          reason = character(), rmsea = numeric(),
                          stringsAsFactors = FALSE)
  step_counter <- 0

  # 3. Función auxiliar para evaluar la estructura factorial
  evaluate_structure <- function(loadings_df, threshold_loading) {
    # Se asume que las columnas que contienen las cargas comienzan con "f"
    loadings_mat <- as.matrix(loadings_df[, grep("^f", names(loadings_df))])
    # Se asume que la tabla tiene una columna "Items" con los nombres de los ítems
    item_names <- loadings_df$Items
    n_items_load <- nrow(loadings_mat)
    ok_items <- rep(FALSE, n_items_load)
    scores <- rep(NA, n_items_load)
    reasons <- rep("", n_items_load)

    for (i in seq_len(n_items_load)) {
      l_i <- abs(loadings_mat[i, ])
      count_above <- sum(l_i > threshold_loading, na.rm = TRUE)
      if (count_above == 1) {
        ok_items[i] <- TRUE
        scores[i] <- max(l_i)
        reasons[i] <- "OK"
      } else if (count_above == 0) {
        ok_items[i] <- FALSE
        scores[i] <- max(l_i)
        reasons[i] <- "No loading > threshold"
      } else {  # cross-loading: más de una carga alta
        sorted <- sort(l_i, decreasing = TRUE)
        diff <- sorted[1] - sorted[2]
        ok_items[i] <- FALSE
        scores[i] <- diff
        reasons[i] <- "Cross-loading"
      }
    }

    # Cantidad de ítems por factor con carga > threshold_loading
    factor_counts <- colSums(loadings_mat > threshold_loading, na.rm = TRUE)
    structure_ok <- all(ok_items) && all(factor_counts >= min_items_per_factor)

    list(structure_ok = structure_ok,
         ok_items = ok_items,
         scores = scores,
         reasons = reasons,
         factor_counts = factor_counts,
         loadings_mat = loadings_mat,
         item_names = item_names)
  }

  # 4. Bucle iterativo hasta cumplir ambos criterios o agotarse las posibilidades
  repeat {
    # Verificamos que queden suficientes ítems para asignar al menos min_items_per_factor por factor
    current_items <- setdiff(items, removed_items)
    if (length(current_items) < n_factors * min_items_per_factor) {
      if (verbose) {
        cat("No quedan ítems suficientes para tener al menos", min_items_per_factor,
            "por cada uno de", n_factors, "factores.\n")
      }
      break
    }
    if (step_counter >= max_steps) {
      if (verbose) cat("Se alcanzó el máximo número de pasos (", max_steps, ").\n")
      break
    }

    # Estimar el modelo EFA actual
    current_model <- tryCatch({
      EFA_modern(data = data,
                 n_factors = n_factors,
                 n_items = n_items,
                 name_items = name_items,
                 estimator = estimator,
                 rotation = rotation,
                 apply_threshold = apply_threshold,
                 exclude_items = removed_items,
                 ...)
    }, error = function(e) {
      stop("Error en EFA_modern: ", e$message)
    })

    # Extraer RMSEA actual
    if (is.null(current_model$Bondades_Original) ||
        is.null(current_model$Bondades_Original$rmsea.scaled[model_index])) {
      current_rmsea <- NA
    } else {
      current_rmsea <- current_model$Bondades_Original$rmsea.scaled[model_index]
    }
    if (verbose) {
      cat("Iteración", step_counter, "- RMSEA actual:", round(current_rmsea, 4), "\n")
    }

    # Evaluar la estructura según las cargas
    loadings_df <- current_model$result_df
    structure_eval <- evaluate_structure(loadings_df, threshold_loading)

    if (verbose) {
      cat("Items por factor (carga > ", threshold_loading, "): ",
          paste(names(structure_eval$factor_counts), structure_eval$factor_counts, collapse = " | "), "\n")

      problematic <- which(!structure_eval$ok_items)
      if (length(problematic) > 0) {
        cat("Ítems problemáticos según carga:\n")
        print(data.frame(Item = structure_eval$item_names[problematic],
                         Score = structure_eval$scores[problematic],
                         Reason = structure_eval$reasons[problematic],
                         stringsAsFactors = FALSE))
      }
    }

    # Comprobamos si se cumplen ambos criterios: RMSEA y estructura
    if (!is.na(current_rmsea) && (current_rmsea <= threshold_rmsea) && structure_eval$structure_ok) {
      if (verbose) {
        cat("Modelo aceptable encontrado: RMSEA =", round(current_rmsea,4),
            "y estructura factorial aceptable.\n")
      }
      break
    }

    # Decisión de eliminación:
    # Prioridad 1: si el RMSEA es demasiado alto (mayor al umbral), se intenta mejorar el ajuste.
    # Prioridad 2: si el RMSEA es aceptable pero la estructura falla, se eliminan ítems problemáticos.
    decision_method <- NULL
    if (!is.na(current_rmsea) && current_rmsea > threshold_rmsea) {
      decision_method <- "rmsea"
    } else if (!structure_eval$structure_ok) {
      decision_method <- "structure"
    }

    if (decision_method == "rmsea") {
      # Evaluar la exclusión de cada ítem candidato para ver cuál mejora más el RMSEA.
      candidate_rmsea <- numeric(length(current_items))
      names(candidate_rmsea) <- current_items

      for (it in current_items) {
        temp_removed <- c(removed_items, it)
        temp_model <- tryCatch({
          EFA_modern(data = data,
                     n_factors = n_factors,
                     n_items = n_items,
                     name_items = name_items,
                     estimator = estimator,
                     rotation = rotation,
                     apply_threshold = apply_threshold,
                     exclude_items = temp_removed,
                     ...)
        }, error = function(e) NULL)
        if (!is.null(temp_model) &&
            !is.null(temp_model$Bondades_Original$rmsea.scaled[model_index])) {
          candidate_rmsea[it] <- temp_model$Bondades_Original$rmsea.scaled[model_index]
        } else {
          candidate_rmsea[it] <- NA
        }
      }

      if (all(is.na(candidate_rmsea))) {
        if (verbose) cat("Ningún modelo convergió al evaluar eliminación basada en RMSEA.\n")
        break
      }

      best_item <- names(which.min(candidate_rmsea))
      best_rmsea <- min(candidate_rmsea, na.rm = TRUE)

      # Solo se remueve si se logra mejorar el RMSEA
      if (!is.na(best_rmsea) && best_rmsea < current_rmsea) {
        removed_items <- c(removed_items, best_item)
        step_counter <- step_counter + 1
        steps_log <- rbind(steps_log,
                           data.frame(step = step_counter,
                                      removed_item = best_item,
                                      reason = "Mejora RMSEA",
                                      rmsea = best_rmsea,
                                      stringsAsFactors = FALSE))
        if (verbose) {
          cat("Paso", step_counter, "- Se remueve", best_item, "por mejora en RMSEA:",
              round(current_rmsea,4), "->", round(best_rmsea,4), "\n")
        }
      } else {
        # Si no se mejora el RMSEA, se pasa a evaluar la estructura
        decision_method <- "structure"
      }
    }

    if (decision_method == "structure") {
      # Seleccionar entre los ítems problemáticos el que tenga el peor desempeño (score mínimo)
      problematic_indices <- which(!structure_eval$ok_items)
      if (length(problematic_indices) == 0) {
        if (verbose) cat("No se identificaron ítems problemáticos según la estructura, pero aún falla el modelo.\n")
        break
      }
      remove_index <- problematic_indices[which.min(structure_eval$scores[problematic_indices])]
      item_to_remove <- structure_eval$item_names[remove_index]
      removed_items <- c(removed_items, item_to_remove)
      step_counter <- step_counter + 1
      steps_log <- rbind(steps_log,
                         data.frame(step = step_counter,
                                    removed_item = item_to_remove,
                                    reason = structure_eval$reasons[remove_index],
                                    rmsea = current_rmsea,
                                    stringsAsFactors = FALSE))
      if (verbose) {
        cat("Paso", step_counter, "- Se remueve", item_to_remove,
            "por problemas en la estructura:", structure_eval$reasons[remove_index], "\n")
      }
    }
  }  # Fin del repeat

  # Se retorna el modelo final (tabla de cargas), los ítems removidos, y el log del proceso
  list(final_structure = if (exists("loadings_df")) loadings_df else NULL,
       removed_items = removed_items,
       steps_log = steps_log,
       iterations = step_counter,
       final_rmsea = current_rmsea)
}
