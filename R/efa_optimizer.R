efa_optimizer <- function(data,
                          name_items,
                          item_range = NULL,
                          n_factors = 3,
                          exclude_items = NULL,
                          # Thresholds (includes Heywood tolerances)
                          thresholds = list(
                            rmsea = 0.08,
                            loading = 0.30,
                            min_items_per_factor = 3,
                            heywood_tol = 1e-6,
                            near_heywood = 0.015
                          ),
                          # Model configuration
                          model_config = list(
                            estimator = "WLSMV",
                            rotation  = "oblimin"
                          ),
                          # AI (optional)
                          use_ai_analysis = FALSE,
                          ai_config = list(
                            api_key = NULL,
                            generate_names = FALSE,
                            only_removed = TRUE,
                            gpt_model = "gpt-3.5-turbo",
                            language = "english",
                            analysis_detail = "detailed",
                            domain_name = "Default Domain",
                            scale_title = "Default Scale Title",
                            construct_definition = "",
                            model_name = "EFA Model",
                            item_definitions = NULL
                          ),
                          verbose = TRUE, ...) {

  # ────────────────────────────────────────────────────────────────────────────
  # Utilities and setup
  # ────────────────────────────────────────────────────────────────────────────
  create_progress_bar <- function(current, total, width = 30) {
    if (total == 0) return("No items")
    percent <- current / total
    filled  <- floor(percent * width)
    bar <- paste0("[", paste0(rep("█", filled), collapse = ""),
                  paste0(rep("░", width - filled), collapse = ""), "]")
    sprintf("%s %d/%d (%.1f%%)", bar, current, total, percent * 100)
  }

  `%||%` <- function(a, b) if (is.null(a)) b else a

  fmt_num <- function(x, digits = 3) {
    # Maneja NULL o vector vacío
    if (is.null(x) || length(x) == 0) return("No disponible")
    # Vector lógico: válido si no es NA y es finito
    ok <- !is.na(x) & is.finite(x)
    # Construye el patrón %.3f, %.2f, etc.
    pat <- paste0("%.", digits, "f")
    out <- character(length(x))
    out[ok]  <- sprintf(pat, x[ok])
    out[!ok] <- "No disponible"
    out
  }


  # Items identification
  if (is.null(item_range)) {
    items <- grep(paste0("^", name_items, "\\d+$"), names(data), value = TRUE)
    if (length(items) == 0) {
      stop("No items found with pattern '", name_items, "'. Specify 'item_range'.")
    }
  } else {
    items <- paste0(name_items, seq(item_range[1], item_range[2]))
  }
  n_items   <- length(items)
  max_steps <- n_items - 1

  if (verbose) {
    cat("\nSTARTING EFA OPTIMIZATION\n")
    cat("Initial items:", n_items, "| Factors:", n_factors,
        "| RMSEA target: ≤", thresholds$rmsea, "\n")
  }

  # AI configuration
  default_ai <- list(
    api_key = NULL, generate_names = FALSE, only_removed = TRUE,
    item_definitions = NULL, domain_name = "Default Domain",
    scale_title = "Default Scale Title",
    construct_definition = "", model_name = "EFA Model",
    gpt_model = "gpt-3.5-turbo",
    language = "english",
    analysis_detail = "detailed"
  )
  ai_config <- modifyList(default_ai, ai_config)

  if (use_ai_analysis && !requireNamespace("httr", quietly = TRUE)) install.packages("httr")
  if (use_ai_analysis && !requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")

  # ────────────────────────────────────────────────────────────────────────────
  # SECCIÓN MEJORADA DE ANÁLISIS CON GPT (incluye razón y RMSEA, y timeline)
  # ────────────────────────────────────────────────────────────────────────────
  analyze_item_with_gpt_improved <- function(item, definition, context,
                                             item_stats = NULL,
                                             action = c("exclude","keep")) {
    if (is.null(ai_config$api_key)) return("AI not configured")
    act <- match.arg(action)

    if (is.null(definition) || definition == "") {
      return(if (act == "exclude") "No definition provided for exclusion."
             else "No definition provided for retention.")
    }

    max_tokens_config <- switch(ai_config$analysis_detail,
                                "brief" = 200,
                                "standard" = 400,
                                "detailed" = 600,
                                400)
    word_limit <- switch(ai_config$analysis_detail,
                         "brief" = "80-100",
                         "standard" = "150-180",
                         "detailed" = "250-300",
                         "150-180")

    r_reason <- item_stats$reason %||% "No especificada"
    r_rmsea  <- item_stats$rmsea_at_removal %||% NA_real_
    r_rmsea_txt <- fmt_num(r_rmsea)

    technical_info <- ""
    if (!is.null(item_stats)) {
      if (tolower(ai_config$language) %in% c("spanish","español")) {
        base_txt <- sprintf(
          "Información técnica: Carga primaria=%s; h²=%s; Razón=%s; RMSEA en ese paso=%s.",
          fmt_num(item_stats$loading),
          fmt_num(item_stats$h2),
          r_reason,
          r_rmsea_txt
        )
        ambi_txt <- ""
        if (!is.null(item_stats$second_loading) || !is.null(item_stats$delta_loading)) {
          ambi_txt <- sprintf(" Ambigüedad: segunda carga=%s; Δ=|λ1|-|λ2|=%s.",
                              fmt_num(item_stats$second_loading),
                              fmt_num(item_stats$delta_loading))
        }
        technical_info <- paste0(base_txt, ambi_txt)
      } else {
        base_txt <- sprintf(
          "Technical information: Primary loading=%s; h²=%s; Reason=%s; RMSEA at that step=%s.",
          fmt_num(item_stats$loading),
          fmt_num(item_stats$h2),
          r_reason,
          r_rmsea_txt
        )
        ambi_txt <- ""
        if (!is.null(item_stats$second_loading) || !is.null(item_stats$delta_loading)) {
          ambi_txt <- sprintf(" Ambiguity: second loading=%s; Δ=|λ1|-|λ2|=%s.",
                              fmt_num(item_stats$second_loading),
                              fmt_num(item_stats$delta_loading))
        }
        technical_info <- paste0(base_txt, ambi_txt)
      }
    }

    if (tolower(ai_config$language) %in% c("spanish","español")) {
      if (act == "exclude") {
        prompt <- sprintf(
          "Como experto en psicometría, proporciona un análisis detallado de por qué el ítem '%s' (\"%s\") fue correctamente eliminado de una escala que mide '%s'. %s

Contexto breve del modelo:
%s

Estructura tu análisis en tres aspectos integrados:
1) Problemas psicométricos identificados: comienza explícitamente indicando la Razón registrada por el algoritmo: '%s' y el RMSEA al momento de la eliminación: %s; enlaza esto con la evidencia (cargas factoriales, comunalidades, cargas cruzadas, posibles dependencias locales) y explica cómo estos factores justifican la salida del ítem.
2) Desalineación conceptual con el constructo central (si aplica) o redundancia con otros ítems.
3) Impacto positivo de su eliminación en la validez y coherencia de la escala (mejoras en ajuste, claridad factorial, parsimonia).

Proporciona un análisis técnico pero fluido, conectando los tres aspectos de forma narrativa (%s palabras).

IMPORTANTE: NO uses formato markdown (sin asteriscos, sin negritas). Escribe en texto plano continuo.",
          item, definition, ai_config$construct_definition, technical_info,
          context %||% "",
          r_reason, r_rmsea_txt,
          word_limit
        )
        system_msg <- "Eres un experto en psicometría y desarrollo de escalas. Proporciona análisis técnicos detallados en español, usando terminología psicométrica precisa. NO uses formato markdown, solo texto plano."
      } else {
        prompt <- sprintf(
          "Como experto en psicometría, justifica detalladamente por qué el ítem '%s' (\"%s\") debe mantenerse en una escala que mide '%s'. %s

Contexto breve del modelo:
%s

Estructura tu análisis en tres aspectos integrados:
1) Fortalezas psicométricas del ítem (cargas, comunalidades, especificidad factorial). Si corresponde, incluye el registro del algoritmo: Razón='%s'; RMSEA en ese momento: %s.
2) Alineación conceptual con el constructo central.
3) Contribución única a la validez de la escala (discriminación, cobertura de contenido, no redundancia).

Proporciona un análisis técnico pero fluido (%s palabras).

IMPORTANTE: NO uses formato markdown (sin asteriscos, sin negritas). Escribe en texto plano continuo.",
          item, definition, ai_config$construct_definition, technical_info,
          context %||% "",
          r_reason, r_rmsea_txt,
          word_limit
        )
        system_msg <- "Eres un experto en psicometría y desarrollo de escalas. Proporciona análisis técnicos detallados en español. NO uses formato markdown, solo texto plano."
      }
    } else {
      if (act == "exclude") {
        prompt <- sprintf(
          "As a psychometrics expert, provide a detailed analysis of why item '%s' (\"%s\") was correctly removed from a scale measuring '%s'. %s

Brief model context:
%s

Structure your analysis in three integrated aspects:
1) Psychometric problems identified: explicitly start by stating the algorithm's recorded Reason: '%s' and the RMSEA at removal: %s; then tie this to evidence (factor loadings, communalities, cross-loadings, potential local dependence) explaining how these justify removal.
2) Conceptual misalignment with the construct (if applicable) or redundancy with other items.
3) Positive impact of removal on scale validity and coherence (fit improvement, factorial clarity, parsimony).

Provide a technical yet flowing analysis, connecting all three aspects narratively (%s words).

IMPORTANT: DO NOT use markdown formatting. Write in continuous plain text.",
          item, definition, ai_config$construct_definition, technical_info,
          context %||% "",
          r_reason, r_rmsea_txt,
          word_limit
        )
        system_msg <- "You are an expert in psychometrics and scale development. Provide detailed technical analyses using precise psychometric terminology. DO NOT use markdown formatting, only plain text."
      } else {
        prompt <- sprintf(
          "As a psychometrics expert, provide a detailed justification for why item '%s' (\"%s\") should be retained in a scale measuring '%s'. %s

Brief model context:
%s

Structure your analysis in three integrated aspects:
1) Psychometric strengths (loadings, communalities, factorial specificity). If relevant, include the algorithm's record: Reason='%s'; RMSEA at that moment: %s.
2) Conceptual alignment with the core construct.
3) Unique contribution to scale validity (discrimination, content coverage, non-redundancy).

Provide a technical yet flowing analysis (%s words).

IMPORTANT: DO NOT use markdown formatting. Write in continuous plain text.",
          item, definition, ai_config$construct_definition, technical_info,
          context %||% "",
          r_reason, r_rmsea_txt,
          word_limit
        )
        system_msg <- "You are an expert in psychometrics and scale development. Provide detailed technical analyses. DO NOT use markdown formatting, only plain text."
      }
    }

    max_attempts <- 3
    for (attempt in 1:max_attempts) {
      resp <- tryCatch({
        httr::POST(
          "https://api.openai.com/v1/chat/completions",
          httr::add_headers(
            Authorization = paste("Bearer", ai_config$api_key),
            `Content-Type` = "application/json"
          ),
          httr::timeout(60),
          body = jsonlite::toJSON(list(
            model = ai_config$gpt_model,
            messages = list(
              list(role = "system", content = system_msg),
              list(role = "user", content = prompt)
            ),
            temperature = 0.3,
            max_tokens = max_tokens_config
          ), auto_unbox = TRUE)
        )
      }, error = function(e) {
        return(NULL)
      })

      if (!is.null(resp)) {
        status <- httr::status_code(resp)
        if (status == 200) {
          content <- httr::content(resp)
          return(tryCatch(
            content$choices[[1]]$message$content,
            error = function(e) "Error extracting GPT response."
          ))
        } else if (status %in% c(502,503,504)) {
          if (attempt < max_attempts) {
            wait_time <- 2^attempt
            if (verbose) {
              msg <- if (tolower(ai_config$language) %in% c("spanish","español")) {
                sprintf("   Servidor ocupado (HTTP %d). Reintentando en %d segundos...\n", status, wait_time)
              } else {
                sprintf("   Server busy (HTTP %d). Retrying in %d seconds...\n", status, wait_time)
              }
              cat(msg)
            }
            Sys.sleep(wait_time)
          } else {
            return(sprintf("Server error after %d attempts (HTTP %d)", max_attempts, status))
          }
        } else if (status == 429) {
          if (attempt < max_attempts) {
            wait_time <- 10 * attempt
            if (verbose) {
              msg <- if (tolower(ai_config$language) %in% c("spanish","español")) {
                sprintf("   Límite de velocidad alcanzado. Esperando %d segundos...\n", wait_time)
              } else {
                sprintf("   Rate limit reached. Waiting %d seconds...\n", wait_time)
              }
              cat(msg)
            }
            Sys.sleep(wait_time)
          } else {
            return("Rate limit exceeded after multiple attempts")
          }
        } else {
          return(paste("GPT error - HTTP", status))
        }
      } else {
        if (attempt < max_attempts) {
          wait_time <- 2^attempt
          if (verbose) {
            msg <- if (tolower(ai_config$language) %in% c("spanish","español")) {
              sprintf("   Error de conexión. Reintentando en %d segundos...\n", wait_time)
            } else {
              sprintf("   Connection error. Retrying in %d seconds...\n", wait_time)
            }
            cat(msg)
          }
          Sys.sleep(wait_time)
        } else {
          return("Connection error after multiple attempts")
        }
      }
    }
    return("Analysis failed after all attempts")
  }

  # Initialize tracking variables
  if (is.null(exclude_items)) exclude_items <- character(0)
  removed_items <- character()
  steps_log <- data.frame(step = integer(), removed_item = character(),
                          reason = character(), rmsea = numeric(), stringsAsFactors = FALSE)
  step_counter <- 0
  mod <- NULL
  last_ev <- NULL
  curr_rmsea <- NA_real_

  # Almacenar estadísticas de ítems capturadas en el instante de eliminación
  item_removal_stats <- list()

  # ────────────────────────────────────────────────────────────────────────────
  # Evaluación de estructura con matriz Φ (oblicua)
  # ────────────────────────────────────────────────────────────────────────────
  evaluate_structure <- function(df, phi, thresholds) {
    load_cols <- which(startsWith(names(df), "f"))
    L <- as.matrix(df[, load_cols, drop = FALSE])
    its <- df$Items

    # Comunalidades y unicidades con Φ
    h2  <- rowSums((L %*% phi) * L)  # diag(L Φ L')
    psi <- 1 - h2

    heywood_flag      <- (psi < -thresholds$heywood_tol) |
      apply(abs(L) > 1 + 1e-6, 1, any)
    near_heywood_flag <- !heywood_flag & (psi >= -thresholds$heywood_tol) &
      (psi < thresholds$near_heywood)

    ok      <- rep(TRUE, nrow(L))
    scores  <- rep(NA_real_, nrow(L))
    reasons <- rep("OK",  nrow(L))

    for (i in seq_len(nrow(L))) {
      if (heywood_flag[i]) {
        ok[i] <- FALSE; reasons[i] <- "Heywood"; scores[i] <- psi[i]; next
      }
      if (near_heywood_flag[i]) {
        ok[i] <- FALSE; reasons[i] <- "Near-Heywood"; scores[i] <- psi[i]; next
      }
      li  <- abs(L[i, ])
      cnt <- sum(li > thresholds$loading, na.rm = TRUE)
      if (cnt == 1) {
        ok[i] <- TRUE;  reasons[i] <- "OK";            scores[i] <- max(li)
      } else if (cnt == 0){
        ok[i] <- FALSE; reasons[i] <- "No loading";    scores[i] <- max(li)
      } else {
        s <- sort(li, TRUE)
        ok[i] <- FALSE; reasons[i] <- "Cross-loading"; scores[i] <- s[1] - s[2]  # menor = peor ambigüedad
      }
    }

    li_mat  <- abs(L)
    max_abs <- apply(li_mat, 1, max)
    primary <- apply(li_mat, 1, which.max)
    primary[ max_abs < thresholds$loading ] <- NA_integer_
    counts  <- tabulate(primary, nbins = ncol(L))

    structure_ok <- all(counts >= thresholds$min_items_per_factor)

    list(mat=L, items=its, ok=ok, scores=scores, reasons=reasons,
         counts=counts, structure_ok=structure_ok, h2=h2, psi=psi,
         heywood=heywood_flag, near_heywood=near_heywood_flag,
         primary=primary)
  }

  # Captura de métricas en el instante de eliminación
  capture_item_stats <- function(it, ev, df, rmsea, reason) {
    load_cols <- which(startsWith(names(df), "f"))
    idx <- which(ev$items == it)
    if (length(idx) == 1) {
      li <- abs(as.numeric(df[idx, load_cols, drop = TRUE]))
      s  <- sort(li, decreasing = TRUE)
      primary_loading <- if (length(li)) max(li, na.rm = TRUE) else NA_real_
      second_loading  <- if (length(s) >= 2) s[2] else NA_real_
      delta_loading   <- if (is.finite(primary_loading) && is.finite(second_loading)) primary_loading - second_loading else NA_real_
      list(
        reason            = reason,
        rmsea_at_removal  = rmsea,
        loading           = primary_loading,
        second_loading    = second_loading,
        delta_loading     = delta_loading,
        h2                = ev$h2[idx],
        psi               = ev$psi[idx],
        primary_factor    = ev$primary[idx]
      )
    } else {
      list(
        reason            = reason,
        rmsea_at_removal  = rmsea,
        loading           = NA_real_,
        second_loading    = NA_real_,
        delta_loading     = NA_real_,
        h2                = NA_real_,
        psi               = NA_real_,
        primary_factor    = NA_integer_
      )
    }
  }

  # ────────────────────────────────────────────────────────────────────────────
  # Main optimization loop
  # ────────────────────────────────────────────────────────────────────────────
  repeat {
    candidates <- setdiff(items, c(exclude_items, removed_items))
    if (length(candidates) < n_factors * thresholds$min_items_per_factor) {
      if (verbose) cat("\nNot enough items remaining to continue.\n"); break
    }
    if (step_counter >= max_steps) {
      if (verbose) cat("\nMaximum steps reached (", max_steps, ").\n"); break
    }

    converged <- TRUE
    tmp <- tryCatch({
      PsyMetricTools::EFA_modern(
        data = data, n_factors = n_factors, n_items = n_items,
        name_items = name_items, estimator = model_config$estimator,
        rotation = model_config$rotation, apply_threshold = FALSE,
        exclude_items = c(exclude_items, removed_items), ...
      )
    }, error = function(e) { if (verbose) cat("Did not converge:", e$message, "\n"); converged <<- FALSE; NULL })

    if (!converged || is.null(tmp) || is.null(tmp$Bondades_Original) || is.null(tmp$result_df)) break
    mod <- tmp
    curr_rmsea <- as.numeric(mod$Bondades_Original$rmsea.scaled[n_factors])

    # Φ oblicua
    phi <- if (!is.null(mod$InterFactor)) as.matrix(mod$InterFactor) else diag(n_factors)

    ev <- evaluate_structure(mod$result_df, phi, thresholds)
    last_ev <- ev

    if (verbose) {
      cat("Iteration", step_counter, "- RMSEA:", sprintf("%.3f", curr_rmsea), "\n")
      cat("Items per factor:", paste(ev$counts, collapse = " | "), "\n")
      cat("\n--- Current factorial structure ---\n")
      df_display <- mod$result_df
      load_cols2 <- which(startsWith(names(df_display), "f"))
      df_display[load_cols2] <- lapply(df_display[load_cols2], function(x)
        ifelse(abs(x) < thresholds$loading, 0, round(x, 3))
      )
      if (inherits(df_display, "tbl_df")) {
        print(df_display, n = Inf, width = Inf)
      } else {
        print(df_display)
      }
      cat("-----------------------------------\n")
    }

    # Priority 1: Heywood
    if (any(ev$heywood)) {
      worst <- ev$items[ which.min(ev$psi) ]
      # Capturar stats ANTES de eliminar
      item_removal_stats[[worst]] <- capture_item_stats(worst, ev, mod$result_df, curr_rmsea, "Heywood")
      removed_items <- c(removed_items, worst)
      step_counter  <- step_counter + 1
      steps_log <- rbind(steps_log, data.frame(
        step = step_counter, removed_item = worst, reason = "Heywood", rmsea = curr_rmsea,
        stringsAsFactors = FALSE
      ))
      if (verbose) cat("Removed", worst, "due to Heywood case (ψ=", sprintf("%.4f", min(ev$psi)), ")\n")
      next
    }

    # Priority 2: Near-Heywood
    if (any(ev$near_heywood)) {
      idx <- which(ev$near_heywood)
      worst <- ev$items[idx[ which.min(ev$psi[idx]) ]]
      item_removal_stats[[worst]] <- capture_item_stats(worst, ev, mod$result_df, curr_rmsea, "Near-Heywood")
      removed_items <- c(removed_items, worst)
      step_counter  <- step_counter + 1
      steps_log <- rbind(steps_log, data.frame(
        step = step_counter, removed_item = worst, reason = "Near-Heywood", rmsea = curr_rmsea,
        stringsAsFactors = FALSE
      ))
      if (verbose) cat("Removed", worst, "due to Near-Heywood case (ψ≈0)\n")
      next
    }

    # Priority 3: CROSS-LOADING primero (más ambiguo primero)
    cross_idx <- which(ev$reasons == "Cross-loading")
    if (length(cross_idx) > 0) {
      order_idx <- cross_idx[order(ev$scores[cross_idx], decreasing = FALSE)]
      selected <- NA_character_
      for (i in order_idx) {
        item <- ev$items[i]
        p    <- ev$primary[i]
        counts2 <- ev$counts
        if (!is.na(p)) counts2[p] <- counts2[p] - 1
        if (all(counts2 >= thresholds$min_items_per_factor)) {
          selected <- item
          break
        }
      }
      if (!is.na(selected)) {
        item_removal_stats[[selected]] <- capture_item_stats(selected, ev, mod$result_df, curr_rmsea, "Cross-loading (priority)")
        removed_items <- c(removed_items, selected)
        step_counter  <- step_counter + 1
        steps_log <- rbind(steps_log, data.frame(
          step = step_counter, removed_item = selected,
          reason = "Cross-loading (priority)", rmsea = curr_rmsea,
          stringsAsFactors = FALSE
        ))
        if (verbose) cat("Removed", selected, "due to: Cross-loading (priority)\n")
        next
      } else {
        if (verbose) cat("Cross-loading items detected but protected by min_items_per_factor; skipping cross-loading removal this step.\n")
      }
    }

    # Chequeos de finalización
    min_items_met <- all(ev$counts >= thresholds$min_items_per_factor)
    if (!is.na(curr_rmsea) && curr_rmsea <= thresholds$rmsea && all(ev$ok) && min_items_met) {
      if (verbose) cat("All criteria met (RMSEA, structure and minimum items). Optimization complete.\n")
      break
    }
    if (!is.na(curr_rmsea) && curr_rmsea <= thresholds$rmsea && all(ev$ok) && !min_items_met) {
      if (verbose) cat("RMSEA acceptable and no structural problems, but insufficient items per factor; stopping.\n")
      break
    }

    # Decisión: estructura si hay problemas; si estructura ok y RMSEA alto, entonces RMSEA
    if (!all(ev$ok)) {
      decision <- "structure"
    } else if (!is.na(curr_rmsea) && curr_rmsea > thresholds$rmsea) {
      decision <- "rmsea"
    } else {
      break
    }

    if (decision == "rmsea") {
      # RMSEA optimization exigiendo estructura OK en el modelo resultante
      cand_stats <- lapply(candidates, function(it) {
        m2 <- tryCatch(PsyMetricTools::EFA_modern(
          data = data, n_factors = n_factors, n_items = n_items, name_items = name_items,
          estimator = model_config$estimator, rotation = model_config$rotation,
          apply_threshold = FALSE, exclude_items = c(exclude_items, removed_items, it), ...
        ), error = function(e) NULL)
        if (is.null(m2) || is.null(m2$result_df)) return(NULL)
        phi2 <- if (!is.null(m2$InterFactor)) as.matrix(m2$InterFactor) else diag(n_factors)
        ev2  <- evaluate_structure(m2$result_df, phi2, thresholds)
        list(rmsea = as.numeric(m2$Bondades_Original$rmsea.scaled[n_factors]),
             ok_min = all(ev2$counts >= thresholds$min_items_per_factor),
             ok_struct = all(ev2$ok))
      })
      cand_rmsea <- sapply(cand_stats, function(cs)
        if (is.null(cs) || !cs$ok_min || !cs$ok_struct) NA_real_ else cs$rmsea)
      names(cand_rmsea) <- candidates

      if (!all(is.na(cand_rmsea))) {
        best_val <- min(cand_rmsea, na.rm = TRUE)
        if (is.finite(best_val) && best_val < curr_rmsea) {
          best <- names(which.min(cand_rmsea))
          # Capturar stats ANTES de eliminar (usar best_val como RMSEA resultante)
          item_removal_stats[[best]] <- capture_item_stats(best, ev, mod$result_df, best_val, "RMSEA improvement")
          removed_items <- c(removed_items, best)
          step_counter  <- step_counter + 1
          steps_log <- rbind(steps_log, data.frame(
            step = step_counter, removed_item = best, reason = "RMSEA improvement", rmsea = best_val,
            stringsAsFactors = FALSE
          ))
          if (verbose) cat("Removed", best, "→ RMSEA improved to:", sprintf("%.3f", best_val), "\n")
          next
        }
      }
      decision <- "structure"
    }

    if (verbose && decision == "structure") cat("STRATEGY: Structural optimization (non-cross-loading)\n")

    if (all(ev$ok)) {
      if (verbose) cat("Structure acceptable but doesn't meet all criteria; stopping optimization.\n")
      break
    }

    # Remover peor problema estructural restante (sin cross-loading)
    prob_idx <- which(!ev$ok & ev$reasons != "Cross-loading")
    if (length(prob_idx) == 0) {
      prob_idx <- which(!ev$ok)
    }
    worst    <- ev$items[ prob_idx[ which.min(ev$scores[prob_idx]) ] ]
    reason   <- ev$reasons[ which(ev$items == worst) ]
    p_worst  <- ev$primary[ which(ev$items == worst) ]
    counts2  <- ev$counts; if (!is.na(p_worst)) counts2[p_worst] <- counts2[p_worst] - 1
    if (all(counts2 >= thresholds$min_items_per_factor)) {
      item_removal_stats[[worst]] <- capture_item_stats(worst, ev, mod$result_df, curr_rmsea, reason)
      removed_items <- c(removed_items, worst)
      step_counter  <- step_counter + 1
      steps_log <- rbind(steps_log, data.frame(
        step = step_counter, removed_item = worst, reason = reason, rmsea = curr_rmsea,
        stringsAsFactors = FALSE
      ))
      if (verbose) cat("Removed", worst, "due to:", reason, "\n")
      next
    } else {
      if (verbose) cat("Structural issue found (", worst, ") but protected by min_items_per_factor; stopping.\n", sep = "")
      break
    }
  }

  if (is.null(mod) || is.null(mod$result_df)) stop("Could not generate a valid EFA model.")

  # Estructura final con umbral para reporte
  df_final  <- mod$result_df
  load_cols <- which(startsWith(names(df_final), "f"))
  df_final[load_cols] <- lapply(df_final[load_cols], function(x)
    ifelse(abs(x) < thresholds$loading, 0, x)
  )

  # Descripción de estructura final
  A  <- as.matrix(df_final[, load_cols, drop = FALSE])
  mA <- apply(abs(A), 1, max)
  w  <- apply(abs(A), 1, which.max)
  w[mA < thresholds$loading] <- NA_integer_
  factor_map_final <- setNames(w, df_final$Items)
  fac_lists <- tapply(names(factor_map_final), factor_map_final, function(v) paste(v, collapse = ", "))
  fac_lists <- fac_lists[!is.na(names(fac_lists))]
  structure_desc <- paste0(
    "Final structure: ",
    paste0("Factor ", names(fac_lists), " contains {", fac_lists, "}", collapse = "; ")
  )

  # ────────────────────────────────────────────────────────────────────────────
  # Timeline (DETALLE DE ELIMINACIÓN DE ÍTEMS) para el contexto de IA
  # ────────────────────────────────────────────────────────────────────────────
  timeline_str <- NULL
  if (nrow(steps_log) > 0) {
    header <- "DETALLE DE ELIMINACIÓN DE ÍTEMS\n step removed_item  reason                     rmsea"
    lines <- sprintf("%3d %-12s %-24s %s",
                     steps_log$step,
                     steps_log$removed_item,
                     steps_log$reason,
                     fmt_num(steps_log$rmsea))
    timeline_str <- paste(c(header, lines), collapse = "\n")
  }

  # ────────────────────────────────────────────────────────────────────────────
  # Análisis conceptual con IA (usa razón + RMSEA + timeline)
  # ────────────────────────────────────────────────────────────────────────────
  conceptual_analysis <- NULL
  if (use_ai_analysis && length(items) > 0 && !is.null(ai_config$api_key) &&
      !is.null(ai_config$item_definitions)) {

    is_spanish <- tolower(ai_config$language) %in% c("spanish","español")

    if (verbose) {
      if (is_spanish) {
        cat("\n═══ Análisis Conceptual con IA ═══\n")
        cat("Modelo:", ai_config$gpt_model, "\n")
        cat("Nivel de detalle:", ai_config$analysis_detail, "\n")
      } else {
        cat("\n═══ AI Conceptual Analysis ═══\n")
        cat("Model:", ai_config$gpt_model, "\n")
        cat("Detail level:", ai_config$analysis_detail, "\n")
      }
    }

    analysis_removed <- NULL
    analysis_kept <- NULL

    combined_context <- paste0(structure_desc, if (!is.null(timeline_str)) paste0("\n\n", timeline_str) else "")

    # Ítems eliminados
    if (length(removed_items) > 0) {
      analysis_removed <- setNames(vector("list", length(removed_items)), removed_items)
      for (i in seq_along(removed_items)) {
        it <- removed_items[i]
        if (verbose) {
          progress <- create_progress_bar(i - 1, length(removed_items), width = 20)
          label <- if (is_spanish) "Analizando ítems eliminados:" else "Analyzing removed items:"
          cat("\r", label, progress, sep = " ")
          flush.console()
        }
        if (it %in% names(ai_config$item_definitions)) {
          analysis_removed[[it]] <- analyze_item_with_gpt_improved(
            it,
            ai_config$item_definitions[[it]],
            combined_context,
            item_stats = item_removal_stats[[it]],
            action = "exclude"
          )
          Sys.sleep(0.5)
        } else {
          analysis_removed[[it]] <- "Item definition not provided"
        }
      }
      if (verbose) {
        progress <- create_progress_bar(length(removed_items), length(removed_items), width = 20)
        label <- if (is_spanish) "Analizando ítems eliminados:" else "Analyzing removed items:"
        cat("\r", label, progress, "\n", sep = " ")
      }
    }

    # Ítems conservados (si se pide)
    if (!ai_config$only_removed) {
      kept <- setdiff(items, removed_items)
      if (length(kept) > 0) {
        analysis_kept <- setNames(vector("list", length(kept)), kept)
        for (i in seq_along(kept)) {
          it <- kept[i]
          if (verbose) {
            progress <- create_progress_bar(i - 1, length(kept), width = 20)
            label <- if (is_spanish) "Analizando ítems conservados:" else "Analyzing retained items:"
            cat("\r", label, progress, sep = " ")
            flush.console()
          }
          kept_stats <- NULL
          if (!is.null(df_final) && it %in% df_final$Items) {
            item_idx <- which(df_final$Items == it)
            if (length(item_idx) > 0) {
              load_values <- as.numeric(df_final[item_idx, load_cols])
              kept_stats <- list(
                reason = "Retained",
                rmsea_at_removal = curr_rmsea,
                loading = max(abs(load_values)),
                second_loading = NA_real_,
                delta_loading = NA_real_,
                h2 = if (!is.null(last_ev) && it %in% last_ev$items) {
                  last_ev$h2[which(last_ev$items == it)[1]]
                } else NA_real_,
                psi = if (!is.null(last_ev) && it %in% last_ev$items) {
                  last_ev$psi[which(last_ev$items == it)[1]]
                } else NA_real_,
                primary_factor = if (!is.null(last_ev) && it %in% last_ev$items) {
                  last_ev$primary[which(last_ev$items == it)[1]]
                } else NA_integer_
              )
            }
          }
          if (it %in% names(ai_config$item_definitions)) {
            analysis_kept[[it]] <- analyze_item_with_gpt_improved(
              it,
              ai_config$item_definitions[[it]],
              combined_context,
              item_stats = kept_stats,
              action = "keep"
            )
            Sys.sleep(0.5)
          } else {
            analysis_kept[[it]] <- "Item definition not provided"
          }
        }
        if (verbose) {
          progress <- create_progress_bar(length(kept), length(kept), width = 20)
          label <- if (is_spanish) "Analizando ítems conservados:" else "Analyzing retained items:"
          cat("\r", label, progress, "\n", sep = " ")
        }
      }
    }

    conceptual_analysis <- list(
      removed = analysis_removed,
      kept = analysis_kept,
      item_stats = item_removal_stats,
      timeline = timeline_str
    )

    if (verbose) {
      complete_msg <- if (is_spanish) "✅ Análisis conceptual completado\n" else "✅ Conceptual analysis completed\n"
      cat(complete_msg)
    }
  }

  if (verbose) {
    cat("\nOptimization completed in", step_counter, "iterations.\n")
    if (length(removed_items)) cat("Items removed:", paste(removed_items, collapse = ", "), "\n")
    cat("Analysis finished.\n")
  }

  list(
    final_structure     = df_final,
    removed_items       = removed_items,
    steps_log           = steps_log,
    iterations          = step_counter,
    final_rmsea         = curr_rmsea,
    bondades_original   = mod$Bondades_Original,
    specifications      = mod$Specifications,
    inter_factor_correlation = {
      if (n_factors > 1 && !is.null(mod$InterFactor)) as.matrix(mod$InterFactor) else diag(n_factors)
    },
    last_h2             = if (!is.null(last_ev)) last_ev$h2  else NULL,
    last_psi            = if (!is.null(last_ev)) last_ev$psi else NULL,
    last_flags          = if (!is.null(last_ev)) list(heywood=last_ev$heywood, near=last_ev$near_heywood) else NULL,
    conceptual_analysis = conceptual_analysis,
    config_used         = list(thresholds = thresholds, model_config = model_config,
                               use_ai_analysis = use_ai_analysis, ai_config = ai_config)
  )
}
