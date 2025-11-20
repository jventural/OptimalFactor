efa_boosting <- function(data,
                         name_items,
                         item_range = NULL,
                         n_factors = 3,
                         exclude_items = NULL,
                         # Thresholds (estructura y correlaciones)
                         thresholds = list(
                           loading = 0.30,
                           min_items_per_factor = 3,
                           heywood_tol = 1e-6,
                           near_heywood = 0.015,
                           min_interfactor_correlation = 0.32
                         ),
                         # Configuración de modelo
                         model_config = list(
                           estimator = "WLSMV",
                           rotation  = "oblimin"
                         ),
                         # Performance
                         performance = list(
                           max_candidates_eval = NULL,  # NULL => evalúa TODOS (en modo greedy)
                           timeout_efa = 30,
                           timeout_optimization = 120,
                           use_timeouts = FALSE
                         ),
                         # ACTIVADOR EXPLÍCITO: GLOBAL vs GREEDY
                         use_global = FALSE,  # FALSE = solo greedy; TRUE = activa búsqueda global (con barra)
                         # Parámetros de búsqueda global
                         global_opt = list(
                           max_drop = 2,                    # tamaño máximo del subconjunto a evaluar a la vez
                           max_global_combinations = 5000,  # límite de combinaciones a evaluar
                           verbose = TRUE,                  # mensajes de la búsqueda global
                           progress_bar = TRUE              # barra de progreso durante la búsqueda global
                         ),
                         # Config de FIT COMPUESTO
                         fit_config = list(
                           targets = list(rmsea = 0.08, srmr = 0.06, cfi = 0.95),
                           margins = list(rmsea = 0.03, srmr = 0.03, cfi = 0.03),
                           base_weights = list(rmsea = 0.50, srmr = 0.25, cfi = 0.25),
                           small_df_cut = 5,          # df < 5 ⇒ bajar peso RMSEA
                           small_df_weights = list(rmsea = 0.15, srmr = 0.45, cfi = 0.40),
                           wlsmv_boost = list(rmsea = 0.8, srmr = 1.2, cfi = 1.1),  # multiplicadores si estimator == WLSMV
                           use_pclose_if_available = TRUE,
                           pclose_bonus = 0.10
                         ),
                         # IA opcional (se mantiene el hook)
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
                         verbose = TRUE,
                         ...) {

  # ───────────────── Utilidades ─────────────────
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
    if (is.null(x) || length(x) == 0) return("No disponible")
    ok <- !is.na(x) & is.finite(x)
    pat <- paste0("%.", digits, "f")
    out <- character(length(x))
    out[ok]  <- sprintf(pat, x[ok])
    out[!ok] <- "No disponible"
    out
  }
  clamp01 <- function(x) pmax(0, pmin(1, x))

  # Items
  if (is.null(item_range)) {
    items <- grep(paste0("^", name_items, "\\d+$"), names(data), value = TRUE)
    if (length(items) == 0) stop("No items found with pattern '", name_items, "'. Specify 'item_range'.")
  } else {
    items <- paste0(name_items, seq(item_range[1], item_range[2]))
  }
  n_items   <- length(items)
  max_steps <- n_items - 1

  # Mezclar defaults
  default_thresholds <- list(loading=0.30, min_items_per_factor=3, heywood_tol=1e-6, near_heywood=0.015, min_interfactor_correlation=0.32)
  thresholds <- modifyList(default_thresholds, thresholds)
  default_perf <- list(max_candidates_eval=NULL, timeout_efa=30, timeout_optimization=120, use_timeouts=FALSE)
  performance <- modifyList(default_perf, performance)
  default_model <- list(estimator="WLSMV", rotation="oblimin")
  model_config <- modifyList(default_model, model_config)
  default_global <- list(max_drop=2, max_global_combinations=5000, verbose=TRUE, progress_bar=TRUE)
  global_opt <- modifyList(default_global, global_opt)
  default_fitcfg <- list(
    targets=list(rmsea=0.08, srmr=0.08, cfi=0.95),
    margins=list(rmsea=0.03, srmr=0.03, cfi=0.03),
    base_weights=list(rmsea=0.50, srmr=0.25, cfi=0.25),
    small_df_cut=5,
    small_df_weights=list(rmsea=0.15, srmr=0.45, cfi=0.40),
    wlsmv_boost=list(rmsea=0.8, srmr=1.2, cfi=1.1),
    use_pclose_if_available=TRUE,
    pclose_bonus=0.10
  )
  fit_config <- modifyList(default_fitcfg, fit_config)

  if (verbose) {
    cat("\n╔════════════════════════════════════════════════════════════════╗\n")
    cat("║   EFA OPTIMIZER v4.1 (Greedy/Global + Objetivo compuesto)      ║\n")
    cat("╚════════════════════════════════════════════════════════════════╝\n\n")
    cat("Items iniciales:", n_items, "| Factores:", n_factors, "\n")
    cat("Targets → RMSEA≤", fit_config$targets$rmsea,
        " | SRMR≤", fit_config$targets$srmr,
        " | CFI≥",  fit_config$targets$cfi, "\n")
    cat("Modo de búsqueda:", if (isTRUE(use_global)) "GLOBAL (con barra de progreso)" else "GREEDY 1×1", "\n")
    cat("Min ítems/factor:", thresholds$min_items_per_factor, "| Loading umbral:", thresholds$loading, "\n\n")
  }

  # IA (hook intacto)
  default_ai <- list(api_key=NULL, generate_names=FALSE, only_removed=TRUE, item_definitions=NULL,
                     domain_name="Default Domain", scale_title="Default Scale Title",
                     construct_definition="", model_name="EFA Model", gpt_model="gpt-3.5-turbo",
                     language="english", analysis_detail="detailed")
  ai_config <- modifyList(default_ai, ai_config)

  if (use_ai_analysis && !requireNamespace("httr", quietly = TRUE)) install.packages("httr")
  if (use_ai_analysis && !requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")

  use_timeouts <- performance$use_timeouts
  if (use_timeouts && !requireNamespace("R.utils", quietly = TRUE)) {
    warning("Package 'R.utils' not found. Timeouts disabled. Install with: install.packages('R.utils')")
    use_timeouts <- FALSE
  }

  # ────────────────────────────────────────────────────────────────────────────
  # FUNCIÓN DE ANÁLISIS CON GPT (incluye razón, RMSEA y reintentos)
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

  # ───────── Funciones auxiliares de ajuste ─────────

  # Verificar correlaciones inter-factoriales mínimas
  check_interfactor_correlations <- function(phi, min_corr) {
    if (is.null(phi) || nrow(phi) < 2) return(list(ok = TRUE, min_value = NA_real_, violated = character(0)))
    n <- nrow(phi)
    violations <- character(0)
    min_value <- 1

    for (i in 1:(n-1)) {
      for (j in (i+1):n) {
        corr_val <- abs(phi[i, j])
        if (corr_val < min_value) min_value <- corr_val
        if (corr_val < min_corr) {
          violations <- c(violations, sprintf("f%d-f%d: %.3f", i, j, corr_val))
        }
      }
    }

    list(ok = length(violations) == 0, min_value = min_value, violated = violations)
  }

  pick_name <- function(nms, patterns) {
    for (pat in patterns) {
      idx <- grep(pat, nms, ignore.case = TRUE)
      if (length(idx) > 0) return(nms[idx[1]])
    }
    NA_character_
  }
  extract_fit <- function(mod, nf) {
    bo <- mod$Bondades_Original
    if (is.null(bo)) return(list(rmsea=NA_real_, srmr=NA_real_, cfi=NA_real_, tli=NA_real_,
                                 pclose=NA_real_, df=NA_real_))
    nms <- names(bo)
    getv <- function(key) if (is.na(key)) NA_real_ else suppressWarnings(as.numeric(bo[[key]][nf]))
    rmsea_nm <- pick_name(nms, c("^rmsea\\.scaled$", "^rmsea\\.robust$", "^rmsea$"))
    srmr_nm  <- pick_name(nms, c("^srmr(\\.scaled)?$", "^srmr$"))
    cfi_nm   <- pick_name(nms, c("^cfi\\.scaled$", "^cfi\\.robust$", "^cfi$"))
    tli_nm   <- pick_name(nms, c("^tli\\.scaled$", "^tli\\.robust$", "^tli$","^nnfi$"))
    pclose_nm<- pick_name(nms, c("^pclose(\\.scaled)?$", "^rmsea\\.pclose$"))
    df_nm    <- pick_name(nms, c("^df(\\.scaled)?$", "^df$"))
    list(
      rmsea = getv(rmsea_nm),
      srmr  = getv(srmr_nm),
      cfi   = getv(cfi_nm),
      tli   = getv(tli_nm),
      pclose= getv(pclose_nm),
      df    = getv(df_nm)
    )
  }
  normalize_w <- function(w) {
    s <- sum(unlist(w))
    lapply(w, function(x) as.numeric(x/s))
  }
  adaptive_weights <- function(df_val, estimator, fitcfg) {
    w <- fitcfg$base_weights
    if (!is.na(df_val) && df_val < fitcfg$small_df_cut) w <- fitcfg$small_df_weights
    if (!is.null(estimator) && toupper(estimator) == "WLSMV") {
      w <- list(
        rmsea = w$rmsea * fitcfg$wlsmv_boost$rmsea,
        srmr  = w$srmr  * fitcfg$wlsmv_boost$srmr,
        cfi   = w$cfi   * fitcfg$wlsmv_boost$cfi
      )
    }
    normalize_w(w)
  }
  index_loss <- function(value, target, margin, direction = c("le","ge")) {
    direction <- match.arg(direction)
    if (is.na(value)) return(0.5)
    if (direction == "le") {
      if (value <= target) return(0)
      return(clamp01((value - target)/margin))
    } else {
      if (value >= target) return(0)
      return(clamp01((target - value)/margin))
    }
  }
  composite_loss <- function(fit, fitcfg, estimator) {
    w <- adaptive_weights(fit$df, estimator, fitcfg)
    L_rmsea <- index_loss(fit$rmsea, fitcfg$targets$rmsea, fitcfg$margins$rmsea, "le")
    L_srmr  <- index_loss(fit$srmr,  fitcfg$targets$srmr,  fitcfg$margins$srmr,  "le")
    L_cfi   <- index_loss(fit$cfi,   fitcfg$targets$cfi,   fitcfg$margins$cfi,   "ge")
    loss <- w$rmsea * L_rmsea + w$srmr * L_srmr + w$cfi * L_cfi
    if (!is.na(fit$df) && fit$df < fitcfg$small_df_cut &&
        isTRUE(fitcfg$use_pclose_if_available) &&
        !is.na(fit$pclose) && fit$pclose >= 0.05) {
      loss <- max(0, loss - fitcfg$pclose_bonus)
    }
    as.numeric(loss)
  }

  # ───────── Evaluación de estructura ─────────
  evaluate_structure <- function(df, phi, thresholds) {
    load_cols <- which(startsWith(names(df), "f"))
    L <- as.matrix(df[, load_cols, drop = FALSE])
    its <- df$Items
    h2  <- rowSums((L %*% phi) * L)
    psi <- pmax(1 - h2, -1)
    heywood_flag      <- (psi < -thresholds$heywood_tol) | apply(abs(L) > 1 + 1e-6, 1, any)
    near_heywood_flag <- !heywood_flag & (psi >= -thresholds$heywood_tol) & (psi < thresholds$near_heywood)
    ok      <- rep(TRUE, nrow(L))
    scores  <- rep(NA_real_, nrow(L))
    reasons <- rep("OK", nrow(L))
    above_thresh <- abs(L) > thresholds$loading
    count_above  <- rowSums(above_thresh, na.rm = TRUE)
    max_loading  <- apply(abs(L), 1, max, na.rm = TRUE)
    heywood_idx <- which(heywood_flag)
    if (length(heywood_idx) > 0) { ok[heywood_idx] <- FALSE; reasons[heywood_idx] <- "Heywood"; scores[heywood_idx] <- psi[heywood_idx] }
    near_hey_idx <- which(near_heywood_flag)
    if (length(near_hey_idx) > 0) { ok[near_hey_idx] <- FALSE; reasons[near_hey_idx] <- "Near-Heywood"; scores[near_hey_idx] <- psi[near_hey_idx] }
    normal_idx <- which(!heywood_flag & !near_heywood_flag)
    no_load_idx <- normal_idx[count_above[normal_idx] == 0]
    if (length(no_load_idx) > 0) { ok[no_load_idx] <- FALSE; reasons[no_load_idx] <- "No loading"; scores[no_load_idx] <- max_loading[no_load_idx] }
    cross_idx <- normal_idx[count_above[normal_idx] > 1]
    if (length(cross_idx) > 0) {
      ok[cross_idx] <- FALSE; reasons[cross_idx] <- "Cross-loading"
      for (i in cross_idx) { s <- sort(abs(L[i, ]), decreasing = TRUE); scores[i] <- s[1] - s[2] }
    }
    ok_idx <- normal_idx[count_above[normal_idx] == 1]
    if (length(ok_idx) > 0) scores[ok_idx] <- max_loading[ok_idx]
    li_mat  <- abs(L)
    max_abs <- apply(li_mat, 1, max)
    primary <- apply(li_mat, 1, which.max)
    primary[max_abs < thresholds$loading] <- NA_integer_
    counts  <- tabulate(primary, nbins = ncol(L))
    structure_ok <- all(counts >= thresholds$min_items_per_factor)
    list(mat=L, items=its, ok=ok, scores=scores, reasons=reasons,
         counts=counts, structure_ok=structure_ok, h2=h2, psi=psi,
         heywood=heywood_flag, near_heywood=near_heywood_flag, primary=primary)
  }

  # Captura stats
  capture_item_stats <- function(it, ev, df, rmsea, reason) {
    load_cols <- which(startsWith(names(df), "f"))
    idx <- which(ev$items == it)
    if (length(idx) == 1) {
      li <- abs(as.numeric(df[idx, load_cols, drop = TRUE]))
      s  <- sort(li, decreasing = TRUE)
      primary_loading <- if (length(li)) max(li, na.rm = TRUE) else NA_real_
      second_loading  <- if (length(s) >= 2) s[2] else NA_real_
      delta_loading   <- if (is.finite(primary_loading) && is.finite(second_loading)) primary_loading - second_loading else NA_real_
      list(reason=reason, rmsea_at_removal=rmsea, loading=primary_loading, second_loading=second_loading,
           delta_loading=delta_loading, h2=ev$h2[idx], psi=ev$psi[idx], primary_factor=ev$primary[idx])
    } else {
      list(reason=reason, rmsea_at_removal=rmsea, loading=NA_real_, second_loading=NA_real_,
           delta_loading=NA_real_, h2=NA_real_, psi=NA_real_, primary_factor=NA_integer_)
    }
  }

  # ───────── Búsqueda GLOBAL con barra de progreso ─────────
  count_combinations <- function(n, kmax) { total <- 0; for (k in 1:kmax) total <- total + choose(n, k); total }
  global_search_best_subset <- function(candidates, base_excluded, curr_loss, thresholds,
                                        n_factors, model_config, use_timeouts, performance,
                                        max_drop, max_global_combinations, verbose_loc=TRUE,
                                        show_progress=TRUE) {
    n_cand <- length(candidates)
    if (n_cand == 0) return(NULL)
    total_comb <- count_combinations(n_cand, max_drop)
    if (total_comb > max_global_combinations) {
      if (verbose_loc) cat(sprintf("⚠ Global search skipped: %d combinations exceed limit (%d)\n", total_comb, max_global_combinations))
      return(NULL)
    }
    if (verbose_loc) cat(sprintf("🔎 Global search (k ≤ %d): evaluating %d combinations...\n", max_drop, total_comb))

    best_subset <- NULL; best_loss <- curr_loss; best_fit <- NULL
    eval_counter <- 0
    pb <- NULL
    if (show_progress) pb <- utils::txtProgressBar(min = 0, max = total_comb, style = 3)

    on.exit({ if (!is.null(pb)) close(pb) }, add = TRUE)

    for (k in 1:max_drop) {
      if (k > n_cand) break
      comb_mat <- utils::combn(candidates, k, simplify = TRUE)
      if (is.vector(comb_mat)) comb_mat <- matrix(comb_mat, nrow = 1L)
      for (j in seq_len(ncol(comb_mat))) {
        subset_excl <- as.character(comb_mat[, j])
        eval_counter <- eval_counter + 1
        if (!is.null(pb)) utils::setTxtProgressBar(pb, eval_counter)

        rem_n <- length(setdiff(items, c(base_excluded, subset_excl)))
        if (rem_n < n_factors * thresholds$min_items_per_factor) next

        m2 <- tryCatch({
          if (use_timeouts) {
            R.utils::withTimeout({
              PsyMetricTools::EFA_modern(
                data=data, n_factors=n_factors, n_items=n_items, name_items=name_items,
                estimator=model_config$estimator, rotation=model_config$rotation,
                apply_threshold=FALSE, exclude_items=c(base_excluded, subset_excl), ...
              )
            }, timeout=performance$timeout_efa, onTimeout="error")
          } else {
            PsyMetricTools::EFA_modern(
              data=data, n_factors=n_factors, n_items=n_items, name_items=name_items,
              estimator=model_config$estimator, rotation=model_config$rotation,
              apply_threshold=FALSE, exclude_items=c(base_excluded, subset_excl), ...
            )
          }
        }, error=function(e) NULL)
        if (is.null(m2) || is.null(m2$result_df)) next

        phi2 <- if (!is.null(m2$InterFactor)) as.matrix(m2$InterFactor) else diag(n_factors)
        df2  <- m2$result_df
        lc2  <- which(startsWith(names(df2), "f"))
        df2[lc2] <- lapply(df2[lc2], function(x) ifelse(abs(x) < thresholds$loading, 0, x))
        ev2 <- tryCatch(evaluate_structure(df2, phi2, thresholds), error=function(e) NULL)
        if (is.null(ev2)) next
        ok_min <- all(ev2$counts >= thresholds$min_items_per_factor)
        ok_struct <- all(ev2$ok)
        if (!ok_min || !ok_struct) next

        fit2 <- extract_fit(m2, n_factors)
        loss2 <- composite_loss(fit2, fit_config, model_config$estimator)
        if (is.finite(loss2) && loss2 < best_loss) { best_loss <- loss2; best_subset <- subset_excl; best_fit <- fit2 }
      }
    }
    if (!is.null(best_subset) && best_loss < curr_loss) list(subset=best_subset, fit=best_fit, loss=best_loss) else NULL
  }

  # ───────── Loop principal ─────────
  exclude_items <- exclude_items %||% character(0)
  removed_items <- character()
  steps_log <- data.frame(step=integer(), removed_item=character(), reason=character(), rmsea=numeric(), stringsAsFactors=FALSE)
  step_counter <- 0
  mod <- NULL
  last_ev <- NULL
  curr_rmsea <- NA_real_
  item_removal_stats <- list()

  max_iterations <- min(50, max_steps)
  iteration_count <- 0
  optimization_start <- Sys.time()

  repeat {
    iteration_count <- iteration_count + 1
    if (iteration_count > max_iterations) { if (verbose) cat("\n⚠ Maximum safety iterations reached.\n"); break }
    candidates <- setdiff(items, c(exclude_items, removed_items))
    if (length(candidates) < n_factors * thresholds$min_items_per_factor) { if (verbose) cat("\n⚠ Not enough items remaining to continue.\n"); break }
    if (step_counter >= max_steps) { if (verbose) cat("\n⚠ Maximum steps reached.\n"); break }

    if (performance$use_timeouts) {
      elapsed <- as.numeric(difftime(Sys.time(), optimization_start, units = "secs"))
      if (elapsed > performance$timeout_optimization) { if (verbose) cat("\n⚠ Optimization timeout reached.\n"); break }
    }

    converged <- TRUE
    tmp <- tryCatch({
      if (use_timeouts) {
        R.utils::withTimeout({
          PsyMetricTools::EFA_modern(
            data=data, n_factors=n_factors, n_items=n_items, name_items=name_items,
            estimator=model_config$estimator, rotation=model_config$rotation,
            apply_threshold=FALSE, exclude_items=c(exclude_items, removed_items), ...
          )
        }, timeout=performance$timeout_efa, onTimeout="error")
      } else {
        PsyMetricTools::EFA_modern(
          data=data, n_factors=n_factors, n_items=n_items, name_items=name_items,
          estimator=model_config$estimator, rotation=model_config$rotation,
          apply_threshold=FALSE, exclude_items=c(exclude_items, removed_items), ...
        )
      }
    }, error=function(e) { if (verbose) { if (grepl("timeout", tolower(e$message))) cat("⏱ EFA timeout\n") else cat("⚠ Did not converge:", e$message, "\n") }; converged <<- FALSE; NULL })

    if (!converged || is.null(tmp) || is.null(tmp$Bondades_Original) || is.null(tmp$result_df)) break
    mod <- tmp
    fit0 <- extract_fit(mod, n_factors)
    curr_rmsea <- as.numeric(fit0$rmsea)
    curr_loss  <- composite_loss(fit0, fit_config, model_config$estimator)

    # Estructura actual
    phi <- if (!is.null(mod$InterFactor)) as.matrix(mod$InterFactor) else diag(n_factors)
    df_eval <- mod$result_df
    load_cols2 <- which(startsWith(names(df_eval), "f"))
    df_eval[load_cols2] <- lapply(df_eval[load_cols2], function(x) ifelse(abs(x) < thresholds$loading, 0, round(x, 3)))
    ev <- evaluate_structure(df_eval, phi, thresholds)
    last_ev <- ev

    if (verbose) {
      cat("\n", paste(rep("─", 70), collapse = ""), "\n", sep = "")
      cat("ITERATION", step_counter, "| Loss:", sprintf("%.3f", curr_loss),
          "| RMSEA:", sprintf("%.3f", curr_rmsea),
          "| SRMR:", fmt_num(fit0$srmr),
          "| CFI:",  fmt_num(fit0$cfi),
          "| df:",   fmt_num(fit0$df, 0), "\n")
      cat("Items per factor:", paste(ev$counts, collapse = " | "), "\n")
      cat(paste(rep("─", 70), collapse = ""), "\n\n", sep = "")
      df_display <- mod$result_df
      load_cols_display <- which(startsWith(names(df_display), "f"))
      df_display[load_cols_display] <- lapply(df_display[load_cols_display], function(x) ifelse(abs(x) < thresholds$loading, 0, round(x, 3)))
      if (inherits(df_display, "tbl_df")) print(df_display, n = Inf, width = Inf) else print(df_display)
      cat("\n")
    }

    # Prioridades estructurales
    if (any(ev$heywood)) {
      worst <- ev$items[ which.min(ev$psi) ]
      item_removal_stats[[worst]] <- capture_item_stats(worst, ev, mod$result_df, curr_rmsea, "Heywood")
      removed_items <- c(removed_items, worst); step_counter <- step_counter + 1
      steps_log <- rbind(steps_log, data.frame(step=step_counter, removed_item=worst, reason="Heywood", rmsea=curr_rmsea, stringsAsFactors=FALSE))
      if (verbose) cat("❌ Removed", worst, "due to: Heywood (ψ min)\n")
      next
    }
    if (any(ev$near_heywood)) {
      idx <- which(ev$near_heywood)
      worst <- ev$items[idx[ which.min(ev$psi[idx]) ]]
      item_removal_stats[[worst]] <- capture_item_stats(worst, ev, mod$result_df, curr_rmsea, "Near-Heywood")
      removed_items <- c(removed_items, worst); step_counter <- step_counter + 1
      steps_log <- rbind(steps_log, data.frame(step=step_counter, removed_item=worst, reason="Near-Heywood", rmsea=curr_rmsea, stringsAsFactors=FALSE))
      if (verbose) cat("❌ Removed", worst, "due to: Near-Heywood (ψ≈0)\n")
      next
    }
    cross_idx <- which(ev$reasons == "Cross-loading")
    if (length(cross_idx) > 0) {
      order_idx <- cross_idx[order(ev$scores[cross_idx], decreasing = FALSE)]
      selected <- NA_character_
      for (i in order_idx) {
        item <- ev$items[i]; p <- ev$primary[i]; counts2 <- ev$counts; if (!is.na(p)) counts2[p] <- counts2[p] - 1
        if (all(counts2 >= thresholds$min_items_per_factor)) { selected <- item; break }
      }
      if (!is.na(selected)) {
        item_removal_stats[[selected]] <- capture_item_stats(selected, ev, mod$result_df, curr_rmsea, "Cross-loading (priority)")
        removed_items <- c(removed_items, selected); step_counter <- step_counter + 1
        steps_log <- rbind(steps_log, data.frame(step=step_counter, removed_item=selected, reason="Cross-loading (priority)", rmsea=curr_rmsea, stringsAsFactors=FALSE))
        if (verbose) cat("❌ Removed", selected, "due to: Cross-loading (priority)\n")
        next
      } else {
        # Cross-loadings protegidos, pero verificar si RMSEA > target
        rmsea_target_reached <- !is.na(curr_rmsea) && curr_rmsea <= fit_config$targets$rmsea
        if (!rmsea_target_reached) {
          if (verbose) cat("⚠ Cross-loadings protected but RMSEA (", fmt_num(curr_rmsea), ") > target (", fit_config$targets$rmsea, "); removing weakest item...\n", sep="")

          # Eliminar ítem con menor carga para mejorar RMSEA
          load_cols <- which(startsWith(names(mod$result_df), "f"))
          max_loadings <- apply(abs(as.matrix(mod$result_df[, load_cols])), 1, max)
          weakest_idx <- which.min(max_loadings)
          worst <- mod$result_df$Items[weakest_idx]

          # Verificar que no viole min_items_per_factor
          p_worst <- ev$primary[which(ev$items == worst)]
          counts2 <- ev$counts
          if (!is.na(p_worst)) counts2[p_worst] <- counts2[p_worst] - 1

          if (all(counts2 >= thresholds$min_items_per_factor)) {
            item_removal_stats[[worst]] <- capture_item_stats(worst, ev, mod$result_df, curr_rmsea, "Weakest loading (RMSEA > target)")
            removed_items <- c(removed_items, worst)
            step_counter <- step_counter + 1
            steps_log <- rbind(steps_log, data.frame(step=step_counter, removed_item=worst, reason="Weakest loading (RMSEA > target)", rmsea=curr_rmsea, stringsAsFactors=FALSE))
            if (verbose) cat("❌ Removed", worst, "| Loading:", fmt_num(max_loadings[weakest_idx]), "\n")
            next
          }
        } else {
          if (verbose) cat("⚠ Cross-loadings detected but protected by min_items_per_factor; RMSEA target reached, stopping.\n")
        }
      }
    }

    # ¿Criterios satisfechos?
    min_items_met <- all(ev$counts >= thresholds$min_items_per_factor)

    # Verificar correlaciones inter-factoriales
    corr_check <- check_interfactor_correlations(phi, thresholds$min_interfactor_correlation)

    if (min_items_met && all(ev$ok) && curr_loss <= 0 + 1e-6 && corr_check$ok) {
      if (verbose) cat("\n✅ All criteria met (estructura OK, ajuste dentro de objetivos, y correlaciones >= ", thresholds$min_interfactor_correlation, "). Fin.\n", sep="")
      break
    }

    # Decisión: estructura vs ajuste
    if (!all(ev$ok)) {
      decision <- "structure"
    } else if (curr_loss > 0 + 1e-6) {
      decision <- "fit"
    } else {
      break
    }

    if (decision == "fit") {
      chosen_subset <- NULL; chosen_fit <- NULL; chosen_loss <- NA_real_

      # 1) GLOBAL si use_global = TRUE
      if (isTRUE(use_global)) {
        gs <- global_search_best_subset(
          candidates=candidates,
          base_excluded=c(exclude_items, removed_items),
          curr_loss=curr_loss,
          thresholds=thresholds,
          n_factors=n_factors,
          model_config=model_config,
          use_timeouts=use_timeouts,
          performance=performance,
          max_drop=global_opt$max_drop,
          max_global_combinations=global_opt$max_global_combinations,
          verbose_loc = isTRUE(global_opt$verbose) && verbose,
          show_progress = isTRUE(global_opt$progress_bar) && verbose
        )
        if (!is.null(gs)) { chosen_subset <- gs$subset; chosen_fit <- gs$fit; chosen_loss <- gs$loss }
      }

      # 2) Si global no mejora o está desactivado → GREEDY 1×1
      if (is.null(chosen_subset)) {
        n_cand <- length(candidates)
        cand_to_eval <- if (!is.null(performance$max_candidates_eval) && n_cand > performance$max_candidates_eval) sample(candidates, performance$max_candidates_eval) else candidates
        if (verbose && !is.null(performance$max_candidates_eval) && n_cand > performance$max_candidates_eval) {
          cat("📊 Evaluating", performance$max_candidates_eval, "of", n_cand, "candidates (random sample)\n")
        }
        best_loss <- curr_loss; best_it <- NULL; best_fit <- NULL
        for (it in cand_to_eval) {
          m2 <- tryCatch({
            if (use_timeouts) {
              R.utils::withTimeout({
                PsyMetricTools::EFA_modern(
                  data=data, n_factors=n_factors, n_items=n_items, name_items=name_items,
                  estimator=model_config$estimator, rotation=model_config$rotation,
                  apply_threshold=FALSE, exclude_items=c(exclude_items, removed_items, it), ...
                )
              }, timeout=performance$timeout_efa, onTimeout="error")
            } else {
              PsyMetricTools::EFA_modern(
                data=data, n_factors=n_factors, n_items=n_items, name_items=name_items,
                estimator=model_config$estimator, rotation=model_config$rotation,
                apply_threshold=FALSE, exclude_items=c(exclude_items, removed_items, it), ...
              )
            }
          }, error=function(e) NULL)
          if (is.null(m2) || is.null(m2$result_df)) next
          phi2 <- if (!is.null(m2$InterFactor)) as.matrix(m2$InterFactor) else diag(n_factors)
          df2  <- m2$result_df
          lc2  <- which(startsWith(names(df2), "f"))
          df2[lc2] <- lapply(df2[lc2], function(x) ifelse(abs(x) < thresholds$loading, 0, x))
          ev2 <- tryCatch(evaluate_structure(df2, phi2, thresholds), error=function(e) NULL)
          if (is.null(ev2)) next
          if (!all(ev2$counts >= thresholds$min_items_per_factor) || !all(ev2$ok)) next

          fit2 <- extract_fit(m2, n_factors)
          loss2 <- composite_loss(fit2, fit_config, model_config$estimator)
          if (is.finite(loss2) && loss2 < best_loss) { best_loss <- loss2; best_it <- it; best_fit <- fit2 }
        }
        if (!is.null(best_it)) { chosen_subset <- best_it; chosen_fit <- best_fit; chosen_loss <- best_loss }
      }

      # 3) Ejecutar eliminación si mejora
      if (!is.null(chosen_subset) && is.finite(chosen_loss) && chosen_loss < curr_loss - 1e-6) {
        k_set <- length(chosen_subset)
        for (best in chosen_subset) {
          item_removal_stats[[best]] <- capture_item_stats(best, ev, mod$result_df, chosen_fit$rmsea,
                                                           if (k_set > 1) sprintf("Global composite fit (k=%d)", k_set) else "Composite fit")
          removed_items <- c(removed_items, best); step_counter <- step_counter + 1
          steps_log <- rbind(steps_log, data.frame(step=step_counter, removed_item=best,
                                                   reason=if (k_set > 1) sprintf("Global composite fit (k=%d)", k_set) else "Composite fit",
                                                   rmsea=as.numeric(chosen_fit$rmsea), stringsAsFactors=FALSE))
        }
        if (verbose) {
          if (length(chosen_subset) == 1) {
            cat("❌ Removed", chosen_subset, "→ Loss:", sprintf("%.3f", chosen_loss),
                " | RMSEA:", fmt_num(chosen_fit$rmsea),
                " | SRMR:",  fmt_num(chosen_fit$srmr),
                " | CFI:",   fmt_num(chosen_fit$cfi), "\n")
          } else {
            cat("❌ Removed {", paste(chosen_subset, collapse = ", "),
                "} → Loss:", sprintf("%.3f", chosen_loss),
                " | RMSEA:", fmt_num(chosen_fit$rmsea),
                " | SRMR:",  fmt_num(chosen_fit$srmr),
                " | CFI:",   fmt_num(chosen_fit$cfi),
                sprintf(" (global k=%d)\n", length(chosen_subset)))
          }
        }
        next
      }

      # si no hubo mejora de ajuste, caemos a estrategia estructural
      decision <- "structure"
    }

    if (verbose && decision == "structure") cat("📐 STRATEGY: Structural optimization (non-cross-loading)\n")

    # Verificar si alcanzamos el target de RMSEA
    rmsea_target_reached <- !is.na(curr_rmsea) && curr_rmsea <= fit_config$targets$rmsea

    # Si estructura OK y RMSEA alcanzado, detener
    if (all(ev$ok) && rmsea_target_reached) {
      if (verbose) cat("\n✓ Target RMSEA achieved (", fmt_num(curr_rmsea), " ≤ ", fit_config$targets$rmsea, ") and structure acceptable; stopping.\n", sep="")
      break
    }

    # Si estructura OK pero RMSEA no alcanzado, eliminar ítem con menor carga
    if (all(ev$ok) && !rmsea_target_reached) {
      if (verbose) cat("⚠ Structure acceptable but RMSEA (", fmt_num(curr_rmsea), ") > target (", fit_config$targets$rmsea, "); removing weakest item to improve fit...\n", sep="")

      # Encontrar el ítem con menor carga primaria
      load_cols <- which(startsWith(names(mod$result_df), "f"))
      max_loadings <- apply(abs(as.matrix(mod$result_df[, load_cols])), 1, max)
      weakest_idx <- which.min(max_loadings)
      worst <- mod$result_df$Items[weakest_idx]
      reason <- "Weakest loading (RMSEA > target)"

      # Verificar que no viole min_items_per_factor
      p_worst <- ev$primary[which(ev$items == worst)]
      counts2 <- ev$counts
      if (!is.na(p_worst)) counts2[p_worst] <- counts2[p_worst] - 1

      if (all(counts2 >= thresholds$min_items_per_factor)) {
        item_removal_stats[[worst]] <- capture_item_stats(worst, ev, mod$result_df, curr_rmsea, reason)
        removed_items <- c(removed_items, worst)
        step_counter <- step_counter + 1
        steps_log <- rbind(steps_log, data.frame(step=step_counter, removed_item=worst, reason=reason, rmsea=curr_rmsea, stringsAsFactors=FALSE))
        if (verbose) cat("❌ Removed", worst, "| Loading:", fmt_num(max_loadings[weakest_idx]), "\n")
        next
      } else {
        if (verbose) cat("\n⚠ Cannot remove weakest item (", worst, ") - would violate min_items_per_factor; stopping.\n", sep="")
        break
      }
    }

    # Remover peor problema no-cross (solo si hay problemas estructurales)
    prob_idx <- which(!ev$ok & ev$reasons != "Cross-loading")
    if (length(prob_idx) == 0) prob_idx <- which(!ev$ok)

    if (length(prob_idx) > 0) {
      worst <- ev$items[ prob_idx[ which.min(ev$scores[prob_idx]) ] ]
      reason <- ev$reasons[ which(ev$items == worst) ]
      p_worst <- ev$primary[ which(ev$items == worst) ]
      counts2 <- ev$counts; if (!is.na(p_worst)) counts2[p_worst] <- counts2[p_worst] - 1

      if (all(counts2 >= thresholds$min_items_per_factor)) {
        item_removal_stats[[worst]] <- capture_item_stats(worst, ev, mod$result_df, curr_rmsea, reason)
        removed_items <- c(removed_items, worst); step_counter <- step_counter + 1
        steps_log <- rbind(steps_log, data.frame(step=step_counter, removed_item=worst, reason=reason, rmsea=curr_rmsea, stringsAsFactors=FALSE))
        if (verbose) cat("❌ Removed", worst, "due to:", reason, "\n")
        next
      } else {
        # Problema estructural protegido, verificar si RMSEA > target
        rmsea_target_reached <- !is.na(curr_rmsea) && curr_rmsea <= fit_config$targets$rmsea
        if (!rmsea_target_reached) {
          if (verbose) cat("\n⚠ Structural issue (", worst, ") protected but RMSEA (", fmt_num(curr_rmsea), ") > target (", fit_config$targets$rmsea, "); removing weakest item...\n", sep="")

          # Eliminar ítem con menor carga para mejorar RMSEA
          load_cols <- which(startsWith(names(mod$result_df), "f"))
          max_loadings <- apply(abs(as.matrix(mod$result_df[, load_cols])), 1, max)
          weakest_idx <- which.min(max_loadings)
          worst_weak <- mod$result_df$Items[weakest_idx]

          # Verificar que no viole min_items_per_factor
          p_worst_weak <- ev$primary[which(ev$items == worst_weak)]
          counts3 <- ev$counts
          if (!is.na(p_worst_weak)) counts3[p_worst_weak] <- counts3[p_worst_weak] - 1

          if (all(counts3 >= thresholds$min_items_per_factor)) {
            item_removal_stats[[worst_weak]] <- capture_item_stats(worst_weak, ev, mod$result_df, curr_rmsea, "Weakest loading (RMSEA > target)")
            removed_items <- c(removed_items, worst_weak)
            step_counter <- step_counter + 1
            steps_log <- rbind(steps_log, data.frame(step=step_counter, removed_item=worst_weak, reason="Weakest loading (RMSEA > target)", rmsea=curr_rmsea, stringsAsFactors=FALSE))
            if (verbose) cat("❌ Removed", worst_weak, "| Loading:", fmt_num(max_loadings[weakest_idx]), "\n")
            next
          } else {
            if (verbose) cat("\n⚠ Cannot remove any more items - min_items_per_factor reached. Final RMSEA:", fmt_num(curr_rmsea), "\n", sep="")
            break
          }
        } else {
          if (verbose) cat("\n⚠ Structural issue found (", worst, ") but protected by min_items_per_factor; RMSEA target reached, stopping.\n", sep = "")
          break
        }
      }
    }
  }

  if (is.null(mod) || is.null(mod$result_df)) stop("Could not generate a valid EFA model.")

  # Estructura final (umbral de reporte)
  df_final  <- mod$result_df
  load_cols <- which(startsWith(names(df_final), "f"))
  df_final[load_cols] <- lapply(df_final[load_cols], function(x) ifelse(abs(x) < thresholds$loading, 0, x))

  # Descripción final
  A  <- as.matrix(df_final[, load_cols, drop = FALSE])
  mA <- apply(abs(A), 1, max)
  w  <- apply(abs(A), 1, which.max)
  w[mA < thresholds$loading] <- NA_integer_
  factor_map_final <- setNames(w, df_final$Items)
  fac_lists <- tapply(names(factor_map_final), factor_map_final, function(v) paste(v, collapse = ", "))
  fac_lists <- fac_lists[!is.na(names(fac_lists))]
  structure_desc <- paste0("Final structure: ", paste0("Factor ", names(fac_lists), " contains {", fac_lists, "}", collapse = "; "))

  # Timeline para IA
  timeline_str <- NULL
  if (nrow(steps_log) > 0) {
    header <- "ITEM ELIMINATION TIMELINE\n step removed_item  reason                          rmsea"
    lines <- sprintf("%3d %-12s %-30s %s", steps_log$step, steps_log$removed_item, steps_log$reason, fmt_num(steps_log$rmsea))
    timeline_str <- paste(c(header, lines), collapse = "\n")
  }

  # ────────────────────────────────────────────────────────────────────────────
  # Análisis conceptual con IA (versión completa con progreso)
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

  # Verificación final de correlaciones inter-factoriales
  phi_final <- if (!is.null(mod$InterFactor)) as.matrix(mod$InterFactor) else diag(n_factors)
  corr_check_final <- check_interfactor_correlations(phi_final, thresholds$min_interfactor_correlation)

  if (verbose) {
    cat("\n╔════════════════════════════════════════════════════════════════╗\n")
    cat("║                  OPTIMIZATION COMPLETED                        ║\n")
    cat("╚════════════════════════════════════════════════════════════════╝\n\n")
    cat("Total iterations:", step_counter, "\n")
    if (length(removed_items)) cat("Items removed:", paste(removed_items, collapse = ", "), "\n")
    cat("Final RMSEA:", fmt_num(extract_fit(mod, n_factors)$rmsea), "\n")

    # Advertencia sobre correlaciones inter-factoriales
    if (!corr_check_final$ok) {
      cat("\n⚠️  ADVERTENCIA: No se alcanzó el criterio de correlación mínima entre factores (>= ", thresholds$min_interfactor_correlation, ")\n", sep="")
      cat("    Correlaciones que no cumplen el criterio:\n")
      for (viol in corr_check_final$violated) {
        cat("    - ", viol, "\n", sep="")
      }
      cat("    Correlación mínima encontrada: ", fmt_num(corr_check_final$min_value), "\n", sep="")
    } else {
      cat("\n✓ Criterio de correlación inter-factorial cumplido (todas >= ", thresholds$min_interfactor_correlation, ")\n", sep="")
    }

    cat("\n✅ Analysis finished successfully.\n\n")
  }

  list(
    final_structure   = df_final,
    removed_items     = removed_items,
    steps_log         = steps_log,
    iterations        = step_counter,
    final_rmsea       = as.numeric(extract_fit(mod, n_factors)$rmsea),
    bondades_original = mod$Bondades_Original,
    specifications    = mod$Specifications,
    inter_factor_correlation = { if (n_factors > 1 && !is.null(mod$InterFactor)) as.matrix(mod$InterFactor) else diag(n_factors) },
    interfactor_check = list(
      criteria_met = corr_check_final$ok,
      min_value = corr_check_final$min_value,
      violations = corr_check_final$violated,
      threshold = thresholds$min_interfactor_correlation
    ),
    last_h2           = if (!is.null(last_ev)) last_ev$h2  else NULL,
    last_psi          = if (!is.null(last_ev)) last_ev$psi else NULL,
    last_flags        = if (!is.null(last_ev)) list(heywood=last_ev$heywood, near=last_ev$near_heywood) else NULL,
    conceptual_analysis = conceptual_analysis,
    config_used       = list(thresholds=thresholds, model_config=model_config, performance=performance,
                             use_global=use_global, global_opt=global_opt,
                             fit_config=fit_config,
                             use_ai_analysis=use_ai_analysis, ai_config=ai_config)
  )
}
