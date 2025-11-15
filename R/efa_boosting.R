efa_boosting <- function(data,
                           name_items,
                           item_range = NULL,
                           n_factors = 3,
                           exclude_items = NULL,
                           # Thresholds (Heywood + mínimos)
                           thresholds = list(
                             rmsea = 0.08,
                             loading = 0.30,
                             min_items_per_factor = 3,
                             heywood_tol = 1e-6,
                             near_heywood = 0.015
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
  default_thresholds <- list(rmsea=0.08, loading=0.30, min_items_per_factor=3, heywood_tol=1e-6, near_heywood=0.015)
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

  use_timeouts <- performance$use_timeouts
  if (use_timeouts && !requireNamespace("R.utils", quietly = TRUE)) {
    warning("Package 'R.utils' not found. Timeouts disabled. Install with: install.packages('R.utils')")
    use_timeouts <- FALSE
  }

  # ───────── Funciones auxiliares de ajuste ─────────
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
      } else if (verbose) cat("⚠ Cross-loadings detected but protected by min_items_per_factor; skipping for now.\n")
    }

    # ¿Criterios satisfechos?
    min_items_met <- all(ev$counts >= thresholds$min_items_per_factor)
    if (min_items_met && all(ev$ok) && curr_loss <= 0 + 1e-6) {
      if (verbose) cat("\n✅ All criteria met (estructura OK y ajuste dentro de objetivos). Fin.\n")
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
    if (all(ev$ok)) { if (verbose) cat("\n✓ Structure acceptable but cannot improve fit further; stopping.\n"); break }

    # Remover peor problema no-cross
    prob_idx <- which(!ev$ok & ev$reasons != "Cross-loading")
    if (length(prob_idx) == 0) prob_idx <- which(!ev$ok)
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
      if (verbose) cat("\n⚠ Structural issue found (", worst, ") but protected by min_items_per_factor; stopping.\n", sep = "")
      break
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

  conceptual_analysis <- NULL
  if (use_ai_analysis && length(items) > 0 && !is.null(ai_config$api_key) && !is.null(ai_config$item_definitions)) {
    # (Deja aquí tu bloque de IA si lo usas)
  }

  if (verbose) {
    cat("\n╔════════════════════════════════════════════════════════════════╗\n")
    cat("║                  OPTIMIZATION COMPLETED                        ║\n")
    cat("╚════════════════════════════════════════════════════════════════╝\n\n")
    cat("Total iterations:", step_counter, "\n")
    if (length(removed_items)) cat("Items removed:", paste(removed_items, collapse = ", "), "\n")
    cat("Final RMSEA:", fmt_num(extract_fit(mod, n_factors)$rmsea), "\n")
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
