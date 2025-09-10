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

  # AI configuration (only if used)
  default_ai <- list(
    api_key = NULL, generate_names = FALSE, only_removed = TRUE,
    item_definitions = NULL, domain_name = "Default Domain",
    scale_title = "Default Scale Title",
    construct_definition = "", model_name = "EFA Model",
    gpt_model = "gpt-3.5-turbo"
  )
  ai_config <- modifyList(default_ai, ai_config)

  if (use_ai_analysis && !requireNamespace("httr", quietly = TRUE)) install.packages("httr")
  if (use_ai_analysis && !requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")

  call_openai_api <- function(prompt) {
    if (is.null(ai_config$api_key)) return(NULL)
    resp <- NULL; attempt <- 1
    while (attempt <= 3) {
      resp <- tryCatch({
        httr::POST("https://api.openai.com/v1/chat/completions",
                   httr::add_headers(Authorization = paste("Bearer", ai_config$api_key),
                                     `Content-Type` = "application/json"),
                   httr::timeout(160),
                   body = jsonlite::toJSON(list(
                     model    = ai_config$gpt_model,
                     messages = list(
                       list(role = "system", content = "You are an expert in psychometrics and factor analysis."),
                       list(role = "user",   content = prompt)
                     ),
                     temperature = 0.5
                   ), auto_unbox = TRUE)
        )
      }, error = function(e) NULL)
      if (!is.null(resp) && httr::status_code(resp) == 200) break
      if (verbose) cat("Attempt", attempt, "failed; retrying...\n")
      attempt <- attempt + 1; Sys.sleep(1)
    }
    resp
  }

  analyze_item_with_gpt <- function(item, definition, context, action = c("exclude","keep")) {
    if (is.null(ai_config$api_key)) return("AI not configured")
    act <- match.arg(action)
    if (is.null(definition) || definition == "") {
      return(if (act == "exclude") "No definition provided for exclusion."
             else "No definition provided for retention.")
    }
    verb <- if (act == "exclude") "exclusion" else "retention"
    prompt <- paste0(
      "You are a psychometrics expert. Concisely justify the ",
      verb, " of item '", item, "' (\"", definition, "\") ",
      "in the context of construct '", ai_config$construct_definition,
      "' and scale '", ai_config$scale_title, "'. ", context,
      " (EFA Model: '", ai_config$model_name, "')."
    )
    resp <- call_openai_api(prompt)
    if (is.null(resp)) return("GPT error.")
    content <- httr::content(resp)
    tryCatch(content$choices[[1]]$message$content,
             error = function(e) "Error extracting GPT response.")
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

  # Structure evaluator with factor correlation matrix (oblique)
  evaluate_structure <- function(df, phi, thresholds) {
    load_cols <- which(startsWith(names(df), "f"))
    L <- as.matrix(df[, load_cols, drop = FALSE])
    its <- df$Items

    # Calculate communalities and uniquenesses with Φ
    h2  <- rowSums((L %*% phi) * L)            # diag(L Φ L')
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
      if (cnt == 1) { ok[i] <- TRUE;  reasons[i] <- "OK";            scores[i] <- max(li) }
      else if (cnt == 0){ ok[i] <- FALSE; reasons[i] <- "No loading"; scores[i] <- max(li) }
      else { s <- sort(li, TRUE); ok[i] <- FALSE; reasons[i] <- "Cross-loading"; scores[i] <- s[1] - s[2] }
    }

    # Count items per factor by primary membership (no double counting)
    li_mat  <- abs(L)
    max_abs <- apply(li_mat, 1, max)
    primary <- apply(li_mat, 1, which.max)
    primary[ max_abs < thresholds$loading ] <- NA_integer_
    counts  <- tabulate(primary, nbins = ncol(L))

    structure_ok <- all(counts >= thresholds$min_items_per_factor)

    list(mat=L, items=its, ok=ok, scores=scores, reasons=reasons,
         counts=counts, structure_ok=structure_ok, h2=h2, psi=psi,
         heywood=heywood_flag, near_heywood=near_heywood_flag)
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

    # Factor correlation matrix for oblimin (if doesn't exist, use identity)
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

    # Priority 1: Remove Heywood cases (most negative ψ)
    if (any(ev$heywood)) {
      worst <- ev$items[ which.min(ev$psi) ]
      removed_items <- c(removed_items, worst)
      step_counter  <- step_counter + 1
      steps_log <- rbind(steps_log, data.frame(
        step = step_counter, removed_item = worst, reason = "Heywood", rmsea = curr_rmsea,
        stringsAsFactors = FALSE
      ))
      if (verbose) cat("Removed", worst, "due to Heywood case (ψ=", sprintf("%.4f", min(ev$psi)), ")\n")
      next
    }

    # Priority 2: Remove Near-Heywood cases (ψ≈0)
    if (any(ev$near_heywood)) {
      idx <- which(ev$near_heywood)
      worst <- ev$items[idx[ which.min(ev$psi[idx]) ]]
      removed_items <- c(removed_items, worst)
      step_counter  <- step_counter + 1
      steps_log <- rbind(steps_log, data.frame(
        step = step_counter, removed_item = worst, reason = "Near-Heywood", rmsea = curr_rmsea,
        stringsAsFactors = FALSE
      ))
      if (verbose) cat("Removed", worst, "due to Near-Heywood case (ψ≈0)\n")
      next
    }

    min_items_met <- all(ev$counts >= thresholds$min_items_per_factor)

    # Check if all criteria are met
    if (!is.na(curr_rmsea) && curr_rmsea <= thresholds$rmsea && all(ev$ok) && min_items_met) {
      if (verbose) cat("All criteria met (RMSEA, structure and minimum items). Optimization complete.\n")
      break
    }
    if (!is.na(curr_rmsea) && curr_rmsea <= thresholds$rmsea && all(ev$ok) && !min_items_met) {
      if (verbose) cat("RMSEA acceptable and no structural problems, but insufficient items per factor; stopping.\n")
      break
    }

    # Decide optimization strategy
    decision <- if (!is.na(curr_rmsea) && curr_rmsea > thresholds$rmsea) "rmsea" else "structure"

    # RMSEA optimization strategy: test removing each candidate while preserving minimum per factor
    if (decision == "rmsea") {
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
             ok_min = all(ev2$counts >= thresholds$min_items_per_factor))
      })
      cand_rmsea <- sapply(cand_stats, function(cs) if (is.null(cs) || !cs$ok_min) NA_real_ else cs$rmsea)
      names(cand_rmsea) <- candidates

      if (!all(is.na(cand_rmsea))) {
        best_val <- min(cand_rmsea, na.rm = TRUE)
        if (is.finite(best_val) && best_val < curr_rmsea) {
          best <- names(which.min(cand_rmsea))
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

    if (verbose && decision == "structure") cat("STRATEGY: Structural optimization\n")

    if (all(ev$ok)) {
      if (verbose) cat("Structure acceptable but doesn't meet all criteria; stopping optimization.\n");
      break
    }

    # Remove worst structural item (lowest score)
    prob_idx <- which(!ev$ok)
    worst    <- ev$items[ prob_idx[ which.min(ev$scores[prob_idx]) ] ]
    reason   <- ev$reasons[ which(ev$items == worst) ]
    removed_items <- c(removed_items, worst)
    step_counter  <- step_counter + 1
    steps_log <- rbind(steps_log, data.frame(
      step = step_counter, removed_item = worst, reason = reason, rmsea = curr_rmsea,
      stringsAsFactors = FALSE
    ))
    if (verbose) cat("Removed", worst, "due to:", reason, "\n")
  }

  if (is.null(mod) || is.null(mod$result_df)) stop("Could not generate a valid EFA model.")

  # Final structure with reporting threshold applied
  df_final  <- mod$result_df
  load_cols <- which(startsWith(names(df_final), "f"))
  df_final[load_cols] <- lapply(df_final[load_cols], function(x)
    ifelse(abs(x) < thresholds$loading, 0, x)
  )

  # Create final factor mapping avoiding assignment to "all zero" rows
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

  # Optional AI analysis on items
  conceptual_analysis <- NULL
  if (use_ai_analysis && length(items) > 0 && !is.null(ai_config$api_key) &&
      !is.null(ai_config$item_definitions)) {
    analysis_removed <- NULL; analysis_kept <- NULL
    if (length(removed_items) > 0) {
      analysis_removed <- setNames(vector("list", length(removed_items)), removed_items)
      for (it in removed_items) {
        analysis_removed[[it]] <- analyze_item_with_gpt(it, ai_config$item_definitions[[it]],
                                                        structure_desc, action = "exclude")
      }
    }
    if (!ai_config$only_removed) {
      kept <- setdiff(items, removed_items)
      if (length(kept) > 0) {
        analysis_kept <- setNames(vector("list", length(kept)), kept)
        for (it in kept) {
          analysis_kept[[it]] <- analyze_item_with_gpt(it, ai_config$item_definitions[[it]],
                                                       structure_desc, action = "keep")
        }
      }
    }
    conceptual_analysis <- list(removed = analysis_removed, kept = analysis_kept)
  }

  if (verbose) {
    cat("Optimization completed in", step_counter, "iterations.\n")
    if (length(removed_items)) cat("Items removed:", paste(removed_items, collapse = ", "), "\n")
    cat("Analysis finished.\n")
  }

  # Return comprehensive results
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
    # Final diagnostics
    last_h2             = if (!is.null(last_ev)) last_ev$h2  else NULL,
    last_psi            = if (!is.null(last_ev)) last_ev$psi else NULL,
    last_flags          = if (!is.null(last_ev)) list(heywood=last_ev$heywood, near=last_ev$near_heywood) else NULL,
    config_used         = list(thresholds = thresholds, model_config = model_config,
                               use_ai_analysis = use_ai_analysis, ai_config = ai_config)
  )
}
