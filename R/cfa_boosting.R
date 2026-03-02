cfa_boosting <- function(data,
                         model,
                         n_sample = NULL,
                         # Thresholds de ajuste
                         thresholds = list(
                           loading = 0.30,
                           min_items_per_factor = 3,
                           rmsea_target = 0.08,
                           cfi_target = 0.95,
                           srmr_target = 0.08
                         ),
                         # Configuración de modelo
                         model_config = list(
                           estimator = "WLSMV",
                           ordered = TRUE
                         ),
                         # Configuración de índices de modificación (Saris-Satorra-van der Veld)
                         mod_indices_config = list(
                           max_covs_to_add = 10,
                           only_within_factor = TRUE,
                           delta = 0.10,
                           power_threshold = 0.75,
                           alpha = 0.05
                         ),
                         # Performance
                         performance = list(
                           max_iterations = 30,
                           timeout_cfa = 60
                         ),
                         verbose = TRUE) {

  # ───────────────── Utilidades ─────────────────
  `%||%` <- function(a, b) if (is.null(a)) b else a

  fmt_num <- function(x, digits = 3) {
    if (is.null(x) || length(x) == 0) return("NA")
    ok <- !is.na(x) & is.finite(x)
    pat <- paste0("%.", digits, "f")
    out <- character(length(x))
    out[ok]  <- sprintf(pat, x[ok])
    out[!ok] <- "NA"
    out
  }

  create_progress_bar <- function(current, total, width = 30) {
    if (total == 0) return("No items")
    percent <- current / total
    filled  <- floor(percent * width)
    bar <- paste0("[", paste0(rep("=", filled), collapse = ""),
                  paste0(rep("-", width - filled), collapse = ""), "]")
    sprintf("%s %d/%d (%.1f%%)", bar, current, total, percent * 100)
  }

  # Clasificar restricción según Saris, Satorra & van der Veld (2009), Table 6
  # Combina MI + EPC + Power del test MI para decidir si hay misespecificación
  classify_restriction <- function(mi_val, epc_val, delta, power_threshold, alpha) {
    if (is.na(mi_val) || is.na(epc_val) || mi_val <= 0) {
      return(list(decision = "I", power = NA_real_))
    }

    c_alpha <- qchisq(1 - alpha, df = 1)
    mi_sig <- mi_val > c_alpha

    # EPC ~ 0: no se puede calcular poder
    if (abs(epc_val) < 1e-10) {
      return(list(decision = if (mi_sig) "EPC:nm" else "I", power = NA_real_))
    }

    # ncp = (MI / EPC^2) * delta^2  [Saris et al., fórmula 10]
    ncp <- (mi_val / epc_val^2) * delta^2

    # Power = P(chi2(1, ncp) > c_alpha)  [fórmula 11]
    power <- 1 - pchisq(c_alpha, df = 1, ncp = ncp)

    # Tabla 6 de decisiones
    if (mi_sig && power < power_threshold) {
      decision <- "m"            # Misespecificación (baja potencia pero significativo)
    } else if (!mi_sig && power >= power_threshold) {
      decision <- "nm"           # No hay misespecificación
    } else if (mi_sig && power >= power_threshold) {
      # MI significativo con alta potencia: inspeccionar relevancia sustantiva del EPC
      decision <- if (abs(epc_val) > delta) "EPC:m" else "EPC:nm"
    } else {
      decision <- "I"            # Inconcluso
    }

    list(decision = decision, power = power)
  }

  # Cargar lavaan
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("El paquete 'lavaan' es requerido. Instálelo con: install.packages('lavaan')")
  }
  library(lavaan)

  # Detectar tamaño de muestra si no se proporciona
  if (is.null(n_sample)) {
    n_sample <- nrow(data)
    if (verbose) cat("Tamaño de muestra detectado: N =", n_sample, "\n")
  }

  # Mezclar defaults
  default_thresholds <- list(loading = 0.30, min_items_per_factor = 3,
                              rmsea_target = 0.08, cfi_target = 0.95, srmr_target = 0.08)
  thresholds <- modifyList(default_thresholds, thresholds)

  default_model_config <- list(estimator = "WLSMV", ordered = TRUE)
  model_config <- modifyList(default_model_config, model_config)

  default_mod_config <- list(max_covs_to_add = 10, only_within_factor = TRUE,
                              delta = 0.10, power_threshold = 0.75, alpha = 0.05)
  mod_indices_config <- modifyList(default_mod_config, mod_indices_config)

  default_perf <- list(max_iterations = 30, timeout_cfa = 60)
  performance <- modifyList(default_perf, performance)

  if (verbose) {
    cat("\n")
    cat(paste(rep("=", 70), collapse = ""), "\n")
    cat("   CFA BOOSTING v1.0 - Optimización de Modelo Confirmatorio\n")
    cat(paste(rep("=", 70), collapse = ""), "\n\n")
    cat("Targets -> RMSEA <=", thresholds$rmsea_target,
        " | CFI >=", thresholds$cfi_target,
        " | SRMR <=", thresholds$srmr_target, "\n")
    cat("Estimador:", model_config$estimator, "\n")
    cat("Min items/factor:", thresholds$min_items_per_factor, "\n")
    cat("Loading mínimo:", thresholds$loading, "\n")
    cat("MI framework: Saris-Satorra-van der Veld (delta=", mod_indices_config$delta,
        ", power>=", mod_indices_config$power_threshold,
        ", alpha=", mod_indices_config$alpha, ")\n\n")
  }

  # ───────────────── Parsear modelo inicial ─────────────────
  parse_model <- function(model_syntax) {
    lines <- strsplit(model_syntax, "\n")[[1]]
    lines <- trimws(lines)
    lines <- lines[lines != "" & !grepl("^#", lines)]

    factors <- list()
    covs <- character(0)

    for (line in lines) {
      if (grepl("=~", line)) {
        parts <- strsplit(line, "=~")[[1]]
        factor_name <- trimws(parts[1])
        items_str <- trimws(parts[2])
        items <- trimws(strsplit(items_str, "\\+")[[1]])
        factors[[factor_name]] <- items
      } else if (grepl("~~", line)) {
        covs <- c(covs, line)
      }
    }

    list(factors = factors, covs = covs)
  }

  # Reconstruir sintaxis del modelo
  build_model_syntax <- function(factors, covs = character(0)) {
    lines <- character(0)
    for (fname in names(factors)) {
      items <- factors[[fname]]
      if (length(items) > 0) {
        lines <- c(lines, paste0(fname, " =~ ", paste(items, collapse = " + ")))
      }
    }
    if (length(covs) > 0) {
      lines <- c(lines, covs)
    }
    paste(lines, collapse = "\n")
  }

  # ───────────────── Extraer índices de ajuste ─────────────────
  # Prefiere versiones .scaled (WLSMV, DWLS, MLR) sobre las estándar.
  # Esto es CRÍTICO: con estimadores robustos, las versiones sin .scaled

  # usan chi-cuadrado no corregido y producen índices incorrectos.
  extract_fit <- function(fit) {
    fm <- tryCatch(fitMeasures(fit), error = function(e) NULL)

    if (is.null(fm)) {
      return(list(chisq = NA, df = NA, pvalue = NA, cfi = NA, tli = NA,
                  rmsea = NA, rmsea.lower = NA, rmsea.upper = NA, srmr = NA))
    }

    # Helper: toma la versión .scaled si existe, si no la estándar
    pick <- function(scaled, standard) {
      if (scaled %in% names(fm)) as.numeric(fm[scaled])
      else if (standard %in% names(fm)) as.numeric(fm[standard])
      else NA_real_
    }

    list(
      chisq       = pick("chisq.scaled",          "chisq"),
      df          = pick("df.scaled",              "df"),
      pvalue      = pick("pvalue.scaled",          "pvalue"),
      cfi         = pick("cfi.scaled",             "cfi"),
      tli         = pick("tli.scaled",             "tli"),
      rmsea       = pick("rmsea.scaled",           "rmsea"),
      rmsea.lower = pick("rmsea.ci.lower.scaled",  "rmsea.ci.lower"),
      rmsea.upper = pick("rmsea.ci.upper.scaled",  "rmsea.ci.upper"),
      srmr        = as.numeric(fm["srmr"])
    )
  }

  # ───────────────── Verificar si cumple targets ─────────────────
  meets_targets <- function(fit_indices) {
    rmsea_ok <- !is.na(fit_indices$rmsea) && fit_indices$rmsea <= thresholds$rmsea_target
    cfi_ok <- !is.na(fit_indices$cfi) && fit_indices$cfi >= thresholds$cfi_target
    srmr_ok <- !is.na(fit_indices$srmr) && fit_indices$srmr <= thresholds$srmr_target

    list(
      all_met = rmsea_ok && cfi_ok && srmr_ok,
      rmsea_ok = rmsea_ok,
      cfi_ok = cfi_ok,
      srmr_ok = srmr_ok
    )
  }

  # ───────────────── Calcular pérdida compuesta ─────────────────
  composite_loss <- function(fit_indices) {
    # Verificar que los índices sean válidos
    if (is.na(fit_indices$rmsea) || is.na(fit_indices$cfi) || is.na(fit_indices$srmr)) {
      return(Inf)  # Retornar infinito si hay valores NA
    }

    rmsea_loss <- max(0, (fit_indices$rmsea - thresholds$rmsea_target) / 0.03)
    cfi_loss <- max(0, (thresholds$cfi_target - fit_indices$cfi) / 0.03)
    srmr_loss <- max(0, (fit_indices$srmr - thresholds$srmr_target) / 0.03)

    # Pesos: RMSEA 50%, CFI 30%, SRMR 20%
    loss <- 0.5 * rmsea_loss + 0.3 * cfi_loss + 0.2 * srmr_loss

    # Asegurar que no sea NA
    if (is.na(loss) || !is.finite(loss)) return(Inf)

    loss
  }

  # ───────────────── Obtener cargas problemáticas ─────────────────
  get_problematic_items <- function(fit, factors) {
    std_sol <- tryCatch(standardizedSolution(fit), error = function(e) NULL)
    if (is.null(std_sol)) return(NULL)

    loadings <- std_sol[std_sol$op == "=~", ]
    problematic <- list()

    for (fname in names(factors)) {
      factor_loadings <- loadings[loadings$lhs == fname, ]
      low_loadings <- factor_loadings[abs(factor_loadings$est.std) < thresholds$loading, ]

      if (nrow(low_loadings) > 0) {
        for (i in 1:nrow(low_loadings)) {
          item <- low_loadings$rhs[i]
          problematic[[item]] <- list(
            factor = fname,
            loading = low_loadings$est.std[i],
            reason = "Low loading"
          )
        }
      }
    }

    problematic
  }

  # ───────────────── Obtener covarianzas con misespecificación (Saris-Satorra) ─────────────────
  get_best_covariances <- function(fit, factors, existing_covs, only_within = TRUE) {
    # Umbral de significancia del MI (chi2 con df=1)
    c_alpha <- qchisq(1 - mod_indices_config$alpha, df = 1)

    mi <- tryCatch({
      modificationIndices(fit, sort. = TRUE, minimum.value = c_alpha)
    }, error = function(e) NULL)

    if (is.null(mi) || nrow(mi) == 0) return(NULL)

    # Filtrar solo covarianzas residuales (~~) entre items
    mi_cov <- mi[mi$op == "~~", ]

    # Excluir varianzas (mismo item)
    mi_cov <- mi_cov[mi_cov$lhs != mi_cov$rhs, ]

    if (nrow(mi_cov) == 0) return(NULL)

    # Crear mapa de items a factores
    item_to_factor <- list()
    for (fname in names(factors)) {
      for (item in factors[[fname]]) {
        item_to_factor[[item]] <- fname
      }
    }

    # Filtrar por within-factor si se requiere
    if (only_within) {
      within_mask <- sapply(1:nrow(mi_cov), function(i) {
        f1 <- item_to_factor[[mi_cov$lhs[i]]]
        f2 <- item_to_factor[[mi_cov$rhs[i]]]
        !is.null(f1) && !is.null(f2) && f1 == f2
      })
      mi_cov <- mi_cov[within_mask, ]
    }

    if (nrow(mi_cov) == 0) return(NULL)

    # Excluir covarianzas ya existentes
    mi_cov$cov_str <- paste0(mi_cov$lhs, " ~~ ", mi_cov$rhs)
    mi_cov$cov_str_rev <- paste0(mi_cov$rhs, " ~~ ", mi_cov$lhs)

    existing_set <- c()
    for (cov in existing_covs) {
      existing_set <- c(existing_set, gsub("\\s+", " ", trimws(cov)))
    }

    mi_cov <- mi_cov[!(mi_cov$cov_str %in% existing_set | mi_cov$cov_str_rev %in% existing_set), ]

    if (nrow(mi_cov) == 0) return(NULL)

    # Clasificar cada par con framework Saris-Satorra-van der Veld (Table 6)
    classifications <- lapply(1:nrow(mi_cov), function(i) {
      classify_restriction(mi_cov$mi[i], mi_cov$epc[i],
                            delta = mod_indices_config$delta,
                            power_threshold = mod_indices_config$power_threshold,
                            alpha = mod_indices_config$alpha)
    })

    mi_cov$decision <- sapply(classifications, `[[`, "decision")
    mi_cov$power_mi <- sapply(classifications, `[[`, "power")

    # Solo mantener misespecificaciones sustantivas: "m" o "EPC:m"
    mi_cov <- mi_cov[mi_cov$decision %in% c("m", "EPC:m"), ]

    if (nrow(mi_cov) == 0) return(NULL)

    # Devolver las mejores
    mi_cov[1:min(nrow(mi_cov), mod_indices_config$max_covs_to_add), ]
  }

  # ───────────────── Ajustar modelo CFA ─────────────────
  fit_cfa <- function(model_syntax) {
    tryCatch({
      cfa(model_syntax,
          data = data,
          estimator = model_config$estimator,
          ordered = model_config$ordered)
    }, error = function(e) {
      if (verbose) cat("  Error al ajustar CFA:", e$message, "\n")
      NULL
    })
  }

  # ───────────────── LOOP PRINCIPAL ─────────────────

  # Parsear modelo inicial
  parsed <- parse_model(model)
  current_factors <- parsed$factors
  current_covs <- parsed$covs

  # Contadores y logs
  iteration <- 0
  removed_items <- character(0)
  added_covs <- character(0)
  steps_log <- data.frame(
    step = integer(),
    action = character(),
    detail = character(),
    rmsea = numeric(),
    cfi = numeric(),
    srmr = numeric(),
    loss = numeric(),
    stringsAsFactors = FALSE
  )

  # Ajustar modelo inicial
  current_syntax <- build_model_syntax(current_factors, current_covs)
  current_fit <- fit_cfa(current_syntax)

  if (is.null(current_fit)) {
    stop("No se pudo ajustar el modelo inicial")
  }

  current_indices <- extract_fit(current_fit)
  current_loss <- composite_loss(current_indices)

  if (verbose) {
    cat("--- MODELO INICIAL ---\n")
    cat("RMSEA:", fmt_num(current_indices$rmsea),
        "| CFI:", fmt_num(current_indices$cfi),
        "| SRMR:", fmt_num(current_indices$srmr),
        "| Loss:", fmt_num(current_loss), "\n\n")
  }

  steps_log <- rbind(steps_log, data.frame(
    step = 0, action = "Initial", detail = "Modelo base",
    rmsea = current_indices$rmsea, cfi = current_indices$cfi,
    srmr = current_indices$srmr, loss = current_loss,
    stringsAsFactors = FALSE
  ))

  # Loop de optimización
  while (iteration < performance$max_iterations) {
    iteration <- iteration + 1

    # Verificar si ya cumple targets
    targets_check <- meets_targets(current_indices)
    if (targets_check$all_met) {
      if (verbose) cat("\n*** TODOS LOS TARGETS ALCANZADOS ***\n")
      break
    }

    if (verbose) {
      cat(paste(rep("-", 60), collapse = ""), "\n")
      cat("ITERACIÓN", iteration, "\n")
      cat(paste(rep("-", 60), collapse = ""), "\n")
    }

    best_action <- NULL
    best_loss <- if (is.finite(current_loss)) current_loss else Inf
    best_fit <- NULL
    best_syntax <- NULL
    best_detail <- NULL

    # ─────────── ESTRATEGIA 1: Eliminar items con carga baja ───────────
    problematic <- get_problematic_items(current_fit, current_factors)

    if (length(problematic) > 0) {
      if (verbose) cat("  Evaluando eliminación de", length(problematic), "items problemáticos...\n")

      for (item in names(problematic)) {
        info <- problematic[[item]]
        factor_name <- info$factor

        # Verificar min_items_per_factor
        if (length(current_factors[[factor_name]]) <= thresholds$min_items_per_factor) {
          next
        }

        # Crear modelo sin el item
        test_factors <- current_factors
        test_factors[[factor_name]] <- setdiff(test_factors[[factor_name]], item)
        test_syntax <- build_model_syntax(test_factors, current_covs)

        test_fit <- fit_cfa(test_syntax)
        if (is.null(test_fit)) next

        # Verificar convergencia del modelo
        if (!lavInspect(test_fit, "converged")) next

        test_indices <- extract_fit(test_fit)
        test_loss <- composite_loss(test_indices)

        # Solo comparar si test_loss es válido y finito
        if (is.finite(test_loss) && test_loss < best_loss) {
          best_loss <- test_loss
          best_fit <- test_fit
          best_syntax <- test_syntax
          best_action <- "remove_item"
          best_detail <- paste0("Eliminar ", item, " (", factor_name, ", λ=", fmt_num(info$loading), ")")
          best_item <- item
          best_factor <- factor_name
        }
      }
    }

    # ─────────── ESTRATEGIA 2: Manejar covarianzas de error (Saris-Satorra) ───────────
    # Para cada par clasificado como misespecificado (m / EPC:m), evaluar 4 opciones:
    # 1) Agregar covarianza
    # 2) Eliminar item 1
    # 3) Eliminar item 2
    # 4) Eliminar ambos items
    best_covs <- get_best_covariances(current_fit, current_factors, current_covs,
                                       only_within = mod_indices_config$only_within_factor)

    if (!is.null(best_covs) && nrow(best_covs) > 0) {
      if (verbose) cat("  Evaluando", nrow(best_covs), "pares misespecificados [Saris-Satorra] (4 opciones c/u)...\n")

      # Crear mapa de items a factores
      item_to_factor <- list()
      for (fname in names(current_factors)) {
        for (item in current_factors[[fname]]) {
          item_to_factor[[item]] <- fname
        }
      }

      for (i in 1:nrow(best_covs)) {
        item1 <- best_covs$lhs[i]
        item2 <- best_covs$rhs[i]
        mi_value <- best_covs$mi[i]
        new_cov <- paste0(item1, " ~~ ", item2)

        factor1 <- item_to_factor[[item1]]
        factor2 <- item_to_factor[[item2]]

        if (verbose) {
          mi_decision <- best_covs$decision[i]
          mi_power <- best_covs$power_mi[i]
          cat("\n    --- Par:", new_cov,
              "(MI=", fmt_num(mi_value, 1),
              ", EPC=", fmt_num(best_covs$epc[i]),
              ", Power=", fmt_num(mi_power),
              ", =>", mi_decision, ") ---\n")
        }

        # ─── Opción 1: Agregar covarianza ───
        test_covs <- c(current_covs, new_cov)
        test_syntax <- build_model_syntax(current_factors, test_covs)
        test_fit <- fit_cfa(test_syntax)

        opt1_loss <- Inf
        opt1_indices <- NULL
        if (!is.null(test_fit) && lavInspect(test_fit, "converged")) {
          opt1_indices <- extract_fit(test_fit)
          opt1_loss <- composite_loss(opt1_indices)

          if (verbose) {
            cat("      [1] Agregar cov:      RMSEA=", fmt_num(opt1_indices$rmsea),
                " | CFI=", fmt_num(opt1_indices$cfi),
                " | SRMR=", fmt_num(opt1_indices$srmr),
                " | Loss=", fmt_num(opt1_loss), "\n")
          }

          if (is.finite(opt1_loss) && opt1_loss < best_loss) {
            best_loss <- opt1_loss
            best_fit <- test_fit
            best_syntax <- test_syntax
            best_action <- "add_cov"
            best_detail <- paste0("Agregar ", new_cov, " (MI=", fmt_num(mi_value, 1), ")")
            best_cov <- new_cov
            best_items_to_remove <- NULL
          }
        } else if (verbose) {
          cat("      [1] Agregar cov:      NO CONVERGE\n")
        }

        # ─── Opción 2: Eliminar item1 ───
        opt2_loss <- Inf
        opt2_indices <- NULL
        if (!is.null(factor1) && length(current_factors[[factor1]]) > thresholds$min_items_per_factor) {
          test_factors <- current_factors
          test_factors[[factor1]] <- setdiff(test_factors[[factor1]], item1)
          test_syntax <- build_model_syntax(test_factors, current_covs)
          test_fit <- fit_cfa(test_syntax)

          if (!is.null(test_fit) && lavInspect(test_fit, "converged")) {
            opt2_indices <- extract_fit(test_fit)
            opt2_loss <- composite_loss(opt2_indices)

            if (verbose) {
              cat("      [2] Eliminar ", item1, ":    RMSEA=", fmt_num(opt2_indices$rmsea),
                  " | CFI=", fmt_num(opt2_indices$cfi),
                  " | SRMR=", fmt_num(opt2_indices$srmr),
                  " | Loss=", fmt_num(opt2_loss), "\n", sep = "")
            }

            if (is.finite(opt2_loss) && opt2_loss < best_loss) {
              best_loss <- opt2_loss
              best_fit <- test_fit
              best_syntax <- test_syntax
              best_action <- "remove_cov_item"
              best_detail <- paste0("Eliminar ", item1, " (", factor1, ") en lugar de cov ", new_cov)
              best_items_to_remove <- list(list(item = item1, factor = factor1))
              best_cov <- NULL
            }
          } else if (verbose) {
            cat("      [2] Eliminar ", item1, ":    NO CONVERGE\n", sep = "")
          }
        } else if (verbose) {
          cat("      [2] Eliminar ", item1, ":    NO PERMITIDO (min items)\n", sep = "")
        }

        # ─── Opción 3: Eliminar item2 ───
        opt3_loss <- Inf
        opt3_indices <- NULL
        if (!is.null(factor2) && length(current_factors[[factor2]]) > thresholds$min_items_per_factor) {
          test_factors <- current_factors
          test_factors[[factor2]] <- setdiff(test_factors[[factor2]], item2)
          test_syntax <- build_model_syntax(test_factors, current_covs)
          test_fit <- fit_cfa(test_syntax)

          if (!is.null(test_fit) && lavInspect(test_fit, "converged")) {
            opt3_indices <- extract_fit(test_fit)
            opt3_loss <- composite_loss(opt3_indices)

            if (verbose) {
              cat("      [3] Eliminar ", item2, ":    RMSEA=", fmt_num(opt3_indices$rmsea),
                  " | CFI=", fmt_num(opt3_indices$cfi),
                  " | SRMR=", fmt_num(opt3_indices$srmr),
                  " | Loss=", fmt_num(opt3_loss), "\n", sep = "")
            }

            if (is.finite(opt3_loss) && opt3_loss < best_loss) {
              best_loss <- opt3_loss
              best_fit <- test_fit
              best_syntax <- test_syntax
              best_action <- "remove_cov_item"
              best_detail <- paste0("Eliminar ", item2, " (", factor2, ") en lugar de cov ", new_cov)
              best_items_to_remove <- list(list(item = item2, factor = factor2))
              best_cov <- NULL
            }
          } else if (verbose) {
            cat("      [3] Eliminar ", item2, ":    NO CONVERGE\n", sep = "")
          }
        } else if (verbose) {
          cat("      [3] Eliminar ", item2, ":    NO PERMITIDO (min items)\n", sep = "")
        }

        # ─── Opción 4: Eliminar ambos items ───
        opt4_loss <- Inf
        opt4_indices <- NULL
        can_remove_both <- (!is.null(factor1) && !is.null(factor2))
        if (can_remove_both) {
          # Verificar que no viole min_items_per_factor
          test_factors <- current_factors

          # Si son del mismo factor, verificar que queden suficientes
          if (factor1 == factor2) {
            can_remove_both <- length(current_factors[[factor1]]) > thresholds$min_items_per_factor + 1
            if (can_remove_both) {
              test_factors[[factor1]] <- setdiff(test_factors[[factor1]], c(item1, item2))
            }
          } else {
            # Factores diferentes
            can_remove_both <- (length(current_factors[[factor1]]) > thresholds$min_items_per_factor &&
                                length(current_factors[[factor2]]) > thresholds$min_items_per_factor)
            if (can_remove_both) {
              test_factors[[factor1]] <- setdiff(test_factors[[factor1]], item1)
              test_factors[[factor2]] <- setdiff(test_factors[[factor2]], item2)
            }
          }

          if (can_remove_both) {
            test_syntax <- build_model_syntax(test_factors, current_covs)
            test_fit <- fit_cfa(test_syntax)

            if (!is.null(test_fit) && lavInspect(test_fit, "converged")) {
              opt4_indices <- extract_fit(test_fit)
              opt4_loss <- composite_loss(opt4_indices)

              if (verbose) {
                cat("      [4] Eliminar ambos:   RMSEA=", fmt_num(opt4_indices$rmsea),
                    " | CFI=", fmt_num(opt4_indices$cfi),
                    " | SRMR=", fmt_num(opt4_indices$srmr),
                    " | Loss=", fmt_num(opt4_loss), "\n")
              }

              if (is.finite(opt4_loss) && opt4_loss < best_loss) {
                best_loss <- opt4_loss
                best_fit <- test_fit
                best_syntax <- test_syntax
                best_action <- "remove_cov_items_both"
                best_detail <- paste0("Eliminar ", item1, " y ", item2, " en lugar de cov ", new_cov)
                best_items_to_remove <- list(
                  list(item = item1, factor = factor1),
                  list(item = item2, factor = factor2)
                )
                best_cov <- NULL
              }
            } else if (verbose) {
              cat("      [4] Eliminar ambos:   NO CONVERGE\n")
            }
          } else if (verbose) {
            cat("      [4] Eliminar ambos:   NO PERMITIDO (min items)\n")
          }
        }
      }
    }

    # ─────────── Aplicar mejor acción ───────────
    if (is.null(best_action)) {
      if (verbose) cat("\n  No se encontraron mejoras posibles. Deteniendo.\n")
      break
    }

    # Actualizar modelo según la acción
    action_label <- ""

    if (best_action == "remove_item") {
      # Eliminación por carga baja
      current_factors[[best_factor]] <- setdiff(current_factors[[best_factor]], best_item)
      removed_items <- c(removed_items, best_item)
      action_label <- "Remove Item (low loading)"

    } else if (best_action == "add_cov") {
      # Agregar covarianza
      current_covs <- c(current_covs, best_cov)
      added_covs <- c(added_covs, best_cov)
      action_label <- "Add Covariance"

    } else if (best_action == "remove_cov_item") {
      # Eliminar un item en lugar de covarianza
      for (item_info in best_items_to_remove) {
        current_factors[[item_info$factor]] <- setdiff(current_factors[[item_info$factor]], item_info$item)
        removed_items <- c(removed_items, item_info$item)
      }
      action_label <- "Remove Item (instead of cov)"

    } else if (best_action == "remove_cov_items_both") {
      # Eliminar ambos items en lugar de covarianza
      for (item_info in best_items_to_remove) {
        current_factors[[item_info$factor]] <- setdiff(current_factors[[item_info$factor]], item_info$item)
        removed_items <- c(removed_items, item_info$item)
      }
      action_label <- "Remove Both Items (instead of cov)"
    }

    current_fit <- best_fit
    current_syntax <- best_syntax
    current_indices <- extract_fit(current_fit)
    current_loss <- best_loss

    # Log
    steps_log <- rbind(steps_log, data.frame(
      step = iteration,
      action = action_label,
      detail = best_detail,
      rmsea = current_indices$rmsea,
      cfi = current_indices$cfi,
      srmr = current_indices$srmr,
      loss = current_loss,
      stringsAsFactors = FALSE
    ))

    if (verbose) {
      cat("  ->", best_detail, "\n")
      cat("     RMSEA:", fmt_num(current_indices$rmsea),
          "| CFI:", fmt_num(current_indices$cfi),
          "| SRMR:", fmt_num(current_indices$srmr),
          "| Loss:", fmt_num(current_loss), "\n")
    }
  }

  # ───────────────── RESULTADOS FINALES ─────────────────

  final_indices <- extract_fit(current_fit)
  final_targets <- meets_targets(final_indices)

  # Cargas estandarizadas finales
  final_loadings <- tryCatch({
    std_sol <- standardizedSolution(current_fit)
    std_sol[std_sol$op == "=~", c("lhs", "rhs", "est.std", "se", "pvalue")]
  }, error = function(e) NULL)

  # Correlaciones entre factores
  final_correlations <- tryCatch({
    std_sol <- standardizedSolution(current_fit)
    factor_names <- names(current_factors)
    cors <- std_sol[std_sol$op == "~~" &
                     std_sol$lhs %in% factor_names &
                     std_sol$rhs %in% factor_names &
                     std_sol$lhs != std_sol$rhs,
                    c("lhs", "rhs", "est.std", "pvalue")]
    cors
  }, error = function(e) NULL)

  # Fiabilidad
  final_reliability <- tryCatch({
    if (requireNamespace("semTools", quietly = TRUE)) {
      semTools::compRelSEM(current_fit, tau.eq = FALSE, ord.scale = model_config$ordered)
    } else {
      NULL
    }
  }, error = function(e) NULL)

  if (verbose) {
    cat("\n")
    cat(paste(rep("=", 70), collapse = ""), "\n")
    cat("   RESULTADOS FINALES\n")
    cat(paste(rep("=", 70), collapse = ""), "\n\n")

    cat("Iteraciones:", iteration, "\n")
    cat("Items eliminados:", ifelse(length(removed_items) > 0,
                                     paste(removed_items, collapse = ", "), "Ninguno"), "\n")
    cat("Covarianzas agregadas:", ifelse(length(added_covs) > 0,
                                          paste(added_covs, collapse = "; "), "Ninguna"), "\n\n")

    cat("--- ÍNDICES DE AJUSTE FINALES ---\n")
    cat("RMSEA:", fmt_num(final_indices$rmsea),
        ifelse(final_targets$rmsea_ok, "(OK)", "(NO CUMPLE)"), "\n")
    cat("CFI:  ", fmt_num(final_indices$cfi),
        ifelse(final_targets$cfi_ok, "(OK)", "(NO CUMPLE)"), "\n")
    cat("SRMR: ", fmt_num(final_indices$srmr),
        ifelse(final_targets$srmr_ok, "(OK)", "(NO CUMPLE)"), "\n\n")

    if (final_targets$all_met) {
      cat("*** MODELO CUMPLE TODOS LOS CRITERIOS DE AJUSTE ***\n")
    } else {
      cat("*** MODELO NO CUMPLE TODOS LOS CRITERIOS ***\n")
    }

    cat("\n--- MODELO FINAL ---\n")
    cat(current_syntax, "\n")
  }

  # Devolver resultados
  list(
    final_fit = current_fit,
    final_syntax = current_syntax,
    final_factors = current_factors,
    final_covs = current_covs,
    fit_indices = final_indices,
    targets_met = final_targets,
    removed_items = removed_items,
    added_covariances = added_covs,
    steps_log = steps_log,
    iterations = iteration,
    standardized_loadings = final_loadings,
    factor_correlations = final_correlations,
    reliability = final_reliability,
    config_used = list(
      thresholds = thresholds,
      model_config = model_config,
      mod_indices_config = mod_indices_config,
      performance = performance
    )
  )
}
