#' Print CFA-Boosting Results
#'
#' Formatted console printer for the object returned by
#' \code{\link{cfa_boosting}}. Displays fit indices, the iteration trail,
#' factor loadings (above a threshold), latent correlations, reliability
#' coefficients and the final lavaan model syntax.
#'
#' @param result Output from \code{\link{cfa_boosting}}.
#' @param show_loadings Logical. Print standardized factor loadings of
#'   the final model. Default \code{TRUE}.
#' @param show_correlations Logical. Print latent factor correlations.
#'   Default \code{TRUE}.
#' @param show_reliability Logical. Print omega and alpha by factor.
#'   Default \code{TRUE}.
#' @param show_steps Logical. Print the per-iteration log of move / drop
#'   / cov operations and the resulting fit indices. Default \code{TRUE}.
#' @param show_model Logical. Print the final lavaan model syntax.
#'   Default \code{TRUE}.
#' @param loading_threshold Numeric. Loadings below this absolute value
#'   are hidden in the printed table. Default \code{0.30}.
#' @param digits Integer. Number of decimal places. Default \code{3}.
#'
#' @return Invisibly returns \code{result}; called for its side effects
#'   on the console.
#'
#' @examples
#' \donttest{
#' data(Data_Personality, package = "OptimalFactor")
#' res <- cfa_boosting(
#'   data       = Data_Personality,
#'   name_items = "P",
#'   n_factors  = 3,
#'   estimator  = "WLSMV",
#'   rotation   = "oblimin")
#'
#' # Full printout (default).
#' print_cfa_boosting(res)
#'
#' # Compact printout — hide steps log, raise the loading threshold, and
#' # show fewer decimals when reporting in a slide deck.
#' print_cfa_boosting(res,
#'   show_steps        = FALSE,
#'   loading_threshold = 0.40,
#'   digits            = 2)
#' }
#'
#' @seealso \code{\link{cfa_boosting}}, \code{\link{export_cfa_boosting}}
#'
#' @export
print_cfa_boosting <- function(result,
                               show_loadings = TRUE,
                               show_correlations = TRUE,
                               show_reliability = TRUE,
                               show_steps = TRUE,
                               show_model = TRUE,
                               loading_threshold = 0.30,
                               digits = 3) {

  # ───────────────── Utilidades ─────────────────
  fmt_num <- function(x, d = digits) {
    if (is.null(x) || length(x) == 0) return("NA")
    if (is.na(x)) return("NA")
    sprintf(paste0("%.", d, "f"), x)
  }

  line_sep <- function(char = "=", width = 70) {
    paste(rep(char, width), collapse = "")
  }

  # ───────────────── Encabezado ─────────────────
  cat("\n")
  cat(line_sep("="), "\n")
  cat("   RESULTADOS CFA BOOSTING\n")
  cat(line_sep("="), "\n\n")

  # ───────────────── Resumen General ─────────────────
  cat("--- RESUMEN ---\n\n")
  cat("Iteraciones realizadas:", result$iterations, "\n")
  cat("Items eliminados:",
      ifelse(length(result$removed_items) > 0,
             paste(result$removed_items, collapse = ", "),
             "Ninguno"), "\n")
  cat("Covarianzas agregadas:",
      ifelse(length(result$added_covariances) > 0,
             length(result$added_covariances),
             0), "\n\n")

  # ───────────────── Índices de Ajuste ─────────────────
  cat("--- ÍNDICES DE AJUSTE ---\n\n")

  fit <- result$fit_indices
  targets <- result$targets_met

  # Tabla de índices
  cat(sprintf("%-15s %10s %10s %10s\n", "Índice", "Valor", "Criterio", "Estado"))
  cat(line_sep("-", 50), "\n")

  cat(sprintf("%-15s %10s %10s %10s\n",
              "RMSEA", fmt_num(fit$rmsea), "<= 0.08",
              ifelse(targets$rmsea_ok, "OK", "NO CUMPLE")))

  cat(sprintf("%-15s %10s %10s %10s\n",
              "CFI", fmt_num(fit$cfi), ">= 0.95",
              ifelse(targets$cfi_ok, "OK", "NO CUMPLE")))

  cat(sprintf("%-15s %10s %10s %10s\n",
              "TLI", fmt_num(fit$tli), ">= 0.95",
              ifelse(!is.na(fit$tli) && fit$tli >= 0.95, "OK", "NO CUMPLE")))

  cat(sprintf("%-15s %10s %10s %10s\n",
              "SRMR", fmt_num(fit$srmr), "<= 0.08",
              ifelse(targets$srmr_ok, "OK", "NO CUMPLE")))

  cat(sprintf("%-15s %10s %10s %10s\n",
              "Chi-cuadrado", fmt_num(fit$chisq, 1), "", ""))

  cat(sprintf("%-15s %10s %10s %10s\n",
              "df", fmt_num(fit$df, 0), "", ""))

  cat("\n")

  if (targets$all_met) {
    cat("*** MODELO CUMPLE TODOS LOS CRITERIOS ***\n\n")
  } else {
    cat("*** MODELO NO CUMPLE TODOS LOS CRITERIOS ***\n\n")
  }

  # ───────────────── Cargas Factoriales ─────────────────
  if (show_loadings && !is.null(result$standardized_loadings)) {
    cat("--- CARGAS FACTORIALES ESTANDARIZADAS ---\n\n")

    loadings <- result$standardized_loadings
    names(loadings) <- c("Factor", "Item", "Carga", "SE", "p")

    # Agregar indicador de carga baja
    loadings$Estado <- ifelse(abs(loadings$Carga) < loading_threshold,
                               "* BAJA", "")

    # Formatear valores
    loadings$Carga <- sprintf("%.3f", loadings$Carga)
    loadings$SE <- sprintf("%.3f", loadings$SE)
    loadings$p <- ifelse(loadings$p < 0.001, "<.001", sprintf("%.3f", loadings$p))

    # Imprimir por factor
    factors <- unique(result$standardized_loadings$lhs)

    for (f in factors) {
      cat(sprintf("\n  %s:\n", f))
      cat(sprintf("  %-8s %8s %8s %8s %8s\n", "Item", "Carga", "SE", "p", ""))
      cat(paste0("  ", line_sep("-", 45)), "\n")

      f_loadings <- loadings[loadings$Factor == f, ]
      for (i in 1:nrow(f_loadings)) {
        cat(sprintf("  %-8s %8s %8s %8s %8s\n",
                    f_loadings$Item[i],
                    f_loadings$Carga[i],
                    f_loadings$SE[i],
                    f_loadings$p[i],
                    f_loadings$Estado[i]))
      }
    }
    cat("\n")
    cat("  * Cargas < ", loading_threshold, " se consideran bajas\n\n", sep = "")
  }

  # ───────────────── Correlaciones entre Factores ─────────────────
  if (show_correlations && !is.null(result$factor_correlations) &&
      nrow(result$factor_correlations) > 0) {
    cat("--- CORRELACIONES ENTRE FACTORES ---\n\n")

    cors <- result$factor_correlations
    names(cors) <- c("Factor1", "Factor2", "r", "p")

    cat(sprintf("  %-12s %-12s %8s %8s\n", "Factor 1", "Factor 2", "r", "p"))
    cat(paste0("  ", line_sep("-", 45)), "\n")

    for (i in 1:nrow(cors)) {
      p_val <- ifelse(cors$p[i] < 0.001, "<.001", sprintf("%.3f", cors$p[i]))
      cat(sprintf("  %-12s %-12s %8.3f %8s\n",
                  cors$Factor1[i],
                  cors$Factor2[i],
                  cors$r[i],
                  p_val))
    }
    cat("\n")
  }

  # ───────────────── Fiabilidad ─────────────────
  if (show_reliability && !is.null(result$reliability)) {
    cat("--- FIABILIDAD (Omega) ---\n\n")

    rel <- result$reliability

    cat(sprintf("  %-15s %8s %10s\n", "Factor", "Omega", "Interpretación"))
    cat(paste0("  ", line_sep("-", 40)), "\n")

    for (i in seq_along(rel)) {
      omega_val <- rel[i]
      interp <- ifelse(omega_val >= 0.80, "Buena",
                       ifelse(omega_val >= 0.70, "Aceptable",
                              ifelse(omega_val >= 0.60, "Cuestionable", "Pobre")))
      cat(sprintf("  %-15s %8.3f %10s\n", names(rel)[i], omega_val, interp))
    }
    cat("\n")
  }

  # ───────────────── Historial de Pasos ─────────────────
  if (show_steps && !is.null(result$steps_log) && nrow(result$steps_log) > 1) {
    cat("--- HISTORIAL DE OPTIMIZACIÓN ---\n\n")

    steps <- result$steps_log

    # Función para acortar nombres de acciones
    shorten_action <- function(action) {
      action <- gsub("Remove Both Items \\(instead of cov\\)", "Elim. ambos", action)
      action <- gsub("Remove Item \\(instead of cov\\)", "Elim. item", action)
      action <- gsub("Remove Item \\(low loading\\)", "Elim. carga baja", action)
      action <- gsub("Add Covariance", "Agregar cov", action)
      action <- gsub("Initial", "Inicial", action)
      action
    }

    # Función para acortar detalles
    shorten_detail <- function(detail) {
      detail <- gsub("en lugar de cov ", "", detail)
      detail <- gsub("Eliminar ", "- ", detail)
      detail <- gsub("Agregar ", "+ ", detail)
      detail <- gsub("Modelo base", "Base", detail)
      if (nchar(detail) > 30) detail <- paste0(substr(detail, 1, 27), "...")
      detail
    }

    cat(sprintf("  %-4s %-16s %-30s %7s %7s %7s\n",
                "Paso", "Acción", "Detalle", "RMSEA", "CFI", "SRMR"))
    cat(paste0("  ", line_sep("-", 75)), "\n")

    for (i in 1:nrow(steps)) {
      action_short <- shorten_action(steps$action[i])
      detail_short <- shorten_detail(steps$detail[i])

      cat(sprintf("  %-4d %-16s %-30s %7.3f %7.3f %7.3f\n",
                  steps$step[i],
                  action_short,
                  detail_short,
                  steps$rmsea[i],
                  steps$cfi[i],
                  steps$srmr[i]))
    }
    cat("\n")
  }

  # ───────────────── Covarianzas Agregadas ─────────────────
  if (length(result$added_covariances) > 0) {
    cat("--- COVARIANZAS DE ERROR AGREGADAS ---\n\n")
    for (cov in result$added_covariances) {
      cat("  ", cov, "\n")
    }
    cat("\n")
  }

  # ───────────────── Modelo Final ─────────────────
  if (show_model) {
    cat("--- SINTAXIS DEL MODELO FINAL (para lavaan) ---\n\n")
    cat(result$final_syntax, "\n\n")
  }

  # ───────────────── Advertencias ─────────────────
  # Detectar problemas potenciales
  warnings_list <- character(0)

  if (!is.null(result$standardized_loadings)) {
    low_loadings <- result$standardized_loadings[abs(result$standardized_loadings$est.std) < loading_threshold, ]
    if (nrow(low_loadings) > 0) {
      warnings_list <- c(warnings_list,
                         paste0("Items con cargas bajas: ",
                                paste(low_loadings$rhs, collapse = ", ")))
    }
  }

  if (!is.null(result$reliability)) {
    low_rel <- names(result$reliability)[result$reliability < 0.70]
    if (length(low_rel) > 0) {
      warnings_list <- c(warnings_list,
                         paste0("Factores con fiabilidad < 0.70: ",
                                paste(low_rel, collapse = ", ")))
    }
  }

  if (length(warnings_list) > 0) {
    cat("--- ADVERTENCIAS ---\n\n")
    for (w in warnings_list) {
      cat("  ! ", w, "\n")
    }
    cat("\n")
  }

  cat(line_sep("="), "\n")

  # Retornar invisible el resultado original
  invisible(result)
}

#' Export CFA Boosting Results to Structured Data Frames
#'
#' Export CFA Boosting results to structured data frames suitable for
#' saving as CSV, embedding in a manuscript table, or further analysis.
#'
#' @param result Output from \code{\link{cfa_boosting}}.
#'
#' @return A named list of data frames (\code{fit_indices},
#'   \code{standardized_loadings}, \code{factor_correlations},
#'   \code{reliability}, \code{steps_log}).
#'
#' @examples
#' \donttest{
#' data(Data_Personality, package = "OptimalFactor")
#' res <- cfa_boosting(Data_Personality, name_items = "P", n_factors = 3)
#' out <- export_cfa_boosting(res)
#' names(out)
#'
#' # Each component is a data.frame ready for write.csv() / openxlsx
#' head(out$fit_indices)
#' head(out$standardized_loadings)
#' head(out$steps_log)
#'
#' # Save the lot to disk in one shot.
#' \dontrun{
#'   for (nm in names(out))
#'     write.csv(out[[nm]],
#'               sprintf("cfa_boost_\%s.csv", nm), row.names = FALSE)
#' }
#' }
#'
#' @seealso \code{\link{cfa_boosting}}, \code{\link{print_cfa_boosting}}
#'
#' @export
export_cfa_boosting <- function(result) {

  list(
    fit_indices = data.frame(
      Index = c("RMSEA", "CFI", "TLI", "SRMR", "Chi-square", "df"),
      Value = c(result$fit_indices$rmsea,
                result$fit_indices$cfi,
                result$fit_indices$tli,
                result$fit_indices$srmr,
                result$fit_indices$chisq,
                result$fit_indices$df),
      Criterion = c("<= 0.08", ">= 0.95", ">= 0.95", "<= 0.08", "", ""),
      Met = c(result$targets_met$rmsea_ok,
              result$targets_met$cfi_ok,
              !is.na(result$fit_indices$tli) && result$fit_indices$tli >= 0.95,
              result$targets_met$srmr_ok,
              NA, NA)
    ),
    loadings = if (!is.null(result$standardized_loadings)) {
      df <- result$standardized_loadings
      names(df) <- c("Factor", "Item", "Loading", "SE", "p_value")
      df
    } else NULL,
    correlations = if (!is.null(result$factor_correlations)) {
      df <- result$factor_correlations
      names(df) <- c("Factor1", "Factor2", "Correlation", "p_value")
      df
    } else NULL,
    reliability = if (!is.null(result$reliability)) {
      data.frame(
        Factor = names(result$reliability),
        Omega = as.numeric(result$reliability)
      )
    } else NULL,
    steps = result$steps_log,
    removed_items = result$removed_items,
    added_covariances = result$added_covariances,
    model_syntax = result$final_syntax
  )
}
