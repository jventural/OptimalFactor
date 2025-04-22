\name{optimal_efa_with_ai}
\alias{optimal_efa_with_ai}
\title{Exploratory Factor Analysis Optimization with AI Assistance}
\description{
This function automatically refines an Exploratory Factor Analysis (EFA) solution by combining model‐fit criteria (scaled RMSEA) and item‐loading quality. At each iteration it:

1. Estimates the specified EFA model on the current set of items.
2. Computes the scaled RMSEA and checks whether it is ≤ \code{threshold_rmsea} and every factor retains at least \code{min_items_per_factor} items.
3. If fit or structure criteria are not met, identifies and removes the item whose exclusion yields the greatest RMSEA improvement or that exhibits structural issues (cross‐loading or no loading).
4. Repeats steps 1–3 until both fit and structure criteria are satisfied or \code{max_steps} iterations are reached.

Optionally, when \code{analyze_removed = TRUE}, it calls the OpenAI API to generate concise justifications for exclusion or retention of each item, using the provided \code{item_definitions}, \code{factor_definitions}, and other contextual parameters.
}
\usage{
optimal_efa_with_ai(
  data,
  items               = NULL,
  n_factors           = 5,
  n_items             = NULL,
  name_items          = "PPTQ",
  estimator           = "WLSMV",
  rotation            = "oblimin",
  threshold_rmsea     = 0.08,
  threshold_loading   = 0.30,
  min_items_per_factor= 2,
  apply_threshold     = TRUE,
  max_steps           = NULL,
  verbose             = TRUE,
  exclude_items       = character(0),
  analyze_removed     = FALSE,
  api_key             = NULL,
  item_definitions    = NULL,
  domain_name         = "Dominio por Defecto",
  scale_title         = "Título de la Escala por Defecto",
  construct_definition= "",
  model_name          = "Modelo EFA",
  gpt_model           = "gpt-3.5-turbo",
  item_factor_map     = NULL,
  factor_definitions  = NULL,
  ...
)
}
\arguments{
  \item{data}{A \code{data.frame} containing the observed variables for the EFA.}
  \item{items}{Character vector of item names to include; if \code{NULL}, names are generated using \code{name_items} and \code{n_items}.}
  \item{n_factors}{Integer. Number of factors to extract (default \code{5}).}
  \item{n_items}{Integer. Number of items per factor when \code{items} is \code{NULL}.}
  \item{name_items}{Character. Prefix for item names (default \code{"PPTQ"}).}
  \item{estimator}{Character. Estimator to use (e.g., \code{"WLSMV"}).}
  \item{rotation}{Character. Rotation method (e.g., \code{"oblimin"}).}
  \item{threshold_rmsea}{Numeric. Maximum allowable scaled RMSEA to stop refinement (default \code{0.08}).}
  \item{threshold_loading}{Numeric. Minimum absolute loading to consider an item well‐loaded (default \code{0.30}).}
  \item{min_items_per_factor}{Integer. Minimum items required per factor (default \code{2}).}
  \item{apply_threshold}{Logical. If \code{TRUE}, zeros out loadings below \code{threshold_loading} in the final solution.}
  \item{max_steps}{Integer or \code{NULL}. Maximum number of iterations; if \code{NULL}, set to \code{length(items) - 1}.}
  \item{verbose}{Logical. If \code{TRUE}, prints progress and removal decisions.}
  \item{exclude_items}{Character vector of items to exclude from the start.}
  \item{analyze_removed}{Logical. If \code{TRUE}, performs conceptual analysis for each removed and retained item via the OpenAI API.}
  \item{api_key}{String. OpenAI API key (required if \code{analyze_removed = TRUE}).}
  \item{item_definitions}{Named list mapping each item to its text definition for AI prompts.}
  \item{domain_name}{Character. Domain or factor names used in AI prompts (ignored if \code{n_factors = 1}).}
  \item{scale_title}{Character. Title of the scale for AI prompts.}
  \item{construct_definition}{Character. Brief definition of the construct for AI prompts.}
  \item{model_name}{Character. Label for the EFA model in AI prompts.}
  \item{gpt_model}{Character. Name of the ChatGPT model to use (e.g., \code{"gpt-3.5-turbo"}).}
  \item{item_factor_map}{Named integer vector mapping items to initial factor assignments (optional; ignored if \code{n_factors = 1}).}
  \item{factor_definitions}{Named list of factor definitions, indexed by factor number (optional; ignored if \code{n_factors = 1}).}
  \item{...}{Additional arguments passed to \code{\link[PsyMetricTools]{EFA_modern}}.}
}
\details{
When \code{n_factors = 1}, the arguments \code{factor_definitions}, \code{domain_name} and \code{item_factor_map} are cleared (not used).

The function proceeds as follows:
\enumerate{
  \item Installs and loads \code{PsyMetricTools} if not already available.
  \item Determines the initial set of items from \code{items} or from \code{name_items} and \code{n_items}.
  \item Enters an iterative loop:
    \enumerate{
      \item Estimates the EFA model with the current items.
      \item Computes the scaled RMSEA.
      \item Evaluates the factor‐loading structure for cross‐loadings or lack of loadings.
      \item If RMSEA ≤ \code{threshold_rmsea} and each factor has ≥ \code{min_items_per_factor}, stops.
      \item Otherwise, removes the item whose exclusion most improves RMSEA or that has the worst structural issue.
    }
  \item Optionally, for each removed and retained item, calls the OpenAI API to generate concise justifications using the provided definitions and context.
}
}
\value{
A list with components:
\item{final_structure}{A \code{data.frame} of final item loadings and factor assignments.}
\item{removed_items}{Character vector of items removed during refinement.}
\item{steps_log}{A \code{data.frame} recording each step: \code{step}, \code{removed_item}, \code{reason}, and \code{rmsea}.}
\item{iterations}{Integer. Total number of iterations performed.}
\item{final_rmsea}{Numeric. Final scaled RMSEA after the last iteration.}
\item{item_factor_map}{Named integer vector mapping each item to its final factor assignment.}
\item{factor_definitions}{List of factor definitions (if provided).}
\item{conceptual_analysis}{List with sublists \code{removed} and \code{kept} containing AI‐generated texts, or \code{NULL} if \code{analyze_removed = FALSE}.}
}
\examples{
\dontrun{
# Example definitions for conceptual analysis
defs <- list(
  Q1 = "I feel confident using advanced statistical methods.",
  Q2 = "I understand the difference between EFA and CFA.",
  Q3 = "I can interpret fit indices like RMSEA and CFI."
)

# Run optimized EFA with AI‐assisted analysis
res_efa <- optimal_efa_with_ai(
  data                 = my_data_frame,
  n_factors            = 1,
  name_items           = "Q",
  n_items              = 3,
  analyze_removed      = TRUE,
  api_key              = Sys.getenv("OPENAI_API_KEY"),
  item_definitions     = defs,
  domain_name          = "Competencia Estadística",
  scale_title          = "Autoevaluación Estadística",
  construct_definition = "Perceived ability in statistical methods.",
  model_name           = "EFA_Q",
  gpt_model            = "gpt-4",
  factor_definitions   = list("1" = "Capacidad estadística percibida")
)

# Inspect final structure
print(res_efa$final_structure)

# View removal log
print(res_efa$steps_log)

# Summary of results
# 1. Mostrar la estructura factorial final
cat("===== Estructura factorial final =====\n")
print(resultado_efa$final_structure)

# 2. Mostrar el log de pasos
cat("\n===== Log de pasos =====\n")
print(resultado_efa$steps_log)

# 3. Resumen de ítems eliminados, RMSEA final e iteraciones
cat("\n===== Resumen =====\n")
cat("Ítems eliminados: ",
    if (length(resultado_efa$removed_items) > 0) {
      paste(resultado_efa$removed_items, collapse = ", ")
    } else {
      "Ninguno"
    },
    "\n", sep = "")


cat("RMSEA final: ", resultado_efa$final_rmsea, "\n", sep = "")
cat("Iteraciones realizadas: ", resultado_efa$iterations, "\n", sep = "")

# 4. Análisis conceptual con IA (si está disponible)
# Función para imprimir de forma ordenada el análisis conceptual
print_conceptual_analysis(resultado_efa)
}
}
\author{
Dr. José Ventura‐León
}
