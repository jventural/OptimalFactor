\name{optimal_cfa_with_ai}
\alias{optimal_cfa_with_ai}
\title{Confirmatory Factor Analysis Optimization with AI Assistance}
\description{
Automatically refines a user‐specified Confirmatory Factor Analysis (CFA) model by iteratively evaluating fit and modification indices. At each step, the function:

1. Fits the CFA model and computes the scaled RMSEA.
2. If RMSEA ≤ \code{rmsea_threshold}, stops.
3. Otherwise, identifies all modification indices (MI) > \code{mi_threshold}.
4. Selects the largest MI and compares the standardized loadings for the two involved items; removes the item with the smaller loading.
5. Records the modification, RMSEA and MI, updates the model syntax by dropping the removed item, and repeats until RMSEA is acceptable, no MI exceed threshold, or \code{max_steps} is reached.

Optionally, when \code{analyze_removed = TRUE}, it uses the OpenAI API to generate concise justifications for exclusion or retention of each item, based on provided \code{item_definitions} and \code{factor_definitions}.
}
\usage{
optimal_cfa_with_ai(
  initial_model,
  data,
  rmsea_threshold      = 0.08,
  mi_threshold         = 3.84,
  max_steps            = 10,
  verbose              = TRUE,
  debug                = FALSE,
  filter_expr          = NULL,
  exclude_items        = character(0),
  analyze_removed      = FALSE,
  api_key              = NULL,
  item_definitions     = NULL,
  domain_name          = "Dominio por Defecto",
  scale_title          = "Título de la Escala por Defecto",
  construct_definition = "",
  model_name           = "Modelo CFA",
  gpt_model            = "gpt-3.5-turbo",
  factor_definitions   = NULL,
  ...
)
}
\arguments{
  \item{initial_model}{A character string with the lavaan‐style CFA model syntax.}
  \item{data}{A \code{data.frame} containing the observed variables.}
  \item{rmsea_threshold}{Numeric. Maximum acceptable scaled RMSEA to stop refinement.}
  \item{mi_threshold}{Numeric. Minimum modification index value to consider.}
  \item{max_steps}{Integer. Maximum number of refinement iterations.}
  \item{verbose}{Logical. If \code{TRUE}, prints progress messages.}
  \item{debug}{Logical. If \code{TRUE}, prints additional debugging information.}
  \item{filter_expr}{An expression to subset \code{data} before fitting (optional).}
  \item{exclude_items}{Character vector of items to never remove.}
  \item{analyze_removed}{Logical. If \code{TRUE}, performs AI‐driven conceptual analysis.}
  \item{api_key}{String. OpenAI API key (required if \code{analyze_removed = TRUE}).}
  \item{item_definitions}{Named list mapping each item to its textual content for AI prompts.}
  \item{domain_name}{Character. Domain or factor label used in AI prompts.}
  \item{scale_title}{Character. Scale title used in AI prompts.}
  \item{construct_definition}{Character. Brief construct definition for AI context.}
  \item{model_name}{Character. Label for the CFA model in AI prompts.}
  \item{gpt_model}{Character. Name of the ChatGPT model (e.g., \code{"gpt-3.5-turbo"}).}
  \item{factor_definitions}{Named list of factor descriptions, indexed by factor name.}
  \item{...}{Additional arguments passed to \code{\link[lavaan]{cfa}}.}
}
\details{
The function proceeds through these phases:

\enumerate{
  \item \strong{Initialization:} Parses the initial model syntax into factor and item lists.
  \item \strong{Filtering:} Optionally subsets \code{data} via \code{filter_expr}.
  \item \strong{Iterative refinement (up to \code{max_steps}):}
    \enumerate{
      \item Fit the current CFA model.
      \item Compute scaled RMSEA; if ≤ \code{rmsea_threshold}, stop.
      \item Extract all MI > \code{mi_threshold}; if none, stop.
      \item Select the single largest MI, compare the two involved items’ standardized loadings, and remove the item with the lower loading.
      \item Update the model syntax by dropping that item and log the step.
    }
  \item \strong{Final fit:} Stores the last fitted \code{lavaan} object and final RMSEA/CFI.
  \item \strong{Conceptual analysis (optional):} For each removed and retained item, calls the OpenAI API to generate justification text.
}
}
\value{
A list with elements:
\item{final_model}{Character. The refined CFA model syntax.}
\item{final_fit}{A \code{lavaan} object of the last fitted model.}
\item{log}{Data frame recording each refinement step (\code{step}, \code{modification}, \code{mi_value}, \code{rmsea}).}
\item{removed_items}{Character vector of items removed.}
\item{alternative_rmsea}{Numeric. Final scaled RMSEA.}
\item{final_cfi}{Numeric. Final scaled CFI.}
\item{conceptual_analysis}{List with sublists \code{removed} and \code{kept} containing AI‐generated texts, or \code{NULL} if not performed.}
}
\examples{
\dontrun{
# Define initial CFA model
model_str <- '
  F1 =~ Q1 + Q2 + Q3
  F2 =~ Q4 + Q5 + Q6
'
# Run optimization without AI
res_cfa <- optimal_cfa_with_ai(
  initial_model = model_str,
  data          = my_data,
  rmsea_threshold = 0.06,
  mi_threshold    = 5,
  max_steps       = 8
)

# Inspect results
cat("Removed items:", paste(res_cfa$removed_items, collapse = ", "), "\n")
lavaan::summary(res_cfa$final_fit)

# Run with AI‐driven analysis
res_cfa_ai <- optimal_cfa_with_ai(
  initial_model       = model_str,
  data                = my_data,
  analyze_removed     = TRUE,
  api_key             = Sys.getenv("OPENAI_API_KEY"),
  item_definitions    = list(Q1="...", Q2="...", Q3="...", Q4="...", Q5="...", Q6="..."),
  factor_definitions  = list(F1="Factor 1 description", F2="Factor 2 description")
)

# 1. Lista de nombres de componentes disponibles
cat("Componentes en resultado_cfa:\n")
print(names(resultado_cfa))

# 2. Registro de pasos (log)
cat("\n1) Log de modificaciones por paso:\n")
print(resultado_cfa$log)

# 3) Modelo final
cat("\n2) Modelo final:\n")
cat(resultado_cfa$final_model, "\n")
# cat("Modelo final:\n", resultado_cfa$final_model)

# 5) Resumen del ajuste del modelo final
cat("\n4) Resumen del ajuste (final_fit):\n")
print(summary(resultado_cfa$final_fit, standardized = T, fit.measures = T, rsquare = T))

# 6) Ítems eliminados
cat("\n5) Ítems eliminados:\n")
cat(paste(resultado_cfa$removed_items, collapse = ", "), "\n")

# 7) Medidas finales
cat("\n6) Medidas finales:\n")
cat("  • RMSEA final:", resultado_cfa$alternative_rmsea, "\n")
cat("  • CFI final:  ", resultado_cfa$final_cfi, "\n")

# 8) Análisis conceptual (si existe)
print_conceptual_analysis(res_cfa_ai)
}
}
\author{
Dr. José Ventura‐León
}
