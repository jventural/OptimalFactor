\name{optimal_cfa_with_ai}
\alias{optimal_cfa_with_ai}
\title{Confirmatory Factor Analysis Optimization with AI Assistance}
\description{
This function iteratively refines a Confirmatory Factor Analysis (CFA) specification by combining two main strategies:
\enumerate{
  \item Systematic removal of items based on standardized loadings and lavaan modification indices, eliminating the item with the least contribution to model fit.
  \item Optional conceptual justification of each removal or retention decision via calls to the OpenAI API when \code{analyze_removed = TRUE}.
}
The process repeats until the scaled RMSEA falls below the specified threshold (\code{rmsea_threshold}) or the maximum number of iterations (\code{max_steps}) is reached.
}
\usage{
optimal_cfa_with_ai(
  initial_model,
  data,
  rmsea_threshold     = 0.08,
  mi_threshold        = 3.84,
  max_steps           = 10,
  verbose             = TRUE,
  debug               = FALSE,
  filter_expr         = NULL,
  exclude_items       = character(0),
  analyze_removed     = FALSE,
  api_key             = NULL,
  item_definitions    = NULL,
  domain_name         = "Default Domain",
  scale_title         = "Default Scale Title",
  construct_definition= "",
  model_name          = "CFA Model",
  gpt_model           = "gpt-3.5-turbo",
  ...
)
}

\arguments{
  \item{initial_model}{A character string specifying the initial CFA model in lavaan syntax (e.g., \code{"F1 =~ x1 + x2 + x3"}).}
  \item{data}{A \code{data.frame} containing the observed variables for CFA.}
  \item{rmsea_threshold}{Numeric. The maximum allowable scaled RMSEA to stop the refinement (default \code{0.08}).}
  \item{mi_threshold}{Numeric. The minimum modification index (MI) value to consider an error covariance or cross-loading candidate (default \code{3.84}).}
  \item{max_steps}{Integer. Maximum number of iterative refinement steps (default \code{10}).}
  \item{verbose}{Logical. If \code{TRUE}, prints detailed progress messages.}
  \item{debug}{Logical. If \code{TRUE}, prints additional debugging information.}
  \item{filter_expr}{An R expression used to subset \code{data} before fitting; if \code{NULL}, no filtering is applied.}
  \item{exclude_items}{Character vector of item names to exclude from the start.}
  \item{analyze_removed}{Logical. If \code{TRUE}, performs conceptual analysis for each removed and retained item via OpenAI.}
  \item{api_key}{String. OpenAI API key (required if \code{analyze_removed = TRUE}).}
  \item{item_definitions}{Named list mapping each item to its textual definition for conceptual prompts.}
  \item{domain_name}{Character vector of domain or factor names, used in AI prompts.}
  \item{scale_title}{String. Title of the scale or questionnaire, for AI prompts.}
  \item{construct_definition}{String. Brief definition of the construct, for AI prompts.}
  \item{model_name}{String. Label for the CFA model, for AI prompts.}
  \item{gpt_model}{String. Name of the ChatGPT model to use (e.g., \code{"gpt-3.5-turbo"} or \code{"gpt-4"}).}
  \item{...}{Additional arguments passed to \code{\link[lavaan]{cfa}} (e.g., \code{estimator}, \code{ordered}, \code{mimic}, etc.).}
}
\details{
The function executes the following steps:
\enumerate{
  \item Optionally filters the data using \code{filter_expr}.
  \item Fits the initial CFA model with \code{lavaan::cfa}.
  \item Computes the scaled RMSEA; if below \code{rmsea_threshold}, stops.
  \item Extracts modification indices via \code{lavaan::modificationIndices} and selects the highest MI.
  \item Compares the standardized loadings of the two parameters involved and removes the item with the lower loading.
  \item Logs the step, including the modification string, MI value, and current RMSEA.
  \item Repeats the process until the RMSEA threshold is met or \code{max_steps} is reached.
  \item If \code{analyze_removed = TRUE}, calls the OpenAI API to generate conceptual justifications for each removed and each retained item.
}
}
\value{
A list with elements:
\item{final_model}{A character string of the CFA specification after all modifications.}
\item{final_fit}{The \code{lavaan} fit object for the final model.}
\item{log}{A \code{data.frame} recording each step’s modification, MI value, and RMSEA.}
\item{removed_items}{Character vector of the names of removed items.}
\item{alternative_fit}{The last \code{lavaan} fit object before stopping.}
\item{alternative_rmsea}{Final scaled RMSEA of the alternative fit.}
\item{final_cfi}{Final scaled CFI of the alternative fit.}
\item{conceptual_analysis}{A list with two sublists, \code{removed} and \code{kept}, containing AI-generated texts; \code{NULL} if not requested.}
}
\examples{
\dontrun{
# Initial model specification
mod0 <- 'F1 =~ x1 + x2 + x3 + x4 + x5'

# Definitions for AI
defs <- list(
  x1 = "Item 1 description",
  x2 = "Item 2 description",
  x3 = "Item 3 description",
  x4 = "Item 4 description",
  x5 = "Item 5 description"
)

# Run the optimization
result <- optimal_cfa_with_ai(
  initial_model        = mod0,
  data                 = my_data_frame,
  rmsea_threshold      = 0.07,
  mi_threshold         = 4.0,
  max_steps            = 8,
  verbose              = TRUE,
  analyze_removed      = TRUE,
  api_key              = "OPENAI_API_KEY",
  item_definitions     = defs,
  domain_name          = c("Factor1", "Factor2"),
  scale_title          = "My Questionnaire",
  construct_definition = "Degree of job satisfaction",
  model_name           = "CFA_JobSat",
  gpt_model            = "gpt-4"
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
if (!is.null(resultado_cfa$conceptual_analysis)) {
  cat("\n7) Análisis conceptual de ítems eliminados:\n")
  print(resultado_cfa$conceptual_analysis$removed)

  cat("\n8) Análisis conceptual de ítems conservados:\n")
  print(resultado_cfa$conceptual_analysis$kept)
}
}
}

\author{
Dr. José Ventura-León
}
