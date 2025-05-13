\name{optimal_efa_with_ai}
\alias{optimal_efa_with_ai}
\title{Exploratory Factor Analysis Optimization with AI Assistance}
\description{
Automatically refines an Exploratory Factor Analysis (EFA) solution by combining
model‐fit criteria (scaled RMSEA) and item‐loading quality, and—when requested—
integrates AI-generated conceptual analyses and factor naming.  At each iteration it:
\enumerate{
  \item Estimates the specified EFA model on the current set of items.
  \item Computes the scaled RMSEA and checks whether it is ≤ \code{threshold_rmsea}
        and every factor retains at least \code{min_items_per_factor} items.
  \item If fit or structure criteria are not met, identifies and removes the item
        whose exclusion yields the greatest RMSEA improvement or that exhibits
        structural issues (cross‐loading or no loading).
  \item Repeats steps 1–3 until both fit and structure criteria are satisfied or
        \code{max_steps} iterations are reached.
}
Optionally, when \code{analyze_removed = TRUE} and \code{item_definitions} are provided,
it calls the OpenAI API to generate concise justifications for both exclusion and
retention of each item, using the provided definitions and the final loading structure
as context.  If \code{generate_factor_names = TRUE}, it also prompts the AI to propose
brief (1–2 word) tentative names for each factor, taking into account the specified
\code{domain_name}, \code{scale_title}, and \code{construct_definition}.
}
\usage{
optimal_efa_with_ai(
  data,
  items                    = NULL,
  n_factors                = 5,
  n_items                  = NULL,
  name_items               = "PPTQ",
  estimator                = "WLSMV",
  rotation                 = "oblimin",
  threshold_rmsea          = 0.08,
  threshold_loading        = 0.30,
  min_items_per_factor     = 2,
  apply_threshold          = TRUE,
  max_steps                = NULL,
  verbose                  = TRUE,
  exclude_items            = character(0),
  analyze_removed          = FALSE,
  api_key                  = NULL,
  item_definitions         = NULL,
  domain_name              = "Dominio por Defecto",
  scale_title              = "Título de la Escala por Defecto",
  construct_definition     = "",
  model_name               = "Modelo EFA",
  gpt_model                = "gpt-3.5-turbo",
  generate_factor_names    = FALSE,
  ...
)
}
\arguments{
  \item{data}{A \code{data.frame} containing the observed variables for the EFA.}
  \item{items}{Character vector of item names to include; if \code{NULL}, names are
                generated using \code{name_items} and \code{n_items}.}
  \item{n_factors}{Integer. Number of factors to extract (default \code{5}).}
  \item{n_items}{Integer. Number of items per factor when \code{items} is \code{NULL}.}
  \item{name_items}{Character. Prefix for item names (default \code{"PPTQ"}).}
  \item{estimator}{Character. Estimator to use (e.g., \code{"WLSMV"}).}
  \item{rotation}{Character. Rotation method (e.g., \code{"oblimin"}).}
  \item{threshold_rmsea}{Numeric. Maximum allowable scaled RMSEA to stop refinement
                         (default \code{0.08}).}
  \item{threshold_loading}{Numeric. Minimum absolute loading to consider an item
                           well‐loaded (default \code{0.30}).}
  \item{min_items_per_factor}{Integer. Minimum items required per factor
                              (default \code{2}).}
  \item{apply_threshold}{Logical. If \code{TRUE}, zeros out loadings below
                         \code{threshold_loading} in the final solution.}
  \item{max_steps}{Integer or \code{NULL}. Maximum number of iterations; if
                    \code{NULL}, set to \code{length(items) - 1}.}
  \item{verbose}{Logical. If \code{TRUE}, prints progress and removal decisions.}
  \item{exclude_items}{Character vector of items to exclude from the start.}
  \item{analyze_removed}{Logical. If \code{TRUE}, performs conceptual analysis for each
                         removed and conserved item via the OpenAI API.}
  \item{api_key}{String. OpenAI API key (required if \code{analyze_removed = TRUE} or
                  \code{generate_factor_names = TRUE}).}
  \item{item_definitions}{Named list mapping each item to its text definition for AI
                          prompts.}
  \item{domain_name}{Character. Domain or factor context used in AI prompts.}
  \item{scale_title}{Character. Scale title used in AI prompts.}
  \item{construct_definition}{Character. Brief definition of the construct used in AI
                              prompts.}
  \item{model_name}{Character. Label for the EFA model in AI prompts.}
  \item{gpt_model}{Character. Name of the ChatGPT model to use (e.g., \code{"gpt-3.5-turbo"}).}
  \item{generate_factor_names}{Logical. If \code{TRUE}, triggers factor naming via AI.}
  \item{\dots}{Additional arguments passed to \code{\link[PsyMetricTools]{EFA_modern}}.}
}
\details{
The function proceeds as follows:
\enumerate{
  \item Installs and loads \code{PsyMetricTools} if not already available.
  \item Determines the initial set of items from \code{items} or from \code{name_items}
        and \code{n_items}.
  \item Enters an iterative loop:
    \enumerate{
      \item Estimates the EFA model with the current items.
      \item Computes the scaled RMSEA.
      \item Evaluates the factor‐loading structure for cross‐loadings or lack of loadings.
      \item If RMSEA ≤ \code{threshold_rmsea} and each factor has ≥
            \code{min_items_per_factor}, stops.
      \item Otherwise, removes the item whose exclusion most improves RMSEA or that
            has the worst structural issue.
    }
  \item If \code{analyze_removed = TRUE}, for each excluded and conserved item, generates
        via OpenAI concise justifications based on the final structure.
  \item If \code{generate_factor_names = TRUE} and \code{api_key} provided, proposes
        tentative names for each factor via AI, considering the specified
        \code{domain_name}, \code{scale_title}, and \code{construct_definition}.
}
}
\value{
A list with components:
\item{final_structure}{A \code{data.frame} of final item loadings and factor assignments.}
\item{removed_items}{Character vector of items removed during refinement.}
\item{steps_log}{A \code{data.frame} recording each step: \code{step},
                 \code{removed_item}, \code{reason}, and \code{rmsea}.}
\item{iterations}{Integer. Total number of iterations performed.}
\item{final_rmsea}{Numeric. Final scaled RMSEA after the last iteration.}
\item{bondades_original}{Original fit indices and other model information from
                         \code{\link[PsyMetricTools]{EFA_modern}}.}
\item{specifications}{Model specifications returned by
                      \code{\link[PsyMetricTools]{EFA_modern}}.}
\item{conceptual_analysis}{A list with elements \code{removed} and \code{kept},
                           each a named list of AI-generated texts per item,
                           or \code{NULL} if \code{analyze_removed = FALSE}.}
\item{factor_names}{Named character vector or list of AI‐proposed factor names,
                    or \code{NULL} if \code{generate_factor_names = FALSE}.}
}
\examples{
\dontrun{
  # Construct definition
  construct_def <- "Las consecuencias positivas de terminar una carrera de grado,
                   tales como prestigio, compensación monetaria y mejora en la autoevaluación."

  # Definiciones de ítems para EFA
  item_defs_efa_ud <- list(
    EAF1  = "Graduarme de la universidad me ayudará a recibir una buena oferta de trabajo.",
    EAF2  = "Graduarme de la universidad me ayudará a ganar un buen sueldo.",
    EAF3  = "Graduarme de la universidad me ayudará a obtener el respeto de los demás.",
    EAF4  = "Graduarme de la universidad me ayudará a hacer un trabajo en el que encuentre satisfacción personal.",
    EAF5  = "Graduarme de la universidad me ayudará a aumentar mi autoestima.",
    EAF6  = "Graduarme de la universidad me ayudará a tener una carrera que sea valorada por mi familia.",
    EAF7  = "Graduarme de la universidad me ayudará a hacer un trabajo que pueda \"marcar la diferencia\" en la vida de las personas.",
    EAF8  = "Graduarme de la universidad me ayudará a trabajar en un campo con grandes oportunidades laborales.",
    EAF9  = "Graduarme de la universidad me ayudará a hacer un trabajo apasionante.",
    EAF10 = "Graduarme de la universidad me ayudará a tener el tipo y la cantidad de contacto social necesario con otras personas."
  )

  # Ejecutar EFA optimizada con asistencia de IA
  res_efa <- optimal_efa_with_ai(
    data                  = df_EFA %>% dplyr::select(EAF1:EAF10),
    items                 = names(df_EFA %>% dplyr::select(EAF1:EAF10)),
    name_items            = "EAF",
    n_factors             = 2,
    analyze_removed       = TRUE,
    api_key               = Sys.getenv("OPENAI_API_KEY"),
    item_definitions      = item_defs_efa_ud,
    construct_definition  = construct_def,
    scale_title           = "EAF Scale",
    model_name            = "EFA Multidimensional",
    generate_factor_names = TRUE,
    domain_name           = "Expectativas académicas de resultados"
  )

  # Ver resultados
  print(res_efa$final_structure)
  print(res_efa$steps_log)
  print(res_efa$conceptual_analysis)
  print(res_efa$factor_names)
}
}
