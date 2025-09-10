\name{efa_optimizer}
\alias{efa_optimizer}
\title{Exploratory Factor Analysis Optimizer with Optional LLM Support}

\description{
Automatically refines an Exploratory Factor Analysis (EFA) solution by combining global fit (scaled RMSEA), item–level loading quality, and explicit detection of Heywood/near-Heywood cases. At each iteration the routine re-fits the model, removes the worst offending item (by structural fault or RMSEA improvement), and stops when predefined fit/structure criteria are met or no admissible improvement remains. Optionally, it can call a Large Language Model (LLM) to generate plain-language justifications for removing/keeping items, with language control and detail level.
}

\usage{
efa_optimizer(
  data,
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
  verbose = TRUE, ...
)
}

\arguments{
\item{\code{data}}{A \code{data.frame} containing observed variables.}
\item{\code{name_items}}{Common item prefix (e.g., \code{"DP"} or \code{"PPTQ"}).}
\item{\code{item_range}}{Integer vector of length 2 with the first/last item indices; if \code{NULL}, items are discovered by the prefix followed by a trailing integer.}
\item{\code{n_factors}}{Number of factors to extract.}
\item{\code{exclude_items}}{Character vector of items to exclude at the start.}
\item{\code{thresholds}}{List of decision thresholds:
\describe{
  \item{\code{rmsea}}{Maximum acceptable scaled RMSEA.}
  \item{\code{loading}}{Minimum salient absolute loading.}
  \item{\code{min_items_per_factor}}{Minimum items retained per factor (based on primary loading).}
  \item{\code{heywood_tol}}{Tolerance for detecting negative uniqueness (\eqn{\psi < -}\code{heywood\_tol}) or impossible loadings.}
  \item{\code{near_heywood}}{Band around zero uniqueness treated as near-Heywood.}
}}
\item{\code{model_config}}{List of EFA fitting options:
\describe{
  \item{\code{estimator}}{Estimator (e.g., \code{"WLSMV"}).}
  \item{\code{rotation}}{Rotation method (e.g., \code{"oblimin"}).}
}}
\item{\code{use_ai_analysis}}{Logical; if \code{TRUE}, requests LLM justifications without affecting the optimization path.}
\item{\code{ai_config}}{List with LLM settings:
\describe{
  \item{\code{api_key}}{API key (character).}
  \item{\code{generate_names}}{If \code{TRUE}, may also request short factor names (if implemented).}
  \item{\code{only_removed}}{If \code{TRUE}, justify only removed items; otherwise also justify retained items.}
  \item{\code{gpt_model}}{LLM model identifier.}
  \item{\code{language}}{Language for justifications; supports at least \code{"english"} and \code{"spanish"}.}
  \item{\code{analysis_detail}}{Verbosity level for the LLM answer; one of \code{"brief"}, \code{"standard"}, \code{"detailed"}.}
  \item{\code{domain_name}, \code{scale_title}, \code{construct_definition}, \code{model_name}}{Context strings injected into prompts.}
  \item{\code{item_definitions}}{Named list mapping item IDs to plain-language item stems/descriptions used by the LLM.}
}}
\item{\code{verbose}}{Logical; if \code{TRUE}, prints progress, current structure, counts per factor, and a progress bar during LLM analysis.}
\item{\code{\dots}}{Additional arguments forwarded to \code{PsyMetricTools::EFA_modern()}.}
}

\details{
\strong{Iteration logic.}
\enumerate{
\item Fit an EFA using \code{PsyMetricTools::EFA_modern} on the current item set (oblique rotation by default).
\item Extract the loading matrix \eqn{L} and the factor correlation matrix \eqn{\Phi} (identity if unavailable).
\item Derive communalities \eqn{h^2 = \mathrm{diag}(L \Phi L')} and uniquenesses \eqn{\psi = 1 - h^2}. Flag:
  \itemize{
    \item Heywood (\eqn{\psi < -}\code{heywood\_tol} or impossible loadings),
    \item near-Heywood (\eqn{\psi} within \code{[-near\_heywood, near\_heywood]}),
    \item no salient loading (all \eqn{|loading| <} \code{loading}),
    \item cross-loadings (2+ salient loadings; severity scored by the gap between the two largest absolute loadings).
  }
\item Enforce \code{min_items_per_factor} using the primary (largest absolute) loading; rows with no salient loading do not count toward any factor.
\item Removal policy (priority order):
  \enumerate{
    \item Remove the most severe Heywood item.
    \item Else remove the worst near-Heywood item.
    \item Else, if RMSEA exceeds \code{thresholds$rmsea}, simulate single-item removals (that preserve \code{min\_items\_per\_factor}) and drop the candidate yielding the best RMSEA improvement.
    \item Else remove the structurally worst item (by diagnostic score).
  }
\item Stop when RMSEA \eqn{\le} threshold and all factors meet \code{min\_items\_per\_factor}, or when no admissible removal improves fit/structure, or when the maximum number of steps is reached.
}

\strong{LLM justifications (optional).}
If \code{use_ai_analysis = TRUE} and \code{item_definitions} are provided, the function can call an LLM via \pkg{httr} to draft plain-language justifications. The language (\code{ai_config$language}) and verbosity (\code{ai_config$analysis_detail}) control the style and length. The request includes, when available, item-level technical statistics (primary loading, \eqn{h^2}, reason for removal, and RMSEA at the removal step). Transient server errors (e.g., HTTP 502/503/504) and rate limits (HTTP 429) are handled with up to three attempts and exponential backoff. This analysis is purely descriptive and does not influence the optimization path.

\strong{Dependencies.}
If LLM is used, \pkg{httr} and \pkg{jsonlite} are required; the function attempts to install them if missing.
}

\value{
A list with:
\describe{
  \item{\code{final_structure}}{Data frame with the final, thresholded loadings and item labels.}
  \item{\code{removed_items}}{Character vector of removed items, in order.}
  \item{\code{steps_log}}{Data frame with \code{step}, \code{removed_item}, \code{reason}, \code{rmsea} (value at that step).}
  \item{\code{iterations}}{Number of iterations executed.}
  \item{\code{final_rmsea}}{Scaled RMSEA at termination.}
  \item{\code{bondades_original}}{Fit indices returned by \code{EFA_modern}.}
  \item{\code{specifications}}{Model specifications returned by \code{EFA_modern}.}
  \item{\code{inter_factor_correlation}}{Estimated \eqn{\Phi} (identity if unavailable or \code{n_factors == 1}).}
  \item{\code{last_h2}}{Vector of communalities from the last iteration.}
  \item{\code{last_psi}}{Vector of uniquenesses from the last iteration.}
  \item{\code{last_flags}}{List with logical vectors \code{heywood} and \code{near} for the last iteration.}
  \item{\code{conceptual_analysis}}{If LLM used, a list with:
    \describe{
      \item{\code{removed}}{Named list of justifications for removed items (or \code{NULL}).}
      \item{\code{kept}}{Named list of justifications for retained items if \code{only_removed = FALSE} (or \code{NULL}).}
      \item{\code{item_stats}}{Named list of per-item statistics passed to the LLM (primary loading, \eqn{h^2}, removal reason, RMSEA at removal).}
    }
  }
  \item{\code{config_used}}{Echo of \code{thresholds}, \code{model_config}, \code{use_ai_analysis}, and \code{ai_config} (including \code{language} and \code{analysis_detail}).}
}
}

\examples{
\dontrun{
# Minimal runnable illustration (without LLM)
set.seed(123)
X <- as.data.frame(matrix(rnorm(300 * 9), ncol = 9))
names(X) <- paste0("DP", 1:9)

res <- efa_optimizer(
  data = X,
  name_items = "DP",
  item_range = c(1, 9),
  n_factors = 3,
  thresholds = list(
    rmsea = 0.08, loading = 0.30, min_items_per_factor = 3,
    heywood_tol = 1e-6, near_heywood = 0.015
  ),
  model_config = list(estimator = "WLSMV", rotation = "oblimin"),
  verbose = TRUE
)

print(res$final_structure)
print(res$steps_log)
res$final_rmsea

# LLM justification sketch (requires a valid API key and item definitions):
# res_ai <- efa_optimizer(
#   data = X,
#   name_items = "DP",
#   item_range = c(1, 9),
#   n_factors = 3,
#   use_ai_analysis = TRUE,
#   ai_config = list(
#     api_key = Sys.getenv("OPENAI_API_KEY"),
#     item_definitions = as.list(setNames(paste("Item", 1:9, "content"), names(X))),
#     language = "spanish",
#     analysis_detail = "standard",
#     only_removed = TRUE
#   ),
#   verbose = TRUE
# )
# str(res_ai$conceptual_analysis, max.level = 1)
}
}

\seealso{
\code{PsyMetricTools::EFA_modern}
}
