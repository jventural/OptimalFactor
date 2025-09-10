\name{efa_optimizer}
\alias{efa_optimizer}
\title{Exploratory Factor Analysis Optimizer with Optional LLM Support}

\description{
Automatically refines an Exploratory Factor Analysis (EFA) solution by combining global fit (scaled RMSEA), item–level loading quality, and explicit detection of Heywood/near-Heywood cases. At each iteration the routine re-fits the model, removes the worst offending item (by structural fault or RMSEA improvement), and stops when predefined fit/structure criteria are met or no admissible improvement remains. Optionally, it can call a Large Language Model (LLM) to generate concise explanations for removing/keeping items.
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
\item{\code{item_range}}{Integer vector of length 2 with the first/last item indices;
if \code{NULL}, items are discovered by the prefix followed by a trailing integer.}
\item{\code{n_factors}}{Number of factors to extract.}
\item{\code{exclude_items}}{Character vector of items to exclude at the start.}
\item{\code{thresholds}}{List of decision thresholds:
\describe{
  \item{\code{rmsea}}{Maximum acceptable scaled RMSEA.}
  \item{\code{loading}}{Minimum salient absolute loading.}
  \item{\code{min_items_per_factor}}{Minimum items retained per factor.}
  \item{\code{heywood_tol}}{Tolerance for detecting negative uniqueness (\eqn{\psi < -}\code{heywood_tol}).}
  \item{\code{near_heywood}}{Band near zero uniqueness treated as near-Heywood.}
}}
\item{\code{model_config}}{List of EFA fitting options:
\describe{
  \item{\code{estimator}}{Estimator (e.g., \code{"WLSMV"}).}
  \item{\code{rotation}}{Rotation method (e.g., \code{"oblimin"}).}
}}
\item{\code{use_ai_analysis}}{Logical; if \code{TRUE}, requests LLM justifications.}
\item{\code{ai_config}}{List with LLM settings:
\describe{
  \item{\code{api_key}}{API key (character).}
  \item{\code{generate_names}}{If \code{TRUE}, also request short factor names (if implemented).}
  \item{\code{only_removed}}{If \code{TRUE}, justify only removed items.}
  \item{\code{gpt_model}}{LLM model identifier.}
  \item{\code{domain_name}, \code{scale_title}, \code{construct_definition}, \code{model_name}}{Context strings for prompts.}
  \item{\code{item_definitions}}{Named list mapping item IDs to their plain-language descriptions.}
}}
\item{\code{verbose}}{Logical; if \code{TRUE}, prints progress and decisions.}
\item{\code{\dots}}{Additional arguments forwarded to \code{PsyMetricTools::EFA_modern()}.}
}

\details{
\strong{Iteration logic.}
\enumerate{
\item Fit an EFA using \code{PsyMetricTools::EFA_modern} on the current item set (oblique rotation by default).
\item Compute scaled RMSEA and extract the loading matrix \eqn{L} and, if available, the factor correlation matrix \eqn{\Phi}.
\item Derive communalities \eqn{h^2 = \mathrm{diag}(L \Phi L')} and uniquenesses \eqn{\psi = 1 - h^2} to flag:
  \itemize{
    \item Heywood (\eqn{\psi < -}\code{heywood_tol} or impossible loadings),
    \item near-Heywood (\eqn{\psi} in \code{[-near_heywood, near_heywood]}),
    \item no salient loading (all \eqn{|loading| <} \code{loading}),
    \item cross-loadings (2+ salient loadings; severity scored by the gap between the two largest absolute loadings).
  }
\item Enforce \code{min_items_per_factor} based on primary (largest absolute) loading, ignoring rows with no salient loading.
\item Removal policy (in order):
  \enumerate{
    \item Remove the most severe Heywood item.
    \item Else remove the worst near-Heywood item.
    \item Else, if RMSEA exceeds \code{thresholds$rmsea}, simulate single-item removals that preserve \code{min_items_per_factor} and drop the candidate yielding the best RMSEA improvement.
    \item Else remove the structurally worst item (by diagnostic score).
  }
\item Stop when RMSEA \eqn{\le} threshold and all factors meet \code{min_items_per_factor}, or when no admissible removal improves fit/structure, or when the maximum steps are reached.
}
If \eqn{\Phi} is not returned by the fit, the identity matrix is used. When \code{use_ai_analysis = TRUE} and \code{item_definitions} are provided, the function optionally calls an LLM to draft concise human-readable justifications; this does not affect the optimization path.
}

\value{
A list with:
\describe{
  \item{\code{final_structure}}{Data frame with the final, thresholded loadings and item labels.}
  \item{\code{removed_items}}{Character vector of removed items, in order.}
  \item{\code{steps_log}}{Data frame with \code{step}, \code{removed_item}, \code{reason}, \code{rmsea}.}
  \item{\code{iterations}}{Number of iterations executed.}
  \item{\code{final_rmsea}}{Scaled RMSEA at termination.}
  \item{\code{bondades_original}}{Fit indices returned by \code{EFA_modern}.}
  \item{\code{specifications}}{Model specifications returned by \code{EFA_modern}.}
  \item{\code{inter_factor_correlation}}{Estimated \eqn{\Phi} (identity if unavailable or \code{n_factors == 1}).}
  \item{\code{last_h2}}{Vector of communalities from the last iteration.}
  \item{\code{last_psi}}{Vector of uniquenesses from the last iteration.}
  \item{\code{last_flags}}{List with logical vectors \code{heywood} and \code{near}.}
  \item{\code{config_used}}{Echo of \code{thresholds}, \code{model_config}, \code{use_ai_analysis}, and \code{ai_config}.}
  \item{\code{conceptual_analysis}}{(If AI used) List of LLM justifications for removed/kept items, or \code{NULL}.}
}
}

\examples{
\dontrun{
# Minimal runnable illustration
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
}
}

\seealso{
\code{PsyMetricTools::EFA_modern}
}
