\name{efa_boosting}
\alias{efa_boosting}
\title{EFA-Boosting Optimization Summary Report}

\description{
Performs an iterative, machine-learning–inspired optimization of Exploratory Factor Analysis (EFA), combining greedy and optionally global subset search to improve model fit (RMSEA, SRMR, CFI) and structural coherence (loadings, Heywood cases, cross-loadings, and minimum items per factor).
When \code{verbose = TRUE}, the function prints the full \emph{EFA-Boosting Optimization Summary Report}, including: (a) detailed iteration diagnostics, (b) elimination criteria, (c) thresholded final structure, (d) fit evolution, and (e) optional GPT-based conceptual analysis of removed items.
Returns a complete list of results invisibly.
}

\usage{
efa_boosting(
  data,
  name_items,
  item_range = NULL,
  n_factors = 3,
  exclude_items = NULL,
  thresholds = list(...),
  model_config = list(...),
  performance = list(...),
  use_global = FALSE,
  global_opt = list(...),
  fit_config = list(...),
  use_ai_analysis = FALSE,
  ai_config = list(...),
  verbose = TRUE,
  ...
)
}

\arguments{

\item{\code{data}}{Data frame containing item responses.}

\item{\code{name_items}}{
Item prefix (e.g., "IT" yields IT1, IT2, …). Used for auto-detection.
}

\item{\code{item_range}}{
Integer vector \code{c(start, end)}.
If \code{NULL}, items are auto-detected using \code{name_items} + digits.
}

\item{\code{n_factors}}{Number of factors to extract.}

\item{\code{exclude_items}}{Character vector of items to exclude before optimization.}

\item{\code{thresholds}}{
Controls all structural decisions:
\itemize{
  \item \code{rmsea}: Target RMSEA threshold for stopping.
  \item \code{loading}: Minimum acceptable loading; below-threshold values are zeroed for evaluation.
  \item \code{min_items_per_factor}: Structural protection rule.
  \item \code{heywood_tol}, \code{near_heywood}: Detection of Heywood and near-Heywood cases.
}
}

\item{\code{model_config}}{
Estimator and rotation, typically:
\itemize{
  \item \code{estimator = "WLSMV"}
  \item \code{rotation = "oblimin"}
}}

\item{\code{performance}}{
Performance and timeout settings:
\itemize{
  \item \code{max_candidates_eval}: Limits greedy evaluation (NULL = evaluate all).
  \item \code{timeout_efa}: Timeout per EFA run (requires R.utils).
  \item \code{timeout_optimization}: Global timeout for full boosting cycle.
  \item \code{use_timeouts}: Enable/disable timeout protection.
}
}

\item{\code{use_global}}{
Enable global subset search (TRUE = hybrid greedy + global search).
}

\item{\code{global_opt}}{
Global search configuration:
\itemize{
  \item \code{max_drop}: Maximum subset size k removed simultaneously.
  \item \code{max_global_combinations}: Hard cap on combinations.
  \item \code{verbose}: Show global search diagnostics.
  \item \code{progress_bar}: Print progress bar during global evaluation.
}
}

\item{\code{fit_config}}{
Configuration for the composite fit index:
\itemize{
  \item \code{targets}: Target RMSEA, SRMR, CFI.
  \item \code{margins}: Allowable deviations.
  \item \code{base_weights}: Weighting of RMSEA/SRMR/CFI.
  \item \code{small_df_cut}: df threshold triggering alternative weights.
  \item \code{small_df_weights}: Weight shift for low-df models.
  \item \code{wlsmv_boost}: Re-weightings applied if estimator = WLSMV.
  \item \code{use_pclose_if_available}: Use p-close >= .05 as bonus.
  \item \code{pclose_bonus}: Amount subtracted from composite loss.
}
}

\item{\code{use_ai_analysis}}{
Enable GPT-based conceptual analysis for each removed (and optionally retained) item.
}

\item{\code{ai_config}}{
Configuration for AI analysis:
\itemize{
  \item \code{api_key}: API key for GPT models.
  \item \code{gpt_model}: e.g., "gpt-3.5-turbo", "gpt-4".
  \item \code{language}: Spanish or English.
  \item \code{analysis_detail}: "brief", "standard", or "detailed".
  \item \code{only_removed}: If FALSE, analyze retained items also.
  \item \code{construct_definition}: Short description of the target construct.
  \item \code{item_definitions}: Named list of item contents.
}
}

\item{\code{verbose}}{
If TRUE, prints the full \emph{EFA-Boosting Optimization Summary Report}, including step-by-step iteration output, loading tables, RMSEA evolution, and AI progress bars.
}

}

\details{

\strong{Optimization strategy.}
The function iteratively improves EFA structure and fit using a hierarchy of rules:
\itemize{
  \item Remove items causing Heywood or near-Heywood problems.
  \item Resolve cross-loadings by eliminating items with smallest ambiguity gap.
  \item Enforce \code{min_items_per_factor}.
  \item If structure is acceptable but RMSEA > target: remove weakest-loading item.
  \item When \code{use_global = TRUE}, evaluate multi-item drops (k ≤ max\_drop) that minimize composite loss.
}

\strong{Composite loss.}
The loss combines RMSEA, SRMR, and CFI with adaptive weighting (small-df correction, WLSMV boosters, and p-close bonus), enabling more stable decisions during early and late stages of optimization.

\strong{Stopping rules.}
The algorithm halts when:
\itemize{
  \item All items satisfy structural rules AND
  \item RMSEA ≤ target AND
  \item Composite loss ≈ 0 (within tolerance), OR
  \item No removal is possible without violating \code{min_items_per_factor}.
}

\strong{EFA-Boosting Optimization Summary Report.}
When \code{verbose = TRUE}, the function prints:
\itemize{
  \item Header summarizing estimator, rotation, thresholds, and search mode.
  \item Full iteration diagnostics: RMSEA, SRMR, CFI, df, composite loss.
  \item Table of per-factor item counts and structure integrity.
  \item Loading matrix thresholded at \code{loading}.
  \item Notifications for all removal events (Heywood, cross-loading, weakest-loading, global-fit).
  \item Final structure summary with item–factor mapping.
  \item Fit indices from the final model.
}

\strong{AI conceptual analysis.}
If enabled, the function:
\itemize{
  \item Generates narrative analyses for removed items (and optionally retained items).
  \item Includes loading statistics, h², ψ, RMSEA at removal, detection reason.
  \item Uses multi-attempt retry with exponential backoff for API robustness.
  \item Includes a timeline of elimination and detailed reasoning.
}

}

\value{
Returns (invisibly) a list containing:
\itemize{
  \item \code{final_structure}: Thresholded loadings.
  \item \code{removed_items}: Items removed in order.
  \item \code{steps_log}: Table of removal events.
  \item \code{iterations}: Number of iterations.
  \item \code{final_rmsea}: Final RMSEA.
  \item \code{bondades_original}: Fit indices from last model.
  \item \code{inter_factor_correlation}: Final factor correlation matrix.
  \item \code{last_h2}, \code{last_psi}: Final communalities and uniquenesses.
  \item \code{last_flags}: Heywood and near-Heywood flags.
  \item \code{conceptual_analysis}: GPT-based structured interpretations.
  \item \code{config_used}: All thresholds and configuration details.
}
}

\examples{
\dontrun{
set.seed(123)
X <- as.data.frame(matrix(rnorm(300 * 15), ncol = 15))
names(X) <- paste0("IT",1:15)

res <- efa_boosting(
  data = X,
  name_items = "IT",
  item_range = c(1,15),
  n_factors = 3,
  use_global = TRUE,
  verbose = TRUE
)

# The full EFA-Boosting Optimization Summary Report will print automatically.
}
}

\seealso{
\code{\link{print_conceptual_analysis}},
\code{\link{export_conceptual_analysis}}
}
\encoding{UTF-8}
