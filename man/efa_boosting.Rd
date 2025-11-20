\name{efa_boosting}
\alias{efa_boosting}
\title{EFA-Boosting Optimization}

\description{
Performs an iterative, machine-learning–inspired optimization of Exploratory Factor Analysis (EFA).
The algorithm combines greedy elimination with optional global subset search (multi-item removal)
to improve both model fit (RMSEA, SRMR, CFI) and structural coherence
(loadings, Heywood and near-Heywood detection, cross-loadings, and minimum items per factor).

A composite loss function with adaptive weighting (small-df correction, WLSMV boosts, p-close bonus)
guides decisions when structure is acceptable but statistical fit requires improvement.

When \code{verbose = TRUE}, the function prints the full
\emph{EFA-Boosting Optimization Summary Report}, including:
(a) detailed iteration diagnostics,
(b) elimination events and reasons,
(c) thresholded final structure,
(d) evolution of fit indices and composite loss, and
(e) optional GPT-based conceptual analysis of removed (and optionally retained) items.

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

\item{\code{data}}{
Data frame containing item responses.
}

\item{\code{name_items}}{
Prefix used to identify items (e.g., \code{"IT"} yields IT1, IT2, …).
Supports automatic item detection.
}

\item{\code{item_range}}{
Integer vector \code{c(start, end)}.
If \code{NULL}, items are auto-detected as \code{name_items} + digits.
}

\item{\code{n_factors}}{
Number of factors to extract.
}

\item{\code{exclude_items}}{
Character vector of items to exclude prior to the optimization loop.
}

\item{\code{thresholds}}{
Structural control settings:
\itemize{
  \item \code{loading}: Minimum acceptable loading; smaller values are thresholded to zero.
  \item \code{min_items_per_factor}: Structural protection rule.
  \item \code{heywood_tol}, \code{near_heywood}: Criteria for Heywood and near-Heywood detection.
  \item \code{min_interfactor_correlation}: Minimum acceptable factor correlation.
}
}

\item{\code{model_config}}{
EFA model configuration:
\itemize{
  \item \code{estimator}: Typically \code{"WLSMV"}.
  \item \code{rotation}: Typically \code{"oblimin"}.
}
}

\item{\code{performance}}{
Performance and timeout options:
\itemize{
  \item \code{max_candidates_eval}: Limit for greedy evaluation (NULL = evaluate all).
  \item \code{timeout_efa}: Timeout per EFA estimation (requires \code{R.utils}).
  \item \code{timeout_optimization}: Global timeout for the boosting cycle.
  \item \code{use_timeouts}: Enable usage of timeouts.
}
}

\item{\code{use_global}}{
Enable hybrid optimization with global subset search (multi-item drops).
}

\item{\code{global_opt}}{
Settings for global search:
\itemize{
  \item \code{max_drop}: Maximum subset size \emph{k} removed simultaneously.
  \item \code{max_global_combinations}: Hard limit on total combinations.
  \item \code{verbose}: Show global search diagnostics.
  \item \code{progress_bar}: Display progress bar during global evaluation.
}
}

\item{\code{fit_config}}{
Configuration for the composite fit index:
\itemize{
  \item \code{targets}: Target RMSEA, SRMR, CFI.
  \item \code{margins}: Allowable deviations from each target.
  \item \code{base_weights}: Weighting of RMSEA / SRMR / CFI.
  \item \code{small_df_cut}: df threshold for weight switching.
  \item \code{small_df_weights}: Alternative weights for low-df models.
  \item \code{wlsmv_boost}: Multiplicative re-weighting when using WLSMV.
  \item \code{use_pclose_if_available}: Use p-close ≥ .05 to reduce composite loss.
  \item \code{pclose_bonus}: Amount subtracted from composite loss.
}
}

\item{\code{use_ai_analysis}}{
Enable GPT-based conceptual analysis for removed items (and optionally retained items).
}

\item{\code{ai_config}}{
Settings for GPT-based analysis:
\itemize{
  \item \code{api_key}: API key for GPT models.
  \item \code{gpt_model}: Model name (e.g., \code{"gpt-4"}, \code{"gpt-3.5-turbo"}).
  \item \code{language}: Output language ("Spanish" or "English").
  \item \code{analysis_detail}: "brief", "standard", or "detailed".
  \item \code{only_removed}: If FALSE, analyze retained items as well.
  \item \code{construct_definition}: Definition of the psychological construct.
  \item \code{item_definitions}: Named list of item contents.
}
}

\item{\code{verbose}}{
If TRUE, prints the full \emph{EFA-Boosting Optimization Summary Report},
including iteration diagnostics, structural changes, model fit evolution,
and AI progress bars.
}

}

\details{

\strong{Optimization strategy.}
The algorithm follows a hierarchical decision system:
\itemize{
  \item Detect and remove Heywood and near-Heywood items.
  \item Resolve cross-loadings by eliminating the item with smallest ambiguity gap.
  \item Enforce \code{min_items_per_factor} at all stages.
  \item If structure is acceptable but RMSEA exceeds the target, remove the weakest-loading item.
  \item If \code{use_global = TRUE}, evaluate all subsets up to size \code{max_drop} that improve the composite loss.
}

\strong{Composite loss.}
A weighted index combining RMSEA, SRMR, and CFI with:
\itemize{
  \item adaptive weights for small df,
  \item WLSMV-specific boosts,
  \item p-close bonus when available.
}
This stabilizes selection decisions during early and late optimization phases.

\strong{Stopping rules.}
The procedure halts when:
\itemize{
  \item all structural criteria are satisfied, AND
  \item RMSEA ≤ target, AND
  \item composite loss is approximately zero, OR
  \item no further removal is possible without violating \code{min_items_per_factor}.
}

\strong{EFA-Boosting Optimization Summary Report.}
When \code{verbose = TRUE}, the function prints:
\itemize{
  \item Initialization header with estimator, rotation, thresholds, and search mode.
  \item Per-iteration diagnostics: RMSEA, SRMR, CFI, df, composite loss.
  \item Structural integrity tables (items per factor).
  \item Thresholded loading matrix.
  \item Removal notifications: Heywood, cross-loading, weakest-loading, global-fit.
  \item Final factor structure and item–factor mapping.
  \item Final model fit indices.
}

\strong{AI conceptual analysis.}
If enabled, the function:
\itemize{
  \item Produces narrative explanations for removed (and optionally retained) items.
  \item Uses item-level statistics: loadings, second loading, ambiguity gap, h², ψ, RMSEA at removal.
  \item Implements retry logic with exponential backoff.
  \item Generates a complete timeline of removal events and reasons.
}

}

\value{
Returns (invisibly) a list containing:
\itemize{
  \item \code{final_structure}: Thresholded loading matrix.
  \item \code{removed_items}: Items removed (in order).
  \item \code{steps_log}: Full elimination log.
  \item \code{iterations}: Number of boosting iterations.
  \item \code{final_rmsea}: Final RMSEA.
  \item \code{bondades_original}: Fit indices from the final model.
  \item \code{inter_factor_correlation}: Final factor correlation matrix.
  \item \code{last_h2}, \code{last_psi}: Final communalities and uniquenesses.
  \item \code{last_flags}: Heywood and near-Heywood flags.
  \item \code{conceptual_analysis}: GPT-based interpretations.
  \item \code{config_used}: All configuration parameters.
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
}
}

\seealso{
\code{\link{print_conceptual_analysis}},
\code{\link{export_conceptual_analysis}}
}

\encoding{UTF-8}
