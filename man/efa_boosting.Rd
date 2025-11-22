\name{efa_boosting}
\alias{efa_boosting}
\title{EFA-Boosting Optimization}

\description{
Performs an iterative, machine-learning–inspired optimization of Exploratory Factor Analysis (EFA).

The algorithm integrates:
(1) greedy item-by-item elimination,
(2) optional global subset search (multi-item removal),
(3) structural rule enforcement (loadings, Heywood / near-Heywood, cross-loadings, minimum items per factor),
(4) adaptive minimum interfactor correlation checks, and
(5) a composite fit index whose weights adapt dynamically to degrees of freedom \emph{and} sample size (df × N), following recommendations by Kenny, Shi, Savalei, and related literature.

The function handles ordinal data (WLSMV) and includes robust corrections, adaptive weighting rules, and an optional GPT-based conceptual analysis of removed (and optionally retained) items.

When \code{verbose = TRUE}, the function prints a complete
\emph{EFA-Boosting Optimization Summary Report} including:

(a) iteration diagnostics (RMSEA, SRMR, CFI, df, composite loss),
(b) identification and elimination of structural problems,
(c) global-search progress bars and decisions,
(d) thresholded final structure,
(e) interfactor-correlation compliance check, and
(f) AI-based conceptual interpretations.

Returns a complete list of results invisibly.
}

\usage{
efa_boosting(
  data,
  name_items,
  item_range = NULL,
  n_factors = 3,
  n_sample = NULL,
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

\item{\code{data}}{Data frame of item responses.}

\item{\code{name_items}}{
Item prefix (e.g., \code{"IT"} produces IT1, IT2, …). Used for auto-detection.
}

\item{\code{item_range}}{
Integer vector \code{c(start, end)}.
If \code{NULL}, items are auto-detected.
}

\item{\code{n_factors}}{Number of factors.}

\item{\code{n_sample}}{
Sample size.
If \code{NULL}, auto-detected as \code{nrow(data)}.
Used for df × N adaptive weighting of the composite fit index.
}

\item{\code{exclude_items}}{Items excluded before starting optimization.}

\item{\code{thresholds}}{
Rules governing structural decisions:
\itemize{
  \item \code{loading}: Minimum acceptable loading.
  \item \code{min_items_per_factor}: Structural protection rule.
  \item \code{heywood_tol}, \code{near_heywood}: Detection thresholds.
  \item \code{min_interfactor_correlation}: Minimum acceptable factor correlation.
}
}

\item{\code{model_config}}{
EFA estimation configuration:
\itemize{
  \item \code{estimator = "WLSMV"}
  \item \code{rotation  = "oblimin"}
}
}

\item{\code{performance}}{
Performance and timeout settings:
\itemize{
  \item \code{max_candidates_eval}: If set, only this many candidates are evaluated in greedy mode.
  \item \code{timeout_efa}: Timeout per EFA run (requires \code{R.utils}).
  \item \code{timeout_optimization}: Global timeout.
  \item \code{use_timeouts}: Enable/disable timeout protection.
}
}

\item{\code{use_global}}{
Enable global subset search (multi-item removal).
}

\item{\code{global_opt}}{
Configuration for global search:
\itemize{
  \item \code{max_drop}: Maximum subset size \emph{k}.
  \item \code{max_global_combinations}: Hard cap to avoid explosion.
  \item \code{verbose}: Print global-search diagnostics.
  \item \code{progress_bar}: Show a visual progress bar.
}
}

\item{\code{fit_config}}{
Configuration of the adaptive composite fit index:
\itemize{
  \item \code{targets}: Target values for RMSEA, SRMR, CFI.
  \item \code{margins}: Tolerances for each index.
  \item \code{base_weights}: Default RMSEA/SRMR/CFI weights.
  \item \code{critical_weights}: df < 5 and N < 200 (RMSEA nearly useless).
  \item \code{df_low_n_high_weights}: df < 5 and N >= 200.
  \item \code{df_mid_n_low_weights}: df 5–19 and N < 200.
  \item \code{df_mid_n_high_weights}: df 5–19 and N >= 200.
  \item \code{critical_df_cut}, \code{moderate_df_cut}: df boundaries.
  \item \code{small_n_cut}: N-boundary for weighting.
  \item \code{wlsmv_boost}: Multiplicative corrections for WLSMV.
  \item \code{use_pclose_if_available}: Enables p-close bonus.
  \item \code{pclose_bonus}: Amount subtracted from composite loss.
}
}

\item{\code{use_ai_analysis}}{
Enable GPT-based conceptual analysis of removed (and optionally retained) items.
}

\item{\code{ai_config}}{
AI analysis configuration:
\itemize{
  \item \code{api_key}: OpenAI API key.
  \item \code{gpt_model}: Model (e.g., \code{"gpt-4"}, \code{"gpt-3.5-turbo"}).
  \item \code{language}: "spanish" or "english".
  \item \code{analysis_detail}: "brief", "standard", "detailed".
  \item \code{domain_name}, \code{scale_title}, \code{model_name}: Metadata included in prompts.
  \item \code{construct_definition}: Theoretical definition of the latent construct.
  \item \code{item_definitions}: Named list with item wording.
  \item \code{only_removed}: If FALSE, retained items are also analyzed.
}
}

\item{\code{verbose}}{
If TRUE, prints the full diagnostic report, global-search bars, item maps,
interfactor-correlation warnings, and AI progress bars.
}

}

\details{

\strong{1. Optimization strategy.}

The algorithm follows a strict hierarchical rule system:
\itemize{
  \item Remove Heywood items (ψ < –tol or |loading| > 1).
  \item Remove near-Heywood items (ψ ≈ 0).
  \item Resolve cross-loadings by removing items with smallest ambiguity gap.
  \item Enforce minimum items per factor.
  \item If structure is acceptable but RMSEA > target: remove the weakest-loading item.
  \item If \code{use_global = TRUE}: evaluate all subsets up to \code{max_drop} using the composite loss.
}

\strong{2. Adaptive composite loss (df × N).}

Weights for RMSEA / SRMR / CFI adapt dynamically according to:
\itemize{
  \item degrees of freedom (df),
  \item sample size (N),
  \item estimator (WLSMV boosters),
  \item p-close ≥ .05 (bonus).
}

This allows stable decisions even when RMSEA is unreliable (df < 5).

\strong{3. Interfactor correlation rule.}

After each iteration and at finalization, factor correlations are checked:
\itemize{
  \item If any |φ_{ij}| < \code{min_interfactor_correlation}, a warning is printed.
}

\strong{4. Global subset search.}

If enabled:
\itemize{
  \item evaluates all subsets of size 1 … k,
  \item uses safe-combination caps,
  \item shows a progress bar,
  \item accepts subset removals only if composite loss strictly improves.
}

\strong{5. AI conceptual analysis.}

If \code{use_ai_analysis = TRUE}:
\itemize{
  \item removed items receive a narrative justification using
        loadings, ambiguity gaps, h², ψ, RMSEA-at-removal, and algorithmic reason,
  \item retained items can also be evaluated,
  \item exponential-backoff retry logic ensures robustness,
  \item a detailed elimination timeline is integrated into the prompt.
}

}

\value{
A list containing:
\itemize{
  \item \code{final_structure}: Thresholded loading matrix.
  \item \code{removed_items}: Items removed (in chronological order).
  \item \code{steps_log}: Full elimination log with the following columns:
    \itemize{
      \item \code{step}: Iteration number
      \item \code{removed_item}: Name of the removed item
      \item \code{reason}: Reason for removal
      \item \code{rmsea}: RMSEA value at the moment of removal (scaled when WLSMV is used)
      \item \code{srmr}: SRMR value at the moment of removal
      \item \code{cfi}: CFI value at the moment of removal (scaled when WLSMV is used)
    }
  \item \code{iterations}: Number of optimization iterations.
  \item \code{final_rmsea}: Final RMSEA.
  \item \code{bondades_original}: Fit indices from the final model.
  \item \code{inter_factor_correlation}: Final φ matrix.
  \item \code{interfactor_check}: Information about violations.
  \item \code{last_h2}, \code{last_psi}: Final communalities and uniquenesses.
  \item \code{last_flags}: Heywood and near-Heywood indicators.
  \item \code{conceptual_analysis}: GPT-based narrative analyses.
  \item \code{config_used}: Full configuration list.
}
}

\examples{
\dontrun{
X <- replicate(12, rnorm(500))
X <- as.data.frame(X); names(X) <- paste0("IT",1:12)

res <- efa_boosting(
  data = X,
  name_items = "IT",
  n_factors = 3,
  n_sample = 500,
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
