\name{efa_boosting}
\alias{efa_boosting}
\title{EFA-Boosting Optimization Summary Report}

\description{
Generates and prints a compact Spanish-language console report summarizing the iterative optimization performed by the \strong{EFA-Boosting} algorithm.
The summary includes:
(a) a structured overview of the boosting process (greedy and/or global),
(b) a minimalist ASCII evolution plot of RMSEA or composite loss,
(c) a detailed step-by-step elimination log (Heywood, Near-Heywood, Cross-loading, Composite Fit, Global Fit, etc.),
(d) the final thresholded loading matrix, and
(e) final fit indices with qualitative interpretations.

The report is automatically displayed when \code{verbose = TRUE}.
The function returns a full results list invisibly.
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
\item{\code{data}}{Data frame containing all item responses.}

\item{\code{name_items}}{Prefix shared by item names (e.g., "IT", "DP", "A").}

\item{\code{item_range}}{
Numeric vector defining item numbering (e.g., \code{c(1,12)}).
If \code{NULL}, items are detected automatically based on \code{name_items}.
}

\item{\code{n_factors}}{Number of factors to extract.}

\item{\code{exclude_items}}{
Optional vector of items to pre-exclude before optimization.
}

\item{\code{thresholds}}{
List controlling elimination rules:
\itemize{
  \item RMSEA target,
  \item minimum loading,
  \item minimum items per factor,
  \item Heywood and Near-Heywood tolerances.
}
}

\item{\code{model_config}}{Estimator and rotation (e.g., WLSMV + oblimin).}

\item{\code{performance}}{
Timeout control and maximum candidate evaluations.
}

\item{\code{use_global}}{
Logical; if \code{TRUE}, enables global subset search (k = 1…max\_drop).
}

\item{\code{global_opt}}{
List defining global search behavior:
\itemize{
  \item subset size (k),
  \item maximum combinations,
  \item verbosity,
  \item progress bar.
}
}

\item{\code{fit_config}}{
Configuration for the composite loss index:
\itemize{
  \item RMSEA/SRMR/CFI targets,
  \item margins,
  \item base weights,
  \item small-df corrections,
  \item WLSMV boosting factors,
  \item p-close bonus.
}
}

\item{\code{use_ai_analysis}}{
Enable GPT-based conceptual analysis of removed items.
}

\item{\code{ai_config}}{
Model, API key, language, and domain-specific information for AI analysis.
}

\item{\code{verbose}}{
If \code{TRUE}, prints the full \strong{EFA-Boosting Optimization Summary Report}.
}

}

\details{
\strong{Report overview.}
When \code{verbose = TRUE}, \code{efa_boosting()} prints the full \emph{EFA-Boosting Optimization Summary Report}, consisting of:

\itemize{
  \item \strong{Process summary}: item counts, RMSEA change, loss reduction, iterations, search mode.
  \item \strong{Evolution plot}: ASCII bar graph for RMSEA or composite loss.
  \item \strong{Elimination log}: ordered removal steps with reasons and RMSEA.
  \item \strong{Final loadings}: thresholded structure with cross-loadings removed.
  \item \strong{Fit indices}: RMSEA, SRMR, CFI, TLI, \eqn{\chi^2} with qualitative interpretation.
}

\strong{Evolution plot (ASCII).}
If plotting is enabled:
\itemize{
  \item Each line shows step index, metric value, and a block bar (\verb{█}) of 0–20 units.
  \item Reference axis marks accompany RMSEA (.05, .08, .10) or loss (.1, .2, .3).
}

\strong{Elimination criteria.}
Items may be removed due to:
\itemize{
  \item Heywood or Near-Heywood cases,
  \item cross-loadings violating structural criteria,
  \item failure to meet composite-fit improvement,
  \item global search yielding lower loss.
}

\strong{Final structure.}
Loadings < threshold are set to zero.
Per-factor item lists are summarized.

\strong{Fit interpretation.}
Rule-of-thumb labels:
\itemize{
  \item RMSEA: \code{<= .05 Excelente}, \code{<= .08 Adecuado}, \code{<= .10 Limitado}, else Deficiente
  \item CFI/TLI: \code{>= .95 Excelente}, \code{>= .90 Adecuado}, else Deficiente
  \item SRMR: \code{<= .08 Adecuado}, else Deficiente
}

\strong{Global search notes.}
Reports subset size k, total combinations evaluated, and whether global optimization improved loss.

}

\value{
A list containing:
\itemize{
  \item \code{final_structure}
  \item \code{removed_items}
  \item \code{steps_log}
  \item \code{iterations}
  \item \code{final_rmsea}
  \item \code{bondades_original}
  \item \code{inter_factor_correlation}
  \item \code{last_h2}, \code{last_psi}
  \item \code{conceptual_analysis}
  \item \code{config_used}
}
Returned invisibly.
}

\examples{
\dontrun{
set.seed(123)
X <- as.data.frame(matrix(rnorm(300*12), ncol = 12))
names(X) <- paste0("IT", 1:12)

res <- efa_boosting(
  data = X,
  name_items = "IT",
  item_range = c(1, 12),
  n_factors = 3,
  use_global = TRUE,
  verbose = TRUE
)

# The console automatically prints the
# EFA-Boosting Optimization Summary Report.
}
}

\seealso{
\code{\link{print_conceptual_analysis}},
\code{\link{export_conceptual_analysis}}
}
\encoding{UTF-8}
