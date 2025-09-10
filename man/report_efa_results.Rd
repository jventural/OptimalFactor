\name{report_efa_results}
\alias{report_efa_results}
\title{Console Report for EFA Optimization Results}

\description{
Prints a compact, Spanish-language console report summarizing the iterative optimization performed by \code{\link{efa_optimizer}}.
The report includes a process summary table, a simple ASCII bar plot of RMSEA evolution, a stepwise removal log, the final (thresholded) loading structure, and common fit indices with rule-of-thumb interpretations.
Designed for quick inspection in interactive sessions; returns \code{invisible(NULL)}.
}

\usage{
report_efa_results(res, show_plot = TRUE)
}

\arguments{
\item{\code{res}}{A result list as returned by \code{\link{efa_optimizer}}, expected to contain:
\itemize{
  \item \code{$final_structure}: data frame of thresholded loadings and item labels.
  \item \code{$removed_items}: character vector of removed items in order.
  \item \code{$steps_log}: data frame with columns \code{step}, \code{removed_item}, \code{reason}, \code{rmsea}.
  \item \code{$final_rmsea}: final scaled RMSEA.
  \item \code{$iterations}: number of iterations executed.
  \item \code{$bondades_original}: fit indices (with columns \code{chisq.scaled}, \code{df.scaled}, \code{rmsea.scaled}, \code{cfi.scaled}, \code{tli.scaled}, \code{srmr}).
}}
\item{\code{show_plot}}{Logical; if \code{TRUE}, prints a simple ASCII bar plot tracing RMSEA across steps.}
}

\details{
\strong{Headers and language.}
All headings and labels are printed in Spanish (e.g., \dQuote{RESUMEN DEL PROCESO}, \dQuote{EVOLUCIÓN DEL RMSEA}, \dQuote{ÍNDICES DE AJUSTE DEL MODELO FINAL}).

\strong{Process summary.}
The report computes:
\itemize{
  \item Initial item count (\code{n\_initial}): retained plus removed.
  \item Final item count (\code{n\_final}) and percent retained.
  \item Initial RMSEA (taken from the first entry of \code{steps\_log$rmsea}) and final RMSEA.
  \item Percent reduction in RMSEA.
  \item Iterations executed.
}
These are displayed in a two-column table.

\strong{RMSEA evolution (ASCII).}
If \code{show_plot = TRUE}, a minimalist bar visualization is printed line-by-line:
\itemize{
  \item Each line shows the step index, RMSEA value, and a bar of \verb{█} characters whose length increases with RMSEA (clamped to 0--20 blocks).
  \item A small axis guide is printed with reference ticks at 0.05, 0.08, 0.10, and 0.13.
}
Note: the first line corresponds to \dQuote{Paso 0} and uses the same initial RMSEA repeated at the start of the sequence for readability.

\strong{Item removal log.}
The stepwise elimination table mirrors \code{res$steps\_log}, formatting RMSEA to three decimals. It shows which item was removed at each step and the reason (e.g., \dQuote{RMSEA improvement}, \dQuote{Cross-loading}, \dQuote{Heywood}, etc.).

\strong{Final structure.}
Prints \code{res$final\_structure} in full, with a note in the header that loads below 0.30 were thresholded to 0 during preparation (as produced by \code{\link{efa_optimizer}}).

\strong{Final fit indices and qualitative interpretation.}
Extracts the last row of \code{res$bondades\_original} and prints:
\itemize{
  \item \eqn{\chi^2} (scaled), df (scaled), RMSEA (scaled), CFI (scaled), TLI (scaled), SRMR.
  \item Rule-of-thumb labels:
    \itemize{
      \item RMSEA: \code{<= .05 = "Excelente"}, \code{<= .08 = "Bueno"}, \code{<= .10 = "Mediocre"}, else \code{"Pobre"}.
      \item CFI/TLI: \code{>= .95 = "Excelente"}, \code{>= .90 = "Bueno"}, else \code{"Pobre"}.
      \item SRMR: \code{<= .08 = "Bueno"}, else \code{"Pobre"}.
    }
}
These qualitative labels are for quick screening and should be interpreted in context.

\strong{Unicode.}
The report uses simple box characters and UTF-8 accents. If your console lacks UTF-8 support, some glyphs may degrade.
}

\value{
\code{invisible(NULL)}. The function is intended for its side effects (console printing).
}

\examples{
\dontrun{
# Simulated minimal example:
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

# Print the Spanish report with ASCII RMSEA evolution:
report_efa_results(res, show_plot = TRUE)
}
}

\seealso{
\code{\link{efa_optimizer}}, \code{\link{print_conceptual_analysis}}, \code{\link{export_conceptual_analysis}}
}
\encoding{UTF-8}
