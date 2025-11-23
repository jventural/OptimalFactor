\name{report_efa_results}
\alias{report_efa_results}
\title{Console Report for EFA Optimization Results}

\description{
Prints a compact, Spanish-language console report summarizing the iterative optimization performed by \code{\link{efa_boosting}}.
The report includes a process summary table, three independent ASCII evolution plots (RMSEA, SRMR, CFI), a stepwise removal log, the final thresholded loading structure, and common fit indices with rule-of-thumb interpretations.
Designed for quick inspection in interactive sessions; returns \code{invisible(NULL)}.
}

\usage{
report_efa_results(res, show_plot = TRUE)
}

\arguments{
\item{\code{res}}{A result list as returned by \code{\link{efa_boosting}}, expected to contain:
\itemize{
  \item \code{$final_structure}: data frame of thresholded factor loadings and item labels.
  \item \code{$removed_items}: character vector of removed items in order.
  \item \code{$steps_log}: data frame with columns \code{step}, \code{removed_item}, \code{reason}, and—if available—\code{rmsea}, \code{srmr}, \code{cfi}.
  \item \code{$final_rmsea}: final scaled RMSEA.
  \item \code{$iterations}: number of iterations executed.
  \item \code{$bondades_original}: fit indices (columns: \code{chisq.scaled}, \code{df.scaled}, \code{rmsea.scaled}, \code{cfi.scaled}, \code{tli.scaled}, \code{srmr}).
  \item \code{$inter_factor_correlation}: optional factor correlation matrix.
  \item \code{$interfactor_check}: optional list describing correlation-threshold checks.
}}
\item{\code{show_plot}}{Logical; if \code{TRUE}, prints three ASCII evolution bars (RMSEA, SRMR, CFI).}
}

\details{
\strong{Headers and language.}
All headings and labels are printed in Spanish (e.g., \dQuote{RESUMEN DEL PROCESO}, \dQuote{EVOLUCIÓN DEL RMSEA},
\dQuote{EVOLUCIÓN DEL SRMR}, \dQuote{EVOLUCIÓN DEL CFI}, \dQuote{ÍNDICES DE AJUSTE DEL MODELO FINAL}).

\strong{Process summary.}
The function computes and prints:
\itemize{
  \item Initial item count (\code{n_initial}) and final count (\code{n_final}).
  \item Percent retained.
  \item Initial and final RMSEA.
  \item Percent reduction in RMSEA.
  \item Iterations executed.
}

These appear in a compact two-column table.

\strong{ASCII evolution plots (RMSEA, SRMR, CFI).}
When \code{show_plot = TRUE}, the function prints three separate bar-style ASCII diagnostics:

\itemize{
  \item \emph{RMSEA evolution:} bar length increases with RMSEA, scaled to the reference range 0.05--0.13.
  \item \emph{SRMR evolution:} bar length increases with SRMR, scaled to 0.03--0.10.
  \item \emph{CFI evolution:} bar length increases with CFI \emph{(higher = better)}, scaled linearly between 0.70 and 1.00.
}

Each block includes an ASCII axis guide:
\itemize{
  \item RMSEA: ticks at 0.05, 0.08, 0.10, 0.13.
  \item SRMR: ticks at 0.03, 0.05, 0.08, 0.10.
  \item CFI: ticks at 0.70, 0.80, 0.90, 0.95, 1.00.
}

Bars are drawn using \verb{█} and capped at a configurable length (default: 20 characters).

\strong{Item removal log.}
The stepwise elimination table mirrors \code{steps_log}, formatting available fit indices (RMSEA, SRMR, CFI) to three decimals.

\strong{Final structure.}
Prints the thresholded loading matrix produced by \code{\link{efa_boosting}} (loadings < 0.30 appear as 0).

\strong{Final fit indices and qualitative labels.}
Extracts the last row of \code{bondades_original} and prints:
\itemize{
  \item Scaled \eqn{\chi^2}, scaled df, RMSEA, CFI, TLI, and SRMR.
  \item Quick qualitative labels:
    \itemize{
      \item RMSEA: \code{<= .05 = Excelente}, \code{<= .08 = Bueno}, \code{<= .10 = Mediocre}, else \code{Pobre}.
      \item CFI/TLI: \code{>= .95 = Excelente}, \code{>= .90 = Bueno}, else \code{Pobre}.
      \item SRMR: \code{<= .08 = Bueno}, else \code{Pobre}.
    }
}
These labels are intended for rapid screening and should be interpreted in context.

\strong{Unicode.}
The report uses UTF-8 box characters (\verb{█}) and accented text. If your console lacks UTF-8 support, glyphs may degrade.
}

\value{
\code{invisible(NULL)}. This function prints to the console and does not return structured output.
}

\examples{
\dontrun{
set.seed(123)
X <- as.data.frame(matrix(rnorm(300 * 9), ncol = 9))
names(X) <- paste0("DP", 1:9)

res <- efa_boosting(
  data = X,
  name_items = "DP",
  item_range = c(1, 9),
  n_factors = 3,
  thresholds = list(
    rmsea = 0.08, loading = 0.30, min_items_per_factor = 3,
    heywood_tol = 1e-6, near_heywood = 0.015
  ),
  model_config = list(estimator = "WLSMV", rotation = "oblimin")
)

report_efa_results(res, show_plot = TRUE)
}
}

\seealso{
\code{\link{efa_boosting}},
\code{\link{print_conceptual_analysis}},
\code{\link{export_conceptual_analysis}}
}
\encoding{UTF-8}
