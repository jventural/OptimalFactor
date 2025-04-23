\name{print_cfa_results}
\alias{print_cfa_results}
\title{Print Results from AI‐Assisted CFA Refinement}
\description{
Prints a structured report of key outputs from \code{optimal_cfa_with_ai} applied to a Confirmatory Factor Analysis (CFA), including available components, modification log, final model syntax, removed items, fit indices, standardized loadings, and reliability.
}
\usage{
print_cfa_results(res)
}
\arguments{
  \item{res}{A \code{list} returned by \code{optimal_cfa_with_ai} (CFA version), containing at minimum the components:
    \code{log}, \code{final_model}, \code{removed_items}, \code{final_rmsea},
    \code{final_cfi}, and \code{final_fit}.}
}
\details{
The function prints the following sections to the console:
\enumerate{
  \item \strong{Available components}: names of all elements in \code{res}.
  \item \strong{Modification log}: data frame \code{res\$log} with each removal step.
  \item \strong{Final model}: the CFA model syntax stored in \code{res\$final_model}.
  \item \strong{Removed items}: list of items removed during refinement.
  \item \strong{Final fit measures}: \code{res\$final_rmsea} and \code{res\$final_cfi}.
  \item \strong{Additional fit indices}: retrieved via
    \code{lavaan::fitMeasures(res$final_fit, c("chisq.scaled", "df.scaled", "srmr", "wrmr", "cfi.scaled", "tli.scaled", "rmsea.scaled"))},
    including the scaled chi-square statistic, scaled degrees of freedom, SRMR, WRMR, scaled CFI, scaled TLI, and scaled RMSEA.
  \item \strong{Standardized loadings}: extracted from
    \code{lavaan::standardizedsolution(res$final_fit)} and filtered to measurement paths (\code{op == "=~"}), showing the final factor loadings for each indicator.
  \item \strong{Reliability}: composite reliability estimates computed with
    \code{semTools::compRelSEM(res$final_fit, tau.eq = FALSE, ord.scale = TRUE)},
    indicating the internal consistency of each factor under an ordinal measurement model.
}
}
\value{
Invisibly returns \code{NULL}. The primary purpose is to print results.
}
\examples{
\dontrun{
# Assume you have run a CFA with optimal_cfa_with_ai:
# res_cfa <- optimal_cfa_with_ai(...)

# Print the organized results:
print_cfa_results(res_cfa)
}}
\author{Dr. José Ventura‐León}
\keyword{print}
