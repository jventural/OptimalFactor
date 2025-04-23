\name{print_efa_results}
\alias{print_efa_results}
\title{Print Results from AI‐Assisted EFA Refinement}
\description{
Prints a structured summary of the key outputs from \code{optimal_efa_with_ai}, including the removed items, final RMSEA, iteration count, final factor structure, step‐by‐step log, and final fit indices.
}
\usage{
print_efa_results(res)
}
\arguments{
  \item{res}{A \code{list} returned by \code{optimal_efa_with_ai}, containing at least the components
    \code{removed_items}, \code{final_rmsea}, \code{iterations},
    \code{final_structure}, \code{steps_log}, and \code{bondades_original}.}
}
\details{
The function prints to the console in the following order:
\enumerate{
  \item A “Summary” block with:
    \itemize{
      \item \code{removed_items}: the items removed during refinement.
      \item \code{final_rmsea}: the scaled RMSEA after the last iteration.
      \item \code{iterations}: total number of iterations performed.
    }
  \item “Factor Structure” showing the final loadings/data frame.
  \item “Step Log” displaying the data frame of each removal step.
  \item “Final Fit Indices” printing the \code{bondades_original} object
    as returned by \code{PsyMetricTools::EFA_modern}.
}
}
\value{
 invisibly returns \code{NULL}. Used for its printing side effect.
}
\examples{
\dontrun{
# Assume you have run:
# res_efa <- optimal_efa_with_ai(...)

# Then print the results:
print_efa_results(res_efa)
}}
\author{Dr. José Ventura‐León}
\keyword{print}
