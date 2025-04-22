\name{print_conceptual_analysis}
\alias{print_conceptual_analysis}
\title{Print Conceptual Analysis of EFA Items}
\description{
Prints the AI‐generated conceptual justifications for items removed or retained during an EFA refinement, as produced by \code{optimal_efa_with_ai}. If no conceptual analysis is available, informs the user accordingly.
}
\usage{
print_conceptual_analysis(resultado)
}
\arguments{
  \item{resultado}{A list returned by \code{optimal_efa_with_ai}, expected to contain a \code{conceptual_analysis} element with sublists \code{removed} and \code{kept}.}
}
\details{
This function inspects \code{resultado$conceptual_analysis}.
If it is \code{NULL}, it prints:
\preformatted{No hay análisis conceptual disponible.}
Otherwise, it prints two sections:

\enumerate{
  \item \strong{Análisis conceptual de ítems eliminados:}
        Under this heading, each removed item name is printed followed by its AI‐generated justification.
  \item \strong{Análisis conceptual de ítems conservados:}
        Under this heading, each retained item name is printed followed by its AI‐generated justification.
}

All output is sent to the console, and the function invisibly returns \code{NULL}.
}
\value{
Invisibly returns \code{NULL} after printing to the console.
}
\examples{
\dontrun{
# Suppose 'res_efa' is obtained from optimal_efa_with_ai(..., analyze_removed = TRUE)
print_conceptual_analysis(res_efa)
}
}
