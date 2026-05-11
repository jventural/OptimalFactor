% Generated manually
% Please edit documentation in R/print_cfa_boosting.R
\name{print_cfa_boosting}
\alias{print_cfa_boosting}
\title{Print CFA-Boosting Results}
\usage{
print_cfa_boosting(
  result,
  show_loadings = TRUE,
  show_correlations = TRUE,
  show_reliability = TRUE,
  show_steps = TRUE,
  show_model = TRUE,
  loading_threshold = 0.30,
  digits = 3
)
}
\arguments{
\item{result}{Output from \code{\link{cfa_boosting}}.}

\item{show_loadings}{Logical. Print standardized factor loadings of the
final model. Default \code{TRUE}.}

\item{show_correlations}{Logical. Print latent factor correlations.
Default \code{TRUE}.}

\item{show_reliability}{Logical. Print omega and alpha by factor.
Default \code{TRUE}.}

\item{show_steps}{Logical. Print the per-iteration log of move / drop /
cov operations and the resulting fit indices. Default \code{TRUE}.}

\item{show_model}{Logical. Print the final lavaan model syntax.
Default \code{TRUE}.}

\item{loading_threshold}{Numeric. Loadings below this absolute value are
hidden in the printed table. Default \code{0.30}.}

\item{digits}{Integer. Number of decimal places. Default \code{3}.}
}
\value{
Invisibly returns \code{result}; called for its side effects on
the console.
}
\description{
Formatted console printer for the object returned by
\code{\link{cfa_boosting}}. Displays fit indices, the iteration trail,
factor loadings (above a threshold), latent correlations, reliability
coefficients and the final lavaan model syntax.
}
\seealso{
\code{\link{cfa_boosting}}, \code{\link{export_cfa_boosting}}
}
