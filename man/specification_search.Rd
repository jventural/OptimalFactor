\name{specification_search}
\alias{specification_search}
\alias{print.specification_search}
\title{Heuristic Specification Search for CFA Models (deprecated)}

\description{
\strong{[Deprecated]} This fit-only search has been superseded by
\code{\link{specification_search_theory}}, which adds a theory-congruence
term to the loss and avoids drifting toward models that fit well but break
the intended factor structure. \code{specification_search()} is kept for
backward compatibility and emits a deprecation warning on every call; new
code should use \code{specification_search_theory()} (set
\code{theory_weight = 0} there to reproduce the fit-only behaviour).

Performs a heuristic specification search over CFA models in the spirit of
MacCallum (1986). For each seed configuration (with a fixed number of
factors), the algorithm runs a hill-climbing loop that evaluates three local
operations at each step: \strong{move} an item between factors, \strong{drop}
an item, and \strong{cov} (add a residual covariance suggested by
modification indices). Optionally, a bifactor variant is fitted for every
seed with k >= 2. The function returns every configuration evaluated, the
subset that meets the user-supplied fit targets, and the best model under
a composite loss.
}

\usage{
specification_search(
  data,
  items,
  seeds = NULL,
  max_factors = 4,
  min_items_factor = 2,
  cfi_target = 0.95,
  rmsea_target = 0.08,
  srmr_target = 0.08,
  max_iter_per_config = 40,
  max_covs = 5,
  max_items_removed = 6,
  try_bifactor = TRUE,
  operations = c("move", "drop", "cov"),
  estimator = "WLSMV",
  ordered = TRUE,
  std.lv = TRUE,
  mi_min = 10,
  mi_top = 3,
  loss_weights = c(rmsea = 0.5, cfi = 0.3, srmr = 0.2),
  patience = 8,
  early_stop_after_meet = 3,
  n_cores = 1,
  verbose = TRUE
)

\method{print}{specification_search}(x, top = 10, \dots)
}

\arguments{
\item{data}{Data frame with the observed item responses.}
\item{items}{Character vector with the names of the items to be searched over.}
\item{seeds}{Named list of initial configurations. Names are the number of
  factors as character (e.g. \code{"3"}). Each element is a list of seeds,
  where every seed is a named list \code{list(F1 = c("it1","it2"), ...)}.
  If \code{NULL}, default block-partition seeds are generated for k = 1 to
  \code{max_factors}.}
\item{max_factors}{Maximum number of factors when \code{seeds = NULL}.}
\item{min_items_factor}{Minimum items per factor preserved during move/drop.}
\item{cfi_target, rmsea_target, srmr_target}{Fit thresholds. A model is flagged
  as successful if CFI >= \code{cfi_target} and RMSEA <= \code{rmsea_target}.}
\item{max_iter_per_config}{Maximum hill-climbing iterations per seed.}
\item{max_covs}{Maximum residual covariances added per configuration.}
\item{max_items_removed}{Maximum items removed across all factors per configuration.}
\item{try_bifactor}{If \code{TRUE}, fit bifactor variant for every k >= 2 seed.}
\item{operations}{Subset of \code{c("move","drop","cov")} controlling enabled ops.}
\item{estimator}{Estimator passed to \code{lavaan::cfa}.}
\item{ordered}{If \code{TRUE}, items are treated as ordered.}
\item{std.lv}{If \code{TRUE}, latent variances are fixed to one.}
\item{mi_min}{Minimum modification-index value considered for covariances.}
\item{mi_top}{Maximum number of top-MI candidates examined at each step.}
\item{loss_weights}{Named numeric vector with weights for the composite loss.}
\item{patience}{Iterations without improvement that stop the hill climb.}
\item{early_stop_after_meet}{Extra iterations allowed after targets are met.}
\item{n_cores}{Number of CPU cores for parallel evaluation of the candidate
models within each greedy iteration (they are independent). Default 1
(sequential). Values > 1 use a PSOCK cluster (works on Windows, where
\code{fork} is unavailable); results are identical to the sequential run,
only faster. A practical choice is \code{parallel::detectCores() - 1}.}
\item{verbose}{Print progress and the MacCallum warning.}
\item{x}{An object returned by \code{specification_search}.}
\item{top}{Number of rows displayed by the print method.}
\item{\dots}{Ignored.}
}

\details{
\strong{Warning (MacCallum, 1986).} Specification search capitalizes on
chance: the more configurations explored, the more likely the resulting
model is sample-specific. If you publish results obtained with this
function you should (a) report the procedure transparently as
exploratory, (b) cross-validate the chosen model with an independent
sample or via bootstrap, and (c) justify each accepted modification on
substantive theoretical grounds. The function prints this warning on
every call unless \code{verbose = FALSE}.
}

\value{
An object of class \code{specification_search}, a list with:
\itemize{
  \item \code{table}: data frame, one row per configuration, ordered by loss.
  \item \code{successful}: subset of \code{table} that meets the targets.
  \item \code{best}: the best result (\code{fit}, \code{syntax}, \code{factors},
        \code{covs}, \code{indices}, \code{bifactor}, \code{meets}, \code{loss}).
  \item \code{results}: every configuration evaluated.
  \item \code{call}: the matched call.
}
}

\section{References}{
MacCallum, R. C. (1986). Specification searches in covariance structure
modeling. \emph{Psychological Bulletin, 100}(1), 107--120.

Saris, W. E., Satorra, A., & van der Veld, W. M. (2009). Testing structural
equation models or detection of misspecifications? \emph{Structural Equation
Modeling, 16}(4), 561--582.
}

\examples{
\dontrun{
data(Data_Personality)
items <- paste0("PPTQ", 1:15)

res <- specification_search(
  data         = Data_Personality,
  items        = items,
  max_factors  = 3,
  estimator    = "MLR",
  ordered      = FALSE,
  try_bifactor = TRUE,
  verbose      = TRUE
)

print(res, top = 8)
res$successful
summary(res$best$fit)
}
}

\seealso{
\code{\link{specification_search_theory}} (recommended replacement),
\code{\link{cfa_boosting}}
}
\keyword{internal}

\encoding{UTF-8}
