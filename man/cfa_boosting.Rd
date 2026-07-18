\name{cfa_boosting}
\alias{cfa_boosting}
\title{CFA-Boosting Optimization}

\description{
Performs iterative optimization of Confirmatory Factor Analysis (CFA) models
using modification indices to improve model fit. The algorithm automatically
adds error covariances based on modification indices while respecting
configurable constraints.
}

\usage{
cfa_boosting(
  data,
  model,
  n_sample = NULL,
  thresholds = list(loading = 0.3, min_items_per_factor = 3,
    rmsea_target = 0.08, cfi_target = 0.95, srmr_target = 0.08),
  model_config = list(estimator = "WLSMV", ordered = TRUE),
  mod_indices_config = list(max_covs_to_add = 10, only_within_factor = TRUE,
    delta = 0.1, power_threshold = 0.75, alpha = 0.05),
  performance = list(max_iterations = 30, timeout_cfa = 60),
  verbose = TRUE
)
}

\arguments{
\item{data}{Data frame containing the observed variables.}

\item{model}{Character string specifying the CFA model in lavaan syntax.}

\item{n_sample}{Sample size. If NULL, auto-detected from data.}

\item{thresholds}{List of fit thresholds:
  \itemize{
    \item \code{loading}: Minimum acceptable loading (default 0.30)
    \item \code{min_items_per_factor}: Minimum items per factor (default 3)
    \item \code{rmsea_target}: Target RMSEA value (default 0.08)
    \item \code{cfi_target}: Target CFI value (default 0.95)
    \item \code{srmr_target}: Target SRMR value (default 0.08)
  }
}

\item{model_config}{Model configuration:
  \itemize{
    \item \code{estimator}: Estimation method (default "WLSMV")
    \item \code{ordered}: Whether variables are ordered (default TRUE)
  }
}

\item{mod_indices_config}{Modification indices configuration using the
  Saris, Satorra & van der Veld (2009) framework (MI + EPC + Power):
  \itemize{
    \item \code{max_covs_to_add}: Maximum covariances to evaluate per iteration (default 10)
    \item \code{only_within_factor}: Only consider within-factor covariances (default TRUE)
    \item \code{delta}: Minimum misspecification size to detect (default 0.10)
    \item \code{power_threshold}: Threshold for high/low power classification (default 0.75)
    \item \code{alpha}: Significance level for the MI test (default 0.05)
  }
}

\item{performance}{Performance settings:
  \itemize{
    \item \code{max_iterations}: Maximum optimization iterations (default 30)
    \item \code{timeout_cfa}: Timeout per CFA run in seconds (default 60)
  }
}

\item{verbose}{Logical. Print progress information.}
}

\value{
A list containing:
\itemize{
  \item \code{final_model}: The optimized lavaan CFA model object
  \item \code{fit_indices}: Final fit indices
  \item \code{added_covariances}: List of error covariances added
  \item \code{iterations}: Number of iterations performed
  \item \code{history}: History of fit indices across iterations
}
}

\examples{
\dontrun{
# Define CFA model
model <- "
  F1 =~ item1 + item2 + item3 + item4
  F2 =~ item5 + item6 + item7 + item8
"

result <- cfa_boosting(
  data = my_data,
  model = model,
  verbose = TRUE
)
}
}

\seealso{
\code{\link{efa_boosting}}, \code{\link{print_cfa_boosting}}
}

\encoding{UTF-8}
