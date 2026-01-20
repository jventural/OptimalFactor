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
  thresholds = list(...),
  model_config = list(...),
  mod_indices_config = list(...),
  performance = list(...),
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

\item{mod_indices_config}{Modification indices configuration:
  \itemize{
    \item \code{min_mi}: Minimum modification index to consider (default 10)
    \item \code{max_covs_to_add}: Maximum covariances to add (default 10)
    \item \code{only_within_factor}: Only add within-factor covariances (default TRUE)
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
