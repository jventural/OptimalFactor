#' Bifactor Statistical Indices
#'
#' @description
#' Computes the auxiliary bifactor statistical indices from a fitted bifactor
#' \code{lavaan} model: ECV (explained common variance), PUC (percent of
#' uncontaminated correlations), omega, omega hierarchical (\eqn{\omega_H}),
#' omega hierarchical subscale (\eqn{\omega_{HS}}), the H construct-replicability
#' index, and the item-level ECV (I-ECV). These indices help decide whether a
#' multidimensional scale can be treated as essentially unidimensional and
#' scored with a single total score (Rodriguez, Reise & Haviland, 2016).
#'
#' @details
#' The model must be a bifactor structure with one general factor loading on all
#' items plus orthogonal specific factors. Standardized loadings are used. With
#' general loadings \eqn{g_i}, specific loadings \eqn{s_i} and errors
#' \eqn{e_i = 1 - g_i^2 - s_i^2}:
#' \itemize{
#'   \item \code{ECV} = \eqn{\sum g_i^2 / \sum (g_i^2 + s_i^2)}.
#'   \item \code{omega_H} = \eqn{(\sum g_i)^2 / Var_{total}}, and \code{omega}
#'         adds the grouped specific variance to the numerator.
#'   \item \code{omega_HS} (per specific factor) = \eqn{(\sum s_i)^2 / Var_{sub}}.
#'   \item \code{H} = \eqn{1/(1 + 1/\sum (\lambda_i^2/(1-\lambda_i^2)))}.
#'   \item \code{PUC} = proportion of item correlations that are between items of
#'         different specific factors.
#'   \item \code{IECV_i} = \eqn{g_i^2 / (g_i^2 + s_i^2)} (item purity toward G).
#' }
#'
#' @param fit A fitted bifactor \code{lavaan} object (general factor + orthogonal
#'   specific factors).
#' @param general Name of the general factor. If \code{NULL} (default) it is
#'   auto-detected as the latent variable that loads on all items.
#'
#' @return A list with: \code{overall} (a one-row data frame with ECV, PUC,
#'   omega, omega_H and H_general), \code{by_factor} (per specific factor: ECV,
#'   omega_S, omega_HS, H) and \code{by_item} (general/specific loadings and
#'   I-ECV). Printed rounded to three decimals.
#'
#' @section References:
#' Rodriguez, A., Reise, S. P., & Haviland, M. G. (2016). Evaluating bifactor
#'   models: Calculating and interpreting statistical indices. \emph{Psychological
#'   Methods, 21}(2), 137--150.
#'
#' @examples
#' \dontrun{
#'   library(lavaan)
#'   mod <- 'G  =~ x1+x2+x3+x4+x5+x6
#'           S1 =~ x1+x2+x3
#'           S2 =~ x4+x5+x6
#'           G ~~ 0*S1 + 0*S2
#'           S1 ~~ 0*S2'
#'   fit <- cfa(mod, data = mydata, ordered = TRUE, estimator = "WLSMV", std.lv = TRUE)
#'   bifactor_indices(fit)
#' }
#' @export
bifactor_indices <- function(fit, general = NULL) {
  if (!requireNamespace("lavaan", quietly = TRUE)) stop("Package 'lavaan' is required.")
  std <- lavaan::standardizedSolution(fit)
  L   <- std[std$op == "=~", c("lhs", "rhs", "est.std")]
  items <- unique(L$rhs)
  facs  <- unique(L$lhs)
  # detectar factor general = el que carga en TODOS los items
  if (is.null(general)) {
    covers <- vapply(facs, function(f) all(items %in% L$rhs[L$lhs == f]), logical(1))
    if (!any(covers)) stop("No se detecto un factor general (que cargue en todos los items). Pase 'general'.")
    general <- facs[which(covers)[1]]
  }
  spec_facs <- setdiff(facs, general)
  g <- setNames(rep(0, length(items)), items)
  s <- g; grp <- setNames(rep(NA_character_, length(items)), items)
  for (i in seq_len(nrow(L))) {
    it <- L$rhs[i]
    if (L$lhs[i] == general) g[it] <- L$est.std[i] else { s[it] <- L$est.std[i]; grp[it] <- L$lhs[i] }
  }
  ei <- 1 - g^2 - s^2
  # ECV
  denom  <- sum(g^2 + s^2)
  ECV    <- sum(g^2) / denom
  ECV_sp <- sapply(spec_facs, function(f) sum(s[grp == f]^2) / denom)
  IECV   <- g^2 / (g^2 + s^2)
  # omegas
  SSg     <- sum(g)^2
  grpsum2 <- sum(sapply(spec_facs, function(f) sum(s[grp == f])^2))
  Vtot    <- SSg + grpsum2 + sum(ei)
  omega   <- (SSg + grpsum2) / Vtot
  omegaH  <- SSg / Vtot
  bysub <- t(sapply(spec_facs, function(f) {
    idx <- which(grp == f); sg <- sum(g[idx]); ss <- sum(s[idx]); ve <- sum(ei[idx])
    Vs  <- sg^2 + ss^2 + ve
    c(ECV = sum(s[idx]^2) / denom, omega_S = (sg^2 + ss^2) / Vs, omega_HS = ss^2 / Vs)
  }))
  Hf <- function(l) { l <- l[!is.na(l)]; 1 / (1 + 1 / sum(l^2 / (1 - l^2))) }
  H_gen <- Hf(g)
  H_sp  <- sapply(spec_facs, function(f) Hf(s[grp == f]))
  # PUC
  p <- length(items); totc <- p * (p - 1) / 2
  cont <- sum(sapply(spec_facs, function(f) { n <- sum(grp == f); n * (n - 1) / 2 }))
  PUC <- 1 - cont / totc

  overall <- data.frame(ECV = ECV, PUC = PUC, omega = omega, omega_H = omegaH, H_general = H_gen)
  by_factor <- data.frame(Factor = spec_facs, ECV = ECV_sp,
                          omega_S = bysub[, "omega_S"], omega_HS = bysub[, "omega_HS"],
                          H = H_sp, row.names = NULL)
  by_item <- data.frame(Item = items, Factor = grp, General = g, Specific = s,
                        I_ECV = IECV, row.names = NULL)
  out <- list(overall = overall, by_factor = by_factor, by_item = by_item, general = general)
  cat("Bifactor statistical indices (general factor:", general, ")\n")
  cat("Overall:\n"); print(round(overall, 3), row.names = FALSE)
  cat("\nBy specific factor:\n"); print(cbind(Factor = by_factor$Factor, round(by_factor[-1], 3)), row.names = FALSE)
  invisible(out)
}
