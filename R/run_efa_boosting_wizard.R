#' Launch the OptimalFactor Wizard (v2)
#'
#' @description
#' Opens a guided 7-phase Shiny wizard built on top of \code{efa_boosting()}
#' and \code{cfa_boosting()}, with reliability and external-validity steps.
#' Designed to match the wizard style of the EasyValidation package so
#' users moving between the two feel at home.
#'
#' The 7 phases are: (1) Data, (2) Configuration, (3) Initial diagnostic
#' (parallel analysis + MAP + Kaiser), (4) EFA boosting with post-boost
#' review, (5) optional CFA boosting, (6) reliability (omega + alpha by
#' factor), (7) external validity (Pearson correlations between factor
#' scores and selected numeric covariates).
#'
#' The original studio app remains available via
#' \code{\link{run_efa_boosting}}; this wizard does not replace it.
#'
#' @param launch.browser Logical. If \code{TRUE} (default) the app opens
#'   in the system browser; otherwise the app object is returned for
#'   manual launch.
#'
#' @return Invisibly returns the Shiny app object; primarily called for
#'   side effects.
#'
#' @section Required packages:
#'   shiny, shinydashboard, DT. Optional but recommended: readxl, psych,
#'   lavaan, semTools.
#'
#' @section Quick start:
#' \preformatted{
#'   library(OptimalFactor)
#'   run_efa_boosting_wizard()
#' }
#'
#' @seealso \code{\link{run_efa_boosting}}, \code{\link{efa_boosting}},
#'   \code{\link{cfa_boosting}}, \code{\link{report_efa_results}},
#'   \code{\link{report_cfa_results}}.
#'
#' @export
run_efa_boosting_wizard <- function(launch.browser = TRUE) {
  required <- c("shiny", "shinydashboard", "DT")
  missing <- required[!vapply(required, requireNamespace, logical(1),
                                quietly = TRUE)]
  if (length(missing) > 0) {
    stop(sprintf(
      "The wizard requires these packages: %s.\nInstall with: install.packages(c(%s))",
      paste(missing, collapse = ", "),
      paste(sprintf("'%s'", missing), collapse = ", ")),
      call. = FALSE)
  }

  app_dir <- system.file("shiny-apps", "efa_boosting_wizard",
                           package = "OptimalFactor")
  if (app_dir == "") {
    candidate <- file.path("inst", "shiny-apps", "efa_boosting_wizard", "app.R")
    if (file.exists(candidate)) {
      app_dir <- dirname(candidate)
    } else {
      stop("Wizard app directory not found. Reinstall OptimalFactor.",
           call. = FALSE)
    }
  }
  shiny::runApp(app_dir, launch.browser = launch.browser)
}
