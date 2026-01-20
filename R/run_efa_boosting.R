#' Launch EFA-Boosting Studio - EFA-Boosting Interactive Analyzer
#'
#' @description
#' Opens EFA-Boosting Studio, an interactive Shiny web application for
#' performing Exploratory Factor Analysis (EFA) optimization using the
#' EFA-Boosting algorithm with real-time console output.
#'
#' @param launch.browser Logical. If TRUE (default), opens the app in the
#'   default web browser. If FALSE, returns the app URL for manual navigation.
#'
#' @return Invisibly returns the Shiny app object. The function is primarily
#'   called for its side effect of launching the interactive application.
#'
#' @details
#' EFA-Boosting Studio provides:
#' \itemize{
#'   \item Modern UI with Bootstrap 5 theme
#'   \item Real-time console with auto-scroll (100ms refresh)
#'   \item Full EFA-Boosting algorithm with adaptive fit indices
#'   \item Global search option for multi-item removal
#'   \item Interactive results visualization
#'   \item CSV/Excel data import
#'   \item Export capabilities for final structure and logs
#'   \item Optional AI-powered item analysis via GPT
#' }
#'
#' @section Quick Start:
#' \preformatted{
#' library(OptimalFactor)
#' run_efa_boosting()
#' }
#'
#' @section Required Packages:
#' The application requires these packages (installed automatically if missing):
#' \itemize{
#'   \item shiny, bslib (UI framework)
#'   \item DT (interactive tables)
#'   \item readxl, readr (data import)
#'   \item ggplot2 (visualizations)
#'   \item future, promises (async execution)
#' }
#'
#' @examples
#' \dontrun{
#' # Launch EFA-Boosting Studio
#' library(OptimalFactor)
#' run_efa_boosting()
#'
#' # Or simply:
#' OptimalFactor::run_efa_boosting()
#' }
#'
#' @seealso \code{\link{efa_boosting}}
#'
#' @export
run_efa_boosting <- function(launch.browser = TRUE) {
  # Check for required packages
  required_pkgs <- c("shiny", "bslib", "DT", "readxl", "readr", "ggplot2", "future", "promises")
  missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]

  if (length(missing_pkgs) > 0) {
    stop(
      "The following packages are required but not installed: ",
      paste(missing_pkgs, collapse = ", "),
      "\nInstall them with: install.packages(c('",
      paste(missing_pkgs, collapse = "', '"), "'))",
      call. = FALSE
    )
  }

  # Find the app directory
  app_dir <- system.file("shiny-apps", "efa_boosting", package = "OptimalFactor")

  if (app_dir == "") {
    stop(
      "Could not find the EFA-Boosting Shiny application. ",
      "Please reinstall the OptimalFactor package.",
      call. = FALSE
    )
  }

  # Launch the app
  shiny::runApp(app_dir, launch.browser = launch.browser)
}
