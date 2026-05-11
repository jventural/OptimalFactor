#' Launch the OptimalFactor Wizard (v2)
#'
#' @description
#' Opens a guided 5-phase Shiny wizard built on top of \code{\link{efa_boosting}}
#' with reliability, external validity and convergent / discriminant
#' validity steps. Coexists with the original studio app
#' \code{\link{run_efa_boosting}} — neither replaces the other.
#'
#' The five phases are:
#' \enumerate{
#'   \item \strong{Data} — load a CSV / Excel / RDS and select the items
#'         of the scale under analysis.
#'   \item \strong{Parallel diagnostic} — multi-method consensus (Kaiser,
#'         Horn's parallel analysis, Velicer's MAP, BIC) suggesting the
#'         number of factors. The user can accept the recommendation or
#'         override it with a theoretical value.
#'   \item \strong{EFA boosting} — runs \code{\link{efa_boosting}} with
#'         the chosen \code{k}, captures the verbose trail (visible in
#'         the \emph{Trace} tab) and, when an OpenAI key is provided,
#'         appends a conceptual analysis of every dropped item.
#'   \item \strong{Reliability} — McDonald's omega (categorical, via
#'         \code{semTools::compRelSEM(ord.scale = TRUE)}) plus Cronbach's
#'         alpha per factor, alongside CFI/TLI/RMSEA/SRMR of the
#'         EFA-derived CFA model.
#'   \item \strong{Validity} — Pearson correlations between factor scores
#'         and (a) standalone criterion variables and (b) comparison
#'         instruments, with automatic detection of multidimensional
#'         comparators (sub-prefixes like \code{DERS_AC1}, \code{DERS_OB1})
#'         and AI-assisted convergent/discriminant classification.
#' }
#'
#' On top of the steps, the wizard offers:
#' \itemize{
#'   \item \strong{Autopilot mode} — the AI walks every step on its own
#'         after a user-configurable read delay, with explicit
#'         \emph{Pause}, \emph{Resume} and \emph{Back} buttons.
#'   \item \strong{Downloadable session log (.txt)} — full audit trail.
#'   \item \strong{Downloadable manuscript (.docx)} — APA-7 "Análisis de
#'         datos" + "Resultados" sections drafted by the AI; tables are
#'         inserted from real session data (the AI never invents
#'         numbers).
#'   \item \strong{IA Chat tab} — free-form chat with full context of the
#'         fitted model, with Markdown rendering of the replies.
#' }
#'
#' @param launch.browser Logical. If \code{TRUE} (default) the app opens
#'   in the system browser; otherwise the app object is returned for
#'   manual launch.
#'
#' @return Invisibly returns the Shiny app object; primarily called for
#'   side effects.
#'
#' @section Required packages:
#'   \code{shiny}, \code{bslib}, \code{DT}. Strongly recommended:
#'   \code{psych}, \code{lavaan}, \code{semTools}, \code{ggplot2},
#'   \code{commonmark}. For AI features: \code{httr}, \code{jsonlite}.
#'   For Word manuscript export: \code{officer}, \code{flextable}. For
#'   autopilot timing: \code{later}.
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
#' @examples
#' \dontrun{
#' # Interactive launch (opens the browser).
#' library(OptimalFactor)
#' run_efa_boosting_wizard()
#'
#' # Headless / programmatic launch — returns the app object for
#' # embedding in a hosting environment.
#' app <- run_efa_boosting_wizard(launch.browser = FALSE)
#' }
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
