\name{export_conceptual_analysis}
\alias{export_conceptual_analysis}
\title{Export LLM Conceptual Analyses to a UTF-8 Text File}

\description{
Writes the formatted conceptual analyses produced by \code{\link{efa_optimizer}} to a UTF-8 encoded text file.
Internally calls \code{\link{print_conceptual_analysis}} with \code{width = 100} and \code{show_stats = TRUE}, capturing its console output via \code{sink()}.
After writing, prints a short confirmation to the console and invisibly returns the output file path.
}

\usage{
export_conceptual_analysis(resultado, file = "conceptual_analysis.txt")
}

\arguments{
\item{\code{resultado}}{A result list returned by \code{\link{efa_optimizer}}; must contain \code{$conceptual_analysis} and \code{$config_used} (see \code{\link{print_conceptual_analysis}}).}
\item{\code{file}}{Character path to the output text file (UTF-8). If it exists, it will be overwritten.}
}

\details{
\strong{What gets written.}
The function captures the full console rendering of \code{\link{print_conceptual_analysis}} (headings, section dividers, emojis, warnings, and technical stats) and writes it to \code{file} using UTF-8 encoding.
Because \code{print_conceptual_analysis} is bilingual, headings will appear in Spanish when \code{resultado$config_used$ai_config$language} is \code{"spanish"} (or \code{"español"}); otherwise, English is used.

\strong{Formatting choices.}
Text is wrapped at 100 characters per line and per-item technical statistics are always included (primary loading, \eqn{h^2}, RMSEA at removal, and removal reason) when available.

\strong{Encoding and Unicode.}
Output is forced to UTF-8. Unicode symbols used by \code{print_conceptual_analysis} (e.g., \verb{📊}, \verb{⚠️}, \verb{❌}) will be preserved; ensure your editor is set to UTF-8 to avoid mojibake.

\strong{Side effects and limitations.}
The function uses \code{sink(file, encoding = "UTF-8")} followed by \code{sink()} to restore the console.
If an error interrupts execution before the second \code{sink()}, the connection may remain diverted; call \code{sink()} manually to reset if needed.
}

\value{
Invisibly returns the \code{file} path (character scalar). The console also receives a brief confirmation message of the form \dQuote{Análisis exportado a: <file>}.
}

\examples{
\dontrun{
# Example mock object (see print_conceptual_analysis() examples for structure)
resultado <- list(
  conceptual_analysis = list(
    removed = list(DP1 = "Desde el punto de vista psicométrico..."),
    kept = list(DP2 = "Psychometrically, the item shows..."),
    item_stats = list(DP1 = list(loading = 0.22, h2 = 0.11, rmsea_at_removal = 0.095, reason = "Cross-loading"))
  ),
  config_used = list(
    ai_config = list(
      language = "spanish",
      item_definitions = list(
        DP1 = "La gente generalmente piensa que soy inmoral porque soy LGBT.",
        DP2 = "Me tratan diferente porque no soy heterosexual."
      )
    )
  )
)

out <- export_conceptual_analysis(resultado, file = tempfile("conceptual_", fileext = ".txt"))
out  # invisible return; prints the path if typed
# Read first lines to verify encoding and content:
head(readLines(out, encoding = "UTF-8"))
}
}

\seealso{
\code{\link{print_conceptual_analysis}}, \code{\link{efa_optimizer}}
}
\encoding{UTF-8}
