\name{print_conceptual_analysis}
\alias{print_conceptual_analysis}
\title{Pretty-print LLM Conceptual Analyses for Removed/Retained Items}

\description{
Formats and prints the item-level conceptual analyses produced by \code{\link{efa_optimizer}} when \code{use_ai_analysis = TRUE}.
Outputs bilingual (English/Spanish) section headers according to \code{resultado$config_used$ai_config$language}, wraps lines to the requested console width, optionally shows per-item technical statistics, and warns if the LLM text appears truncated or contains API error messages. The function does not modify objects and returns \code{invisible(NULL)}.
}

\usage{
print_conceptual_analysis(resultado, width = 80, show_stats = FALSE)
}

\arguments{
\item{\code{resultado}}{A result list returned by \code{\link{efa_optimizer}} containing \code{$conceptual_analysis}, \code{$config_used}, and (optionally) \code{$conceptual_analysis$item_stats}.}
\item{\code{width}}{Integer console width in characters used to wrap paragraphs (passed to \code{strwrap}).}
\item{\code{show_stats}}{Logical; if \code{TRUE}, prints technical statistics (primary loading, \eqn{h^2}, RMSEA at removal, and removal reason) when available for each item.}
}

\details{
\strong{What is printed.}
The function prints two sections, when available:
\enumerate{
  \item Conceptual analysis of \emph{removed} items.
  \item Conceptual analysis of \emph{retained} items (if \code{ai_config$only_removed = FALSE} in \code{\link{efa_optimizer}}).
}
For each item, the output shows:
\itemize{
  \item A header line with the item ID and its plain-language definition from \code{resultado$config_used$ai_config$item_definitions}.
  \item Optional \strong{TECHNICAL INFORMATION} / \strong{INFORMACIÓN TÉCNICA} (when \code{show_stats = TRUE}) including primary loading, \eqn{h^2}, RMSEA at removal, and the recorded reason (e.g., ``Cross-loading'', ``Heywood'').
  \item A three-part narrative derived from the LLM text:
    \enumerate{
      \item \emph{Psychometric Problems/Strengths} (for removed/retained items, respectively),
      \item \emph{Conceptual Misalignment/Alignment},
      \item \emph{Removal/Retention Benefit}.
    }
}

\strong{Section detection and cleaning.}
The function attempts to segment the LLM paragraph into the three sections using multilingual keyword heuristics (Spanish/English).
If keywords are not detected, it splits the text into three approximately equal blocks (by sentences).
Basic cleaning removes Markdown markers (e.g., \verb{**}) and numbering artifacts and ensures sentence-final punctuation if missing.

\strong{Truncation and error handling.}
Heuristics flag potentially truncated analyses (e.g., very short text, missing terminal punctuation, or trailing cut words) and print a warning note.
If known API error patterns are present (e.g., ``HTTP 503'', ``Rate limit'', ``Connection error''), an explicit error block is printed with a suggestion to rerun the analysis for that item.

\strong{Language and headings.}
Headings and status messages are printed in Spanish when \code{resultado$config_used$ai_config$language} is \code{"spanish"} (or \code{"español"}); otherwise, English is used.

\strong{Unicode.}
The console output includes simple Unicode characters (e.g., box rules, emojis like \verb{📊}, \verb{⚠️}, \verb{❌}).
If your console does not support UTF-8, some symbols may render as placeholders.
}

\value{
\code{invisible(NULL)}. The function is used for its side effects (console printing).
}

\examples{
\dontrun{
# Minimal mock object illustrating the expected structure:
resultado <- list(
  conceptual_analysis = list(
    removed = list(
      DP1 = "Desde el punto de vista psicométrico, el ítem muestra carga baja y cruces..."
    ),
    kept = list(
      DP2 = "Psychometrically, the item shows strong primary loading and specificity..."
    ),
    item_stats = list(
      DP1 = list(loading = 0.22, h2 = 0.11, rmsea_at_removal = 0.095, reason = "Cross-loading")
    )
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

# Print in Spanish with technical stats:
print_conceptual_analysis(resultado, width = 80, show_stats = TRUE)

# Switch to English:
resultado$config_used$ai_config$language <- "english"
print_conceptual_analysis(resultado, width = 70, show_stats = FALSE)
}
}

\seealso{
\code{\link{efa_optimizer}}
}
\encoding{UTF-8}
