# Pretty-print LLM Conceptual Analyses for Removed/Retained Items

Formats and prints the item-level conceptual analyses produced by
[`efa_optimizer`](https://jventural.github.io/OptimalFactor/reference/efa_optimizer.md)
when `use_ai_analysis = TRUE`. Outputs bilingual (English/Spanish)
section headers according to `resultado$config_used$ai_config$language`,
wraps lines to the requested console width, optionally shows per-item
technical statistics, and warns if the LLM text appears truncated or
contains API error messages. The function does not modify objects and
returns `invisible(NULL)`.

## Usage

``` r
print_conceptual_analysis(resultado, width = 80, show_stats = FALSE)
```

## Arguments

- `resultado`:

  A result list returned by
  [`efa_optimizer`](https://jventural.github.io/OptimalFactor/reference/efa_optimizer.md)
  containing `$conceptual_analysis`, `$config_used`, and (optionally)
  `$conceptual_analysis$item_stats`.

- `width`:

  Integer console width in characters used to wrap paragraphs (passed to
  `strwrap`).

- `show_stats`:

  Logical; if `TRUE`, prints technical statistics (primary loading,
  \\h^2\\, RMSEA at removal, and removal reason) when available for each
  item.

## Details

**What is printed.** The function prints two sections, when available:

1.  Conceptual analysis of *removed* items.

2.  Conceptual analysis of *retained* items (if
    `ai_config$only_removed = FALSE` in
    [`efa_optimizer`](https://jventural.github.io/OptimalFactor/reference/efa_optimizer.md)).

For each item, the output shows:

- A header line with the item ID and its plain-language definition from
  `resultado$config_used$ai_config$item_definitions`.

- Optional **TECHNICAL INFORMATION** / **INFORMACIÓN TÉCNICA** (when
  `show_stats = TRUE`) including primary loading, \\h^2\\, RMSEA at
  removal, and the recorded reason (e.g., “Cross-loading”, “Heywood”).

- A three-part narrative derived from the LLM text:

  1.  *Psychometric Problems/Strengths* (for removed/retained items,
      respectively),

  2.  *Conceptual Misalignment/Alignment*,

  3.  *Removal/Retention Benefit*.

**Section detection and cleaning.** The function attempts to segment the
LLM paragraph into the three sections using multilingual keyword
heuristics (Spanish/English). If keywords are not detected, it splits
the text into three approximately equal blocks (by sentences). Basic
cleaning removes Markdown markers (e.g., `**`) and numbering artifacts
and ensures sentence-final punctuation if missing.

**Truncation and error handling.** Heuristics flag potentially truncated
analyses (e.g., very short text, missing terminal punctuation, or
trailing cut words) and print a warning note. If known API error
patterns are present (e.g., “HTTP 503”, “Rate limit”, “Connection
error”), an explicit error block is printed with a suggestion to rerun
the analysis for that item.

**Language and headings.** Headings and status messages are printed in
Spanish when `resultado$config_used$ai_config$language` is `"spanish"`
(or `"español"`); otherwise, English is used.

**Unicode.** The console output includes simple Unicode characters
(e.g., box rules, emojis like `📊`, `⚠️`, `❌`). If your console does
not support UTF-8, some symbols may render as placeholders.

## Value

`invisible(NULL)`. The function is used for its side effects (console
printing).

## See also

[`efa_optimizer`](https://jventural.github.io/OptimalFactor/reference/efa_optimizer.md)

## Examples

``` r
if (FALSE) { # \dontrun{
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
} # }
```
