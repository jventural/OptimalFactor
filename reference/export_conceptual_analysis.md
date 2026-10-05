# Export LLM Conceptual Analyses to a UTF-8 Text File

Writes the formatted conceptual analyses produced by
[`efa_optimizer`](https://jventural.github.io/OptimalFactor/reference/efa_optimizer.md)
to a UTF-8 encoded text file. Internally calls
[`print_conceptual_analysis`](https://jventural.github.io/OptimalFactor/reference/print_conceptual_analysis.md)
with `width = 100` and `show_stats = TRUE`, capturing its console output
via [`sink()`](https://rdrr.io/r/base/sink.html). After writing, emits a
short confirmation via
[`message()`](https://rdrr.io/r/base/message.html) and invisibly returns
the output file path.

## Usage

``` r
export_conceptual_analysis(resultado, file)
```

## Arguments

- resultado:

  A result list returned by
  [`efa_optimizer`](https://jventural.github.io/OptimalFactor/reference/efa_optimizer.md);
  must contain `$conceptual_analysis` and `$config_used` (see
  [`print_conceptual_analysis`](https://jventural.github.io/OptimalFactor/reference/print_conceptual_analysis.md)).

- file:

  Character path to the output text file (UTF-8). Required (there is no
  default, so nothing is written to the working directory unless asked);
  for a throw-away file use, e.g.,
  `file.path(tempdir(), "conceptual_analysis.txt")`. If it exists, it
  will be overwritten.

## Details

**What gets written.** The function captures the full console rendering
of
[`print_conceptual_analysis`](https://jventural.github.io/OptimalFactor/reference/print_conceptual_analysis.md)
(headings, section dividers, emojis, warnings, and technical stats) and
writes it to `file` using UTF-8 encoding. Because
`print_conceptual_analysis` is bilingual, headings will appear in
Spanish when `resultado$config_used$ai_config$language` is `"spanish"`
(or `"español"`); otherwise, English is used.

**Formatting choices.** Text is wrapped at 100 characters per line and
per-item technical statistics are always included (primary loading,
\\h^2\\, RMSEA at removal, and removal reason) when available.

**Encoding and Unicode.** Output is forced to UTF-8. Unicode symbols
used by `print_conceptual_analysis` (e.g., `a bar chart symbol`,
`a warning sign`, `a cross mark`) will be preserved; ensure your editor
is set to UTF-8 to avoid mojibake.

**Side effects and limitations.** The function uses
`sink(file, encoding = "UTF-8")` followed by
[`sink()`](https://rdrr.io/r/base/sink.html) to restore the console. If
an error interrupts execution before the second
[`sink()`](https://rdrr.io/r/base/sink.html), the connection may remain
diverted; call [`sink()`](https://rdrr.io/r/base/sink.html) manually to
reset if needed.

## Value

Invisibly returns the `file` path (character scalar). A brief
confirmation message is also emitted with
[`message()`](https://rdrr.io/r/base/message.html), of the form
“Análisis exportado a: \<file\>”.

## See also

[`print_conceptual_analysis`](https://jventural.github.io/OptimalFactor/reference/print_conceptual_analysis.md),
[`efa_optimizer`](https://jventural.github.io/OptimalFactor/reference/efa_optimizer.md)

## Examples

``` r
# Example mock object (see print_conceptual_analysis() examples for structure)
resultado <- list(
  conceptual_analysis = list(
    removed = list(DP1 = "Desde el punto de vista psicométrico..."),
    kept = list(DP2 = "Psychometrically, the item shows..."),
    item_stats = list(DP1 = list(loading = 0.22, h2 = 0.11,
                                 rmsea_at_removal = 0.095, reason = "Cross-loading"))
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

out <- export_conceptual_analysis(resultado,
                                  file = file.path(tempdir(), "conceptual_analysis.txt"))
#> Análisis exportado a: /tmp/RtmpNtsLuD/conceptual_analysis.txt
out  # invisible return; prints the path if typed
#> [1] "/tmp/RtmpNtsLuD/conceptual_analysis.txt"
# Read first lines to verify encoding and content:
head(readLines(out, encoding = "UTF-8"))
#> [1] ""                                             
#> [2] "═════════════════════════════════════════════"
#> [3] "ANÁLISIS CONCEPTUAL DE ÍTEMS ELIMINADOS "     
#> [4] ""                                             
#> [5] "═════════════════════════════════════════════"
#> [6] ""                                             
```
