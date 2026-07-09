# Package index

## EFA Optimization

Functions for Exploratory Factor Analysis optimization

- [`efa_boosting()`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md)
  : EFA-Boosting Optimization
- [`efa_optimizer()`](https://jventural.github.io/OptimalFactor/reference/efa_optimizer.md)
  : Exploratory Factor Analysis Optimizer with Optional LLM Support
- [`optimal_efa_with_ai()`](https://jventural.github.io/OptimalFactor/reference/optimal_efa_with_ai.md)
  : Exploratory Factor Analysis Optimization with AI Assistance
- [`redundancy_short_form()`](https://jventural.github.io/OptimalFactor/reference/redundancy_short_form.md)
  : Redundancy-Guided Short Form of a Unidimensional Scale

## CFA Optimization

Functions for Confirmatory Factor Analysis optimization

- [`cfa_boosting()`](https://jventural.github.io/OptimalFactor/reference/cfa_boosting.md)
  : CFA-Boosting Optimization
- [`optimal_cfa_with_ai()`](https://jventural.github.io/OptimalFactor/reference/optimal_cfa_with_ai.md)
  : Confirmatory Factor Analysis Optimization with AI Assistance
- [`specification_search_theory()`](https://jventural.github.io/OptimalFactor/reference/specification_search_theory.md)
  : Theory-Guided Specification Search for CFA Models
- [`cross_validate_cfa()`](https://jventural.github.io/OptimalFactor/reference/cross_validate_cfa.md)
  : Split-Half Cross-Validation of a Factor Model
- [`bifactor_indices()`](https://jventural.github.io/OptimalFactor/reference/bifactor_indices.md)
  : Bifactor Statistical Indices
- [`print(`*`<specification_search>`*`)`](https://jventural.github.io/OptimalFactor/reference/print.specification_search.md)
  : Print method for specification_search

## Results Display

Functions for displaying and exporting results

- [`print_efa_results()`](https://jventural.github.io/OptimalFactor/reference/print_efa_results.md)
  : Print Results from AI‐Assisted EFA Refinement
- [`print_cfa_results()`](https://jventural.github.io/OptimalFactor/reference/print_cfa_results.md)
  : Print Results from AI‐Assisted CFA Refinement
- [`print_cfa_boosting()`](https://jventural.github.io/OptimalFactor/reference/print_cfa_boosting.md)
  : Print CFA-Boosting Results
- [`report_efa_results()`](https://jventural.github.io/OptimalFactor/reference/report_efa_results.md)
  : Console Report for EFA Optimization Results
- [`report_cfa_results()`](https://jventural.github.io/OptimalFactor/reference/report_cfa_results.md)
  : Report the CFA boosting results
- [`export_cfa_boosting()`](https://jventural.github.io/OptimalFactor/reference/export_cfa_boosting.md)
  : Export CFA Boosting Results to Structured Data Frames
- [`imprimir_items_eliminados()`](https://jventural.github.io/OptimalFactor/reference/imprimir_items_eliminados.md)
  : Impresión de Ítems Eliminados

## AI Analysis

Functions for AI-powered conceptual analysis

- [`print_conceptual_analysis()`](https://jventural.github.io/OptimalFactor/reference/print_conceptual_analysis.md)
  : Pretty-print LLM Conceptual Analyses for Removed/Retained Items
- [`export_conceptual_analysis()`](https://jventural.github.io/OptimalFactor/reference/export_conceptual_analysis.md)
  : Export LLM Conceptual Analyses to a UTF-8 Text File

## Example Data

Datasets included in the package

- [`Data_Expectativas`](https://jventural.github.io/OptimalFactor/reference/Data_Expectativas.md)
  : Expectativas Dataset
- [`Data_Personality`](https://jventural.github.io/OptimalFactor/reference/Data_Personality.md)
  : Personality Dataset

## Deprecated

Kept for backward compatibility. Use specification_search_theory()
instead, which adds a theory-congruence penalty to the loss.

- [`specification_search()`](https://jventural.github.io/OptimalFactor/reference/specification_search.md)
  [`print(`*`<specification_search>`*`)`](https://jventural.github.io/OptimalFactor/reference/specification_search.md)
  : Heuristic Specification Search for CFA Models (deprecated)
