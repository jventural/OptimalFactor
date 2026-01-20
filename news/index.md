# Changelog

## OptimalFactor 1.0.0

### Initial CRAN Release

#### Main Features

- **efa_boosting()**: Advanced iterative EFA optimization algorithm
  - Greedy and global search strategies for optimal item selection
  - Adaptive composite fit indices (RMSEA, SRMR, CFI) with weights based
    on df x N
  - Automatic Heywood case and near-Heywood detection
  - Cross-loading identification and removal
  - Minimum items per factor enforcement
  - Interfactor correlation verification
  - Optional AI-assisted conceptual analysis via GPT models
- **run_efa_boosting()**: Launch EFA-Boosting Studio
  - Interactive Shiny application for EFA optimization
  - Real-time console output with auto-scroll
  - Modern Bootstrap 5 interface
  - CSV/Excel data import
  - Configurable fit targets and thresholds
  - Option to exclude specific items
  - Export results to CSV
- **cfa_boosting()**: Confirmatory Factor Analysis optimization
  - Modification indices-based model improvement
  - Automatic covariance addition
  - Fit index monitoring
- **print_conceptual_analysis()**: Formatted output for AI analyses
  - Bilingual support (English/Spanish)
  - Technical statistics display
  - Narrative formatting
- **export_conceptual_analysis()**: Export analysis to various formats
  - Word document export
  - Plain text export
  - Formatted tables

#### Data

- `Data_Expectativas`: Example dataset for expectation scale
- `Data_Personality`: Example personality assessment dataset

#### Dependencies

- Requires PsyMetricTools for EFA_modern function
- lavaan for structural equation modeling
- psych for factor analysis utilities

#### References

- Kenny, D. A., & McCoach, D. B. (2003). Effect of the number of
  variables on measures of fit in structural equation modeling.
  Structural Equation Modeling, 10(3), 333-351.
- Shi, D., Lee, T., & Maydeu-Olivares, A. (2019). Understanding the
  model size effect on SEM fit indices. Educational and Psychological
  Measurement, 79(2), 310-334.
