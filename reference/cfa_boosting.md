# CFA-Boosting Optimization

Performs iterative optimization of Confirmatory Factor Analysis (CFA)
models using modification indices to improve model fit. The algorithm
automatically adds error covariances based on modification indices while
respecting configurable constraints.

## Usage

``` r
cfa_boosting(
  data,
  model,
  n_sample = NULL,
  thresholds = list(loading = 0.3, min_items_per_factor = 3,
    rmsea_target = 0.08, cfi_target = 0.95, srmr_target = 0.08,
    enforce_loading = TRUE, cross_loading = 0.3,
    enforce_simple_structure = TRUE),
  model_config = list(estimator = "WLSMV", ordered = TRUE),
  mod_indices_config = list(max_covs_to_add = 10, only_within_factor = TRUE,
    delta = 0.1, power_threshold = 0.75, alpha = 0.05),
  performance = list(max_iterations = 30, timeout_cfa = 60),
  verbose = TRUE
)
```

## Arguments

- data:

  Data frame containing the observed variables.

- model:

  Character string specifying the CFA model in lavaan syntax.

- n_sample:

  Sample size. If NULL, auto-detected from data.

- thresholds:

  List of fit thresholds:

  - `loading`: Minimum acceptable loading (default 0.30)

  - `min_items_per_factor`: Minimum items per factor (default 3)

  - `rmsea_target`: Target RMSEA value (default 0.08)

  - `cfi_target`: Target CFI value (default 0.95)

  - `srmr_target`: Target SRMR value (default 0.08)

  - `enforce_loading`: Treat `loading` as an admissibility floor rather
    than a hint (default `TRUE`). An item whose standardized loading
    falls below it is removed even when the fit targets are already met,
    unless removing it would breach `min_items_per_factor`. `FALSE`
    restores the behaviour of versions up to 1.3.0, where the loop
    stopped as soon as the fit targets were satisfied and the loadings
    were never inspected. Global fit does not reveal a weak item: in
    simulation an item loading .20 in the population sat comfortably
    inside a model with RMSEA = .03. Enforcing the floor can leave
    global fit slightly worse while making the retained set defensible,
    which is the trade this option makes explicit.

  - `cross_loading`: Standardized magnitude above which an item is
    declared to load on a foreign factor (default 0.30).

  - `enforce_simple_structure`: Remove items that load on a factor other
    than their own (default `TRUE`). The loading floor catches the item
    that measures nothing; this catches the one that measures two
    things, which the floor cannot see because a cross-loading item
    still loads acceptably on its own factor, and which global fit does
    not reveal either: an omitted cross-loading is absorbed by the
    interfactor correlation, so in simulation six items cross-loading at
    .60 left RMSEA at .053 and CFI at .997. Detection uses the
    modification index of the absent loading, but never significance
    alone: the standardized expected parameter change must also reach
    `cross_loading`, since a significant index with a trivial EPC is
    precisely the capitalization on chance this is meant to avoid.

- model_config:

  Model configuration:

  - `estimator`: Estimation method (default "WLSMV")

  - `ordered`: Whether variables are ordered (default TRUE)

- mod_indices_config:

  Modification indices configuration using the Saris, Satorra & van der
  Veld (2009) framework (MI + EPC + Power):

  - `max_covs_to_add`: Maximum covariances to evaluate per iteration
    (default 10)

  - `only_within_factor`: Only consider within-factor covariances
    (default TRUE)

  - `delta`: Minimum misspecification size to detect (default 0.10)

  - `power_threshold`: Threshold for high/low power classification
    (default 0.75)

  - `alpha`: Significance level for the MI test (default 0.05)

- performance:

  Performance settings:

  - `max_iterations`: Maximum optimization iterations (default 30)

  - `timeout_cfa`: Timeout per CFA run in seconds (default 60)

- verbose:

  Logical. Print progress information.

## Value

A list containing:

- `final_model`: The optimized lavaan CFA model object

- `fit_indices`: Final fit indices

- `added_covariances`: List of error covariances added

- `iterations`: Number of iterations performed

- `history`: History of fit indices across iterations

## See also

[`efa_boosting`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md),
[`print_cfa_boosting`](https://jventural.github.io/OptimalFactor/reference/print_cfa_boosting.md)

## Examples

``` r
# \donttest{
data(Data_Personality)
model <- '
F1 =~ PPTQ1 + PPTQ2 + PPTQ3 + PPTQ4 + PPTQ5
F2 =~ PPTQ6 + PPTQ7 + PPTQ8 + PPTQ9 + PPTQ10
F3 =~ PPTQ11 + PPTQ12 + PPTQ13 + PPTQ14 + PPTQ15
'
result <- cfa_boosting(
  data    = Data_Personality,
  model   = model,
  verbose = TRUE
)
#> Tamaño de muestra detectado: N = 100 
#> 
#> ====================================================================== 
#>    CFA BOOSTING v1.0 - Optimización de Modelo Confirmatorio
#> ====================================================================== 
#> 
#> Targets -> RMSEA <= 0.08  | CFI >= 0.95  | SRMR <= 0.08 
#> Estimador: WLSMV 
#> Min items/factor: 3 
#> Loading mínimo: 0.3 
#> MI framework: Saris-Satorra-van der Veld (delta= 0.1 , power>= 0.75 , alpha= 0.05 )
#> 
#> Warning: lavaan->lavaan():  
#>    the first indicator of the following latent variable(s) is a poor item; 
#>    switching to another marker item (to set the metric) to avoid convergence 
#>    problems; use bad.marker.crit = 0 to switch off this behavior: F2 (PPTQ6 
#>    -> PPTQ10)
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -3.404395e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> --- MODELO INICIAL ---
#> RMSEA: 0.107 | CFI: 0.759 | SRMR: 0.120 | Loss: 2.618 
#> 
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> Warning: lavaan->lavaan():  
#>    the first indicator of the following latent variable(s) is a poor item; 
#>    switching to another marker item (to set the metric) to avoid convergence 
#>    problems; use bad.marker.crit = 0 to switch off this behavior: F2 (PPTQ6 
#>    -> PPTQ10)
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 3.187548e-17) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#>   -> Eliminado PPTQ13 por carga 0.147 < piso 0.3 
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> Warning: lavaan->lavaan():  
#>    the first indicator of the following latent variable(s) is a poor item; 
#>    switching to another marker item (to set the metric) to avoid convergence 
#>    problems; use bad.marker.crit = 0 to switch off this behavior: F2 (PPTQ6 
#>    -> PPTQ10), F3 (PPTQ12 -> PPTQ15)
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -9.139381e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#>   -> Eliminado PPTQ11 por carga 0.174 < piso 0.3 
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> Warning: lavaan->lavaan():  
#>    the first indicator of the following latent variable(s) is a poor item; 
#>    switching to another marker item (to set the metric) to avoid convergence 
#>    problems; use bad.marker.crit = 0 to switch off this behavior: F2 (PPTQ6 
#>    -> PPTQ10), F3 (PPTQ12 -> PPTQ15)
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -8.563139e-18) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#>   -> Eliminado PPTQ8 por carga 0.238 < piso 0.3 
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> Warning: lavaan->lavaan():  
#>    the first indicator of the following latent variable(s) is a poor item; 
#>    switching to another marker item (to set the metric) to avoid convergence 
#>    problems; use bad.marker.crit = 0 to switch off this behavior: F3 (PPTQ12 
#>    -> PPTQ15)
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -2.096797e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#>   -> Eliminado PPTQ7 por carga -0.245 < piso 0.3 
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> Warning: lavaan->lavaan():  
#>    the first indicator of the following latent variable(s) is a poor item; 
#>    switching to another marker item (to set the metric) to avoid convergence 
#>    problems; use bad.marker.crit = 0 to switch off this behavior: F3 (PPTQ12 
#>    -> PPTQ15)
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -2.127868e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#>   -> Eliminado PPTQ1 por carga cruzada en F3 (EPC = 17.426 )
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> Warning: lavaan->lavaan():  
#>    the first indicator of the following latent variable(s) is a poor item; 
#>    switching to another marker item (to set the metric) to avoid convergence 
#>    problems; use bad.marker.crit = 0 to switch off this behavior: F3 (PPTQ12 
#>    -> PPTQ15)
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -5.347706e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#>   -> Eliminado PPTQ2 por carga cruzada en F3 (EPC = 31.093 )
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> ------------------------------------------------------------ 
#> ITERACIÓN 7 
#> ------------------------------------------------------------ 
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#>   Evaluando 1 pares misespecificados [Saris-Satorra] (4 opciones c/u)...
#> 
#>     --- Par: PPTQ9 ~~ PPTQ10 (MI= 6.0 , EPC= 0.272 , Power= 0.147 , => m ) ---
#> Warning: lavaan->lavaan():  
#>    the first indicator of the following latent variable(s) is a poor item; 
#>    switching to another marker item (to set the metric) to avoid convergence 
#>    problems; use bad.marker.crit = 0 to switch off this behavior: F3 (PPTQ12 
#>    -> PPTQ15)
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -5.844440e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#>       [1] Agregar cov:      RMSEA= 0.148  | CFI= 0.829  | SRMR= 0.103  | Loss= 2.500 
#>       [2] Eliminar PPTQ9:    NO PERMITIDO (min items)
#>       [3] Eliminar PPTQ10:    NO PERMITIDO (min items)
#>       [4] Eliminar ambos:   NO PERMITIDO (min items)
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#>   -> Agregar PPTQ9 ~~ PPTQ10 (MI=6.0) 
#>      RMSEA: 0.148 | CFI: 0.829 | SRMR: 0.103 | Loss: 2.500 
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> ------------------------------------------------------------ 
#> ITERACIÓN 8 
#> ------------------------------------------------------------ 
#>   Evaluando eliminación de 1 items problemáticos...
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> 
#>   No se encontraron mejoras posibles. Deteniendo.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> 
#> ====================================================================== 
#>    RESULTADOS FINALES
#> ====================================================================== 
#> 
#> Iteraciones: 8 
#> Items eliminados: PPTQ13, PPTQ11, PPTQ8, PPTQ7, PPTQ1, PPTQ2 
#> Covarianzas agregadas: PPTQ9 ~~ PPTQ10 
#> 
#> --- ÍNDICES DE AJUSTE FINALES ---
#> RMSEA: 0.148 (NO CUMPLE) 
#> CFI:   0.829 (NO CUMPLE) 
#> SRMR:  0.103 (NO CUMPLE) 
#> 
#> *** MODELO NO CUMPLE TODOS LOS CRITERIOS ***
#> 
#> --- MODELO FINAL ---
#> F1 =~ PPTQ3 + PPTQ4 + PPTQ5
#> F2 =~ PPTQ6 + PPTQ9 + PPTQ10
#> F3 =~ PPTQ12 + PPTQ14 + PPTQ15
#> PPTQ9 ~~ PPTQ10 
result$removed_items
#> [1] "PPTQ13" "PPTQ11" "PPTQ8"  "PPTQ7"  "PPTQ1"  "PPTQ2" 
result$added_covariances
#> [1] "PPTQ9 ~~ PPTQ10"
# }
```
