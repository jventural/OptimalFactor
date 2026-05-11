# =============================================================================
# Ejemplo de uso: specification_search()
# Paquete: OptimalFactor
# =============================================================================
#
# El ejemplo simula 9 items con tres factores teoricos (3 items por factor) y
# corre una Specification Search heuristica desde k=1 hasta k=4. Para cada
# semilla se prueba tambien la variante bifactor (k >= 2).
#
# El proposito del ejemplo es comprobar que la funcion:
#   (1) genera semillas por bloques cuando no se pasa 'seeds'
#   (2) ejecuta el hill-climbing con operaciones move/drop/cov
#   (3) devuelve la tabla, los exitosos y el mejor modelo
#   (4) imprime la advertencia de MacCallum (1986)
#
# Tambien se muestra como pasar SEMILLAS PERSONALIZADAS (incluida una con la
# asignacion teorica correcta) para ver que el procedimiento la converge.
# =============================================================================
devtools::install("D:/14. LIBRERIAS/OptimalFactor")
suppressPackageStartupMessages({
  library(lavaan)
  library(OptimalFactor)
})

set.seed(2026)

# ---- 1. Datos simulados ----
sim_model <- '
  F1 =~ 0.75*x1 + 0.70*x2 + 0.65*x3
  F2 =~ 0.75*x4 + 0.70*x5 + 0.65*x6
  F3 =~ 0.75*x7 + 0.70*x8 + 0.65*x9
  F1 ~~ 0.30*F2
  F1 ~~ 0.30*F3
  F2 ~~ 0.30*F3
'
df <- lavaan::simulateData(sim_model, sample.nobs = 500, seed = 2026)
items <- paste0("x", 1:9)

cat("Datos simulados: N =", nrow(df), "| items =", length(items), "\n\n")

# ---- 2. Ejecucion con semillas por defecto (k = 1..4) ----
res_default <- specification_search(
  data         = df,
  items        = items,
  max_factors  = 4,
  estimator    = "ML",          # datos continuos simulados
  ordered      = FALSE,
  try_bifactor = TRUE,
  max_iter_per_config = 12,     # acotado para el ejemplo
  max_covs     = 2,
  max_items_removed = 2,
  verbose      = TRUE
)

cat("\n=========== TABLA (top 10) ===========\n")
print(res_default, top = 10)

cat("\n=========== EXITOSOS ===========\n")
if (nrow(res_default$successful) > 0) {
  print(res_default$successful, row.names = FALSE)
} else {
  cat("Ninguna configuracion alcanzo los targets en este ejemplo.\n")
}

# ---- 3. Ejecucion con semilla teorica correcta ----
seeds_teoricas <- list(
  "3" = list(
    list(F1 = c("x1","x2","x3"),
         F2 = c("x4","x5","x6"),
         F3 = c("x7","x8","x9"))
  )
)

res_teorico <- specification_search(
  data         = df,
  items        = items,
  seeds        = seeds_teoricas,
  estimator    = "ML",
  ordered      = FALSE,
  try_bifactor = FALSE,
  max_iter_per_config = 6,
  verbose      = TRUE
)

cat("\n=========== Mejor modelo (semilla teorica) ===========\n")
print(res_teorico, top = 3)

# ---- 4. Inspeccionar el mejor fit ----
cat("\n=========== Fit del mejor modelo ===========\n")
fm <- lavaan::fitMeasures(res_teorico$best$fit,
                          c("cfi","tli","rmsea","srmr","chisq","df"))
print(round(fm, 4))

cat("\nFIN del ejemplo.\n")
