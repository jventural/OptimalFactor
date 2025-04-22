\name{optimal_cfa_with_ai}
\alias{optimal_cfa_with_ai}
\title{Optimización de Análisis Factorial Confirmatorio con apoyo de IA}
\description{
Esta función mejora de forma iterativa una especificación de Análisis Factorial Confirmatorio (CFA) combinando dos estrategias principales: (1) eliminación de ítems con cargas factoriales comparativamente bajas según índices de modificación, y (2) justificación conceptual de las exclusiones o conservaciones de ítems mediante la API de OpenAI. El proceso se repite hasta que el RMSEA escalado del modelo alcanza el umbral deseado o se agota el número máximo de pasos.
}
\usage{
optimal_cfa_with_ai(
  initial_model,
  data,
  rmsea_threshold = 0.08,
  mi_threshold = 3.84,
  max_steps = 10,
  verbose = TRUE,
  debug = FALSE,
  filter_expr = NULL,
  exclude_items = character(0),
  analyze_removed = FALSE,
  api_key = NULL,
  item_definitions = NULL,
  domain_name = "Dominio por Defecto",
  scale_title = "Título de la Escala por Defecto",
  construct_definition = "",
  model_name = "Modelo CFA",
  gpt_model = "gpt-3.5-turbo",
  ...
)
}
\arguments{
  \item{initial_model}{Cadena de texto con la especificación inicial del modelo CFA en sintaxis \code{lavaan} (e.g., \code{"F1 =~ x1 + x2 + x3"}).}
  \item{data}{\code{data.frame} que contiene las variables observadas para estimar el modelo CFA.}
  \item{rmsea_threshold}{Valor numérico del RMSEA escalado máximo permitido para detener el refinamiento (por defecto \code{0.08}).}
  \item{mi_threshold}{Valor numérico mínimo de índice de modificación (\emph{MI}) para considerar una correlación de error como candidata (por defecto \code{3.84}).}
  \item{max_steps}{Número máximo de iteraciones/pasos permitidos en el proceso de mejora (por defecto \code{10}).}
  \item{verbose}{Lógico; si \code{TRUE}, imprime mensajes detallados de cada paso.}
  \item{debug}{Lógico; si \code{TRUE}, imprime información adicional de depuración.}
  \item{filter_expr}{Expresión de R para filtrar \code{data} antes del ajuste; si \code{NULL}, no se aplica filtro.}
  \item{exclude_items}{Vector de caracteres con nombres de ítems que deben excluirse desde el inicio.}
  \item{analyze_removed}{Lógico; si \code{TRUE}, realiza además un análisis conceptual de ítems eliminados y conservados mediante IA.}
  \item{api_key}{Cadena con la clave de API para OpenAI (requerida si \code{analyze_removed = TRUE}).}
  \item{item_definitions}{Lista nombrada que asocia cada ítem con su redacción para el análisis conceptual.}
  \item{domain_name}{Nombre descriptivo del dominio o factor (solo para prompts de IA).}
  \item{scale_title}{Título de la escala o cuestionario (solo para prompts de IA).}
  \item{construct_definition}{Definición breve del constructo medido (solo para prompts de IA).}
  \item{model_name}{Etiqueta del modelo CFA (solo para prompts de IA).}
  \item{gpt_model}{Modelo de ChatGPT a usar (por defecto \code{"gpt-3.5-turbo"}, pueden usarse variantes como \code{"gpt-4"}).}
  \item{...}{Argumentos adicionales para pasar a \code{\link[lavaan]{cfa}} (e.g., \code{estimator}, \code{ordered}, \code{mimic}, etc.).}
}
\details{
La función realiza los siguientes pasos:
\enumerate{
  \item Aplica opcionalmente un filtro a los datos.
  \item Ajusta el modelo CFA inicial con \code{lavaan::cfa}.
  \item Calcula el RMSEA escalado; si está por debajo de \code{rmsea_threshold}, detiene el proceso.
  \item Extrae los índices de modificación (\code{lavaan::modificationIndices}) y selecciona el más alto.
  \item Compara las cargas estándar de los dos ítems afectados para determinar cuál eliminar.
  \item Remueve el ítem correspondiente de la especificación del modelo.
  \item Registra la modificación, el valor de MI y el RMSEA en cada iteración.
  \item Repite hasta alcanzar el umbral de RMSEA o agotar \code{max_steps}.
  \item Si \code{analyze_removed = TRUE}, invoca la API de OpenAI para justificar cada exclusión o conservación de ítem, imprimiendo mensajes en modo \code{verbose}.
}
}
\value{
Una lista con los siguientes componentes:
\item{final_model}{Cadena de texto con la especificación CFA final luego de todas las modificaciones.}
\item{final_fit}{Objeto de ajuste \code{lavaan} correspondiente al modelo final.}
\item{log}{\code{data.frame} que registra, por cada paso, la modificación aplicada, el valor de MI y el RMSEA resultante.}
\item{removed_items}{Vector de caracteres con los nombres de los ítems eliminados.}
\item{alternative_fit}{Objeto de ajuste \code{lavaan} del último modelo estimado antes de detenerse.}
\item{alternative_rmsea}{Valor final de RMSEA escalado del modelo alternativo.}
\item{final_cfi}{Valor final de CFI escalado del modelo alternativo.}
\item{conceptual_analysis}{Lista con sublistas \code{removed} y \code{kept} que contienen los textos generados por IA para cada ítem, o \code{NULL} si \code{analyze_removed = FALSE}.}
}
\examples{
\dontrun{
# Especificación inicial
mod0 <- 'F1 =~ x1 + x2 + x3 + x4 + x5'

# Definiciones para IA
defs <- list(
  x1 = "Descripción del ítem 1",
  x2 = "Descripción del ítem 2",
  x3 = "Descripción del ítem 3",
  x4 = "Descripción del ítem 4",
  x5 = "Descripción del ítem 5"
)

# Ejecución
res_cfa <- optimal_cfa_with_ai(
  initial_model    = mod0,
  data             = mi_df,
  rmsea_threshold  = 0.07,
  mi_threshold     = 4.0,
  max_steps        = 8,
  verbose          = TRUE,
  analyze_removed  = TRUE,
  api_key          = Sys.getenv("OPENAI_API_KEY"),
  item_definitions = defs,
  domain_name      = "Satisfacción Laboral",
  scale_title      = "Cuestionario de Satisfacción",
  construct_definition = "Grado de satisfacción con el entorno laboral.",
  model_name       = "CFA_SAT",
  gpt_model        = "gpt-4"
)

# 1. Lista de nombres de componentes disponibles
cat("Componentes en resultado_cfa:\n")
print(names(res_cfa))

# 2. Registro de pasos (log)
cat("\n1) Log de modificaciones por paso:\n")
print(res_cfa$log)

# 3) Modelo final
cat("\n2) Modelo final:\n")
cat(res_cfa$final_model, "\n")
# cat("Modelo final:\n", res_cfa$final_model)

# 4) Modelo alternativo (último antes de parar)
cat("\n3) Modelo alternativo:\n")
cat(res_cfa$alternative_model, "\n")

# 5) Resumen del ajuste del modelo final
cat("\n4) Resumen del ajuste (final_fit):\n")
print(summary(res_cfa$final_fit, fit.measures = TRUE))

# 6) Ítems eliminados
cat("\n5) Ítems eliminados:\n")
cat(paste(res_cfa$removed_items, collapse = ", "), "\n")

# 7) Medidas finales
cat("\n6) Medidas finales:\n")
cat("  • RMSEA final:", res_cfa$alternative_rmsea, "\n")
cat("  • CFI final:  ", res_cfa$final_cfi, "\n")

# 8) Análisis conceptual (si existe)
if (!is.null(res_cfa$conceptual_analysis)) {
  cat("\n7) Análisis conceptual de ítems eliminados:\n")
  print(res_cfa$conceptual_analysis$removed)

  cat("\n8) Análisis conceptual de ítems conservados:\n")
  print(res_cfa$conceptual_analysis$kept)
}
}
}
\author{
Dr. José Ventura-León
}
