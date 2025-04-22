\name{optimal_efa_with_ai}
\alias{optimal_efa_with_ai}
\title{Optimización de Análisis Factorial Exploratorio con apoyo de IA}
\description{
Esta función refina de forma automática una solución de Análisis Factorial Exploratorio (EFA) combinando criterios de bondad de ajuste (RMSEA) y calidad de las cargas factoriales. En cada iteración se elimina el ítem cuya remoción mejora más el RMSEA o que presenta problemas de estructura (cross‑loading o sin carga significativa). Además, opcionalmente, los ítems eliminados y conservados pueden analizarse conceptualmente mediante la API de OpenAI, justificando exclusiones o conservaciones.
}
\usage{
optimal_efa_with_ai(
  data,
  items = NULL,
  n_factors = 5,
  n_items = NULL,
  name_items = "PPTQ",
  estimator = "WLSMV",
  rotation = "oblimin",
  threshold_rmsea = 0.08,
  threshold_loading = 0.30,
  min_items_per_factor = 2,
  apply_threshold = TRUE,
  max_steps = NULL,
  verbose = TRUE,
  exclude_items = character(0),
  analyze_removed = FALSE,
  api_key = NULL,
  item_definitions = NULL,
  domain_name = "Dominio por Defecto",
  scale_title = "Título de la Escala por Defecto",
  construct_definition = "",
  model_name = "Modelo EFA",
  gpt_model = "gpt-3.5-turbo",
  ...
)
}
\arguments{
  \item{data}{Un \code{data.frame} con las variables a incluir en el EFA.}
  \item{items}{Vector de nombres de ítems a usar; si es \code{NULL}, se generan a partir de \code{name_items} y \code{n_items}.}
  \item{n_factors}{Número de factores a extraer (por defecto 5).}
  \item{n_items}{Número de ítems por factor cuando \code{items} es \code{NULL}.}
  \item{name_items}{Prefijo de los nombres de ítem (por defecto \"PPTQ\").}
  \item{estimator}{Estimador de lavaan a usar (p. ej. \"WLSMV\").}
  \item{rotation}{Método de rotación (p. ej. \"oblimin\").}
  \item{threshold_rmsea}{Valor máximo permitido de RMSEA escalado para detener el proceso (por defecto 0.08).}
  \item{threshold_loading}{Umbral mínimo de carga absoluta para considerar un ítem “bien cargado” (por defecto 0.30).}
  \item{min_items_per_factor}{Mínimo de ítems requeridos por factor para validar la estructura (por defecto 2).}
  \item{apply_threshold}{Lógico; si \code{TRUE}, aplica umbrales de carga al modelo interno.}
  \item{max_steps}{Número máximo de iteraciones/ítems a remover; si es \code{NULL}, se calcula como \code{length(items)-1}.}
  \item{verbose}{Lógico; si \code{TRUE}, muestra mensajes de progreso y decisión en cada paso.}
  \item{exclude_items}{Vector de ítems a excluir de antemano.}
  \item{analyze_removed}{Lógico; si \code{TRUE}, realiza análisis conceptual de ítems eliminados/conservados mediante IA.}
  \item{api_key}{Clave de autenticación para la API de OpenAI (necesaria si \code{analyze_removed = TRUE}).}
  \item{item_definitions}{Lista nombrada con el texto de cada ítem para análisis conceptual.}
  \item{domain_name}{Nombre descriptivo del dominio o subescala (solo para prompts de IA).}
  \item{scale_title}{Título de la escala (solo para prompts de IA).}
  \item{construct_definition}{Definición breve del constructo medido (solo para prompts de IA).}
  \item{model_name}{Etiqueta del modelo EFA (solo para prompts de IA).}
  \item{gpt_model}{Modelo de ChatGPT a usar (p. ej. \"gpt-3.5-turbo\" o \"gpt-4\").}
  \item{...}{Argumentos adicionales que se pasarán a \code{PsyMetricTools::EFA_modern()}.}
}
\details{
La función comienza instalando y cargando \code{PsyMetricTools} si fuera necesario.
Luego selecciona los ítems iniciales y monta un bucle iterativo donde en cada paso:
\enumerate{
  \item Se estima el modelo EFA con los ítems restantes.
  \item Se computa el RMSEA escalado.
  \item Se evalúa la estructura factorial revisando cargas absolutas y cross‑loadings.
  \item Si el RMSEA está por debajo del umbral y la estructura es válida, se detiene.
  \item Si no, decide entre remover el ítem cuya exclusión mejora más el RMSEA o el ítem problemático con peor score.
}
Opcionalmente, tras el refinamiento, cada ítem eliminado y conservado puede analizarse conceptualmente mediante la API de OpenAI, imprimiendo en \code{verbose} mensajes de inicio de análisis para cada ítem.
}
\value{
Una lista con los siguientes elementos:
\item{final_structure}{\code{data.frame} con las cargas y asignaciones finales de ítems a factores.}
\item{removed_items}{Vector de nombres de los ítems eliminados durante el proceso.}
\item{steps_log}{\code{data.frame} que registra cada paso: ítem removido, motivo y RMSEA resultante.}
\item{iterations}{Número total de iteraciones ejecutadas.}
\item{final_rmsea}{Valor final de RMSEA escalado tras la última iteración.}
\item{conceptual_analysis}{Lista con sublistas \code{removed} y \code{kept} que contienen los textos generados por IA para cada ítem, o \code{NULL} si no se solicitó.}
}
\examples{
\dontrun{
# Definir las definiciones de ítems
defs <- list(
  Q1 = "Me siento cómodo usando métodos estadísticos avanzados.",
  Q2 = "Entiendo la diferencia entre EFA y CFA.",
  Q3 = "Puedo interpretar índices de ajuste como RMSEA y CFI."
)
# Ejecutar EFA optimizada con análisis conceptual
res <- optimal_efa_with_ai(
  data                 = mi_dataframe,
  n_factors            = 1,
  name_items           = "Q",
  n_items              = 3,
  analyze_removed      = TRUE,
  api_key              = "OPENAI_API_KEY",
  item_definitions     = defs,
  domain_name          = "Competencia Estadística",
  scale_title          = "Autoevaluación Estadística",
  construct_definition = "Habilidad percibida en métodos estadísticos.",
  model_name           = "EFA_Q",
  gpt_model            = "gpt-4"
)
# 1. Mostrar la estructura factorial final
cat("===== Estructura factorial final =====\n")
print(resultado_efa$final_structure)

# 2. Mostrar el log de pasos
cat("\n===== Log de pasos =====\n")
print(resultado_efa$steps_log)

# 3. Resumen de ítems eliminados, RMSEA final e iteraciones
cat("\n===== Resumen =====\n")
cat("Ítems eliminados: ",
    if (length(resultado_efa$removed_items) > 0) {
      paste(resultado_efa$removed_items, collapse = ", ")
    } else {
      "Ninguno"
    },
    "\n", sep = "")


cat("RMSEA final: ", resultado_efa$final_rmsea, "\n", sep = "")
cat("Iteraciones realizadas: ", resultado_efa$iterations, "\n", sep = "")

# 4. Análisis conceptual con IA (si está disponible)
if (!is.null(resultado_efa$conceptual_analysis)) {
  # ítems eliminados
  if (!is.null(resultado_efa$conceptual_analysis$removed) &&
      length(resultado_efa$conceptual_analysis$removed) > 0) {
    cat("\n===== Análisis conceptual de ítems eliminados =====\n")
    print(resultado_efa$conceptual_analysis$removed)
  } else {
    cat("\nNo hay análisis conceptual de ítems eliminados.\n")
  }
  # ítems conservados
  if (!is.null(resultado_efa$conceptual_analysis$kept) &&
      length(resultado_efa$conceptual_analysis$kept) > 0) {
    cat("\n===== Análisis conceptual de ítems conservados =====\n")
    print(resultado_efa$conceptual_analysis$kept)
  } else {
    cat("\nNo hay análisis conceptual de ítems conservados.\n")
  }
}
}
}
\author{
Dr. José Ventura-León
}
