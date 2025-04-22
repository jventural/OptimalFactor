\name{imprimir_items_eliminados}
\alias{imprimir_items_eliminados}
\title{Impresión de Ítems Eliminados}
\description{
Imprime en pantalla un mensaje que lista los ítems eliminados. Si el vector de ítems está vacío, indica explícitamente que no hubo eliminaciones.
}
\usage{
imprimir_items_eliminados(removed_items)
}
\arguments{
  \item{removed_items}{\code{character} vector. Nombres de los ítems que fueron eliminados.}
}
\details{
La función construye un mensaje uniendo los elementos de \code{removed_items} con comas.
- Si \code{length(removed_items) > 0}, muestra la lista de nombres.
- Si \code{length(removed_items) == 0}, muestra “Ninguno”.
Finalmente, antepone la etiqueta “Ítems eliminados: ” y añade un salto de línea.
}
\value{
\code{NULL}. La función se usa por su efecto de impresión en consola.
}
\examples{
# Con ítems eliminados
imprimir_items_eliminados(c("PPTQ5", "PPTQ10", "PPTQ15"))
#> Ítems eliminados: PPTQ5, PPTQ10, PPTQ15

# Sin ítems eliminados
imprimir_items_eliminados(character(0))
#> Ítems eliminados: Ninguno
}
\author{
Dr. José Ventura‐León
}
