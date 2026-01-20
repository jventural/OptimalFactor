# Impresión de Ítems Eliminados

Imprime en pantalla un mensaje que lista los ítems eliminados. Si el
vector de ítems está vacío, indica explícitamente que no hubo
eliminaciones.

## Usage

``` r
imprimir_items_eliminados(removed_items)
```

## Arguments

- removed_items:

  `character` vector. Nombres de los ítems que fueron eliminados.

## Details

La función construye un mensaje uniendo los elementos de `removed_items`
con comas. - Si `length(removed_items) > 0`, muestra la lista de
nombres. - Si `length(removed_items) == 0`, muestra “Ninguno”.
Finalmente, antepone la etiqueta “Ítems eliminados: ” y añade un salto
de línea.

## Value

`NULL`. La función se usa por su efecto de impresión en consola.

## Author

Dr. José Ventura‐León

## Examples

``` r
# Con ítems eliminados
imprimir_items_eliminados(c("PPTQ5", "PPTQ10", "PPTQ15"))
#> Error in imprimir_items_eliminados(c("PPTQ5", "PPTQ10", "PPTQ15")): could not find function "imprimir_items_eliminados"
#> Ítems eliminados: PPTQ5, PPTQ10, PPTQ15

# Sin ítems eliminados
imprimir_items_eliminados(character(0))
#> Error in imprimir_items_eliminados(character(0)): could not find function "imprimir_items_eliminados"
#> Ítems eliminados: Ninguno
```
