
```r
knitr::opts_chunk$set(warning = FALSE, 
                      message = FALSE,
                      cache = TRUE)
library(kableExtra)
```

```
## Warning: package 'kableExtra' was built under R version 4.2.2
```

```r
tba <- function(dat, cap = NA){
  kable(dat,
      format = "html", digits =  4,
      caption = cap) %>% 
     kable_styling(bootstrap_options = "striped", full_width = F)%>%
         kable_classic(full_width = F, html_font = "Arial Narrow")
}
```

# Análisis descriptivo de las estimaciones directas. 

En la sección anterior, se llevó a cabo una estimación directa para cada categoría individualmente en cada municipio (dominio) presente en la muestra. Ahora, para evaluar la calidad de los resultados obtenidos, realizaremos un análisis descriptivo. Se emplean varias medidas de calidad, entre ellas, se cuenta el número de dominios que tienen dos o más unidades primarias de muestreo (UPM), así como el efecto de diseño mayor a 1 y las varianzas mayores a 0. Estas medidas nos permitirán determinar la fiabilidad de nuestros resultados y tomar decisiones informadas en función de ellos. 

## Lectura de librerías
Las librerías que emplearemos en esta sección del código son: 

  -   La librería **ggplot2** es una librería de visualización de datos en R que permite crear gráficos estadísticos personalizados y de alta calidad. Ofrece una amplia variedad de opciones para crear gráficos de barras, gráficos de líneas, gráficos de dispersión, gráficos de cajas, entre otros. Esta librería se destaca por su capacidad para personalizar los gráficos en función de las necesidades del usuario, lo que permite crear gráficos complejos con múltiples capas y características.

  -   La librería **dplyr** es una librería utilizada para procesar y transformar datos en R. Ofrece una variedad de funciones para filtrar, ordenar, agrupar, resumir y transformar datos. Esta librería es muy útil para realizar tareas comunes de análisis de datos, como limpieza de datos, recodificación de variables, cálculo de variables derivadas, entre otras.

  -   La librería **patchwork** es una librería utilizada para crear paneles de visualización personalizados y complejos en R. Ofrece una amplia variedad de opciones para combinar y organizar gráficos, así como para agregar anotaciones y elementos decorativos. Esta librería es especialmente útil para crear paneles de visualización que incluyan varios gráficos y tablas.

  -   La librería **kableExtra** es una librería utilizada para crear tablas personalizadas y estilizadas en R. Ofrece una amplia variedad de opciones para personalizar el formato de las tablas, incluyendo la capacidad de agregar estilos y colores, resaltar celdas específicas, agregar pies de página, y mucho más.



```r
library(ggplot2)
library(dplyr)
library(patchwork)
library(kableExtra)
select <- dplyr::select
```

## Definiendo algunas funciones. 

  -   La función `NAs_deff(x)` cuenta la cantidad de valores NAs en un vector `x`.

  -   La función `cont_deff()` realiza el conteo del número de elementos en un vector `x` que son mayores a 1 y devuelve el resultado en la variable `deff`.

  -   La función `cont_deff_upm()` realiza un conteo similar al de la función `cont_deff()`, pero solo considera los elementos que también cumplen con la condición de que el número de unidades primarias de muestreo (UPM) es mayor o igual a 2. 
  
  -   La función `var_conteo()` regresa el porcentaje de elementos que son mayores a 1 en un ventor de `x`
  

```r
NAs_deff <- function(x){(sum(is.na(x))/length(x))*100}
cont_deff <- function(x){deff = (sum(x>1,na.rm = TRUE)/length(x))*100}

cont_deff_upm <- function(x, nupm) {
  deff = (sum(x > 1 & nupm >= 2, na.rm = TRUE)/length(x))*100
}
var_conteo <- function(x)(sum(x>0)/length(x))*100
```

## Lectura de las estimaciones directas

El siguiente comando lee los resultados de las estimaciones directas y sus medidas de calidad para cada domino, los cules fueron calculados previamente. 


```r
indicador_dam <- readRDS('01 Modelo de area/CHL/2017/Data/indicador_dam.Rds')
```

## Descriptivo de las estimaciones directas 

A continuación se realiza el computo de algunos medidas de resumen para las estimaciones directas, estas medidas serán empleadas para indagar por la calidad de los resultados obtenidos. 

### Conteo de los dominios con deff no estimados 


```r
indicador_dam %>%
  summarise_at(.vars = vars(matches("_deff")), NAs_deff) %>%
 tba()
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> Ocupado_deff </th>
   <th style="text-align:right;"> Desocupado_deff </th>
   <th style="text-align:right;"> Inactivo_deff </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1.5432 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>
Los resultados muestran que el $1.5\%$ de los dominios no fue posible estimar el `Deff` de la categoría de Desocupados. 
 
### Conteo de los dominios con deff mayores que 1


```r
indicador_dam %>% summarise_at(.vars =vars( matches("_deff")),
                               cont_deff) %>%
  tba()
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> Ocupado_deff </th>
   <th style="text-align:right;"> Desocupado_deff </th>
   <th style="text-align:right;"> Inactivo_deff </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 153 </td>
   <td style="text-align:right;"> 134 </td>
   <td style="text-align:right;"> 162 </td>
  </tr>
</tbody>
</table>
La tabla muestra que un $50\%$ o menos de los dominios tienen un `Deff` mayores que 1. 

### Porcentaje de dominios con estimación de la varianza mayor a 0


```r
indicador_dam %>% summarise_at(.vars = vars( matches("_var")),
                               var_conteo)  %>%
  tba()
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> Ocupado_var </th>
   <th style="text-align:right;"> Desocupado_var </th>
   <th style="text-align:right;"> Inactivo_var </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.9846 </td>
   <td style="text-align:right;"> 0.9969 </td>
  </tr>
</tbody>
</table>

El número de dominios con 2 o más upm `{r sum(indicador_dam$n_upm>=2)}`

### Conteo de dominios con dos o más upm y deff mayor 1 simultáneamente.  


```r
indicador_dam %>% summarise_at(
    .vars = vars(matches("_deff")),
    cont_deff_upm ,
    nupm = indicador_dam$n_upm
  ) %>% tba()
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> Ocupado_deff </th>
   <th style="text-align:right;"> Desocupado_deff </th>
   <th style="text-align:right;"> Inactivo_deff </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 47.2222 </td>
   <td style="text-align:right;"> 41.358 </td>
   <td style="text-align:right;"> 50 </td>
  </tr>
</tbody>
</table>

### Selección de los dominios que empleamos en el modelo de área. 
Despues de realizar las validaciones anteriores se establece como regla incluir en el estudio los dominios que posean 
  -   Dos o más upm por dominio. 
  -   Contar con un resultado en el `Deff`
  
Los resultados nos deja con una base de 309 dominios (309 comunas)


```r
indicador_dam1 <- indicador_dam %>% 
  filter(n_upm >= 2, !is.na(Desocupado_deff))
```

### Guardar archivos 
  

```r
saveRDS(object = indicador_dam1, "01 Modelo de area/CHL/2017/Data/base_modelo.Rds")
```

