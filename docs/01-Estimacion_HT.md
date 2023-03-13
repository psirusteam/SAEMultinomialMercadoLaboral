

# Estimación directa para las comunas de Chile. 

-   La Encuesta de Caracterización Socioeconómica Nacional (CASEN) de Chile 2017.
-   Los niveles de estimación o dominios de estudio para los cuales la muestra fue representativa son: nacional, nacional urbano, nacional rural y regional.
-   El diseño corresponde a una muestra probabilística, estratificada y bietápica, siendo los estratos conformados por la dupla Comuna-Área.
-   La base de datos de la encuesta cuenta con 216439 registros, distribuidos en 1637 upm


## Lectura de la encuesta.

Para el procesamiento de la información se emplea el archivo de la encuesta disponible en los repositorios de CEPAL,  las cuales han sido estandarizadas previamente.  

El proceso de la lectura inicia con el cargue de las siguientes librerías.

-   TeachingSampling: Esta librería proporciona herramientas para la enseñanza y práctica de métodos de muestreo. Incluye funciones para generar diseños de muestreo, realizar muestreo aleatorio simple y estratificado, entre otros.

-   survey: Esta librería proporciona herramientas para el análisis de datos de encuestas complejas. Permite crear y manejar diseños de muestreo, ajustar modelos estadísticos a datos de encuestas, estimar errores estándar y realizar pruebas de hipótesis. Es especialmente útil para datos de encuestas que involucren estratificación, conglomerados o ponderación.

-   tidyverse: Esta librería es un conjunto de paquetes diseñados para trabajar juntos y facilitar la manipulación, visualización y modelado de datos en R. Incluye ggplot2 para gráficos, dplyr para manipulación de datos, tidyr para limpieza de datos, readr para lectura de datos, entre otros.

-   srvyr: Esta librería extiende la funcionalidad de la librería survey, permitiendo el uso de la sintaxis "tidyverse" en el análisis de datos de encuestas complejas. Permite la manipulación y visualización de datos de encuestas mediante la integración con el paquete dplyr.

-   haven: Esta librería proporciona herramientas para la lectura, escritura y manejo de datos en formato SPSS, SAS y Stata. Permite la importación y exportación de datos entre R y otros programas estadísticos, así como la manipulación de metadatos y variables etiquetadas.



```r
library(survey)
library(tidyverse)
library(srvyr)
library(TeachingSampling)
library(haven)
```

La lectura de la base es realizada con la siguiente linea de código. 

```r
encuesta <- readRDS('01 Modelo de area/CHL/2017/Data/encuesta_2017.Rds')
```

El siguiente código realiza una serie de transformaciones de datos utilizando el paquete `tidyverse`. Primero, se asigna a la variable `length_upm` la longitud máxima de los valores en la columna `_upm` de la encuesta. De manera similar, se asigna a la variable `length_estrato` la longitud máxima de los valores en la columna `_estrato`. Luego, se utiliza la función `transmute()` para crear nuevas variables y modificar las existentes. Se crea la variable `dam` (División Administrativa Mayor) a partir de la columna `dam_ee`, se convierte en factor y se utiliza la función `str_pad()` para agregar ceros a la izquierda hasta que tenga una longitud de 2 caracteres. De manera similar, se crea la variable `dam2` a partir de la columna `comuna`, se convierte en factor y se utiliza la función `str_pad()` para agregar ceros a la izquierda hasta que tenga una longitud de 5 caracteres. También se crean variables `nombre_dam` y `nombre_dam2` a partir de las mismas columnas `dam_ee` y `comuna`, respectivamente, pero se convierten en factores que utilizan los valores y etiquetas específicas. Se crea la variable `upm` a partir de la columna `_upm`, se utiliza la función `str_pad()` para agregar ceros a la izquierda hasta que tenga una longitud igual a `length_upm`. Se crea la variable `estrato` de manera similar a partir de la columna `_estrato` y utilizando `length_estrato`. Finalmente, se crean las variables `fep` y `empleo`, que se asignan a las columnas `_fep` y `condact3`, respectivamente. El resultado final es un conjunto de datos transformado con nuevas variables creadas y algunas columnas modificadas.







```r
## 
length_upm <- max(nchar(encuesta[["_upm"]]))
length_estrato <- max(nchar(encuesta[["_estrato"]]))

encuesta <-
  encuesta %>%
  transmute(
    dam = as_factor(dam_ee,levels  = "values"),
    dam = str_pad(string = dam, width = 2, pad = "0"),
  
     dam2 = as_factor(comuna,levels  = "values"),
     dam2 = str_pad(string =dam2, width = 5, pad = "0"),
    
    nombre_dam = as_factor(dam_ee,levels  = "labels"),
    nombre_dam2 = as_factor(comuna,levels  = "labels"),
    
    upm = str_pad(string = `_upm`, width = length_upm, pad = "0"),
    estrato = str_pad(string = `_estrato`, width = length_estrato , pad = "0"),
    fep = `_fep`, 
    empleo = as_factor(condact3,levels  = "values"),
    etiquetas_empleo = as_factor(condact3, levels  = "labels")
  )
head(encuesta, 10) %>% tba(cap = "Encuesta trasnformada")
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:unnamed-chunk-4)Encuesta trasnformada</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> dam </th>
   <th style="text-align:left;"> dam2 </th>
   <th style="text-align:left;"> nombre_dam </th>
   <th style="text-align:left;"> nombre_dam2 </th>
   <th style="text-align:left;"> upm </th>
   <th style="text-align:left;"> estrato </th>
   <th style="text-align:right;"> fep </th>
   <th style="text-align:left;"> empleo </th>
   <th style="text-align:left;"> etiquetas_empleo </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 01101 </td>
   <td style="text-align:left;"> Tarapacá </td>
   <td style="text-align:left;"> Iquique </td>
   <td style="text-align:left;"> 01100100001 </td>
   <td style="text-align:left;"> 011001 </td>
   <td style="text-align:right;"> 39 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Ocupado </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 01101 </td>
   <td style="text-align:left;"> Tarapacá </td>
   <td style="text-align:left;"> Iquique </td>
   <td style="text-align:left;"> 01100100001 </td>
   <td style="text-align:left;"> 011001 </td>
   <td style="text-align:right;"> 39 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Ocupado </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 01101 </td>
   <td style="text-align:left;"> Tarapacá </td>
   <td style="text-align:left;"> Iquique </td>
   <td style="text-align:left;"> 01100100001 </td>
   <td style="text-align:left;"> 011001 </td>
   <td style="text-align:right;"> 39 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Ocupado </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 01101 </td>
   <td style="text-align:left;"> Tarapacá </td>
   <td style="text-align:left;"> Iquique </td>
   <td style="text-align:left;"> 01100100001 </td>
   <td style="text-align:left;"> 011001 </td>
   <td style="text-align:right;"> 39 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Ocupado </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 01101 </td>
   <td style="text-align:left;"> Tarapacá </td>
   <td style="text-align:left;"> Iquique </td>
   <td style="text-align:left;"> 01100100001 </td>
   <td style="text-align:left;"> 011001 </td>
   <td style="text-align:right;"> 39 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Ocupado </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 01101 </td>
   <td style="text-align:left;"> Tarapacá </td>
   <td style="text-align:left;"> Iquique </td>
   <td style="text-align:left;"> 01100100001 </td>
   <td style="text-align:left;"> 011001 </td>
   <td style="text-align:right;"> 39 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Inactivo </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 01101 </td>
   <td style="text-align:left;"> Tarapacá </td>
   <td style="text-align:left;"> Iquique </td>
   <td style="text-align:left;"> 01100100001 </td>
   <td style="text-align:left;"> 011001 </td>
   <td style="text-align:right;"> 39 </td>
   <td style="text-align:left;"> -1 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 01101 </td>
   <td style="text-align:left;"> Tarapacá </td>
   <td style="text-align:left;"> Iquique </td>
   <td style="text-align:left;"> 01100100001 </td>
   <td style="text-align:left;"> 011001 </td>
   <td style="text-align:right;"> 39 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Ocupado </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 01101 </td>
   <td style="text-align:left;"> Tarapacá </td>
   <td style="text-align:left;"> Iquique </td>
   <td style="text-align:left;"> 01100100001 </td>
   <td style="text-align:left;"> 011001 </td>
   <td style="text-align:right;"> 39 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Desocupado </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 01101 </td>
   <td style="text-align:left;"> Tarapacá </td>
   <td style="text-align:left;"> Iquique </td>
   <td style="text-align:left;"> 01100100001 </td>
   <td style="text-align:left;"> 011001 </td>
   <td style="text-align:right;"> 39 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Ocupado </td>
  </tr>
</tbody>
</table>

Realizando algunas validaciones sobre la base de datos:


```r
encuesta %>% group_by( empleo, etiquetas_empleo) %>% tally() %>% 
  tba(cap = "Niveles de la variable empleo")
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:unnamed-chunk-5)Niveles de la variable empleo</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> empleo </th>
   <th style="text-align:left;"> etiquetas_empleo </th>
   <th style="text-align:right;"> n </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> -1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 41363 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Ocupado </td>
   <td style="text-align:right;"> 92417 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Desocupado </td>
   <td style="text-align:right;"> 8671 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Inactivo </td>
   <td style="text-align:right;"> 73567 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> NR </td>
   <td style="text-align:right;"> 421 </td>
  </tr>
</tbody>
</table>

## Definición del diseño muestral 

El primer comando options establece una opción global para el paquete survey, que se utiliza para realizar análisis de datos con muestreo complejo. En este caso, se establece la opción survey.lonely.psu en "adjust", lo que significa que los PSUs (unidades primarias de muestreo) solitarios se ajustarán automáticamente en los análisis.

El segundo comando crea un objeto de diseño de encuesta utilizando el paquete `srvyr`, a partir del conjunto de datos encuesta previamente transformado. La función `as_survey_design()` toma varios argumentos que especifican cómo se realizó el diseño de la encuesta. En este caso, se especifica que la variable `estrato` se utiliza como `estrato`, la variable `upm` se utiliza como identificador de PSU, y la variable `fep` se utiliza como peso de muestreo. También se establece el argumento `nest` en `TRUE` para indicar que el objeto de diseño incluirá los datos anidados dentro de cada PSU.


```r
options(survey.lonely.psu= 'adjust' )
diseno <- encuesta %>%
  as_survey_design(
    strata = estrato,
    ids = upm,
    weights = fep,
    nest=T
  )
```


## Estimación directa del indicador

Para realizar la estimación directa para cada dominio de la variable `dam2` en el objeto de diseño `diseno` se emplea el siguiente código. Primero, el objeto de diseño se agrupa por la variable `dam2` utilizando la función `group_by_at()`. Luego, se filtran las observaciones para incluir solo aquellas en las que la variable empleo es igual a 1, 2 o 3 (es decir, ocupado, desocupado o inactivo, respectivamente) utilizando la función `filter()`.

A continuación, se utilizan las funciones `sum()` y `unweighted` para calcular el número de observaciones en cada nivel de la variable empleo. Luego, se utilizan las funciones `survey_mean()` para calcular la media del indicador de empleo en cada nivel de la variable `empleo` (es decir, la proporción de personas ocupadas, desocupadas e inactivas). Los argumentos `vartype = c("se", "var")` y `deff = T` se utilizan para estimar el error estándar y la varianza de la media;  el efecto del diseño, respectivamente.


```r
indicador_dam <-
  diseno %>% group_by_at("dam2") %>% 
  filter(empleo %in% c(1:3)) %>%
  summarise(
    n_ocupado = unweighted(sum(empleo == 1)),
    n_desocupado = unweighted(sum(empleo == 2)),
    n_inactivo = unweighted(sum(empleo == 3)),
    Ocupado = survey_mean(empleo == 1,
      vartype = c("se",  "var"),
      deff = T
    ),
    Desocupado = survey_mean(empleo == 2,
                          vartype = c("se",  "var"),
                          deff = T
    ),
    Inactivo = survey_mean(empleo == 3,
                          vartype = c("se",  "var"),
                          deff = T
    )
  )

indicador_dam %>% select(dam2,Ocupado,Desocupado,Inactivo) %>% 
  head(10) %>% 
  tba(cap = "Estimación directa")
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:unnamed-chunk-7)Estimación directa</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> dam2 </th>
   <th style="text-align:right;"> Ocupado </th>
   <th style="text-align:right;"> Desocupado </th>
   <th style="text-align:right;"> Inactivo </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 01101 </td>
   <td style="text-align:right;"> 0.5975 </td>
   <td style="text-align:right;"> 0.0478 </td>
   <td style="text-align:right;"> 0.3547 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01107 </td>
   <td style="text-align:right;"> 0.5619 </td>
   <td style="text-align:right;"> 0.0539 </td>
   <td style="text-align:right;"> 0.3842 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01401 </td>
   <td style="text-align:right;"> 0.5369 </td>
   <td style="text-align:right;"> 0.0499 </td>
   <td style="text-align:right;"> 0.4133 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01402 </td>
   <td style="text-align:right;"> 0.7529 </td>
   <td style="text-align:right;"> 0.0000 </td>
   <td style="text-align:right;"> 0.2471 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01404 </td>
   <td style="text-align:right;"> 0.5772 </td>
   <td style="text-align:right;"> 0.0095 </td>
   <td style="text-align:right;"> 0.4132 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 01405 </td>
   <td style="text-align:right;"> 0.5630 </td>
   <td style="text-align:right;"> 0.0188 </td>
   <td style="text-align:right;"> 0.4182 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02101 </td>
   <td style="text-align:right;"> 0.5448 </td>
   <td style="text-align:right;"> 0.0557 </td>
   <td style="text-align:right;"> 0.3995 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02102 </td>
   <td style="text-align:right;"> 0.6002 </td>
   <td style="text-align:right;"> 0.0680 </td>
   <td style="text-align:right;"> 0.3319 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02103 </td>
   <td style="text-align:right;"> 0.6766 </td>
   <td style="text-align:right;"> 0.0838 </td>
   <td style="text-align:right;"> 0.2397 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02104 </td>
   <td style="text-align:right;"> 0.5391 </td>
   <td style="text-align:right;"> 0.0576 </td>
   <td style="text-align:right;"> 0.4034 </td>
  </tr>
</tbody>
</table>

Ahora, se realiza el conteo de las PSU por dominios y se incorpora la información a la base que resulta del paso anterior. 


```r
indicador_dam <- encuesta %>% dplyr::select(dam2, upm) %>%
  distinct() %>% 
  group_by(dam2) %>% 
  tally(name = "n_upm") %>% 
  inner_join(indicador_dam, by = "dam2")
```

Guardar los archivos resultantes


```r
saveRDS(indicador_dam,'01 Modelo de area/CHL/2017/Data/indicador_dam.Rds' )
```

