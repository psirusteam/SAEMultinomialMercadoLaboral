
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

# Validación visual del Benchmark

## Lectura de librerias 

plotly: Esta librería permite la creación de gráficos interactivos en R. Es muy útil para visualizar datos en línea y crear gráficos dinámicos con herramientas como zoom, tooltips y animaciones.

dplyr: Permite manipular y transformar datos de forma fácil y rápida. Tiene una sintaxis sencilla y clara para filtrar, seleccionar, agrupar y resumir datos.

  -   `tidyr`: Ayuda a transformar datos de un formato a otro. Es muy útil para trabajar con datos desordenados o de formato ancho.

  -   `forcats`: Esta librería ayuda en la manipulación de variables categóricas en R. Permite crear, manipular y transformar factores.

  -   `survey`: Es una librería que permite el análisis de datos de encuestas utilizando métodos estadísticos apropiados para muestras complejas. Permite diseñar muestras estratificadas, con conglomerados y ponderadas.

  -   `srvyr`: Es una extensión de la librería dplyr para análisis de datos de encuestas complejas. Facilita la manipulación y visualización de datos de encuestas complejas.

  -   `haven`: Es una librería para la importación y exportación de datos de otros programas estadísticos, como SPSS, SAS y Stata.

  -   `stringr`: Ayuda en la manipulación de cadenas de caracteres en R. Permite la extracción de patrones de cadenas de caracteres, la manipulación de cadenas y la limpieza de datos.

  -   `patchwork`: Es una librería para la creación de gráficos complejos en R. Permite la combinación de múltiples gráficos en uno solo, con la posibilidad de modificar el tamaño y la posición de cada uno.



```r
library(plotly)
library(dplyr)
library(tidyr)
library(forcats)
library(survey)
library(srvyr)
library(haven)
library(stringr)
library(patchwork)
```

## Lectura de encuesta.

El código está leyendo un archivo RDS llamado `encuesta_2017.Rds` y almacenándolo en un objeto llamado `encuesta`. Luego está leyendo otro archivo RDS que tiene el nombre `estimaciones_Bench_fit_multinomial_con_covariable.rds` y almacenando el contenido en un objeto llamado `estimaciones`. Finalmente, está filtrando las filas en el objeto `estimaciones` donde la columna `dam` es `NA`.


```r
encuesta <- readRDS("01 Modelo de area/CHL/2017/Data/encuesta_2017.Rds")
mod <- "fit_multinomial_con_covariable"
infile <- paste0("01 Modelo de area/CHL/2017/Data/estimaciones_Bench_",mod,".rds") 

estimaciones <- readRDS(infile)
estimaciones %>% filter(is.na(dam))
```

```
## # A tibble: 0 × 34
## # … with 34 variables: dam2 <chr>, n_upm <int>, n_ocupado <int>,
## #   n_desocupado <int>, n_inactivo <int>, Ocupado <dbl>, Ocupado_se <dbl>,
## #   Ocupado_var <dbl>, Ocupado_deff <dbl>, Desocupado <dbl>,
## #   Desocupado_se <dbl>, Desocupado_var <dbl>, Desocupado_deff <dbl>,
## #   Inactivo <dbl>, Inactivo_se <dbl>, Inactivo_var <dbl>, Inactivo_deff <dbl>,
## #   Ocupado_mod <dbl>, Desocupado_mod <dbl>, Inactivo_mod <dbl>,
## #   Ocupado_mod_sd <dbl>, Desocupado_mod_sd <dbl>, Inactivo_mod_sd <dbl>, …
```

## Estimación agregada 

Esta parte de código toma el objeto estimaciones y lo resume por el campo `dam`. Para cada comuna, se calcula la suma ponderada de las variables _Ocupado_mod_, _Desocupado_mod_ e _Inactivo_mod_ (estimaciones del modelo) y de las variables _Ocupado_Bench_, _Desocupado_Bench_ e _Inactivo_Bench_ (estimaciones de la encuesta benchmark). Esto resulta en una tabla con seis columnas: `dam`, `Ocupado_mod`, `Desocupado_mod`, `Inactivo_mod`, `Ocupado_Bench`, `Desocupado_Bench` y `Inactivo_Bench`.


```r
############## Estimación agregada ######################
estimaciones_agregada <- estimaciones %>%
  group_by(dam) %>% 
  summarise(
    Ocupado_mod = sum(wi*Ocupado_mod),
    Desocupado_mod = sum(wi*Desocupado_mod),
    Inactivo_mod = sum(wi*Inactivo_mod),
    Ocupado_Bench = sum(wi*Ocupado_Bench),
            Desocupado_Bench = sum(wi*Desocupado_Bench),
            Inactivo_Bench = sum(wi*Inactivo_Bench))
```

## Creación de objeto diseno

Este código comienza definiendo las longitudes máximas de los campos `_upm` y `_estrato` en la tabla `encuesta`, y luego utiliza la función `transmute()` del paquete `dplyr` para crear una nueva tabla `encuesta` que contiene las columnas `dam`, `nombre_dam`, `upm`, `estrato`, `fep` y `empleo`.

Para la columna `dam`, se utiliza la variable `dam_ee` para asignar un valor alfanumérico de dos dígitos para cada región y dominio (comuna). La función `as_factor()` convierte la variable a factor, con los valores y etiquetas correspondientes. La función `str_pad()` es utilizada para agregar ceros a la izquierda a la cadena de caracteres si la longitud de la cadena es menor que `width`.

Para las columnas `upm` y `estrato`, la función `str_pad()` es utilizada para agregar ceros a la izquierda a la cadena de caracteres si la longitud de la cadena es menor que `width`.

Las columnas `fep` y `empleo` simplemente se asignan a la tabla encuesta desde las columnas correspondientes de la tabla original.

Después, se establece la opción `survey.lonely.psu` como `adjust`, lo que indica al paquete `survey` cómo tratar unidades de muestreo solitarias. A continuación, se utiliza la función `as_survey_design()` para crear un objeto de diseño de encuesta que puede ser utilizado para realizar análisis con el paquete `survey`.

Se especifica que la variable `estrato` se utiliza como `estrato` y que `upm` se utiliza como identificador de unidad primaria de muestreo. Además, se incluye la variable `fep` como peso de diseño de muestreo. Se utiliza la opción `nest=T` para especificar que se desea crear un diseño anidado, lo que significa que cada unidad primaria de muestreo puede tener varias unidades secundarias de muestreo.


```r
length_upm <- max(nchar(encuesta[["_upm"]]))
length_estrato <- max(nchar(encuesta[["_estrato"]]))

encuesta <-
  encuesta %>%
  transmute(
    dam = as_factor(dam_ee,levels  = "values"),
    dam = str_pad(string =dam, width = 2, pad = "0"),
    nombre_dam = as_factor(dam_ee,levels  = "labels"),
    upm = str_pad(string = `_upm`, width = length_upm, pad = "0"),
    estrato = str_pad(string = `_estrato`, width = length_estrato , pad = "0"),
    fep = `_fep`, 
    empleo = condact3
  )


options(survey.lonely.psu= 'adjust' )
diseno <- encuesta %>%
  as_survey_design(
    strata = estrato,
    ids = upm,
    weights = fep,
    nest=T
  )
```

## Estimación del indicador

El código proporciona un indicador agregado para los tres categorias de la variable empleo en cada región. Primero, se agrupa `diseno` por región (`dam`) y se filtran solo los casos con empleo entre `1` y `3`. Luego, se utilizan las funciones `survey_ratio()` del paquete `survey` para calcular la proporción de personas en cada categoría de empleo ( _Ocupado_, _Desocupado_ e _Inactivo_) en cada región. Por último, se seleccionan las columnas de `dam`, `Ocupado`, `Desocupado` e `Inactivo` para crear un marco de datos que incluya el indicador agregado.



```r
indicador_agregado <-
  diseno %>% group_by_at("dam") %>% 
  filter(empleo %in% c(1:3)) %>%
  summarise(
    nd = unweighted(n()),
    Ocupado = survey_ratio(numerator = (empleo == 1), 
                           denominator = 1,vartype = c("ci"), level = 0.95 ),
    Desocupado = survey_ratio(numerator =( empleo == 2),denominator = 1,
                              vartype = c("ci"), level = 0.95                             
    ),
    Inactivo = survey_ratio(numerator =  (empleo == 3), denominator = 1,
                            vartype = c("ci"), level = 0.95                             
    )
  )


data_plot <- left_join(estimaciones_agregada, indicador_agregado)
```

## Gráfica de validación 

La gráfica de validación compara las estimaciones directas, las del modelo y las ajustadas por benchmarking. En este código se genera un gráfico utilizando la librería `ggplot2` de R. Primero, se seleccionan las variables correspondientes a las estimaciones del modelo para la categoría de empleo correspondiente ( _Ocupado_, _Desocupado_ e _Inactivo_), se transforman los datos a formato largo con la función `gather()` y se asignan etiquetas para las distintas estimaciones. Luego, se crea un `data.frame` con los límites de los intervalos de confianza para las estimaciones obtenidas directamente de la encuesta. Finalmente, se utiliza la función `ggplot()` para graficar los datos, agregando una capa para mostrar los intervalos de confianza y otra capa para mostrar los valores de las estimaciones.

### Ocupado 


```r
temp_ocupado <- data_plot %>% select(dam,nd, starts_with("Ocupado"))


temp_ocupado_1 <- temp_ocupado %>% select(-Ocupado_low, -Ocupado_upp) %>%
  gather(key = "Estimacion",value = "value", -nd,-dam) %>% 
  mutate(Estimacion = case_when(Estimacion == "Ocupado_mod" ~ "Modelo de área",
                                Estimacion == "Ocupado_Bench" ~ "Modelo de área (bench)",
                                Estimacion == "Ocupado"~ "Directo"))
lims_IC_ocupado <-  temp_ocupado %>%
  select(dam,nd,value = Ocupado,Ocupado_low, Ocupado_upp) %>% 
  mutate(Estimacion = "Directo")

p_ocupado <- ggplot(temp_ocupado_1,
            aes(
              x = fct_reorder2(dam, dam, nd),
              y = value,
              shape = Estimacion,
              color = Estimacion
            )) +
  geom_errorbar(
    data = lims_IC_ocupado,
    aes(ymin = Ocupado_low ,
        ymax = Ocupado_upp, x = dam),
    width = 0.2,
    linewidth = 1
  )  +
  geom_jitter(size = 3)+
  labs(x = "Dam", title = "Ocupado")
```

### Desocupado


```r
temp_Desocupado <- data_plot %>% select(dam,nd, starts_with("Desocupado"))


temp_Desocupado_1 <- temp_Desocupado %>% select(-Desocupado_low, -Desocupado_upp) %>%
  gather(key = "Estimacion",value = "value", -nd,-dam) %>% 
  mutate(Estimacion = case_when(Estimacion == "Desocupado_mod" ~ "Modelo de área",
                                Estimacion == "Desocupado_Bench" ~ "Modelo de área (bench)",
                                Estimacion == "Desocupado"~ "Directo"))
lims_IC_Desocupado <-  temp_Desocupado %>%
  select(dam,nd,value = Desocupado,Desocupado_low, Desocupado_upp) %>% 
  mutate(Estimacion = "Directo")

p_Desocupado <- ggplot(temp_Desocupado_1,
                    aes(
                      x = fct_reorder2(dam, dam, nd),
                      y = value,
                      shape = Estimacion,
                      color = Estimacion
                    )) +
  geom_errorbar(
    data = lims_IC_Desocupado,
    aes(ymin = Desocupado_low ,
        ymax = Desocupado_upp, x = dam),
    width = 0.2,
    linewidth = 1
  )  +
  geom_jitter(size = 3)+
  labs(x = "Dam", title = "Desocupado")
```

### Inactivo 


```r
temp_Inactivo <- data_plot %>% select(dam,nd, starts_with("Inactivo"))


temp_Inactivo_1 <- temp_Inactivo %>% select(-Inactivo_low, -Inactivo_upp) %>%
  gather(key = "Estimacion",value = "value", -nd,-dam) %>% 
  mutate(Estimacion = case_when(Estimacion == "Inactivo_mod" ~ "Modelo de área",
                                Estimacion == "Inactivo_Bench" ~ "Modelo de área (bench)",
                                Estimacion == "Inactivo"~ "Directo"))
lims_IC_Inactivo <-  temp_Inactivo %>%
  select(dam,nd,value = Inactivo,Inactivo_low, Inactivo_upp) %>% 
  mutate(Estimacion = "Directo")

p_Inactivo <- ggplot(temp_Inactivo_1,
                       aes(
                         x = fct_reorder2(dam, dam, nd),
                         y = value,
                         shape = Estimacion,
                         color = Estimacion
                       )) +
  geom_errorbar(
    data = lims_IC_Inactivo,
    aes(ymin = Inactivo_low ,
        ymax = Inactivo_upp, x = dam),
    width = 0.2,
    linewidth = 1
  )  +
  geom_jitter(size = 3)+
  labs(x = "Dam", title = "Inactivo")

p_ocupado/p_Desocupado/p_Inactivo
```

<img src="06-Grafico_BENCH_files/figure-html/unnamed-chunk-9-1.svg" width="672" />

