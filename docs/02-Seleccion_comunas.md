
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

## Lectura de librerías


```r
library(ggplot2)
library(dplyr)
library(patchwork)
library(kableExtra)
select <- dplyr::select
```

## Definiendo algunas funciones útiles.  


```r
NAs_deff <- function(x){sum(is.na(x))}
cont_deff <- function(x){deff = sum(x>1,na.rm = TRUE)}

cont_deff_upm <- function(x, nupm) {
  deff = sum(x > 1 & nupm >= 2, na.rm = TRUE)
}
var_conteo <- function(x)sum(x>0)/length(x)
```

## Lectura de las estimaciones directas

```r
indicador_dam <- readRDS('01 Modelo de area/CHL/2017/Data/indicador_dam.Rds')
```

## Descriptivo de las estimaciones directas 

### Conteo de los dominios con deff no estimados 


```r
indicador_dam %>% summarise_at(.vars = vars(matches("_deff")), NAs_deff) %>%
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
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>

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

### Número de dominios con 2 o más upm


```r
sum(indicador_dam$n_upm>=2)
```

```
## [1] 310
```

### Conteo de dominios con dos o más upm y deff mayor 1 simultáneamente.  


```r
indicador_dam %>% summarise_at(
    .vars = vars(matches("_deff")),
    cont_deff_upm ,
    nupm = indicador_dam$n_upm
  ) %>%tba()
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

### Selección de los dominios que empleamos en el modelo de área. 


```r
indicador_dam1 <- indicador_dam %>% 
  filter(n_upm >= 2, !is.na(Desocupado_deff))
```

### Guardar archivos 

```r
saveRDS(object = indicador_dam1, "01 Modelo de area/CHL/2017/Data/base_modelo.Rds")
```

