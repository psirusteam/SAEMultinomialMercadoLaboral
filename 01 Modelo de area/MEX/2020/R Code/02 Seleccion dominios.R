################################################################################
## Title:        Modelo área para estimación del mercado laboral              ##
## Author:       Stalyn Guerrero - Andrés Gutiérrez                           ##
## Date:         02-2023                                                      ##
################################################################################
rm(list = ls())
library(ggplot2)
library(dplyr)
library(patchwork)
library(kableExtra)
select <- dplyr::select

## Definiendo algunas funciones simples 

NAs_deff <- function(x){sum(is.na(x))}
cont_deff <- function(x){deff = sum(x>1,na.rm = TRUE)}

cont_deff_upm <- function(x, nupm) {
  deff = sum(x > 1 & nupm >= 2, na.rm = TRUE)
}
var_conteo <- function(x)sum(x>0)/length(x)

## Lectura de las estimaciones directas 

indicador_dam <- readRDS('01 Modelo de area/MEX/2020/Data/indicador_dam.Rds')

### Descriptivo de las estimaciones directas 

## Conteo de los dominios con deff no estimados 

indicador_dam %>% summarise_at(.vars = vars(matches("_deff")), NAs_deff) %>%
  kableExtra::kbl(format = "html",
                  ) %>%
  kableExtra::kable_classic(font_size = 40)

# Conteo de los dominios con deff mayores que 1

indicador_dam %>% summarise_at(.vars =vars( matches("_deff")),
                               cont_deff)%>%
  kableExtra::kbl(format = "html",
                  ) %>%
  kableExtra::kable_classic(font_size = 40)
## Porcentaje de dominios con estimación de la varianza mayor a 0

indicador_dam %>% summarise_at(.vars = vars( matches("_var")),
                               var_conteo)  %>%
  kableExtra::kbl(format = "html",
                  ) %>%
  kableExtra::kable_classic(font_size = 40)

## Número de dominios con 2 o más upm
sum(indicador_dam$n_upm>=2)

## Conteo de dominios con dos o más upm y deff mayor 1 simultáneamente.  

  indicador_dam %>% summarise_at(
    .vars = vars(matches("_deff")),
    cont_deff_upm ,
    nupm = indicador_dam$n_upm
  ) %>%
  kableExtra::kbl(format = "html",
  ) %>%
  kableExtra::kable_classic(font_size = 40)


## Selección de los dominios que empleamos en el modelo de área. 
indicador_dam1 <- indicador_dam %>% 
  filter(n_upm >= 2, !is.na(Desocupado_deff),
         !is.na(Ocupado_deff),
         !is.na(Inactivo_deff),
         Desocupado_var > 0.00000001,
         Ocupado_var > 0.00000001,
         Inactivo_var > 0.00000001
         ) 

summary(indicador_dam1)

saveRDS(object = indicador_dam1, "01 Modelo de area/MEX/2020/Data/base_modelo.Rds")
