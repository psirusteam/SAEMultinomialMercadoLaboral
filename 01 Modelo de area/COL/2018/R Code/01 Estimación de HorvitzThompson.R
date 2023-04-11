################################################################################
## Title:        Modelo área para estimación del mercado laboral              ##
## Returns:      Estimación de Horvitz Thompson para los dominios             ##
## Author:       Stalyn Guerrero - Andrés Gutiérrez                           ##
## Date:         02-2023                                                      ##
################################################################################

#Librerias--------------------------------------------------------------------------------------------
rm(list = ls())

library(survey)
library(tidyverse)
library(srvyr)
library(TeachingSampling)
library(haven)

# Lectura de las bases de datos ------------------------------------------------
encuesta <- readRDS('01 Modelo de area/COL/2018/Data/encuestaCOL18N1.rds')
## 
encuesta <-
  encuesta %>%
  transmute(
    dam = dam_ee,
    dam2,
    fep = `_fep`, 
    upm = segmento,
    estrato = paste0(dam, haven::as_factor(area_ee,levels = "values")),
    empleo = condact3
  )
id_dominio <- "dam2"
######
# 1  = [Ocupado]   
# -1 = [NA]        
# 3  = [Inactivo]  
# 2  = [Desocupado]
# 9  = [NR] 

distinct(encuesta, empleo)
table(encuesta$empleo, useNA = "always")

#Creación de objeto diseno--------------------------------------- 

options(survey.lonely.psu= 'adjust' )
diseno <- encuesta %>%
  as_survey_design(
    strata = estrato,
    ids = upm,
    weights = fep,
    nest=T
  )


summary(diseno)
#Estimación del indicador ----------------
indicador_dam <-
  diseno %>% group_by_at(id_dominio) %>% 
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

### Conteos de upms por dominios 
indicador_dam <- encuesta %>% select(id_dominio, upm) %>%
  distinct() %>% 
  group_by_at(id_dominio) %>% 
  tally(name = "n_upm") %>% 
  inner_join(indicador_dam, by = id_dominio)

#Guardar data----------------------------
saveRDS(indicador_dam,'01 Modelo de area/COL/2018/Data/indicador_dam.Rds' )
