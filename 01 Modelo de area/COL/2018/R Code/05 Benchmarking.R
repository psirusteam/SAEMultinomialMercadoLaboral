################################################################################
## Title:        Modelo área para estimación del mercado laboral              ##
## Returns:      Estimaciones ajustadas por Benchmark                         ##
## Author:       Stalyn Guerrero - Andrés Gutiérrez                           ##
## Date:         02-2023                                                      ##
################################################################################
## Benchmark

#Librerias--------------------------------------------------------------------------------------------
rm(list = ls())

library(survey)
library(tidyverse)
library(srvyr)
library(TeachingSampling)
library(haven)
library(sampling)

# Lectura de las bases de datos ------------------------------------------------
encuesta <- readRDS("01 Modelo de area/COL/2018/Data/encuestaCOL18N1.rds")
infile <- paste0("01 Modelo de area/COL/2018/Data/estimaciones.rds") 
out_estimacion <- paste0("01 Modelo de area/COL/2018/Data/estimaciones_Bench.rds") 

estimaciones <- readRDS(infile)

## Conteos agregados por dam y dam2 
conteo_pp_dam <- readRDS("01 Modelo de area/COL/2018/Data/censo_dam2.rds") %>%
  filter(edad > 1)  %>% 
  group_by(dam , dam2) %>% 
  summarise(pp_dam2 = sum(n),.groups = "drop") %>% 
mutate(pp_dam = sum(pp_dam2))

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

#Creación de objeto diseno--------------------------------------- 

options(survey.lonely.psu= 'adjust' )
diseno <- encuesta %>%
  as_survey_design(
    strata = estrato,
    ids = upm,
    weights = fep,
    nest=T
  )



#Cálculo del indicador ----------------
indicador_agregado <-
  diseno %>%
  filter(empleo %in% c(1:3)) %>%
  summarise(
    Ocupado = survey_ratio(numerator = (empleo == 1), 
                           denominator = 1 ),
    Desocupado = survey_ratio(numerator =( empleo == 2),denominator = 1
                             
    ),
    Inactivo = survey_ratio(numerator =  (empleo == 3), denominator = 1
                           
    )
  ) %>% select(Ocupado,Desocupado, Inactivo)

# temp <-
#   gather(indicador_agregado, key = "agregado", 
#          value = "estimacion", -dam) %>%
#   mutate(nombre = paste0("dam_", dam,"_", agregado))

Razon_empleo <- indicador_agregado


###########################################
###########################################
###           Benchmarking              ###
###     (Gutiérrez - Guerrero, 2022)    ###
###########################################
###########################################
names_cov <-  "Nacional"
estimaciones_mod <- estimaciones %>% transmute(
  dam = substr(dam2,1,2),
  dam2,Ocupado_mod,Desocupado_mod,Inactivo_mod) %>% 
  inner_join(conteo_pp_dam ) %>% 
  mutate(wi = pp_dam2/pp_dam,
         Nacional = "1")


estimaciones_mod %>% summarise(wi = sum(wi)) %>% 
  data.frame()

estimaciones_mod %<>%
  fastDummies::dummy_cols(select_columns = names_cov,
                          remove_selected_columns = FALSE)

Xdummy <- estimaciones_mod %>% select(matches("Nacional_")) %>% 
  mutate_at(vars(matches("_\\d")) ,
            list(Ocupado = function(x) x*estimaciones_mod$Ocupado_mod,
                 Desocupado = function(x) x*estimaciones_mod$Desocupado_mod,
                 Inactivo = function(x) x*estimaciones_mod$Inactivo_mod)) %>% 
  select((matches("Ocupado|Desocupado|Inactivo"))) 

## Validación de la concordancia del nombre 
colnames(Xdummy) == names(Razon_empleo)
data.frame(Modelo = colSums(Xdummy*estimaciones_mod$wi),
Estimacion_encuesta = t(Razon_empleo))

names_ocupado <- grep(pattern = "^O", x = colnames(Xdummy),value = TRUE)
names_descupados <- grep(pattern = "^D", x = colnames(Xdummy),value = TRUE)
names_inactivo <- grep(pattern = "^I", x = colnames(Xdummy),value = TRUE)


## Ocupado 
gk_ocupado <- calib(Xs = Xdummy[,names_ocupado] %>% as.matrix(), 
            d =  estimaciones_mod$wi,
            total = Razon_empleo[names_ocupado] %>% as.matrix(),
            method="linear",max_iter = 5000,) 

checkcalibration(Xs = Xdummy[,names_ocupado] %>% as.matrix(), 
                 d =estimaciones_mod$wi,
                 total = Razon_empleo[names_ocupado] %>% as.matrix(),
                 g = gk_ocupado,)

summary(gk_ocupado)
## Desocupado 
gk_desocupado <- calib(Xs = Xdummy[,names_descupados]%>% as.matrix(), 
                    d =  estimaciones_mod$wi,
                    total = Razon_empleo[names_descupados]%>% as.matrix(),
                    method="linear",max_iter = 5000,) 

checkcalibration(Xs = Xdummy[,names_descupados]%>% as.matrix(), 
                 d =estimaciones_mod$wi,
                 total = Razon_empleo[names_descupados]%>% as.matrix(),
                 g = gk_desocupado,)

summary(gk_desocupado)
## Inactivo
gk_Inactivo <- calib(Xs = Xdummy[,names_inactivo]%>% as.matrix(), 
                    d =  estimaciones_mod$wi,
                    total = Razon_empleo[names_inactivo]%>% as.matrix(),
                    method="linear",max_iter = 5000,) 

checkcalibration(Xs = Xdummy[,names_inactivo]%>% as.matrix(), 
                 d =estimaciones_mod$wi,
                 total = Razon_empleo[names_inactivo]%>% as.matrix(),
                 g = gk_Inactivo,)

summary(gk_Inactivo)


jpeg(file = "01 Modelo de area/COL/2018/Output/Plot_Bench_gk.jpeg")
par(mfrow = c(1,3))
hist(gk_ocupado)
hist(gk_desocupado)
hist(gk_Inactivo)
dev.off()

estimacionesBench <- estimaciones_mod %>%
  mutate(gk_ocupado, gk_desocupado, gk_Inactivo) %>%
  transmute(
    dam,
    dam2,
    wi,gk_ocupado, gk_desocupado, gk_Inactivo,
    Ocupado_Bench = Ocupado_mod*gk_ocupado,
    Desocupado_Bench = Desocupado_mod*gk_desocupado,
    Inactivo_Bench = Inactivo_mod*gk_Inactivo
  ) 
#############################33
# validación

estimacionesBench %>%
  summarise(Ocupado_Bench = sum(wi*Ocupado_Bench),
            Desocupado_Bench = sum(wi*Desocupado_Bench),
            Inactivo_Bench = sum(wi*Inactivo_Bench)) 
  
indicador_agregado

estimaciones <- inner_join(estimaciones,estimacionesBench)


# Ordenando la base con las estimaciones directas y 
# predichas

saveRDS(object = estimaciones, file = out_estimacion)


