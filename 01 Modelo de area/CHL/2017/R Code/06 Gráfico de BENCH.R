################################################################################
## Title:        Modelo área para estimación del mercado laboral              ##
## Author:       Stalyn Guerrero - Andrés Gutiérrez                           ##
## Date:         02-2023                                                      ##
################################################################################
## validación visual del Benchmark
rm(list = ls())
library(plotly)
library(dplyr)
library(tidyr)
library(forcats)
library(survey)
library(srvyr)

encuesta <- readRDS("01 Modelo de area/CHL/2017/Data/encuesta_2017.Rds")
mod <- "fit_multinomial_con_covariable"

infile <- paste0("01 Modelo de area/CHL/2017/Data/estimaciones_Bench_",mod,".rds") 

estimaciones <- readRDS(infile)
estimaciones %>% filter(is.na(dam))
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

#Creación de objeto diseno--------------------------------------- 
## 
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


#Estimación del indicador ----------------
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

## Ocupado 
temp_ocupado <- data_plot %>% select(dam,nd, starts_with("Ocupado"))


temp_ocupado_1 <- temp_ocupado %>% select(-Ocupado_low, -Ocupado_upp) %>%
  gather(key = "Estimacion",value = "value", -nd,-dam) %>% 
  mutate(Estimacion = case_when(Estimacion == "Ocupado_mod" ~ "Modelo de area",
                                Estimacion == "Ocupado_Bench" ~ "Modelo de area (bench)",
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

## Desocupado 

temp_Desocupado <- data_plot %>% select(dam,nd, starts_with("Desocupado"))


temp_Desocupado_1 <- temp_Desocupado %>% select(-Desocupado_low, -Desocupado_upp) %>%
  gather(key = "Estimacion",value = "value", -nd,-dam) %>% 
  mutate(Estimacion = case_when(Estimacion == "Desocupado_mod" ~ "Modelo de area",
                                Estimacion == "Desocupado_Bench" ~ "Modelo de area (bench)",
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


## Inactivo 

temp_Inactivo <- data_plot %>% select(dam,nd, starts_with("Inactivo"))


temp_Inactivo_1 <- temp_Inactivo %>% select(-Inactivo_low, -Inactivo_upp) %>%
  gather(key = "Estimacion",value = "value", -nd,-dam) %>% 
  mutate(Estimacion = case_when(Estimacion == "Inactivo_mod" ~ "Modelo de area",
                                Estimacion == "Inactivo_Bench" ~ "Modelo de area (bench)",
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
