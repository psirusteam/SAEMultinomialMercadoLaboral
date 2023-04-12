################################################################################
## Title:        Modelo área para estimación del mercado laboral              ##
## Returns:      Predicciones del modelo de area y estadísticas de calidad    ##
## Author:       Stalyn Guerrero - Andrés Gutiérrez                           ##
## Date:         02-2023                                                      ##
################################################################################

#######################
###--- Librerías ---###
#######################
rm(list = ls())
library(stringr)
library(magrittr)
library(ggplot2)
library(patchwork)
library(tidyverse)
library(forcats)
library(cmdstanr)
library(bayesplot)
library(posterior)
select <- dplyr::select

#### Lectura de bases de datos 
indicador_dam <- readRDS('01 Modelo de area/PER/2017/Data/base_modelo.Rds')
dam_pred <- readRDS('01 Modelo de area/PER/2017/Data/dam_pred.rds')


infile <- paste0("01 Modelo de area/PER/2017/Data/fit_rtanmultinomial_con_covariable_satelite.Rds")
out_estimacion <- paste0("01 Modelo de area/PER/2017/Data/estimaciones.rds") 

fit <- readRDS(infile) 


## Estimacion directa 
P <- 3 
D <- nrow(indicador_dam)
D1 <- nrow(dam_pred)
theta_dir <- indicador_dam %>%  
  transmute(dam2,
    n = n_desocupado + n_ocupado + n_inactivo,
            Ocupado, Desocupado, Inactivo) 

## Estimación del modelo. 
theta_obs <- summary(fit, pars = "theta")$summary[, "mean"]
theta_pred <- summary(fit, pars = "theta_pred")$summary[, "mean"]

## Ordenando la matrix de theta 
theta_obs_ordenado <- matrix(theta_obs, 
                             nrow = D,
                             ncol = P,byrow = TRUE) 

colnames(theta_obs_ordenado) <- c("Ocupado_mod", "Desocupado_mod", "Inactivo_mod")
theta_obs_ordenado%<>% as.data.frame()
theta_obs_ordenado <- cbind(dam2 = indicador_dam$dam2,
                            theta_obs_ordenado)

theta_pred_ordenado <- matrix(theta_pred, 
                             nrow = D1,
                             ncol = P,byrow = TRUE)

colnames(theta_pred_ordenado) <- c("Ocupado_mod", "Desocupado_mod", "Inactivo_mod")
theta_pred_ordenado%<>% as.data.frame()
theta_pred_ordenado <- cbind(dam2 = dam_pred$dam2, theta_pred_ordenado)


estimaciones_obs <- full_join(theta_dir, 
                              bind_rows(theta_obs_ordenado,theta_pred_ordenado)) 


############## coeficiente de variación 
theta_obs_sd <- summary(fit, pars = "theta")$summary[, "sd"]
theta_pred_sd <- summary(fit, pars = "theta_pred")$summary[, "sd"]

theta_obs_ordenado_sd <- matrix(theta_obs_sd, 
                             nrow = D,
                             ncol = P,byrow = TRUE) 

colnames(theta_obs_ordenado_sd) <- c("Ocupado_mod_sd", "Desocupado_mod_sd", "Inactivo_mod_sd")
theta_obs_ordenado_sd%<>% as.data.frame()
theta_obs_ordenado_sd <- cbind(dam2 = indicador_dam$dam2,
                            theta_obs_ordenado_sd)
theta_obs_ordenado_cv <- theta_obs_ordenado_sd[,-1]/theta_obs_ordenado[,-1]

colnames(theta_obs_ordenado_cv) <- c("Ocupado_mod_cv", "Desocupado_mod_cv", "Inactivo_mod_cv")

theta_obs_ordenado_cv <- cbind(dam2 = indicador_dam$dam2,
                               theta_obs_ordenado_cv)

theta_pred_ordenado_sd <- matrix(theta_pred_sd, 
                              nrow = D1,
                              ncol = P,byrow = TRUE)

colnames(theta_pred_ordenado_sd) <- c("Ocupado_mod_sd", "Desocupado_mod_sd", "Inactivo_mod_sd")
theta_pred_ordenado_sd%<>% as.data.frame()
theta_pred_ordenado_sd <- cbind(dam2 = dam_pred$dam2, theta_pred_ordenado_sd)

theta_pred_ordenado_cv <- theta_pred_ordenado_sd[,-1]/theta_pred_ordenado[,-1]

colnames(theta_pred_ordenado_cv) <- c("Ocupado_mod_cv", "Desocupado_mod_cv", "Inactivo_mod_cv")

theta_pred_ordenado_cv <- cbind(dam2 = dam_pred$dam2, theta_pred_ordenado_cv)

############# uniendo las estimaciones, sd, cv #########
theta_obs_ordenado <- full_join(theta_obs_ordenado,theta_obs_ordenado_sd) %>% 
  full_join(theta_obs_ordenado_cv)

theta_pred_ordenado <- full_join(theta_pred_ordenado,theta_pred_ordenado_sd) %>% 
  full_join(theta_pred_ordenado_cv)


estimaciones_obs <- full_join(indicador_dam,
                              bind_rows(theta_obs_ordenado, theta_pred_ordenado))



p_ocupado <- ggplot(data = estimaciones_obs, 
       aes(y = Ocupado, 
           x = Ocupado_mod
           )) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, col = "red")  +
  labs(x = "Modelo de area",
                      y = "Estimacion directa" , 
       title = "Ocupado")+
  theme_bw(base_size = 20)

p_Desocupado <- ggplot(data = estimaciones_obs, 
                    aes(y = Desocupado, 
                        x = Desocupado_mod
                    )) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, col = "red")  +
  labs(x = "Modelo de area",
       y = "Estimacion directa", 
       title = "Desocupado")+
  theme_bw(base_size = 20)

p_Inactivo <- ggplot(data = estimaciones_obs, 
                       aes(y = Inactivo, 
                           x = Inactivo_mod
                       )) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, col = "red")  +
  labs(x = "Modelo de area",
       y = "Estimacion directa" , 
       title = "Inactivo")+
  theme_bw(base_size = 20)

p_ocupado|p_Desocupado|p_Inactivo

##########################################
## Comparación de los error estándar  
p_ocupado <- ggplot(data = estimaciones_obs, 
                    aes(y = Ocupado_se, 
                        x = Ocupado_mod_sd
                    )) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, col = "red")  +
  labs(x = "Modelo de area",
       y = "Estimacion directa" , 
       title = "Ocupado (se)")+
  theme_bw(base_size = 20)

p_Desocupado <- ggplot(data = estimaciones_obs, 
                       aes(y = Desocupado_se, 
                           x = Desocupado_mod_sd
                       )) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, col = "red")  +
  labs(x = "Modelo de area",
       y = "Estimacion directa", 
       title = "Desocupado (se)")+
  theme_bw(base_size = 20)

p_Inactivo <- ggplot(data = estimaciones_obs, 
                     aes(y = Inactivo_se, 
                         x = Inactivo_mod_sd
                     )) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, col = "red")  +
  labs(x = "Modelo de area",
       y = "Estimacion directa" , 
       title = "Inactivo(se)")+
  theme_bw(base_size = 20)

p_ocupado |
p_Desocupado  |
p_Inactivo 


##########################################
## Comparación de los cv  
p_ocupado <- ggplot(data = estimaciones_obs, 
                    aes(y = Ocupado_se/Ocupado, 
                        x = Ocupado_mod_cv
                    )) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, col = "red")  +
  labs(x = "Modelo de area",
       y = "Estimacion directa" , 
       title = "Ocupado (cv)")+
  theme_bw(base_size = 20)

p_Desocupado <- ggplot(data = estimaciones_obs, 
                       aes(y = Desocupado_se/Desocupado, 
                           x = Desocupado_mod_sd
                       )) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, col = "red")  +
  labs(x = "Modelo de area",
       y = "Estimacion directa", 
       title = "Desocupado(cv)")+
  theme_bw(base_size = 20)

p_Inactivo <- ggplot(data = estimaciones_obs, 
                     aes(y = Inactivo_se/Inactivo, 
                         x = Inactivo_mod_sd
                     )) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, col = "red")  +
  labs(x = "Modelo de area",
       y = "Estimacion directa" , 
       title = "Inactivo(cv)")+
  theme_bw(base_size = 20)

p_ocupado |
  p_Desocupado  |
  p_Inactivo 


### Gráficos de los ppc 
color_scheme_set("brightblue")
theme_set(theme_bw(base_size = 15))
y_pred_B <- as.array(fit, pars = "theta") %>%
  as_draws_matrix()
  
rowsrandom <- sample(nrow(y_pred_B), 100)

theta_1<-  grep(pattern = "1]",x = colnames(y_pred_B),value = TRUE)
theta_2<-  grep(pattern = "2]",x = colnames(y_pred_B),value = TRUE)
theta_3<-  grep(pattern = "3]",x = colnames(y_pred_B),value = TRUE)
y_pred1 <- y_pred_B[rowsrandom,theta_1 ]
y_pred2 <- y_pred_B[rowsrandom,theta_2 ]
y_pred3 <- y_pred_B[rowsrandom,theta_3 ]

ppc_dens_overlay(y = as.numeric(theta_dir$Ocupado), y_pred1)/
  ppc_dens_overlay(y = as.numeric(theta_dir$Desocupado), y_pred2)/
  ppc_dens_overlay(y = as.numeric(theta_dir$Inactivo), y_pred3)

### Convergencia de las cadenas 
mcmc_rhat(summary(fit, pars = "theta")$summary[,"Rhat"])
mcmc_rhat(summary(fit, pars = "theta_pred")$summary[,"Rhat"])
# Ordenando la base con las estimaciones directas y 
# predichas

saveRDS(object = estimaciones_obs, file = out_estimacion)

