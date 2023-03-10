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
select <- dplyr::select

#### Lectura de bases de datos 
indicador_dam <- readRDS('01 Modelo de area/CHL/2017/Data/indicador_dam.Rds')
dam_pred <- readRDS('01 Modelo de area/CHL/2017/Data/dam_pred.rds')
mod <- "fit_multinomial_con_covariable"

infile <- paste0("01 Modelo de area/CHL/2017/Data/",mod,".rds")
out_estimacion <- paste0("01 Modelo de area/CHL/2017/Data/estimaciones_",mod,".rds") 

fit <- readRDS(infile) 


## Estimacion directa 
indicador_dam1 <- indicador_dam  %>% 
  filter(n_upm >= 2 & !is.na(Desocupado_deff)) 
P <- 3 
D <- nrow(indicador_dam1)
D1 <- nrow(dam_pred)
theta_dir <- indicador_dam1 %>%  
  transmute(dam2,
    n = n_desocupado + n_ocupado + n_inactivo,
            Ocupado, Desocupado, Inactivo) 

## Estimación del modelo. 
theta_obs <- fit$summary("theta")
theta_pred <- fit$summary("theta_pred")

## Ordenando la matrix de theta 
theta_obs_ordenado <- matrix(theta_obs$mean, 
                             nrow = D,
                             ncol = P,byrow = FALSE) 

colnames(theta_obs_ordenado) <- c("Ocupado_mod", "Desocupado_mod", "Inactivo_mod")
theta_obs_ordenado%<>% as.data.frame()
theta_obs_ordenado <- cbind(dam2 = indicador_dam1$dam2,
                            theta_obs_ordenado)

theta_pred_ordenado <- matrix(theta_pred$mean, 
                             nrow = D1,
                             ncol = P,byrow = FALSE)

colnames(theta_pred_ordenado) <- c("Ocupado_mod", "Desocupado_mod", "Inactivo_mod")
theta_pred_ordenado%<>% as.data.frame()
theta_pred_ordenado <- cbind(dam2 = dam_pred$dam2, theta_pred_ordenado)


estimaciones_obs <- full_join(theta_dir, 
                              bind_rows(theta_obs_ordenado,theta_pred_ordenado)) 


############## coeficiente de variación 
theta_obs_ordenado_sd <- matrix(theta_obs$sd, 
                             nrow = D,
                             ncol = P,byrow = FALSE) 

colnames(theta_obs_ordenado_sd) <- c("Ocupado_mod_sd", "Desocupado_mod_sd", "Inactivo_mod_sd")
theta_obs_ordenado_sd%<>% as.data.frame()
theta_obs_ordenado_sd <- cbind(dam2 = indicador_dam1$dam2,
                            theta_obs_ordenado_sd)
theta_obs_ordenado_cv <- theta_obs_ordenado_sd[,-1]/theta_obs_ordenado[,-1]

colnames(theta_obs_ordenado_cv) <- c("Ocupado_mod_cv", "Desocupado_mod_cv", "Inactivo_mod_cv")

theta_obs_ordenado_cv <- cbind(dam2 = indicador_dam1$dam2,
                               theta_obs_ordenado_cv)

theta_pred_ordenado_sd <- matrix(theta_pred$sd, 
                              nrow = D1,
                              ncol = P,byrow = FALSE)

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


estimaciones_obs <- full_join(indicador_dam1,
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

p_ocupado + xlim(0,0.05)|
p_Desocupado  + xlim(0,0.05)|
p_Inactivo +  xlim(0,0.05)


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

p_ocupado + xlim(0,0.1)|
  p_Desocupado  + xlim(0,0.05)|
  p_Inactivo +  xlim(0,0.05)




### Gráficos de los ppc 
color_scheme_set("brightblue")
theme_set(theme_bw(base_size = 15))
y_pred_B <- fit$draws(variables = "theta", 
                      format = "matrix")
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
mcmc_rhat(theta_obs$rhat)

# Ordenando la base con las estimaciones directas y 
# predichas

saveRDS(object = estimaciones_obs, file = out_estimacion)

