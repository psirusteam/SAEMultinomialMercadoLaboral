################################################################################
## Title:        Modelo área para estimación del mercado laboral              ##
## Author:       Stalyn Guerrero - Andrés Gutiérrez                           ##
## Date:         02-2023                                                      ##
################################################################################

###--- Limpieza de memoria ---###
rm(list = ls())
gc()

#######################
###--- Librerías ---###
#######################

library(survey)
library(srvyr)
library(stringr)
library(magrittr)
library(ggplot2)
library(patchwork)
library(tidyverse)
library(cmdstanr)
library(bayesplot)
select <- dplyr::select

#### Lectura de bases de datos 
indicador_dam <- readRDS('01 Modelo de area/CHL/2017/Data/base_modelo.Rds')
statelevel_predictors_df <- readRDS('01 Modelo de area/CHL/2017/Data/satelitales_media.rds') 
## Estandarizando las variables para controlar el efecto de la escala. 
statelevel_predictors_df %<>%
  mutate_if(is.numeric,
            function(x)as.numeric(scale(x)))

## Realizando ajuste sobre el deff estimado. 

indicador_dam1 <- indicador_dam %>%
   mutate(
    id_orden = 1:n(),
    Ocupado_deff = ifelse(Ocupado_deff < 1, 1, Ocupado_deff),
    Desocupado_deff = ifelse(Desocupado_deff < 1, 1, Desocupado_deff),
    Inactivo_deff = ifelse(Inactivo_deff < 1, 1, Inactivo_deff)
  )

## Identificando los municipios para predicción

X_pred <- anti_join(statelevel_predictors_df,
                    indicador_dam1 %>% select(dam2))

X_pred %>% select(dam2) %>% 
  saveRDS(file = "01 Modelo de area/CHL/2017/Data/dam_pred.rds")
## Obteniendo la matrix 
X_pred %<>%
  data.frame() %>%
  select(-dam2)  %>%  as.matrix()

## Identificando los dominios para realizar estimación del modelo

X_obs <- inner_join(indicador_dam1 %>% select(dam2, id_orden),
                    statelevel_predictors_df) %>%
  arrange(id_orden) %>%
  data.frame() %>%
  select(-dam2, -id_orden)  %>%  as.matrix()
##########################################

D <- nrow(indicador_dam1)
P <- 3 # Ocupado, desocupado, inactivo.
Y_tilde <- matrix(NA, D, P)
n_tilde <- matrix(NA, D, P)
Y_hat <- matrix(NA, D, P)

# n efectivos ocupado
n_tilde[,1] <- (indicador_dam1$Ocupado*(1 - indicador_dam1$Ocupado))/indicador_dam1$Ocupado_var
Y_tilde[,1] <- n_tilde[,1]* indicador_dam1$Ocupado


# n efectivos desocupado
n_tilde[,2] <- (indicador_dam1$Desocupado*(1 - indicador_dam1$Desocupado))/indicador_dam1$Desocupado_var
Y_tilde[,2] <- n_tilde[,2]* indicador_dam1$Desocupado

# n efectivos Inactivo
n_tilde[,3] <- (indicador_dam1$Inactivo*(1 - indicador_dam1$Inactivo))/indicador_dam1$Inactivo_var
Y_tilde[,3] <- n_tilde[,3]* indicador_dam1$Inactivo

#########################################################################
ni_hat = rowSums(Y_tilde)
Y_hat[,1] <- ni_hat* indicador_dam1$Ocupado
Y_hat[,2] <- ni_hat* indicador_dam1$Desocupado
Y_hat[,3] <- ni_hat* indicador_dam1$Inactivo

hat_p <- Y_hat/rowSums(Y_hat)
plot(hat_p[,1],indicador_dam1$Ocupado)
plot(hat_p[,2],indicador_dam1$Desocupado)
plot(hat_p[,3],indicador_dam1$Inactivo)
################################
cor(hat_p,X_obs)
#### Covariables 
X1_obs <- cbind(matrix(1,nrow = D,ncol = 1),X_obs)
K = ncol(X1_obs)
D1 <- nrow(X_pred)
X1_pred <- cbind(matrix(1,nrow = D1,ncol = 1),X_pred)
                 
################################################################################
sample_data <- list(D = D,
                    P = P,
                    K = K,
                    hat_y = Y_hat,
                    X_obs = X1_obs,
                    X_pred = X1_pred,
                    D1 = D1)
fit2 <-
  cmdstan_model(stan_file = "01 Modelo de area/0funciones/01 Multinomial_simple_pred.stan",
                compile = TRUE)


fit_mcmc2 <- fit2$sample(
  iter_sampling = 2000, 
  iter_warmup = 2000,
  data = sample_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4
)

fit_mcmc2$save_object(
  file = "01 Modelo de area/CHL/2017/Data/fit_multinomial_con_covariable.Rds")

fit_mcmc2 <- readRDS("01 Modelo de area/CHL/2017/Data/fit_multinomial_con_covariable.Rds")

## Valores estimados para beta

fit_mcmc2$print("beta")

## Valores estimados para la matriz de correlación
fit_mcmc2$summary("Omega")

## Validación dela convergencia de las cadenas 
a <- fit_mcmc2$summary()

# valores cercanos a 1 nos dice que las cadenas convergen.  
mcmc_rhat(a$rhat)

a %>% filter(rhat>1.05,
             !grepl(pattern = "theta_pred",x =variable)
             ) %>% arrange(desc(rhat))

## Evaluación visual de las cadenas para beta 
(mcmc_dens_chains(fit_mcmc2$draws("beta")) +
    mcmc_areas(fit_mcmc2$draws("beta")))/ 
  mcmc_trace(fit_mcmc2$draws("beta"))

## extrayendo las estimaciones para theta 
theta_temp <- fit_mcmc2$summary("theta")
theta_temp_pred <- fit_mcmc2$summary("theta_pred")

## Organizando los resultados en una matriz. 
theta_fh <- matrix(theta_temp$mean, nrow = D,ncol = P,byrow = FALSE)
rowSums(theta_fh)

theta_fh_pred <- matrix(theta_temp_pred$mean, nrow = D1,ncol = P,byrow = FALSE)
rowSums(theta_fh_pred)

## Resultados informativos sobre el comportamiento del modelo. 
mean(indicador_dam1$Ocupado)
mean(theta_fh[,1])

mean(indicador_dam1$Desocupado)
mean(theta_fh[,2])

mean(indicador_dam1$Inactivo)
mean(theta_fh[,3])

## Preparando el gráfico comparativo entre  la estimación dir y el modelo
par(mfrow = c(1,3))

plot(theta_fh[,1],indicador_dam1$Ocupado)
abline(a = 0,b = 1, col = "red")
plot(theta_fh[,2],indicador_dam1$Desocupado)
abline(a = 0,b = 1, col = "red")
plot(theta_fh[,3],indicador_dam1$Inactivo)
abline(a = 0,b = 1, col = "red")

## Preparando el ppc 
y_pred_B <- fit_mcmc2$draws(variables = "theta", format = "matrix")
rowsrandom <- sample(nrow(y_pred_B), 500)

theta_1<-  grep(pattern = "1]",x = colnames(y_pred_B),value = TRUE)
theta_2<-  grep(pattern = "2]",x = colnames(y_pred_B),value = TRUE)
theta_3<-  grep(pattern = "3]",x = colnames(y_pred_B),value = TRUE)
y_pred1 <- y_pred_B[rowsrandom,theta_1 ]
y_pred2 <- y_pred_B[rowsrandom,theta_2 ]
y_pred3 <- y_pred_B[rowsrandom,theta_3 ]

ppc_dens_overlay(y = as.numeric(indicador_dam1$Ocupado), y_pred1)/
  ppc_dens_overlay(y = as.numeric(indicador_dam1$Desocupado), y_pred2)/
  ppc_dens_overlay(y = as.numeric(indicador_dam1$Inactivo), y_pred3)

