
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

# Estimación del modelo de área de respuesta multinomial 

La Estimación del modelo de área de respuesta multinomial es una técnica estadística utilizada para analizar datos provenientes de encuestas que involucran múltiples categorías de respuesta y están diseñadas a nivel de áreas geográficas. Esta técnica es una extensión del modelo de área de respuesta binomial, el cual se utiliza para analizar encuestas con dos posibles respuestas.

El Modelo multinomial logístico es un tipo de modelo de regresión utilizado para analizar datos de respuesta categóricos que tienen más de dos categorías. Este modelo es una extensión del modelo de regresión logística binaria, el cual se utiliza para analizar datos de respuesta binaria.

## Lectura de librerías 

  -   La librería *survey* es una herramienta de análisis de datos que se utiliza para realizar análisis estadísticos de encuestas y estudios de muestreo complejos. Esta librería proporciona una variedad de herramientas para realizar análisis de regresión, estimaciones de varianza y diseño de muestras.

  -   La librería *srvyr* es una librería de R que permite trabajar con datos de encuestas en el formato de `data.frames` de la librería *dplyr*. Esta librería es especialmente útil para realizar análisis de encuestas y muestras complejas utilizando la sintaxis de *dplyr*.
  
  -   La librería *stringr* es una librería de R que proporciona herramientas para manipular y procesar cadenas de caracteres. Esta librería es especialmente útil para limpiar y transformar datos de texto.

  -   La librería *magrittr* es una librería de R que proporciona una sintaxis más legible y fácil de usar para encadenar y componer funciones. Esta librería es especialmente útil para escribir código más limpio y fácil de entender.

  -   La librería *ggplot2* es una herramienta de visualización de datos en R que permite crear gráficos estadísticos personalizados y de alta calidad. Ofrece una amplia variedad de opciones para crear gráficos de barras, gráficos de líneas, gráficos de dispersión, gráficos de cajas, entre otros. Esta librería se destaca por su capacidad para personalizar los gráficos en función de las necesidades del usuario, lo que permite crear gráficos complejos con múltiples capas y características.

  -   La librería *patchwork* es una librería utilizada para crear paneles de visualización personalizados y complejos en R. Ofrece una amplia variedad de opciones para combinar y organizar gráficos, así como para agregar anotaciones y elementos decorativos. Esta librería es especialmente útil para crear paneles de visualización que incluyan varios gráficos y tablas.

  -   La librería *tidyverse* es una colección de paquetes de R que se utilizan para manipular, procesar y visualizar datos de manera eficiente. Incluye varias librerías como `dplyr`, `ggplot2`, `tidyr`, entre otras, que proporcionan herramientas para limpiar, transformar y visualizar datos de manera efectiva.

  -   La librería *cmdstanr* es una librería de R que permite interactuar con el software de modelado Bayesianos CmdStan. Esta librería es especialmente útil para ajustar modelos Bayesianos complejos y realizar inferencia posterior.

  -   La librería *bayesplot* es una librería de visualización de datos que proporciona herramientas para crear gráficos estadísticos y diagnosticar modelos Bayesianos. Esta librería es especialmente útil para explorar la distribución posterior y evaluar la calidad del ajuste del modelo.



```r
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
```

### Lectura de bases de datos

El archivo `base_modelo.Rds` ubicado en la ruta `01 Modelo de area/CHL/2017/Data/` es leído utilizando la función `readRDS()` y se asigna a la variable `indicador_dam`. Este archivo contiene la información de las estimaciones directas por dominios.

El archivo `satelitales_media.rds` ubicado en la ruta `01 Modelo de area/CHL/2017/Data/` es leído utilizando la función `readRDS()` y se asigna a la variable `statelevel_predictors_df`. Este archivo  contiene los datos de variables a nivel estatal que se utilizarán como predictores en el modelo de área.


```r
indicador_dam <- readRDS('01 Modelo de area/CHL/2017/Data/base_modelo.Rds')
statelevel_predictors_df <- readRDS('01 Modelo de area/CHL/2017/Data/satelitales_media.rds')
```

después de la lectura se realiza la estandarización  de las variables para controlar el efecto de la escala. 


```r
statelevel_predictors_df %<>%
  mutate_if(is.numeric,
            function(x)as.numeric(scale(x)))
```

## Realizando ajuste sobre el deff estimado. 

Este código utiliza la librería `dplyr` para modificar el objeto `indicador_dam` y crear una nueva variable `id_orden`, que indica el número de orden de cada observación dentro del conjunto de datos.

Además, se utiliza la función `mutate()` para modificar los valores de tres variables específicas (`Ocupado_deff`, `Desocupado_deff` e `Inactivo_deff`) del conjunto de datos `indicador_dam`. Estas variables el efecto de diseño para cada categoría de ocupación en la encuesta.

La función `ifelse()` se utiliza para evaluar si los valores de las variables son menores que 1, y en ese caso se les asigna un valor de 1. Esto se hace para asegurarse de que el efecto de diseño no sea menor que 1 y así evitar problemas en el análisis posterior.


```r
indicador_dam1 <- indicador_dam %>%
   mutate(
    id_orden = 1:n(),
    Ocupado_deff = ifelse(Ocupado_deff < 1, 1, Ocupado_deff),
    Desocupado_deff = ifelse(Desocupado_deff < 1, 1, Desocupado_deff),
    Inactivo_deff = ifelse(Inactivo_deff < 1, 1, Inactivo_deff)
  )
```

## Definición del modelo multinomial

-   Sea $K$ el número de categorías de la variable de interés $𝑌\sim multinimial\left(\boldsymbol{\theta}\right)$, con $\boldsymbol{\theta}=\left(p_{1},p_{2},\dots ,p_{k}\right)$ y $\sum_{k=1}^{K}p_{k}=1$.

-   Sea $N_i$ el número de elementos en el i-ésiamo dominio y $N_{ik}$ el número de elementos que tienen la k-ésima categoría, note que $\sum_{k=1}^{K}N_{ik}=N_{i}$ y $p_{ik}=\frac{N_{ik}}{N_{i}}$.

-   Sea $\hat{p}_{ik}$ la estimación directa de $p_{ik}$ y $v_{ik}=Var\left(\hat{p}_{ik}\right)$ y denote el estimador de la varianza por $\hat{v}_{ik}=\widehat{Var}\left(\hat{p}_{ik}\right)$


Note que el efecto diseño cambia entre categoría, por tanto, lo primero será definir el tamaño de muestra efectivo por categoría. Esto es:

La estimación de $\tilde{n}$ esta dado por $\tilde{n}_{ik} = \frac{(\tilde{p}_{ik}\times(1-\tilde{p}_{ik}))}{\hat{v}_{ik}},$

$\tilde{y}_{ik}=\tilde{n}_{ik}\times\hat{p}_{ik}$

luego, $\hat{n}_{i} = \sum_{k=1}^{K}\tilde{y}_{ik}$

de donde se sigue que $\hat{y}_{ik} = \hat{n}_i\times \hat{p}_{ik}$


Sea $\boldsymbol{\theta}=\left(p_{1},p_{2}, p_{3}\right)^{T}=\left(\frac{N_{i1}}{N_{i}},\frac{N_{i2}}{N_{i}}\frac{N_{i3}}{N_{i}}\right)^{T}$, entonces el modelo multinomial para el i-ésimo dominio estaría dado por:

$$
\left(\tilde{y}_{i1},\tilde{y}_{i2},\tilde{y}_{i3}\right)\mid\hat{n}_{i},\boldsymbol{\theta}_{i}\sim multinomial\left(\hat{n}_{i},\boldsymbol{\theta}_{i}\right)
$$ 
Ahora, puede escribir $p_{ik}$ como :

$\ln\left(\frac{p_{i2}}{p_{i1}}\right)=\boldsymbol{X}_{i}^{T}\beta_{2} + u_{i2}$ y
$\ln\left(\frac{p_{i3}}{p_{i1}}\right)=\boldsymbol{X}_{i}^{T}\beta_{3}+ u_{i3}$



Dada la restricción $1 = p_{i1} + p_{i2} + p_{i3}$ entonces 
$$p_{i1} + p_{i1}(e^{\boldsymbol{X}_{i}^{T}\boldsymbol{\beta_{2}}}+  u_{i2})+p_{i1}(e^{\boldsymbol{X}_{i}^{T}\boldsymbol{\beta}_{3}} + u_{i3})$$ de donde se sigue que 

$$
p_{i1}=\frac{1}{1+e^{\boldsymbol{X}_{i}^{T}\boldsymbol{\beta_{2}}}+ u_{i2}+e^{\boldsymbol{X_{i}}^{T}\boldsymbol{\beta_{2}}}+ u_{i3}}
$$

Las expresiones para $p_{i2}$ y $p_{i3}$ estarían dadas por: 

$$
p_{i2}=\frac{e^{\boldsymbol{X}_{i}^{T}\boldsymbol{\beta}_{2}} + u_{i2}}{1+e^{\boldsymbol{X}_{i}^{T}\boldsymbol{\beta_{2}}}+ u_{i2}+e^{\boldsymbol{X_{i}}^{T}\boldsymbol{\beta_{2}}}+ u_{i3}}
$$

$$
p_{i3}=\frac{e^{\boldsymbol{X}_{i}^{T}\boldsymbol{\beta}_{3}}+ u_{i3}}{1+e^{\boldsymbol{X}_{i}^{T}\boldsymbol{\beta_{2}}}+ u_{i2}+e^{\boldsymbol{X_{i}}^{T}\boldsymbol{\beta_{3}}}+ u_{i3}}
$$
dado la naturaleza de la variable, se puede suponer que $cor(u_{i2},u_{i3})\ne 0$

## Modelo programando en `STAN`

El código presenta la implementación de un modelo multinomial logístico de área de respuesta utilizando el lenguaje de programación `STAN`. En este modelo, se asume que la variable de respuesta en cada dominio sigue una distribución multinomial con una estructura de correlación desconocida entre las diferentes categorías. Además, se asume que los parámetros que rigen la relación entre las variables predictoras y la variable de respuesta son diferentes en cada dominio y se modelan como efectos aleatorios.

La sección de *functions* define una función auxiliar llamada `pred_theta()`, que se utiliza para predecir los valores de la variable de respuesta en los dominios no observados. La sección de `data` contiene las variables de entrada del modelo, incluyendo el número de dominios, el número de categorías de la variable de respuesta, las estimaciones directas de la variable de respuesta en cada dominio, las covariables observadas en cada dominio y las covariables correspondientes a los dominios no observados.

La sección de *parameters* define los parámetros desconocidos del modelo, incluyendo la matriz de parámetros *beta*, que contiene los coeficientes que relacionan las covariables con la variable de respuesta en cada categoría. También se incluyen los desviaciones estándar de los efectos aleatorios, la matriz de correlación entre los efectos aleatorios y la matriz de efectos aleatorios en sí.

En la sección de *transformed parameters* se define el vector de parámetros `theta`, que contiene las probabilidades de pertenencia a cada categoría de la variable de respuesta en cada dominio. Se utilizan los efectos aleatorios para ajustar los valores de `theta` en cada dominio.

En la sección de *model* se define la estructura del modelo y se incluyen las distribuciones a priori para los parámetros desconocidos. En particular, se utiliza una distribución normal para los coeficientes de la matriz beta y una distribución LKJ para la matriz de correlación entre los efectos aleatorios. Finalmente, se calcula la función de verosimilitud de la distribución multinomial para las estimaciones directas de la variable de respuesta en cada dominio.

La sección de *generated quantities* se utiliza para calcular las predicciones de la variable de respuesta en los dominios no observados utilizando la función auxiliar definida previamente. También se calcula la matriz de correlación entre los efectos aleatorios.


```r
functions {
  matrix pred_theta(matrix Xp, int p, matrix beta){
  int D1 = rows(Xp);
  real num1[D1, p];
  real den1[D1];
  matrix[D1,p] theta_p;
  
  for(d in 1:D1){
    num1[d, 1] = 1;
    num1[d, 2] = exp(Xp[d, ] * beta[1, ]' ) ;
    num1[d, 3] = exp(Xp[d, ] * beta[2, ]' ) ;
    
    den1[d] = sum(num1[d, ]);
  }
  
  for(d in 1:D1){
    for(i in 2:p){
    theta_p[d, i] = num1[d, i]/den1[d];
    }
    theta_p[d, 1] = 1/den1[d];
   }

  return theta_p  ;
  }
  
}

data {
  int<lower=1> D; // número de dominios 
  int<lower=1> P; // categorías
  int<lower=1> K; // cantidad de regresores
  int hat_y[D, P]; // estimaciones directa
  matrix[D, K] X_obs; // matriz de covariables
  int<lower=1> D1; // número de dominios no observados 
  matrix[D1, K] X_pred; // matriz de covariables para los no observados
}
  

parameters {
  matrix[P-1, K] beta;// matriz de parámetros 
  vector<lower=0>[P-1] sigma_u;       // random effects standard deviations
  // declare L_u to be the Choleski factor of a 2x2 correlation matrix
  cholesky_factor_corr[P-1] L_u;
  matrix[P-1, D] z_u;                  
}

transformed parameters {
  simplex[P] theta[D];// vector de parámetros;
  real num[D, P];
  real den[D];
  // this transform random effects so that they have the correlation
  // matrix specified by the correlation matrix above
  matrix[P-1, D] u; // random effect matrix
  u = diag_pre_multiply(sigma_u, L_u) * z_u;
  
  for(d in 1:D){
    num[d, 1] = 1;
    num[d, 2] = exp(X_obs[d, ] * beta[1, ]' + u[1, d]) ;
    num[d, 3] = exp(X_obs[d, ] * beta[2, ]' + u[2, d]) ;
    
    den[d] = sum(num[d, ]);
  }
  
  for(d in 1:D){
    for(p in 2:P){
    theta[d, p] = num[d, p]/den[d];
    }
    theta[d, 1] = 1/den[d];
  }
}

model {
  L_u ~ lkj_corr_cholesky(1); // LKJ prior for the correlation matrix
  to_vector(z_u) ~ normal(0, 10000);
  // sigma_u ~ cauchy(0, 50);
  sigma_u ~ inv_gamma(0.0001, 0.0001);
  
  for(p in 2:P){
    for(k in 1:K){
      beta[p-1, k] ~ normal(0, 10000);
    }
    }
  
  for(d in 1:D){
    target += multinomial_lpmf(hat_y[d, ] | theta[d, ]); 
  }
}

  
generated quantities {
  matrix[D1,P] theta_pred;
  matrix[2, 2] Omega;
  Omega = L_u * L_u'; // so that it return the correlation matrix
  
 theta_pred = pred_theta(X_pred, P, beta);
}
```


## Identificando los municipios para predicción


```r
X_pred <- anti_join(statelevel_predictors_df,
                    indicador_dam1 %>% select(dam2))
```

Guardar el identificador de municipio a predecir. 


```r
X_pred %>% select(dam2) %>% 
  saveRDS(file = "01 Modelo de area/CHL/2017/Data/dam_pred.rds")
```

## Obteniendo la matriz


```r
X_pred %<>%
  data.frame() %>%
  select(-dam2)  %>%  as.matrix()
```

## Identificando los dominios para realizar estimación del modelo

El código siguiente realiza una serie de operaciones con dos conjuntos de datos: `indicador_dam1` y `statelevel_predictors_df`. En primer lugar, utiliza la función `inner_join()` de `dplyr` para unir ambos `data.frames` por la columna `id_orden`. Luego, se ordenan las filas por esta misma columna y se eliminan las columnas `dam2` e `id_orden`. Finalmente, se convierte el resultado en una matriz y se asigna a la variable `X_obs`. En resumen, este código prepara los datos para ser utilizados en un modelo de regresión logística multinomial de área de respuesta.


```r
X_obs <- inner_join(indicador_dam1 %>% select(dam2, id_orden),
                    statelevel_predictors_df) %>%
  arrange(id_orden) %>%
  data.frame() %>%
  select(-dam2, -id_orden)  %>%  as.matrix()
```

## Identificando los argumentos para `STAN`
Creando $\tilde{y}$, $\tilde{n}$ y $\hat{y}$


```r
D <- nrow(indicador_dam1)
P <- 3 # Ocupado, desocupado, inactivo.
Y_tilde <- matrix(NA, D, P)
n_tilde <- matrix(NA, D, P)
Y_hat <- matrix(NA, D, P)
```

Realizando los calculos necesarios para cada categoria. 


```r
# n efectivos ocupado
n_tilde[,1] <- (indicador_dam1$Ocupado*(1 - indicador_dam1$Ocupado))/indicador_dam1$Ocupado_var
Y_tilde[,1] <- n_tilde[,1]* indicador_dam1$Ocupado


# n efectivos desocupado
n_tilde[,2] <- (indicador_dam1$Desocupado*(1 - indicador_dam1$Desocupado))/indicador_dam1$Desocupado_var
Y_tilde[,2] <- n_tilde[,2]* indicador_dam1$Desocupado

# n efectivos Inactivo
n_tilde[,3] <- (indicador_dam1$Inactivo*(1 - indicador_dam1$Inactivo))/indicador_dam1$Inactivo_var
Y_tilde[,3] <- n_tilde[,3]* indicador_dam1$Inactivo
```

Calculando $\hat{y}$

```r
ni_hat = rowSums(Y_tilde)
Y_hat[,1] <- ni_hat* indicador_dam1$Ocupado
Y_hat[,2] <- ni_hat* indicador_dam1$Desocupado
Y_hat[,3] <- ni_hat* indicador_dam1$Inactivo
```

Para realizar la validación de los calculos construimos el siguiente gráfico. 


```r
hat_p <- Y_hat/rowSums(Y_hat)
par(mfrow = c(1,3))
plot(hat_p[,1],indicador_dam1$Ocupado, main = "Ocupado", 
     xlab =  "hat_p", ylab = "Estimacion directa")
plot(hat_p[,2],indicador_dam1$Desocupado, 
     main = "Desocupado", 
     xlab =  "hat_p", ylab = "Estimacion directa")
plot(hat_p[,3],indicador_dam1$Inactivo,main = "Inactivo", 
     xlab =  "hat_p", ylab = "Estimacion directa")
```

<img src="03-EstimacionSAE_multinomial_files/figure-html/unnamed-chunk-14-1.svg" width="672" />

ahora, la correlación de las covariables y las estimaciones directas


```r
rr <- cor(hat_p,X_obs) %>% data.frame() 
row.names(rr)<- c("Ocupado", "Desocupado", "Inactivo")
tba(rr)
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> luces_nocturnas </th>
   <th style="text-align:right;"> cubrimiento_cultivo </th>
   <th style="text-align:right;"> cubrimiento_urbano </th>
   <th style="text-align:right;"> modificacion_humana </th>
   <th style="text-align:right;"> accesibilidad_hospitales </th>
   <th style="text-align:right;"> accesibilidad_hosp_caminado </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Ocupado </td>
   <td style="text-align:right;"> 0.4920 </td>
   <td style="text-align:right;"> -0.1208 </td>
   <td style="text-align:right;"> 0.4485 </td>
   <td style="text-align:right;"> 0.2944 </td>
   <td style="text-align:right;"> 0.1127 </td>
   <td style="text-align:right;"> 0.0923 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Desocupado </td>
   <td style="text-align:right;"> 0.1402 </td>
   <td style="text-align:right;"> -0.1097 </td>
   <td style="text-align:right;"> 0.1098 </td>
   <td style="text-align:right;"> 0.1228 </td>
   <td style="text-align:right;"> -0.0765 </td>
   <td style="text-align:right;"> -0.0944 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Inactivo </td>
   <td style="text-align:right;"> -0.5401 </td>
   <td style="text-align:right;"> 0.1550 </td>
   <td style="text-align:right;"> -0.4869 </td>
   <td style="text-align:right;"> -0.3347 </td>
   <td style="text-align:right;"> -0.0915 </td>
   <td style="text-align:right;"> -0.0655 </td>
  </tr>
</tbody>
</table>

Agregando el intercepto a la matriz de covariables


```r
X1_obs <- cbind(matrix(1,nrow = D,ncol = 1),X_obs)
K = ncol(X1_obs)
D1 <- nrow(X_pred)
X1_pred <- cbind(matrix(1,nrow = D1,ncol = 1),X_pred)
```

### Preparando argumentos para `STAN`

El código siguiente crea un objeto de lista llamado `sample_data` que contiene los datos necesarios para estimar el modelo de área de respuesta multinomial. La lista incluye el número total de dominios (`D`), el número de categorías (`P`), la cantidad de covariables (`K`), las estimaciones directas de las categorías en cada dominio (`hat_y`), la matriz de covariables de los dominios observados (`X_obs`), la matriz de covariables de los dominios no observados (`X_pred`) y el número de dominios no observados (`D1`). Este objeto de lista será utilizado posteriormente en la estimación del modelo utilizando la función `stan`.


```r
sample_data <- list(D = D,
                    P = P,
                    K = K,
                    hat_y = Y_hat,
                    X_obs = X1_obs,
                    X_pred = X1_pred,
                    D1 = D1)
```

### Preparando el modelo en `STAN`


```r
fit2 <-
  cmdstan_model(stan_file = "01 Modelo de area/0funciones/01 Multinomial_simple_pred.stan",
                compile = TRUE)
```

### Ejecutando el modelo en `STAN`

Este código ejecuta el muestreo MCMC utilizando el modelo especificado en `fit2`, con un número de iteraciones de muestreo y calentamiento de 2000 cada uno, y utilizando los datos en `sample_data`. También especifica una semilla de 123 para reproducibilidad y ejecuta cuatro cadenas paralelas en cuatro núcleos. El resultado es una lista `fit_mcmc2` que contiene los valores muestreados de los parámetros del modelo y otros diagnósticos del muestreo.


```r
fit_mcmc2 <- fit2$sample(
  iter_sampling = 2000, 
  iter_warmup = 2000,
  data = sample_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4
)
```

Guardando el modelo 

```r
fit_mcmc2$save_object(
  file = "01 Modelo de area/CHL/2017/Data/fit_multinomial_con_covariable.Rds")
```

leer el modelo previamente ejecutado 


```r
fit_mcmc2 <- readRDS("01 Modelo de area/CHL/2017/Data/fit_multinomial_con_covariable.Rds")
```

### Valores estimados para beta

```r
fit_mcmc2$summary("beta") %>% as.data.frame() %>% tba()
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> variable </th>
   <th style="text-align:right;"> mean </th>
   <th style="text-align:right;"> median </th>
   <th style="text-align:right;"> sd </th>
   <th style="text-align:right;"> mad </th>
   <th style="text-align:right;"> q5 </th>
   <th style="text-align:right;"> q95 </th>
   <th style="text-align:right;"> rhat </th>
   <th style="text-align:right;"> ess_bulk </th>
   <th style="text-align:right;"> ess_tail </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> beta[1,1] </td>
   <td style="text-align:right;"> -2.4861 </td>
   <td style="text-align:right;"> -2.4855 </td>
   <td style="text-align:right;"> 0.0428 </td>
   <td style="text-align:right;"> 0.0437 </td>
   <td style="text-align:right;"> -2.5573 </td>
   <td style="text-align:right;"> -2.4174 </td>
   <td style="text-align:right;"> 1.0023 </td>
   <td style="text-align:right;"> 1019.1951 </td>
   <td style="text-align:right;"> 2598.776 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> beta[2,1] </td>
   <td style="text-align:right;"> -0.1591 </td>
   <td style="text-align:right;"> -0.1595 </td>
   <td style="text-align:right;"> 0.0184 </td>
   <td style="text-align:right;"> 0.0183 </td>
   <td style="text-align:right;"> -0.1895 </td>
   <td style="text-align:right;"> -0.1280 </td>
   <td style="text-align:right;"> 1.0050 </td>
   <td style="text-align:right;"> 1324.1582 </td>
   <td style="text-align:right;"> 2640.008 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> beta[1,2] </td>
   <td style="text-align:right;"> -0.0966 </td>
   <td style="text-align:right;"> -0.0963 </td>
   <td style="text-align:right;"> 0.0899 </td>
   <td style="text-align:right;"> 0.0895 </td>
   <td style="text-align:right;"> -0.2459 </td>
   <td style="text-align:right;"> 0.0518 </td>
   <td style="text-align:right;"> 1.0059 </td>
   <td style="text-align:right;"> 860.8015 </td>
   <td style="text-align:right;"> 1992.299 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> beta[2,2] </td>
   <td style="text-align:right;"> -0.2346 </td>
   <td style="text-align:right;"> -0.2350 </td>
   <td style="text-align:right;"> 0.0391 </td>
   <td style="text-align:right;"> 0.0391 </td>
   <td style="text-align:right;"> -0.2990 </td>
   <td style="text-align:right;"> -0.1709 </td>
   <td style="text-align:right;"> 1.0083 </td>
   <td style="text-align:right;"> 1097.2804 </td>
   <td style="text-align:right;"> 1956.711 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> beta[1,3] </td>
   <td style="text-align:right;"> -0.2137 </td>
   <td style="text-align:right;"> -0.2141 </td>
   <td style="text-align:right;"> 0.0509 </td>
   <td style="text-align:right;"> 0.0503 </td>
   <td style="text-align:right;"> -0.2981 </td>
   <td style="text-align:right;"> -0.1295 </td>
   <td style="text-align:right;"> 1.0044 </td>
   <td style="text-align:right;"> 816.9598 </td>
   <td style="text-align:right;"> 1888.151 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> beta[2,3] </td>
   <td style="text-align:right;"> -0.0429 </td>
   <td style="text-align:right;"> -0.0430 </td>
   <td style="text-align:right;"> 0.0220 </td>
   <td style="text-align:right;"> 0.0221 </td>
   <td style="text-align:right;"> -0.0793 </td>
   <td style="text-align:right;"> -0.0066 </td>
   <td style="text-align:right;"> 1.0053 </td>
   <td style="text-align:right;"> 977.6790 </td>
   <td style="text-align:right;"> 1723.405 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> beta[1,4] </td>
   <td style="text-align:right;"> -0.1609 </td>
   <td style="text-align:right;"> -0.1603 </td>
   <td style="text-align:right;"> 0.0748 </td>
   <td style="text-align:right;"> 0.0737 </td>
   <td style="text-align:right;"> -0.2829 </td>
   <td style="text-align:right;"> -0.0378 </td>
   <td style="text-align:right;"> 1.0053 </td>
   <td style="text-align:right;"> 1114.3069 </td>
   <td style="text-align:right;"> 2116.905 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> beta[2,4] </td>
   <td style="text-align:right;"> 0.0145 </td>
   <td style="text-align:right;"> 0.0148 </td>
   <td style="text-align:right;"> 0.0324 </td>
   <td style="text-align:right;"> 0.0318 </td>
   <td style="text-align:right;"> -0.0401 </td>
   <td style="text-align:right;"> 0.0681 </td>
   <td style="text-align:right;"> 1.0051 </td>
   <td style="text-align:right;"> 1052.9843 </td>
   <td style="text-align:right;"> 2159.417 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> beta[1,5] </td>
   <td style="text-align:right;"> 0.2879 </td>
   <td style="text-align:right;"> 0.2883 </td>
   <td style="text-align:right;"> 0.0975 </td>
   <td style="text-align:right;"> 0.0977 </td>
   <td style="text-align:right;"> 0.1281 </td>
   <td style="text-align:right;"> 0.4457 </td>
   <td style="text-align:right;"> 1.0085 </td>
   <td style="text-align:right;"> 744.0500 </td>
   <td style="text-align:right;"> 1651.688 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> beta[2,5] </td>
   <td style="text-align:right;"> 0.0627 </td>
   <td style="text-align:right;"> 0.0629 </td>
   <td style="text-align:right;"> 0.0423 </td>
   <td style="text-align:right;"> 0.0420 </td>
   <td style="text-align:right;"> -0.0087 </td>
   <td style="text-align:right;"> 0.1331 </td>
   <td style="text-align:right;"> 1.0059 </td>
   <td style="text-align:right;"> 886.6170 </td>
   <td style="text-align:right;"> 1545.972 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> beta[1,6] </td>
   <td style="text-align:right;"> 0.0298 </td>
   <td style="text-align:right;"> 0.0256 </td>
   <td style="text-align:right;"> 0.1727 </td>
   <td style="text-align:right;"> 0.1726 </td>
   <td style="text-align:right;"> -0.2482 </td>
   <td style="text-align:right;"> 0.3195 </td>
   <td style="text-align:right;"> 1.0024 </td>
   <td style="text-align:right;"> 959.8096 </td>
   <td style="text-align:right;"> 1645.669 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> beta[2,6] </td>
   <td style="text-align:right;"> -0.1213 </td>
   <td style="text-align:right;"> -0.1216 </td>
   <td style="text-align:right;"> 0.0741 </td>
   <td style="text-align:right;"> 0.0748 </td>
   <td style="text-align:right;"> -0.2412 </td>
   <td style="text-align:right;"> 0.0014 </td>
   <td style="text-align:right;"> 1.0035 </td>
   <td style="text-align:right;"> 1076.9747 </td>
   <td style="text-align:right;"> 1916.019 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> beta[1,7] </td>
   <td style="text-align:right;"> -0.3286 </td>
   <td style="text-align:right;"> -0.3231 </td>
   <td style="text-align:right;"> 0.6318 </td>
   <td style="text-align:right;"> 0.6351 </td>
   <td style="text-align:right;"> -1.3925 </td>
   <td style="text-align:right;"> 0.7048 </td>
   <td style="text-align:right;"> 1.0031 </td>
   <td style="text-align:right;"> 1066.6718 </td>
   <td style="text-align:right;"> 1780.399 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> beta[2,7] </td>
   <td style="text-align:right;"> -0.1183 </td>
   <td style="text-align:right;"> -0.1166 </td>
   <td style="text-align:right;"> 0.2715 </td>
   <td style="text-align:right;"> 0.2703 </td>
   <td style="text-align:right;"> -0.5650 </td>
   <td style="text-align:right;"> 0.3219 </td>
   <td style="text-align:right;"> 1.0032 </td>
   <td style="text-align:right;"> 1151.2476 </td>
   <td style="text-align:right;"> 2512.283 </td>
  </tr>
</tbody>
</table>

### Valores estimados para la matriz de correlación


```r
fit_mcmc2$summary("Omega")%>% as.data.frame() %>% tba()
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> variable </th>
   <th style="text-align:right;"> mean </th>
   <th style="text-align:right;"> median </th>
   <th style="text-align:right;"> sd </th>
   <th style="text-align:right;"> mad </th>
   <th style="text-align:right;"> q5 </th>
   <th style="text-align:right;"> q95 </th>
   <th style="text-align:right;"> rhat </th>
   <th style="text-align:right;"> ess_bulk </th>
   <th style="text-align:right;"> ess_tail </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Omega[1,1] </td>
   <td style="text-align:right;"> 1.0000 </td>
   <td style="text-align:right;"> 1.0000 </td>
   <td style="text-align:right;"> 0.0000 </td>
   <td style="text-align:right;"> 0.0000 </td>
   <td style="text-align:right;"> 1.0000 </td>
   <td style="text-align:right;"> 1.0000 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Omega[2,1] </td>
   <td style="text-align:right;"> 0.3437 </td>
   <td style="text-align:right;"> 0.3474 </td>
   <td style="text-align:right;"> 0.0648 </td>
   <td style="text-align:right;"> 0.0636 </td>
   <td style="text-align:right;"> 0.2327 </td>
   <td style="text-align:right;"> 0.4449 </td>
   <td style="text-align:right;"> 1.0064 </td>
   <td style="text-align:right;"> 732.5882 </td>
   <td style="text-align:right;"> 1447.365 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Omega[1,2] </td>
   <td style="text-align:right;"> 0.3437 </td>
   <td style="text-align:right;"> 0.3474 </td>
   <td style="text-align:right;"> 0.0648 </td>
   <td style="text-align:right;"> 0.0636 </td>
   <td style="text-align:right;"> 0.2327 </td>
   <td style="text-align:right;"> 0.4449 </td>
   <td style="text-align:right;"> 1.0064 </td>
   <td style="text-align:right;"> 732.5882 </td>
   <td style="text-align:right;"> 1447.365 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Omega[2,2] </td>
   <td style="text-align:right;"> 1.0000 </td>
   <td style="text-align:right;"> 1.0000 </td>
   <td style="text-align:right;"> 0.0000 </td>
   <td style="text-align:right;"> 0.0000 </td>
   <td style="text-align:right;"> 1.0000 </td>
   <td style="text-align:right;"> 1.0000 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
</tbody>
</table>


### Validación dela convergencia de las cadenas 

valores cercanos a 1 indica que las cadenas hicieron convergencia.


```r
a <- fit_mcmc2$summary()

mcmc_rhat(a$rhat)
```

<img src="03-EstimacionSAE_multinomial_files/figure-html/unnamed-chunk-24-1.svg" width="672" />



```r
a %>% filter(rhat>1.05,
             !grepl(pattern = "theta_pred",x =variable)
             ) %>% arrange(desc(rhat)) %>% 
  tba()
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> variable </th>
   <th style="text-align:right;"> mean </th>
   <th style="text-align:right;"> median </th>
   <th style="text-align:right;"> sd </th>
   <th style="text-align:right;"> mad </th>
   <th style="text-align:right;"> q5 </th>
   <th style="text-align:right;"> q95 </th>
   <th style="text-align:right;"> rhat </th>
   <th style="text-align:right;"> ess_bulk </th>
   <th style="text-align:right;"> ess_tail </th>
  </tr>
 </thead>
<tbody>
  <tr>

  </tr>
</tbody>
</table>

### Evaluación visual de las cadenas para beta 

El código dado genera un gráfico de densidad (mcmc_dens_chains), un gráfico de áreas (mcmc_areas) y un gráfico de traza (mcmc_trace) para cada parámetro en la matriz "beta" en la salida del modelo de MCMC "fit_mcmc2". Los gráficos de densidad y de áreas muestran la distribución posterior de cada parámetro en la matriz "beta" para cada cadena del modelo. 


```r
(mcmc_dens_chains(fit_mcmc2$draws("beta")) +
    mcmc_areas(fit_mcmc2$draws("beta")))/ 
  mcmc_trace(fit_mcmc2$draws("beta"))
```

<img src="03-EstimacionSAE_multinomial_files/figure-html/unnamed-chunk-26-1.svg" width="672" />

### extrayendo las estimaciones para theta 


```r
theta_temp <- fit_mcmc2$summary("theta")
theta_temp_pred <- fit_mcmc2$summary("theta_pred")
```

### Organizando los resultados en una matriz. 

La primera parte del código crea una matriz `theta_fh`" de dimensiones D x P, en la que se colocan los valores medios de `theta_temp`. Los argumentos `nrow` y `ncol` definen las filas y columnas de la matriz, mientras que `byrow` establece que los valores se colocan por columnas. Luego se utiliza la función `rowSums()` para sumar los valores por filas.

El segundo bloque de código es similar al primero, pero crea una matriz `theta_fh_pred` de dimensiones D1 x P, con los valores medios de `theta_temp_pred`. Luego se utiliza la función `rowSums` para sumar los valores por filas.


```r
theta_fh <- matrix(theta_temp$mean, nrow = D,ncol = P,byrow = FALSE)
rowSums(theta_fh)
```

```
##   [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
##  [38] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
##  [75] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
## [112] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
## [149] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
## [186] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
## [223] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
## [260] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
## [297] 1 1 1 1 1 1 1 1 1 1 1 1 1
```

```r
theta_fh_pred <- matrix(theta_temp_pred$mean, nrow = D1,ncol = P,byrow = FALSE)
rowSums(theta_fh_pred)
```

```
##  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
```

#### Resultados informativos sobre el comportamiento del modelo. 

```r
mean(indicador_dam1$Ocupado)
```

```
## [1] 0.5086088
```

```r
mean(theta_fh[,1])
```

```
## [1] 0.5092394
```

```r
mean(indicador_dam1$Desocupado)
```

```
## [1] 0.0473751
```

```r
mean(theta_fh[,2])
```

```
## [1] 0.04688798
```

```r
mean(indicador_dam1$Inactivo)
```

```
## [1] 0.4440161
```

```r
mean(theta_fh[,3])
```

```
## [1] 0.4438726
```

### Preparando el gráfico comparativo entre  la estimación dir y el modelo


```r
par(mfrow = c(1,3))
plot(theta_fh[,1],indicador_dam1$Ocupado)
abline(a = 0,b = 1, col = "red")
plot(theta_fh[,2],indicador_dam1$Desocupado)
abline(a = 0,b = 1, col = "red")
plot(theta_fh[,3],indicador_dam1$Inactivo)
abline(a = 0,b = 1, col = "red")
```

<img src="03-EstimacionSAE_multinomial_files/figure-html/unnamed-chunk-30-1.svg" width="672" />

### Preparando el ppc 

En el siguiente código se utiliza para realizar una validación cruzada predictiva (PPC, por las siglas en inglés) en los datos de prueba. Primero, se extraen las muestras de la distribución posterior predictiva para las variables $\theta$ (`y_pred_B`). Luego, se seleccionan aleatoriamente 500 filas de `y_pred_B`. A continuación, se separan las columnas de `y_pred_B` correspondientes a cada categoría de ocupación (Ocupado, Desocupado, Inactivo) y se crean los vectores `y_pred1`, `y_pred2` y `y_pred3`, respectivamente. Finalmente, se utiliza la función `ppc_dens_overlay()` para graficar la densidad de las predicciones en cada categoría y compararlas con los datos observados correspondientes.



```r
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
```

<img src="03-EstimacionSAE_multinomial_files/figure-html/unnamed-chunk-31-1.svg" width="672" />

