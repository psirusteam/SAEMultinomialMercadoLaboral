################################################################################
## Title:        Modelo área para estimación del mercado laboral              ##
## Returns:      Mapa de las estimaciones y los cv por dam2                   ##
## Author:       Stalyn Guerrero - Andrés Gutiérrez                           ##
## Date:         02-2023                                                      ##
################################################################################
rm(list = ls())
# Loading required libraries ----------------------------------------------

library(tidyverse)
library(magrittr)
library(patchwork)
library(sp)
library(sf)
library(tmap)
library(RColorBrewer)
library(maptools)
library(DescTools)
mod <- "fit_multinomial_con_covariable"

infile <- paste0("01 Modelo de area/CHL/2017/Data/estimaciones_Bench_",mod,".rds") 
outOcupado <-  paste0("01 Modelo de area/CHL/2017/Output/Ocupados_",mod,".pdf") 
outDesocupado <-  paste0("01 Modelo de area/CHL/2017/Output/Desocupados_",mod,".pdf") 
outInactivo <-  paste0("01 Modelo de area/CHL/2017/Output/Inactivo_",mod,".pdf") 
outMosaico <-  paste0("01 Modelo de area/CHL/2017/Output/Mosaico_",mod,".pdf") 

estimaciones <- readRDS(infile)

## leer shape del pais
ShapeSAE <- read_sf("01 Modelo de area/CHL/2017/ShapeDeptoCHL/comunas.shp")
ShapeSAE %<>% mutate(dam2 = str_pad(
  string = cod_comuna,
  width = 5,
  pad = "0"
),
dam = str_pad(
  string = codregion,
  width = 2,
  pad = "0"
)
) %>% 
  filter(dam != "00", !dam2 %in% c("05201", "13603"))

# 13603: Isla de Isla de Maipo
# 05201: Isla de Pascua
ShapeSAE %>% as.data.frame() %>% select(dam2,dam,Comuna) %>% 
  view()
#########################################
P1_empleo <- tm_shape(ShapeSAE %>%
                           inner_join(estimaciones))
brks_ocupado <- seq(0.3,0.8,0.1)
brks_desocupado <- seq(0,0.2,0.05)
brks_inactivo <- seq(0.15,0.6, 0.09)

Mapa_ocupado <-
  P1_empleo +
  tm_fill("Ocupado_mod",
          breaks = brks_ocupado,
          title = "Ocupado",
          palette = "-Blues") +
  tm_layout(
    legend.only = FALSE,
    legend.height = -0.5,
    legend.width = -0.5,
    asp = 1.5,
    legend.text.size = 3,
    legend.title.size = 3
  )

tmap_save(
  Mapa_ocupado,
  outOcupado,
  width = 6920,
  height = 4080,
  asp = 0
)

Mapa_desocupado <-
  P1_empleo + tm_fill(
    "Desocupado_mod",
    breaks = brks_desocupado,
    title =  "Desocupado",
    palette = "YlOrRd"
  ) + tm_layout( 
    legend.only = FALSE,
    legend.height = -0.5,
    legend.width = -0.5,
    asp = 1.5,
    legend.text.size = 3,
    legend.title.size = 3)

tmap_save(
  Mapa_desocupado,
  outDesocupado,
  width = 6920,
  height = 4080,
  asp = 0
)

Mapa_Inactivo <-
  P1_empleo + tm_fill(
      "Inactivo_mod",
    title =  "Inactivo",
    breaks = brks_inactivo,
    palette = "YlGn"
  ) + tm_layout( 
    legend.only = FALSE,
    legend.height = -0.5,
    legend.width = -0.5,
    asp = 1.5,
    legend.text.size = 3,
    legend.title.size = 3)

tmap_save(
  Mapa_Inactivo,
  outInactivo,
  width = 6920,
  height = 4080,
  asp = 0
)

mosaico <- tmap_arrange(Mapa_Inactivo, Mapa_ocupado,Mapa_desocupado,ncol = 3,
             nrow = 1 )

tmap_save(
  mosaico,
  outMosaico,
  width = 8920,
  height = 4080,
  asp = 0
)
