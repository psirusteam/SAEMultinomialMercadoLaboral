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

infile <- paste0("01 Modelo de area/PER/2017/Data/estimaciones_Bench.rds") 
outOcupado <-  paste0("01 Modelo de area/PER/2017/Output/Map_Ocupados.png") 
outDesocupado <-  paste0("01 Modelo de area/PER/2017/Output/Map_Desocupados.png") 
outInactivo <-  paste0("01 Modelo de area/PER/2017/Output/Map_Inactivo.png") 
outMosaico <-  paste0("01 Modelo de area/PER/2017/Output/Map_Mosaico.png") 

estimaciones <- readRDS(infile)

## leer shape del pais
ShapeSAE <- read_sf("01 Modelo de area/PER/2017/Shape/PER_dam2.shp")

#########################################
P1_empleo <- tm_shape(ShapeSAE %>%
                           left_join(estimaciones))
brks_ocupado <- seq(0.4,1,0.1)
brks_desocupado <- seq(0,0.1,0.02)
brks_inactivo <- seq(0.10,0.65, 0.09)

Mapa_ocupado <-
  P1_empleo +
  tm_fill("Ocupado_Bench",
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
    "Desocupado_Bench",
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
      "Inactivo_Bench",
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
