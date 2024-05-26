# Instalar y cargar las bibliotecas necesarias
#install.packages("sf")
#install.packages("ggplot2")
#install.packages("readxl")
#install.packages("dplyr")

if (!require("pacman")) install.packages("pacman") # Isntalar pacman (sino está instalada)
require(pacman) # llamar pacman

# Limpiar el entorno
rm(list = ls())

## llamar y/o instalar librerias
p_load(tidyverse,rio,
       viridis, ## paletas de colores 
       sf, ## datos espaciales
       leaflet, ## visualizaciones
       tmaptools, ## geocodificar
       ggsn, ## map scale bar 
       tidycensus) ## packages with census data

library(dplyr)
library(sf)
library(ggplot2)
library(readxl)

# Cargar los datos desde un archivo Excel
bog <- read_excel("1. Datos/Consolidado.xlsx", 
                  sheet = "Base")
#View(bog)

# Verificar las columnas y tipos de datos
str(bog)

# Transformar la columna 'geometry' de caracteres a números
bog <- bog %>%
  mutate(geometry = gsub("c\\(|\\)", "", geometry)) %>%
  separate(geometry, into = c("lon", "lat"), sep = ", ") %>%
  mutate(
    lon = as.numeric(lon),
    lat = as.numeric(lat)
  )

# Verificar si 'price' está presente y tiene valores correctos
summary(bog$price)

# Convertir a objeto sf
bog_sf <- st_as_sf(bog, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# Plotear el mapa con las casas disponibles
p <- ggplot(data = bog_sf) +
  geom_sf(aes(fill = price), color = "black", size = 0.1) +
  scale_fill_gradient(low = "yellow", high = "red", name = "Valor mediano\n del metro cuadrado") +
  theme_minimal() +
  labs(title = "Mapa de Bogotá con precios de viviendas") +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )

# Mostrar el gráfico
print(p)

ggsave(plot=p , filename="C:/Users/windows/Documents/GitHub/Problem_Set_1/Taller-No.3/Taller No.3/4. Gráficos/mapa_Hogares.pdf" , width=6.5 , height=8)

## Otro MAPA de Bogota.

## import data
bog = import("C:/Users/windows/Documents/GitHub/Problem_Set_1/Taller-No.3/Taller No.3/1. Datos/Value_bog.rds")

library(sf)
library(ggplot2)

# Convertir a objeto sf si no lo es
if (!inherits(bog, "sf")) {
  bog <- st_as_sf(bog)
}

# Asegurarse de que la columna de geometría está correctamente identificada
st_geometry(bog) <- bog$geometry

# Plotear el mapa

p = ggplot(data=bog) + geom_sf(mapping = aes(fill=q_price) , size=0.3 , col=NA)  +
  scale_fill_manual(values=c("yellow","#FF9900","#FF6600","#CC0000","#990000"),
                    name="Valor mediano\n del metro cuadrado") 
p

## save plot
ggsave(plot=p , filename="C:/Users/windows/Documents/GitHub/Problem_Set_1/Taller-No.3/Taller No.3/4. Gráficos/mapa_Valor_Medio.pdf" , width=6.5 , height=8)



##=== plot distances ===##

## plot 
ggplot(bog, aes(x=dist_CBD/1000, y=price_surface_median/1000000)) +
  geom_point(shape=1) + geom_smooth(method="lm") + 
  theme_bw() + labs(x="Distancía al centro de negocios de la ciudad (kilómetros)",
                    y="Valor mediano del metro cuadrado (millones)")

## save plot
ggsave(filename="output/bog_dist_cbd.png" , width=6.5 , height=7)

