
# Limpiar el entorno
rm(list = ls())

#Librerias
library(pacman)
library(tidyverse)
library(rio)
library(viridis)
library(leaflet)
library(tmaptools)
library(tidycensus)
library(dplyr)
library(sf)
library(ggplot2)
library(readxl)
library(summarytools)
library(officer)

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

# Seleccionar las variables de interés
variables <- c("price", "rooms", "property_type", "Comercial", "Inflacion_Acum", "Precio_04_2024")
bog_subset <- bog[, variables]

# Obtener estadísticas descriptivas
desc_stats <- summarytools::descr(bog_subset)

# Crear un nuevo documento de Word
doc <- read_docx()

# Agregar el título
doc <- doc %>%
  body_add_par("Estadísticas Descriptivas", style = "heading 1")

# Convertir las estadísticas descriptivas a una tabla
flextable_desc_stats <- flextable::flextable(desc_stats)

# Agregar las estadísticas descriptivas al documento como tabla
doc <- doc %>%
  body_add_flextable(flextable_desc_stats)

# Guardar el documento de Word en la ruta especificada
print(doc, "C:/Users/windows/Documents/GitHub/Problem_Set_1/Taller-No.3/Taller No.3/3. Tablas/estadisticas_descriptivas.docx") 

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

ggsave(plot=p , filename="C:/Users/windows/Documents/GitHub/Problem_Set_1/Taller-No.3/Taller No.3/4. Gráficos/mapa_Hogares.png" , width=6.5 , height=8)

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
ggsave(plot=p , filename="C:/Users/windows/Documents/GitHub/Problem_Set_1/Taller-No.3/Taller No.3/4. Gráficos/mapa_Valor_Medio.png" , width=6.5 , height=8)

  
## plot 
ggplot(bog, aes(x=dist_CBD/1000, y=price_surface_median/1000000)) +
  geom_point(shape=1) + geom_smooth(method="lm") + 
  theme_bw() + labs(x="Distancía al centro de negocios de la ciudad (kilómetros)",
                    y="Valor mediano del metro cuadrado (millones)")

## save plot
ggsave(filename="C:/Users/windows/Documents/GitHub/Problem_Set_1/Taller-No.3/Taller No.3/4. Gráficos/Distancia a negocios.png" , width=6.5 , height=7)

