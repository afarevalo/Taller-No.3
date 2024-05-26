# Instalar y cargar las bibliotecas necesarias
#install.packages("sf")
#install.packages("ggplot2")
#install.packages("readxl")
#install.packages("dplyr")

# Limpiar el entorno
rm(list = ls())

library(dplyr)
library(sf)
library(ggplot2)
library(readxl)

# Cargar los datos desde un archivo Excel
bog <- read_excel("1. Datos/Consolidado.xlsx", 
                  sheet = "Base")
#View(bog)

# Separar las coordenadas de la columna 'geometry'
# Suponiendo que 'geometry' es una lista de coordenadas
bog <- bog %>%
  mutate(
    lon = sapply(geometry, function(x) x[1]),
    lat = sapply(geometry, function(x) x[2])
  )

# Separar las coordenadas de la columna 'geometry'
bog <- bog %>%
  mutate(
    lon = sapply(geometry, function(x) x[1]),
    lat = sapply(geometry, function(x) x[2])
  )

# Verificar que 'lon' y 'lat' están correctamente extraídas
head(bog)

# Convertir las coordenadas en un objeto sf
bog <- st_as_sf(bog, coords = c("lon", "lat"), crs = 4326)

# Crear el mapa
p <- ggplot(data = bog) +
  geom_sf(aes(fill = price), size = 0.3, col = NA) +
  scale_fill_gradient(low = "yellow", high = "red", name = "Precio de la vivienda") +
  theme_minimal()

# Mostrar el gráfico
print(p)


# Verificar que 'lon' y 'lat' están correctamente extraídas
head(bog)

# Convertir las coordenadas en un objeto sf
bog <- st_as_sf(bog, coords = c("lon", "lat"), crs = 4326)

# Crear el mapa
p <- ggplot(data = bog) +
  geom_sf(aes(fill = price), size = 0.3, col = NA) +
  scale_fill_gradient(low = "yellow", high = "red", name = "Precio de la vivienda") +
  theme_minimal()

# Mostrar el gráfico
print(p)
