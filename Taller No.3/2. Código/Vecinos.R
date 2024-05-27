
# Limpiar el entorno
rm(list = ls())

#Librerias
library(pacman)
library(skimr)
library(gstat)
library(leaflet)
library(nngeo)
library(spdep)
library(osmdata)
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

### Importar

## Inmuebles
houses <- import("C:/Users/windows/Documents/GitHub/Problem_Set_1/Taller-No.3/Taller No.3/1. Datos/house_prices.rds")
class(houses)

## dataframe to sf
houses <- st_as_sf(x = houses, ## datos
                   coords=c("lon","lat"), ## coordenadas
                   crs=4326) ## CRS
class(houses)

## Censo
mnz <- import("C:/Users/windows/Documents/GitHub/Problem_Set_1/Taller-No.3/Taller No.3/1. Datos/mgn_censo_2018.rds")
mnz

### Filtrar datos

## get chapinero
chapinero <- getbb(place_name = "UPZ Chapinero, Bogota", 
                   featuretype = "boundary:administrative", 
                   format_out = "sf_polygon") %>% .$multipolygon

## crop puntos con poligono
house_chapi <- st_intersection(x = houses , y = chapinero)

## Recuperar información de las covariables**

## missing 
table(is.na(house_chapi$rooms))
house_chapi %>% lapply(function(x) table(is.na(x)))

## Aplicacion
house_chapi$description[49] ## explore description

str_locate_all(string = house_chapi$description[49] , pattern = "136 mts") ## detect pattern

str_extract(string=house_chapi$description[49] , pattern= "136 mts") ## extrac pattern

x <- "[:space:]+[:digit:]+[:space:]+mts" ## pattern

str_locate_all(string = house_chapi$description[49] , pattern = x) ## detect pattern

str_extract(string=house_chapi$description[49] , pattern= x) ## extrac pattern

## make new var
house_chapi <- house_chapi %>% 
  mutate(new_surface = str_extract(string=description , pattern= x))
table(house_chapi$new_surface)

## another pattern
y = "[:space:]+[:digit:]+[:space:]+m2"
house_chapi = house_chapi %>% 
  mutate(new_surface = ifelse(is.na(new_surface)==T,
                              str_extract(string=house$description , pattern= y),
                              new_surface))
table(house_chapi$new_surface) %>% head()

## Dependencia espacial

## sf to sp
house_chapi_sp <- house_chapi %>% as_Spatial()
house_chapi_sp

## estimations
variogram(price/1000000 ~ 1, house_chapi_sp , cloud = F , cressie=T) %>% plot()

house_chapi_sp$normal <- rnorm(n = nrow(house_chapi_sp),
                               mean = mean(house_chapi_sp$price/1000000),
                               sd = 1000)

db_plot = left_join(x = variogram(price/1000000 ~ 1, house_chapi_sp, cloud = F , cressie=T) %>% mutate(estimate=gamma) %>% select(dist,estimate),
                    y = variogram(normal ~ 1, house_chapi_sp, cloud = F , cressie=T) %>% mutate(normal=gamma) %>% select(dist,normal),"dist") 

db_plot %>% head()

plot = ggplot(db_plot) + 
  geom_point(aes(x=dist, y=normal , fill="Datos aleatorios (Dist. Normal)"), shape=21, alpha=0.5, size=5 ) +
  geom_point(aes(x=dist, y=estimate , fill="Precio de la vivienda (properati)"), shape=21, alpha=0.5, size=5 ) +
  labs(caption = "Fuente: Properati", y = "Semivariograma", x = "Distancia de separación entre inmuebles", fill = "") + theme_bw()
plot
export(plot,"C:/Users/windows/Documents/GitHub/Problem_Set_1/Taller-No.3/Taller No.3/4. Gráficos/variograma.rds")

## save plot
ggsave(filename="C:/Users/windows/Documents/GitHub/Problem_Set_1/Taller-No.3/Taller No.3/4. Gráficos/variograma.png" , width=6.5 , height=7)



#### Vecinos espaciales

### Vecinos en la manzana
house_chapi = house_chapi %>%
  group_by(MANZ_CCNCT) %>%
  mutate(new_surface_2=median(surface_total,na.rm=T))

table(is.na(house_chapi$surface_total))

table(is.na(house_chapi$surface_total),
      is.na(house_chapi$new_surface_2)) # ahora solo tenemos menos missing values

### **4.2 Vecinos mas cercanos**

## definir submuestra
new_chapi <- house_chapi[st_buffer(house_chapi[100,],200),]

new_chapi

## obtener objeto sp
house_chapi_poly <- new_chapi %>% st_buffer(20) %>% as_Spatial() # poligonos

## obtener vecinos
nb_chapi = poly2nb(pl=house_chapi_poly , queen=T) # opcion reina

## vecinos del inmueble 29
nb_chapi[[29]]

leaflet() %>% addTiles() %>% 
  addCircles(data=new_chapi[29,],col="red") %>% 
  addCircles(data=new_chapi[nb_chapi[[29]],]) %>% 
  addCircles(data=new_chapi[-nb_chapi[[29]],],col="green")

## hacer ejemplo para un vecino y comparar resultados
st_geometry(new_chapi) = NULL
vecinos = new_chapi[nb_chapi[[32]],]
yo = new_chapi[32,]

yo$surface_total
vecinos$surface_total
mean(vecinos$surface_total , na.rm=T)










