## paquetes que se utilizarán 
install.packages("tidyverse")
install.packages("data.table")
install.packages("sf")
install.packages("leaflet")
install.packages("leaflet.extras")
install.packages("leafem")
install.packages("terra")
install.packages("rgdal")

# Llamar librerías
library(tidyverse)
library(data.table)
library(leaflet)
library(sf)
library(terra)
library(rgdal)
library(leaflet.extras)
library(leafem)


#cargar csv
felinos <-
  read_delim(
    file = "felinos.csv",
    col_select = c(
      "species",
      "stateProvince",
      "locality",
      "eventDate")
    ) 

# Lectura de un archivo CSV con registros de presencia de felinos en Costa Rica
felinos <-
  st_read(
    "felinos.csv",
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude", # argumento columna de longitud decimal
      "Y_POSSIBLE_NAMES=decimalLatitude"   # argumento columna de latitud decimal
    ),
    quiet = TRUE
  )
# Retirar datos vacíos 
felinos <- drop_na(data = felinos)

# Cambiar nombres 
felinos <-
  felinos |>
  rename( 
    especies = species,
    provincia= stateProvince,
    localidad= locality,
    fecha = eventDate)


### Tabla
felinos |>
  data.table(options = list (
    pageLength = 5,
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
  ))


### Los métodos son funciones que se usan para la programación orientada a objetos. ver en la consolads
### cargar datos del SNIT 
# Lectura de una capa vectorial (GeoJSON) de provincias de Costa Rica
asp <-
  st_read(
    "asp.shp",
    quiet = TRUE # para evitar el despliegue de mensajes
  )

# Clase del objeto asp
class(asp)
# Clase del objeto felinos 
class (felinos)



# Asignación de un CRS al objeto felinos
st_crs(felinos) <- 4326 #WGS84

#revisar la proyección de felinos 
st_crs(felinos) 


## El método st_transform() transforma un objeto sf a un nuevo CRS.

# Transformación del CRS del objeto provincias a WGS84 (EPSG = 4326)
asp <-
  asp |>
  st_transform(4326) 

#### MAPA WEB

## cargar ráster 
# Lectura de una capa raster de altitud
altitud <-
  rast(
    "altitud.tif"
  )
class(altitud)

# Información general sobre el objeto altitud
altitud
# CRS del objeto altitud
crs(altitud)
# Paleta de colores de altitud de Costa Rica
colores_altitud <-
  colorNumeric(terrain.colors(10),
               values(altitud),
               na.color = "transparent")

# Mapa leaflet básico con capas de altitud, ASP y registros de presencia de felinos
leaflet() |>
  setView(# centro y nivel inicial de acercamiento
    lng = -84.19452,
    lat = 9.852735,
    zoom = 7) |>  
  addTiles(group = "OpenStreetMap", "Esri.NatGeoWorldMap") |>
  addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Esri.NatGeoWorldMap") |> #agregar otra capa base siemrpe con los pipes
  addProviderTiles(providers$Esri.WorldTopoMap , group = "Esri.WorldTopoMap")|># capa base de OSM y NatGeo Map
  addRasterImage( # capa raster de altitud
    raster(altitud), # conversión de SpatRaster a RasterLayer 
    colors = colores_altitud, # paleta de colores
    opacity = 0.6,
    group = "Altitud"
  ) |>
  addLegend(
    title = "Altitud",
    values = values(altitud),
    pal = colores_altitud,
    position = "bottomleft",
    group = "Altitud"
  ) |>
  addPolygons(
    data = asp,
    color = "red",
    fillColor = "transparent",
    stroke = TRUE,
    weight = 1.5,
    group = "Áreas Silvestres Protegidas",
    popup = paste(
      paste0("<strong>Área Silvestre Protegida: </strong>", asp$nombre_asp),
      sep = '<br/>'
    )    
  ) |>
  addCircleMarkers(
    data = felinos,
    stroke = F,
    radius = 4,
    fillColor = 'blue',
    fillOpacity = 1,
    group = "Felinos",
    popup = paste(
      paste0("<strong>Especie: </strong>", felinos$especies),
      paste0("<strong>Provincia: </strong>", felinos$provincia),
      paste0("<strong>Localidad: </strong>", felinos$localidad),
      paste0("<strong>Fecha: </strong>", felinos$fecha),
      sep = '<br/>'
    )    
  ) |>
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Esri.NatGeoWorldMap"),
    overlayGroups = c("Altitud","Áreas Silvestres Protegidas","Felinos" )) |> # control de capas
  addResetMapButton() |> # botón de reinicio
  addSearchOSM() |> # búsqueda en OSM
  addMouseCoordinates() |> # coordenadas del puntero del ratón
  addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = FALSE)) |> # barra de escala
  addMiniMap(position = "bottomright") # mapa de ubicación