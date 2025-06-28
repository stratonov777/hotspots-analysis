# 1_prepare_data(tpoint;osm).R

# Установка рабочей директории
setwd("B:/YandexDisk/Projects/Analysis_hotspots_in_Russian_regions") 

# Загрузка необходимых библиотек
library(sf)
library(dplyr)
library(raster)

# --- 0. Приведение всех файлов к WGS 84 ---

# Функция для преобразования CRS шейп-файла
transform_to_wgs84 <- function(shapefile) {
  objects <- st_read(shapefile)

  # Проверка наличия CRS
  if (is.na(st_crs(objects))) {
    st_crs(objects) <- 4326
  }

  if (st_crs(objects) != st_crs(4326)) {
    objects <- st_transform(objects, 4326)
    st_write(objects, shapefile, overwrite = TRUE)
  }
  return(objects)
}


# --- 1. Подготовка границ Воронежской области ---

# Загрузка границ области и установка CRS
svrd_border <- st_read("data/raw/borders/Bashkortostan.shp")
# Bashkortostan.shp
# Chelyabinsk.shp")
# Khanty-Mansiy.shp")
# Kurgan.shp")
# Sverdlovsk.shp")
# Tyumen.shp")
# Voronezh.shp")
# Yamal-Nenets.shp")

st_crs(svrd_border) <- 4326

# Пути к шейп-файлам с объектами
roads_path <- "data/raw/OSM/gis_osm_roads.shp"
railways_path <- "data/raw/OSM/gis_osm_railways.shp"
waterways_path <- "data/raw/OSM/gis_osm_waterways.shp"
water_path <- "data/raw/OSM/gis_osm_water.shp"
buildings_path <- "data/raw/OSM/gis_osm_buildings.shp"

# Преобразование CRS для всех шейп-файлов
transform_to_wgs84(roads_path)
transform_to_wgs84(railways_path)
transform_to_wgs84(waterways_path)
transform_to_wgs84(water_path)
transform_to_wgs84(buildings_path)


# Функция для вырезания объектов внутри границ области
crop_to_svrd <- function(shapefile_path) {
  objects <- st_read(shapefile_path)

  # Проверка наличия CRS и преобразование, если необходимо
  if (is.na(st_crs(objects))) {
    st_crs(objects) <- 4326
  }
  objects <- st_transform(objects, st_crs(svrd_border))

  cropped_objects <- st_intersection(objects, svrd_border)
  return(cropped_objects)
}

# Вырезание объектов и сохранение в новые шейп-файлы
roads_svrd <- crop_to_svrd(roads_path)
st_write(roads_svrd, "data/processed/OSM/gis_osm_roads_Bashkortostan.shp")

railways_svrd <- crop_to_svrd(railways_path)
st_write(railways_svrd, "data/processed/OSM/gis_osm_railways_Bashkortostan.shp")

waterways_svrd <- crop_to_svrd(waterways_path)
st_write(waterways_svrd, "data/processed/OSM/gis_osm_waterways_Bashkortostan.shp")

water_svrd <- crop_to_svrd(water_path)
st_write(water_svrd, "data/processed/OSM/gis_osm_water_Bashkortostan.shp")

buildings_svrd <- crop_to_svrd(buildings_path)
st_write(buildings_svrd, "data/processed/OSM/gis_osm_buildings_Bashkortostan.shp")


# --- 2. Обработка данных о термоточках ---

# Список файлов с термоточками
tpoint_files <- list.files("data/raw/Modis", pattern = "*.csv", full.names = TRUE)

# Чтение и объединение данных из всех файлов
tpoints_all <- lapply(tpoint_files, read.csv) %>%
  bind_rows()

# Удаление строк с пропущенными значениями в координатах
tpoints_all <- tpoints_all[complete.cases(tpoints_all[, c("latitude", "longitude")]), ]

# Преобразование данных в пространственный объект
tpoints_sf <- st_as_sf(tpoints_all, coords = c("longitude", "latitude"), crs = 4326)

# Выбор точек внутри границ области
tpoints_svrd <- st_intersection(tpoints_sf, svrd_border)

# Удаление повторяющихся столбцов (кроме geometry)
tpoints_svrd <- tpoints_svrd[, !duplicated(names(tpoints_svrd))]

# Добавление координат в dataframe
tpoints_svrd$longitude <- st_coordinates(tpoints_svrd)[,1]
tpoints_svrd$latitude <- st_coordinates(tpoints_svrd)[,2]

# Преобразование в обычный dataframe
tpoints_df <- as.data.frame(tpoints_svrd)

# Порядок столбцов для сохранения
column_order <- c("latitude", "longitude", "brightness", "scan", "track", "acq_date",
                  "acq_time", "satellite", "instrument", "confidence", "version",
                  "bright_t31", "frp", "daynight", "type")

# Выбор столбцов в нужном порядке
tpoints_df <- tpoints_df[, column_order]

# Сохранение данных о термоточках
write.csv(tpoints_df, "data/processed/tpoints_Bashkortostan.csv", row.names = FALSE)

# Сохранение данных о термоточках с перезаписью файла
# st_write(tpoints_svrd, "data/processed/tpoints_Bashkortostan.shp", delete_dsn = TRUE)
