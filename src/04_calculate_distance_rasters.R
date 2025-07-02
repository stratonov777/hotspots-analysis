# =============================================================================
# 04_calculate_distance_rasters.R (Версия 3.0 - Надежный метод terra)
# -----------------------------------------------------------------------------
# Назначение:
#   Создает растровые слои расстояний. Использует гибридный подход:
#   быстрая растеризация через GDAL и быстрый расчет расстояний на
#   растре средствами пакета terra. Этот метод не зависит от системных
#   настроек и является самым надежным.
#
# Автор: StratonovDD (адаптация и доработка: Партнер программиста)
# Дата: 02 июля 2025 г.
# =============================================================================

# --- 1. Загрузка библиотек ---
print("--- 1. Загрузка библиотек ---")
if (!require("terra")) install.packages("terra"); library(terra)
if (!require("sf")) install.packages("sf"); library(sf)
if (!require("here")) install.packages("here"); library(here)

# --- 2. Конфигурация ---
print("--- 2. Настройка параметров ---")

regions_to_process <- c(
  "Bashkortostan", "Chelyabinsk", "Khanty-Mansiy", "Sverdlovsk",
  "Tyumen", "Voronezh", "Yamal-Nenets", "Kurgan"
)

osm_layers_to_process <- c(
  "roads", "railways", "waterways", "water_a", "buildings_a"
)

base_rasters_path   <- here::here("data", "processed", "base_rasters")
osm_vectors_path    <- here::here("data", "processed", "OSM")
output_path_base    <- here::here("data", "processed", "distance_rasters")

target_res <- 100 
overwrite_files <- TRUE


# --- 3. Основной цикл по регионам и слоям ---
print("--- 3. Начало расчета растров расстояний (гибридный метод GDAL+terra) ---")

for (region_name in regions_to_process) {
  message(paste("\n======================================================="))
  message(paste("Обработка региона:", region_name))
  
  base_raster_path <- file.path(base_rasters_path, paste0("base_raster_", region_name, ".tif"))
  if (!file.exists(base_raster_path)) {
    warning(paste("  Не найдена базовая маска для региона:", region_name, ". Пропуск."))
    next
  }
  base_raster <- terra::rast(base_raster_path)
  base_ext <- ext(base_raster)
  
  region_output_folder <- file.path(output_path_base, region_name)
  dir.create(region_output_folder, showWarnings = FALSE, recursive = TRUE)
  
  for (layer_name in osm_layers_to_process) {
    message(paste("  -> Расчет расстояния до слоя:", layer_name))
    
    input_vector_path <- file.path(osm_vectors_path, region_name, paste0("gis_osm_", layer_name, "_", region_name, ".shp"))
    output_raster_path <- file.path(region_output_folder, paste0("distance_to_", layer_name, ".tif"))
    
    if (file.exists(output_raster_path) && !overwrite_files) {
      message("    Файл уже существует. Пропускаем.")
      next
    }
    if (!file.exists(input_vector_path)) {
      warning(paste("    Векторный файл не найден:", input_vector_path, ". Пропускаем."))
      next
    }
    
    # --- Шаг 1: Растеризация через GDAL ---
    temp_rasterize_path <- file.path(tempdir(), "rasterized_temp.tif")
    sf::gdal_utils(
      util = "rasterize",
      source = input_vector_path,
      destination = temp_rasterize_path,
      options = c("-burn", "1", "-tr", target_res, target_res, "-te", base_ext$xmin, base_ext$ymin, base_ext$xmax, base_ext$ymax, "-ot", "Byte", "-init", "0")
    )
    message("    Шаг 1/2: Вектор успешно растеризован.")
    
    # --- Шаг 2: Расчет расстояний средствами terra ---
    rasterized_layer <- terra::rast(temp_rasterize_path)
    
    # Устанавливаем 0 как NA, чтобы distance искала расстояние от ячеек NA до ячеек со значением 1
    rasterized_layer[rasterized_layer == 0] <- NA
    
    # Рассчитываем расстояние. Этот метод очень быстрый для растровых данных.
    distance_raster <- terra::distance(rasterized_layer)
    
    # Маскируем результат по точным границам региона
    final_raster <- terra::mask(distance_raster, base_raster)
    
    # Сохраняем итоговый растр
    terra::writeRaster(
      final_raster,
      output_raster_path,
      overwrite = TRUE,
      datatype = 'FLT4S',
      gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9", "TILED=YES")
    )
    
    unlink(temp_rasterize_path)
    
    message(paste("    Шаг 2/2: УСПЕХ! Растр расстояний сохранен в:", output_raster_path))
  }
}
message("\n=======================================================")
message("Полная обработка и создание растров расстояний завершены.")