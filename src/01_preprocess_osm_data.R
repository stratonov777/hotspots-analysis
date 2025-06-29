# =============================================================================
# 01_preprocess_osm_data.R
# -----------------------------------------------------------------------------
# Назначение:
#   Выполняет эффективную предварительную обработку исходных векторных
#   данных OSM, используя только возможности пакета 'sf'.
#   Применяется двухэтапная фильтрация для ускорения обрезки:
#   1. Быстрая фильтрация по габаритному прямоугольнику (bounding box) с
#      использованием пространственного индекса.
#   2. Точная обрезка по границам региона на предварительно
#      отфильтрованном подмножестве данных.
#
# Автор: StratonovDD
# Дата: 28 июня 2025 г.
# =============================================================================

# --- 1. Загрузка библиотек ---
if (!require("sf")) install.packages("sf"); library(sf)
if (!require("here")) install.packages("here"); library(here)

# --- 2. Конфигурация ---
# В этом блоке определяются все настраиваемые параметры скрипта.

regions_to_process <- c(
  "Bashkortostan", "Chelyabinsk", "Khanty-Mansiy", "Sverdlovsk",
  "Tyumen", "Voronezh", "Yamal-Nenets", "Kurgan"
)

# Определение относительных путей
borders_base_path <- here::here("data", "raw", "borders")
osm_base_path     <- here::here("data", "raw", "OSM")
processed_base_path <- here::here("data", "processed", "OSM")

# Целевая система координат
target_crs <- "EPSG:32637" # WGS 84 / UTM zone 37N

# Информация об исходных OSM-слоях
osm_layers_info <- list(
  roads       = "gis_osm_roads.shp",
  railways    = "gis_osm_railways.shp",
  waterways   = "gis_osm_waterways.shp",
  water_a     = "gis_osm_water.shp",
  buildings_a = "gis_osm_buildings.shp"
)

# --- 3. Основной цикл обработки по слоям OSM ---
# Внешний цикл итерирует по типам OSM-слоев для экономии памяти.

for (layer_name in names(osm_layers_info)) {
  
  source_layer_path <- file.path(osm_base_path, osm_layers_info[[layer_name]])
  if (!file.exists(source_layer_path)) {
    warning(paste("Исходный файл не найден:", source_layer_path, ". Пропускаем слой."))
    next
  }
  
  # 3.1. Загрузка одного полного OSM-слоя
  message(paste("\n======================================================="))
  message(paste("Загрузка и обработка слоя:", layer_name))
  
  osm_full_layer <- st_read(source_layer_path, quiet = TRUE) %>%
    st_transform(crs = 4326)
  
  message("Слой успешно загружен. Начало обрезки по регионам...")
  
  # 3.2. Внутренний цикл по регионам
  for (region_name in regions_to_process) {
    
    message(paste("--- Обработка для региона:", region_name, "---"))
    
    border_path <- file.path(borders_base_path, paste0(region_name, ".shp"))
    if (!file.exists(border_path)) {
      warning(paste("  Файл границы не найден:", border_path, ". Пропуск."))
      next
    }
    
    region_border <- st_read(border_path, quiet = TRUE) %>%
      st_transform(crs = 4326)
    
    # --- Двухэтапная фильтрация ---
    
    # Шаг А и Б: Быстрая фильтрация по индексу (прямоугольнику)
    # Эта операция выбирает из большого слоя только те объекты, которые
    # пересекаются с габаритным прямоугольником границы региона.
    # sf делает это эффективно, используя пространственный индекс.
    message("  Шаг 1/3: Быстрая фильтрация по индексу...")
    osm_subset <- osm_full_layer[region_border, ]
    
    if (nrow(osm_subset) == 0) {
      message("  Пересекающихся объектов не найдено. Пропускаем.")
      next
    }
    message(paste("  Найдено", nrow(osm_subset), "потенциальных объектов."))
    
    # Шаг В: Точная обрезка на малом подмножестве данных
    message("  Шаг 2/3: Точная обрезка...")
    # st_make_valid() для стабильности при работе с потенциально некорректными геометриями
    clipped_layer <- st_intersection(st_make_valid(osm_subset), st_make_valid(region_border))
    
    # Шаг Г: Перепроецирование в целевую систему координат
    message("  Шаг 3/3: Перепроецирование...")
    final_layer <- st_transform(clipped_layer, crs = target_crs)
    
    # Сохранение результата
    if (nrow(final_layer) > 0) {
      output_dir_region <- file.path(processed_base_path, region_name)
      dir.create(output_dir_region, showWarnings = FALSE, recursive = TRUE)
      
      output_filename <- paste0("gis_osm_", layer_name, "_", region_name, ".shp")
      output_path <- file.path(output_dir_region, output_filename)
      
      st_write(final_layer, output_path, append = FALSE, quiet = TRUE)
      message(paste("  УСПЕХ: Результат сохранен в:", output_path))
    }
  } # Конец цикла по регионам
  
  # 3.3. Очистка памяти перед следующим большим файлом
  rm(osm_full_layer)
  gc()
  message(paste("Обработка слоя", layer_name, "завершена. Память очищена."))
  
} # Конец внешнего цикла по слоям OSM

message("\n=======================================================")
message("Полная обработка всех OSM-слоев для всех регионов завершена.")