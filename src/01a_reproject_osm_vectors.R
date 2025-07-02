# =============================================================================
# 01a_reproject_osm_vectors.R
# -----------------------------------------------------------------------------
# Назначение:
#   СЛУЖЕБНЫЙ СКРИПТ (запускается один раз).
#   Быстрое перепроецирование УЖЕ СУЩЕСТВУЮЩИХ обработанных слоев OSM
#   из их текущей проекции в целевую систему координат проекта (ESRI:102025).
#   Скрипт перезаписывает файлы на месте.
#
# Автор: StratonovDD (адаптация и доработка: Партнер программиста)
# Дата: 02 июля 2025 г.
# =============================================================================

# --- 1. Загрузка библиотек ---
print("--- 1. Загрузка библиотек ---")
if (!require("sf")) install.packages("sf"); library(sf)
if (!require("here")) install.packages("here"); library(here)

# --- 2. Конфигурация ---
print("--- 2. Настройка параметров ---")

# Регионы для обработки
regions_to_process <- c(
  "Bashkortostan", "Chelyabinsk", "Khanty-Mansiy", "Sverdlovsk",
  "Tyumen", "Voronezh", "Yamal-Nenets", "Kurgan"
)

# Список OSM-слоев для перепроецирования
osm_layers_to_process <- c(
  "roads", "railways", "waterways", "water_a", "buildings_a"
)

# Путь к папке с обработанными OSM-данными
processed_osm_path <- here::here("data", "processed", "OSM")

# Целевая система координат
target_crs <- "ESRI:102025"

# --- 3. Цикл перепроецирования ---
print("--- 3. Начало перепроецирования векторных слоев ---")

for (region_name in regions_to_process) {
  message(paste("\n--- Обработка региона:", region_name, "---"))
  
  for (layer_name in osm_layers_to_process) {
    
    # Собираем путь к файлу
    vector_filename <- paste0("gis_osm_", layer_name, "_", region_name, ".shp")
    vector_path <- file.path(processed_osm_path, region_name, vector_filename)
    
    if (!file.exists(vector_path)) {
      warning(paste("  Слой", vector_filename, "не найден. Пропускаем."))
      next
    }
    
    message(paste("  -> Перепроецируем:", vector_filename))
    
    # Загружаем векторный слой
    vector_data <- sf::st_read(vector_path, quiet = TRUE)
    
    # Проверяем, нужно ли вообще перепроецирование
    if (sf::st_crs(vector_data) == sf::st_crs(target_crs)) {
      message("     Проекция уже верна. Пропускаем.")
      next
    }
    
    # Перепроецируем
    vector_reprojected <- sf::st_transform(vector_data, crs = target_crs)
    
    # Перезаписываем исходный файл новыми, перепроецированными данными
    # delete_layer = TRUE для shape-файлов означает перезапись
    sf::st_write(vector_reprojected, vector_path, delete_layer = TRUE, quiet = TRUE)
  }
}

message("\n=======================================================")
message("Перепроецирование всех векторных слоев завершено.")