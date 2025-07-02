# =============================================================================
# test_gdal_call.R
# -----------------------------------------------------------------------------
# Назначение:
#   ДИАГНОСТИЧЕСКИЙ СКРИПТ для тестирования прямого вызова утилиты
#   gdal_proximity.py из R. Скрипт пытается обработать один файл и
#   перехватывает все сообщения (включая ошибки) от GDAL.
# =============================================================================

# --- 1. Настройка ---
print("--- 1. Настройка библиотек и параметров для теста ---")
if (!require("terra")) install.packages("terra"); library(terra)
if (!require("sf")) install.packages("sf"); library(sf)
if (!require("here")) install.packages("here"); library(here)

# --- Параметры для одного тестового файла ---
region_to_test <- "Khanty-Mansiy" # Регион для теста
layer_to_test  <- "roads"          # Слой для теста
target_res     <- 1000

# --- Пути ---
base_rasters_path   <- here::here("data", "processed", "base_rasters")
osm_vectors_path    <- here::here("data", "processed", "OSM")
output_path_base    <- here::here("data", "processed", "distance_rasters")

# --- 2. Подготовка тестовых данных ---
print("--- 2. Подготовка данных для одного файла ---")

# Пути для конкретного тестового случая
base_raster_path <- file.path(base_rasters_path, paste0("base_raster_", region_to_test, ".tif"))
input_vector_path <- file.path(osm_vectors_path, region_to_test, paste0("gis_osm_", layer_to_test, "_", region_to_test, ".shp"))
# Создаем папку для вывода, если ее нет
dir.create(file.path(output_path_base, region_to_test), showWarnings = FALSE, recursive = TRUE)
output_raster_path <- file.path(output_path_base, region_to_test, paste0("TEST_distance_to_", layer_to_test, ".tif"))

# Проверяем наличие входных файлов
if (!file.exists(base_raster_path) || !file.exists(input_vector_path)) {
  stop("Один из входных файлов для теста не найден. Проверьте пути.")
}

# --- Шаг 1: Растеризация ---
message(" -> Шаг 1: Растеризация тестового вектора...")
base_info <- terra::rast(base_raster_path)
base_ext <- ext(base_info)
temp_rasterize_path <- file.path(tempdir(), "test_rasterized.tif")

sf::gdal_utils(
  util = "rasterize",
  source = input_vector_path,
  destination = temp_rasterize_path,
  options = c("-burn", "1", "-tr", target_res, target_res, "-te", base_ext$xmin, base_ext$ymin, base_ext$xmax, base_ext$ymax, "-ot", "Byte")
)
message("    Временный растеризованный файл создан.")

# --- 3. ТЕСТИРОВАНИЕ СИСТЕМНОГО ВЫЗОВА ---
print("\n--- 3. ЗАПУСК ТЕСТА: Прямой вызов gdal_proximity.py с перехватом вывода ---")

# Собираем команду и аргументы
command <- "gdal_proximity.py"
args <- c(
  "-distunits", "GEO",
  "-ot", "Float32",
  temp_rasterize_path, # Исходный файл
  output_raster_path   # Выходной файл
)

# Выполняем команду и ПЕРЕХВАТЫВАЕМ ВЕСЬ ВЫВОД (стандартный и ошибки)
gdal_output <- system2(
  command, 
  args = args,
  stdout = TRUE,
  stderr = TRUE
)

# --- 4. Анализ результатов теста ---
print("\n--- 4. РЕЗУЛЬТАТЫ ТЕСТА ---")

# Выводим все, что сказал GDAL
if (length(gdal_output) > 0) {
  print("Перехваченный вывод от GDAL:")
  cat(gdal_output, sep = "\n")
} else {
  print("GDAL не вернул никаких сообщений.")
}

# Проверяем, был ли файл реально создан
if (file.exists(output_raster_path)) {
  print("\n[ВЕРДИКТ]: УСПЕХ! Тестовый растр был успешно создан на диске.")
} else {
  print("\n[ВЕРДИКТ]: СБОЙ! Тестовый растр НЕ был создан, несмотря на сообщения.")
}

# Очистка
unlink(temp_rasterize_path)
print("\n--- Тест завершен. ---")