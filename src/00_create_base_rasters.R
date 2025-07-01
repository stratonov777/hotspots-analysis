# =============================================================================
# 00_create_base_rasters.R
# -----------------------------------------------------------------------------
# Назначение:
#   Скрипт для создания легковесных базовых растров (масок)
#   для каждого исследуемого региона. Эти маски будут использоваться в
#   качестве шаблонов во всех последующих скриптах растрового анализа.
#
# Автор: StratonovDD / Партнер программиста (реализация)
# Дата: 01 июля 2025 г.
# =============================================================================

# --- 1. Загрузка библиотек ---
if (!require("terra")) install.packages("terra"); library(terra)
if (!require("sf")) install.packages("sf"); library(sf)
if (!require("here")) install.packages("here"); library(here)

# --- 2. Конфигурация ---
message("--- Настройка параметров ---")
regions_to_process <- c(
  "Bashkortostan", "Chelyabinsk", "Khanty-Mansiy", "Sverdlovsk",
  "Tyumen", "Voronezh", "Yamal-Nenets", "Kurgan"
)

# Параметры целевых растров
target_crs      <- "ESRI:102025" # Единая проекция проекта
target_res      <- 1000          # Разрешение в метрах (1 км)
overwrite_masks <- FALSE         # Перезаписывать маски, если они уже существуют?

# Пути
borders_input_path <- here::here("data", "raw", "borders")
masks_output_path  <- here::here("data", "processed", "base_rasters")

# Создаем папку для масок, если ее нет
dir.create(masks_output_path, showWarnings = FALSE, recursive = TRUE)

# --- 3. Цикл создания масок ---
message("--- Начало создания базовых растров-масок ---")
for (region_name in regions_to_process) {
  
  message(paste("Обработка региона:", region_name))
  
  # Определяем пути к файлам
  border_file_path <- file.path(borders_input_path, paste0(region_name, ".shp"))
  mask_output_file <- file.path(masks_output_path, paste0("base_raster_", region_name, ".tif"))
  
  # Проверяем, существует ли файл и нужно ли его перезаписывать
  if (file.exists(mask_output_file) && !overwrite_masks) {
    message("  Маска уже существует. Пропускаем.")
    next
  }
  
  if (!file.exists(border_file_path)) {
    warning(paste("  Файл границы не найден для региона:", region_name, ". Пропускаем."))
    next
  }
  
  # Загружаем и трансформируем векторную границу
  region_vector <- sf::st_read(border_file_path, quiet = TRUE) %>%
    sf::st_transform(crs = target_crs)
  
  # Создаем пустой растр с нужными параметрами на основе охвата вектора
  base_raster <- terra::rast(extent = ext(region_vector), resolution = target_res, crs = target_crs)
  
  # "Вжигаем" полигон в растр. Пиксели внутри полигона получат значение 1, остальные - NA.
  # Это самый эффективный способ создания маски.
  region_mask <- terra::rasterize(region_vector, base_raster, field = 1)
  names(region_mask) <- region_name
  
  # Сохраняем растр с максимальным сжатием и самым легковесным типом данных
  terra::writeRaster(
    region_mask,
    mask_output_file,
    overwrite = TRUE,
    datatype = 'INT1U', # 1-байтовое целое без знака (0-255), идеально для маски 0/1
    gdal = c("COMPRESS=DEFLATE", "ZLEVEL=9", "TILED=YES")
  )
  
  message(paste("  УСПЕХ: Маска сохранена в:", mask_output_file))
}

message("--- Создание всех базовых масок завершено. ---")