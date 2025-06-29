# =============================================================================
# 02_preprocess_hotspots_data.R
# -----------------------------------------------------------------------------
# Назначение:
#   Выполняет предварительную обработку данных о термоточках (MODIS).
#   Скрипт последовательно читает каждый исходный CSV-файл, преобразует
#   в пространственный формат, а затем в цикле обрезает по границам
#   каждого из 8 регионов. Результаты для каждого региона накапливаются
#   и в конце сохраняются в единый итоговый файл.
#
# Автор: StratonovDD
# Дата: 29 июня 2025 г.
# =============================================================================

# --- 1. Загрузка библиотек ---
if (!require("sf")) install.packages("sf"); library(sf)
if (!require("here")) install.packages("here"); library(here)
if (!require("dplyr")) install.packages("dplyr"); library(dplyr)
if (!require("readr")) install.packages("readr"); library(readr)

# --- 2. Конфигурация ---
regions_to_process <- c(
  "Bashkortostan", "Chelyabinsk", "Khanty-Mansiy", "Sverdlovsk",
  "Tyumen", "Voronezh", "Yamal-Nenets", "Kurgan"
)

# Определение путей
borders_base_path   <- here::here("data", "raw", "borders")
hotspots_source_path <- here::here("data", "raw", "Modis")
processed_base_path <- here::here("data", "processed", "hotspots")

# Создаем папку для итоговых файлов, если ее нет
dir.create(processed_base_path, showWarnings = FALSE, recursive = TRUE)

# --- 3. Подготовка границ регионов ---
# Загрузим все границы в один список, чтобы не читать их с диска в цикле
message("Загрузка границ регионов...")
borders_list <- list()
for (region_name in regions_to_process) {
  border_path <- file.path(borders_base_path, paste0(region_name, ".shp"))
  if (file.exists(border_path)) {
    borders_list[[region_name]] <- st_read(border_path, quiet = TRUE) %>%
      st_transform(crs = 4326)
  } else {
    warning(paste("Файл границы для", region_name, "не найден. Регион будет пропущен."))
  }
}
# Обновляем список регионов на случай, если какие-то файлы не нашлись
regions_to_process <- names(borders_list)

# --- 4. Обработка термоточек ---

# Находим все CSV файлы в папке
tpoint_files <- list.files(hotspots_source_path, pattern = "\\.csv$", full.names = TRUE)

if (length(tpoint_files) == 0) {
  stop("В папке", hotspots_source_path, "не найдено CSV файлов с термоточками.")
}

# Создаем пустой список для накопления результатов по каждому региону
results_by_region <- setNames(vector("list", length(regions_to_process)), regions_to_process)

# Внешний цикл по файлам с термоточками (экономно к памяти)
for (i in seq_along(tpoint_files)) {
  
  csv_file <- tpoint_files[i]
  message(paste0("Обработка файла ", i, " из ", length(tpoint_files), ": ", basename(csv_file)))
  
  # Читаем один CSV
  hotspots_chunk <- read_csv(csv_file, show_col_types = FALSE) %>%
    filter(!is.na(latitude) & !is.na(longitude))
  
  if (nrow(hotspots_chunk) == 0) {
    message("  Файл пуст или не содержит координат. Пропускаем.")
    next
  }
  
  # Преобразуем в sf-объект
  hotspots_sf <- st_as_sf(hotspots_chunk, coords = c("longitude", "latitude"), crs = 4326)
  
  # Внутренний цикл по регионам - обрезаем текущую порцию данных
  for (region_name in regions_to_process) {
    region_border <- borders_list[[region_name]]
    
    # st_filter - быстрая операция для точек
    clipped_points <- st_filter(hotspots_sf, region_border)
    
    if (nrow(clipped_points) > 0) {
      # Добавляем обрезанные точки в соответствующий список региона
      results_by_region[[region_name]] <- bind_rows(
        results_by_region[[region_name]],
        clipped_points
      )
    }
  }
  
  # Очистка памяти
  rm(hotspots_chunk, hotspots_sf)
  gc()
}

# --- 5. Сохранение итоговых файлов ---
message("\n=======================================================")
message("Все файлы обработаны. Сохранение итоговых данных по регионам...")

# Плюс мы добавим 'longitude' и 'latitude', которые создадим на следующем шаге.
column_order <- c(
  "latitude", "longitude", "brightness", "scan", "track", "acq_date",
  "acq_time", "satellite", "instrument", "confidence", "version",
  "bright_t31", "frp", "daynight", "type"
)

for (region_name in regions_to_process) {
  
  final_df_raw_sf <- results_by_region[[region_name]]
  
  if (!is.null(final_df_raw_sf) && nrow(final_df_raw_sf) > 0) {
    
    # =========================================================================
    # Функция st_coordinates() извлекает из геометрии матрицу с X и Y.
    # Мы используем mutate(), чтобы создать новые колонки с этими значениями.
    final_df_with_coords <- final_df_raw_sf %>%
      mutate(
        longitude = sf::st_coordinates(.)[, 1],
        latitude  = sf::st_coordinates(.)[, 2]
      )
    # =========================================================================
    
    # Теперь, когда у нас есть отдельные колонки, преобразуем в обычный data.frame,
    # удаляя сложный столбец геометрии.
    final_df <- as.data.frame(final_df_with_coords) %>%
      select(-geometry)
    
    # Находим, какие из желаемых колонок реально есть в данных
    existing_columns <- intersect(column_order, colnames(final_df))
    
    # Выбираем только существующие колонки в заданном порядке
    final_df_selected <- final_df[, existing_columns]
    
    output_filename <- paste0("hotspots_", region_name, "_2000_2023.csv")
    output_path <- file.path(processed_base_path, output_filename)
    
    # Сохраняем отформатированный data.frame
    write_csv(final_df_selected, output_path)
    message(paste("  УСПЕХ: Сохранено", nrow(final_df_selected), "термоточек для региона", region_name))
    
  } else {
    message(paste("  Для региона", region_name, "не найдено термоточек. Файл не создан."))
  }
}

message("Обработка термоточек для всех регионов завершена.")
ы