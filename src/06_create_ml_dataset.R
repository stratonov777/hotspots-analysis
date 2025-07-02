# =============================================================================
# 06_create_ml_dataset.R
# -----------------------------------------------------------------------------
# Назначение:
#   Создает итоговый датасет для машинного обучения. Скрипт генерирует
#   случайные точки для каждого региона и для каждой точки извлекает
#   значения из всех статических и динамических растровых слоев за
#   каждый временной период. Включает очистку и обогащение данных.
#
# Автор: StratonovDD (адаптация и доработка: Партнер программиста)
# Дата: 02 июля 2025 г.
# =============================================================================

# --- 1. ЗАГРУЗКА БИБЛИОТЕК И НАСТРОЙКА ---
print("--- 1. Настройка проекта ---")
if (!require("terra")) install.packages("terra"); library(terra)
if (!require("sf")) install.packages("sf"); library(sf)
if (!require("dplyr")) install.packages("dplyr"); library(dplyr)
if (!require("readr")) install.packages("readr"); library(readr)
if (!require("tidyr")) install.packages("tidyr"); library(tidyr)
if (!require("here")) install.packages("here"); library(here)

# --- Параметры ---
regions_to_process <- c(
  "Bashkortostan", "Chelyabinsk", "Khanty-Mansiy", "Sverdlovsk",
  "Tyumen", "Voronezh", "Yamal-Nenets", "Kurgan"
)
number_of_random_points <- 200      # Количество случайных точек на регион
target_crs <- "ESRI:102025"       # Единая проекция проекта
base_raster_res <- 100              # Целевое разрешение для гармонизации (100м)
set.seed(123) # для воспроизводимости

# --- Пути ---
path_borders         <- here::here("data", "raw", "borders")
path_landcover       <- here::here("data", "processed", "landcover")
path_dist_rasters    <- here::here("data", "processed", "distance_rasters")
path_dem             <- here::here("data", "processed", "DEM")
path_monthly_rasters <- here::here("data", "processed", "monthly_rasters")
path_lagged_rasters  <- here::here("data", "processed", "lagged_rasters")
path_output_dir      <- here::here("data", "processed", "final_dataset")
dir.create(path_output_dir, showWarnings = FALSE, recursive = TRUE)
path_output_csv <- file.path(path_output_dir, "ml_dataset_all_regions.csv")


# --- 2. ОСНОВНОЙ ЦИКЛ ПО РЕГИОНАМ ---
print("--- 2. Начало основного цикла по регионам ---")

# Список для сбора итоговых таблиц по каждому региону
all_regions_data_list <- list()

for (region_name in regions_to_process) {
  message(paste("\n======================================================="))
  message(paste("Обработка региона:", region_name))
  
  # --- 2.1. Динамическое определение путей для текущего региона ---
  path_borders_shp_region      <- file.path(path_borders, paste0(region_name, ".shp"))
  path_landcover_raster_region <- file.path(path_landcover, paste0(region_name, "_landcover.tif"))
  path_dem_raster_region       <- file.path(path_dem, paste0(region_name, ".tif"))
  path_dist_rasters_region     <- file.path(path_dist_rasters, region_name)
  path_monthly_rasters_region  <- file.path(path_monthly_rasters, region_name)
  path_lagged_rasters_region   <- file.path(path_lagged_rasters, region_name)
  
  # --- 2.2. Подготовка статичных данных для региона ---
  message(" -> Подготовка статичных данных...")
  
  region_borders <- st_read(path_borders_shp_region, quiet = TRUE) %>% st_transform(crs = target_crs)
  
  # Генерируем случайные точки и сразу добавляем имя региона
  random_points_sf <- st_sample(region_borders, size = number_of_random_points, type = "random") %>%
    st_as_sf() %>%
    mutate(
      point_id = paste0(region_name, "_", 1:n()), # Уникальный ID точки
      region = region_name                       # !! ДОБАВЛЕНИЕ СТОЛБЦА РЕГИОНА !!
    )
  
  # Создаем шаблонный растр для гармонизации с высоким разрешением
  template_raster <- rast(ext(region_borders), resolution = base_raster_res, crs = target_crs)
  
  # Извлекаем данные DEM (в его родной проекции для эффективности)
  dem_raster_raw <- rast(path_dem_raster_region)
  points_reprojected_for_dem <- st_transform(random_points_sf, crs = crs(dem_raster_raw))
  dem_values <- terra::extract(dem_raster_raw, points_reprojected_for_dem, ID = FALSE)
  dem_df <- tibble(point_id = random_points_sf$point_id, dem = dem_values[[1]])
  
  # Собираем и извлекаем данные из других статичных растров (landcover, расстояния)
  static_raster_files <- c(
    `landcover` = path_landcover_raster_region,
    `dist_roads` = file.path(path_dist_rasters_region, "distance_to_roads.tif"),
    `dist_railways` = file.path(path_dist_rasters_region, "distance_to_railways.tif"),
    `dist_water_a` = file.path(path_dist_rasters_region, "distance_to_water_a.tif"),
    `dist_waterways` = file.path(path_dist_rasters_region, "distance_to_waterways.tif"),
    `dist_buildings_a` = file.path(path_dist_rasters_region, "distance_to_buildings_a.tif")
  )
  
  static_raster_files_exist <- static_raster_files[file.exists(static_raster_files)]
  
  static_values_list <- lapply(names(static_raster_files_exist), function(name) {
    r <- rast(static_raster_files_exist[name])
    method <- if (name == "landcover") "near" else "bilinear"
    # Ресамплируем растры с другим разрешением к нашему шаблону
    r_harmonized <- terra::resample(r, template_raster, method = method)
    terra::extract(r_harmonized, random_points_sf, ID = FALSE)
  })
  
  static_df_others <- bind_cols(static_values_list)
  names(static_df_others) <- names(static_raster_files_exist)
  static_df_final <- static_df_others %>% 
    mutate(point_id = random_points_sf$point_id) %>%
    left_join(dem_df, by = "point_id")
  
  # --- 2.3. Извлечение динамических данных для региона ---
  message(" -> Извлечение динамических данных...")
  
  # Сканируем доступные периоды (год_месяц) для данного региона
  existing_periods <- list.dirs(path_monthly_rasters_region, full.names = FALSE, recursive = TRUE) %>%
    grep("^[0-9]{4}/[0-9]{2}$", ., value = TRUE) %>%
    sub("/", "_", .)
  
  if (length(existing_periods) == 0) {
    warning(paste("Не найдено периодов с данными для региона:", region_name))
    next
  }
  
  monthly_data_list <- list()
  for (period_id in existing_periods) {
    year <- as.numeric(substr(period_id, 1, 4))
    month <- as.numeric(substr(period_id, 6, 7))
    
    dynamic_files <- c(
      `fire_density` = file.path(path_monthly_rasters_region, as.character(year), sprintf("%02d", month), paste0("fire_density_", period_id, ".tif")),
      `temperature` = file.path(path_monthly_rasters_region, as.character(year), sprintf("%02d", month), paste0("mean_temp_", period_id, ".tif")),
      `precipitation` = file.path(path_monthly_rasters_region, as.character(year), sprintf("%02d", month), paste0("accum_precip_", period_id, ".tif")),
      `precip_lag3` = file.path(path_lagged_rasters_region, paste0("precip_lag3_", period_id, ".tif")),
      `precip_lag6` = file.path(path_lagged_rasters_region, paste0("precip_lag6_", period_id, ".tif")),
      `temp_lag3` = file.path(path_lagged_rasters_region, paste0("temp_lag3_", period_id, ".tif")),
      `temp_lag6` = file.path(path_lagged_rasters_region, paste0("temp_lag6_", period_id, ".tif"))
    )
    
    monthly_values <- lapply(dynamic_files, function(f) {
      if (!file.exists(f)) return(tibble(value = NA_real_))
      r <- rast(f)
      r_harmonized <- terra::resample(r, template_raster, method = "bilinear")
      terra::extract(r_harmonized, random_points_sf, ID = FALSE)
    })
    
    monthly_df <- bind_cols(monthly_values)
    names(monthly_df) <- names(dynamic_files)
    monthly_df <- monthly_df %>% mutate(point_id = random_points_sf$point_id, period = period_id)
    monthly_data_list[[period_id]] <- monthly_df
  }
  
  # --- 2.4. Сборка данных для одного региона ---
  message(" -> Сборка итоговой таблицы для региона...")
  coords_df <- as.data.frame(st_coordinates(random_points_sf)) %>% rename(x = X, y = Y)
  
  region_final_df <- bind_rows(monthly_data_list) %>%
    left_join(static_df_final, by = "point_id") %>%
    mutate(x = coords_df$x[match(point_id, random_points_sf$point_id)],
           y = coords_df$y[match(point_id, random_points_sf$point_id)],
           region = region_name)
  
  all_regions_data_list[[region_name]] <- region_final_df
}

# --- 3. ФИНАЛЬНАЯ ОБРАБОТКА И СОХРАНЕНИЕ ---
message("\n=======================================================")
message("--- 3. Финальная сборка, очистка и обогащение ---")

# Собираем данные всех регионов в одну большую таблицу
if(length(all_regions_data_list) == 0) {
  stop("Не удалось собрать данные ни для одного региона.")
}
final_raw_df <- bind_rows(all_regions_data_list)

# Очистка (заполнение пропусков)
message(" -> Очистка данных...")
cleaned_df <- final_raw_df %>%
  mutate(fire_density = ifelse(is.na(fire_density), 0, fire_density)) %>%
  group_by(region) %>% # Заполняем пропуски средним значением В ПРЕДЕЛАХ РЕГИОНА
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  ungroup() %>%
  # Если после этого остались NA (например, целый столбец был пустым), заменяем их на 0
  mutate(across(everything(), ~replace_na(., 0)))

# Обогащение (создание новых признаков)
message(" -> Обогащение признаков...")
features_df <- cleaned_df %>%
  separate(period, into = c("year", "month"), sep = "_", remove = FALSE, convert = TRUE) %>%
  mutate(
    month_sin = sin(2 * pi * month / 12),
    month_cos = cos(2 * pi * month / 12),
    landcover = factor(paste0("lc_", as.integer(landcover)))
  )

# One-Hot Encoding для landcover
message(" -> One-Hot Encoding...")
final_df_encoded <- features_df %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = landcover, values_from = value, values_fill = 0)

# Финальный выбор и упорядочивание столбцов
message(" -> Финальное упорядочивание и сохранение...")
final_df_selected <- final_df_encoded %>%
  select(
    region, point_id, x, y, period, year, month,
    fire_density, # Целевая переменная
    month_sin, month_cos, temperature, precipitation,
    starts_with("precip_lag"), starts_with("temp_lag"), # Лаги
    dem, starts_with("dist_"), # Статика
    starts_with("lc_") # One-hot encoded landcover
  ) %>%
  arrange(region, point_id, year, month)

glimpse(final_df_selected)

# Сохранение итогового файла
write_csv(final_df_selected, path_output_csv)
message(paste("\nИтоговый ML-датасет успешно сохранен в:", path_output_csv))
message("--- Работа скрипта завершена. ---")