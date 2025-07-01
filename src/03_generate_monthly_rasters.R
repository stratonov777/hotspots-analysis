# =============================================================================
# 03_generate_monthly_rasters.R (Версия 4.0 - Финальная)
# -----------------------------------------------------------------------------
# Назначение:
#   Создает ежемесячные динамические растровые слои.
#   Возвращена оригинальная, надежная логика парсинга дат из NetCDF.
#
# Автор: StratonovDD (адаптация и доработка: Партнер программиста)
# Дата: 01 июля 2025 г.
# =============================================================================

# --- 1. Загрузка библиотек ---
print("--- 1. Загрузка библиотек ---")
if (!require("terra")) install.packages("terra"); library(terra)
if (!require("sf")) install.packages("sf"); library(sf)
if (!require("dplyr")) install.packages("dplyr"); library(dplyr)
if (!require("readr")) install.packages("readr"); library(readr)
if (!require("lubridate")) install.packages("lubridate"); library(lubridate)
if (!require("ncdf4")) install.packages("ncdf4"); library(ncdf4)
if (!require("adehabitatHR")) install.packages("adehabitatHR"); library(adehabitatHR)
if (!require("raster")) install.packages("raster"); library(raster)
if (!require("here")) install.packages("here"); library(here)


# --- 2. Конфигурация ---
print("--- 2. Настройка параметров ---")

regions_to_process <- c(
  "Bashkortostan", "Chelyabinsk", "Khanty-Mansiy", "Sverdlovsk",
  "Tyumen", "Voronezh", "Yamal-Nenets", "Kurgan"
)

weather_files_map <- list(
  t2m = here::here("data", "raw", "ERA5_monthly_averaged_data_on_single_levels", "data_stream-moda_stepType-avgua.nc"),
  tp  = here::here("data", "raw", "ERA5_monthly_averaged_data_on_single_levels", "data_stream-moda_stepType-avgad.nc")
)

base_rasters_path   <- here::here("data", "processed", "base_rasters")
hotspots_path       <- here::here("data", "processed", "hotspots")
output_base_path    <- here::here("data", "processed", "monthly_rasters")

start_year      <- 2000
end_year        <- 2023
target_months   <- 1:12
target_crs      <- "ESRI:102025"
target_res      <- 1000
kde_h           <- 15000
overwrite_files <- TRUE
time_var_name_in_nc <- "valid_time" 

# --- 3. Вспомогательная функция для поиска слоя в ОДНОМ файле ---
find_layer_in_nc <- function(nc_file_path, var_name, target_year, target_month) {
  if (!file.exists(nc_file_path)) {
    warning(paste("Файл NetCDF не найден:", nc_file_path)); return(NULL)
  }
  
  nc_connection <- try(ncdf4::nc_open(nc_file_path), silent = TRUE)
  if (inherits(nc_connection, "try-error")) return(NULL)
  
  if (!(var_name %in% names(nc_connection$var)) || !(time_var_name_in_nc %in% names(nc_connection$dim))) {
    ncdf4::nc_close(nc_connection); return(NULL)
  }
  
  time_vals_raw <- try(ncdf4::ncvar_get(nc_connection, time_var_name_in_nc), silent = TRUE)
  if (inherits(time_vals_raw, "try-error")) {
    ncdf4::nc_close(nc_connection); return(NULL)
  }
  
  time_units_att <- ncdf4::ncatt_get(nc_connection, time_var_name_in_nc, "units")
  ncdf4::nc_close(nc_connection)
  
  if (!time_units_att$hasatt) return(NULL)
  time_units <- time_units_att$value
  
  origin_parts <- unlist(strsplit(trimws(time_units), " since "))
  if (length(origin_parts) < 2) return(NULL)
  
  time_origin_str <- origin_parts[2]
  time_origin <- lubridate::parse_date_time(time_origin_str, 
                                            orders = c("Ymd HMS z", "Ymd HMS", "Ymd HM z", "Ymd HM", "Ymd H z", "Ymd H", "Ymd z", "Ymd"), 
                                            tz = "UTC", quiet = TRUE)
  
  if (is.na(time_origin)) return(NULL)
  
  ## >> ФИНАЛЬНОЕ ИСПРАВЛЕНИЕ <<
  ## Добавлена логика для обработки секунд
  if (grepl("second", origin_parts[1], ignore.case = TRUE)) {
    nc_dates <- time_origin + lubridate::dseconds(time_vals_raw)
  } else if (grepl("hour", origin_parts[1], ignore.case = TRUE)) {
    nc_dates <- time_origin + lubridate::dhours(time_vals_raw)
  } else if (grepl("day", origin_parts[1], ignore.case = TRUE)) {
    nc_dates <- time_origin + lubridate::ddays(time_vals_raw)
  } else {
    return(NULL) # Неподдерживаемые единицы времени
  }
  
  layer_index <- which(lubridate::year(nc_dates) == target_year & lubridate::month(nc_dates) == target_month)
  
  if (length(layer_index) == 1) {
    return(layer_index)
  }
  
  return(NULL)
}

# --- 4. Основной цикл по регионам ---
print(paste("\n--- 4. Начало основного цикла по регионам ---"))

for (region_name in regions_to_process) {
  message(paste("\n======================================================="))
  message(paste("Обработка региона:", region_name))
  
  region_hotspots_path <- file.path(hotspots_path, paste0("hotspots_", region_name, "_2000_2023.csv"))
  base_raster_path <- file.path(base_rasters_path, paste0("base_raster_", region_name, ".tif"))
  
  if (!file.exists(base_raster_path) || !file.exists(region_hotspots_path)) {
    warning(paste("Пропуск региона", region_name, "- отсутствует базовый файл маски или термоточек."))
    next
  }
  
  base_raster <- terra::rast(base_raster_path)
  fire_points_sf <- readr::read_csv(region_hotspots_path, show_col_types = FALSE) %>%
    dplyr::mutate(
      acq_date_parsed = lubridate::ymd(acq_date),
      year = lubridate::year(acq_date_parsed),
      month = lubridate::month(acq_date_parsed)
    ) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    sf::st_transform(crs = target_crs)
  
  for (year_iter in start_year:end_year) {
    for (month_iter in target_months) {
      month_str <- sprintf("%02d", month_iter)
      year_month_label <- paste(year_iter, month_str, sep = "_")
      message(paste("\n--- Обработка:", region_name, "-", year_month_label, "---"))
      
      output_folder <- file.path(output_base_path, region_name, as.character(year_iter), month_str)
      dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
      
      # (Блоки KDE, Температуры и Осадков остаются без изменений, т.к. ошибка была в функции выше)
      
      # --- KDE ---
      fire_density_path <- file.path(output_folder, paste0("fire_density_", year_month_label, ".tif"))
      if (!file.exists(fire_density_path) || overwrite_files) {
        current_fires <- fire_points_sf %>% dplyr::filter(year == year_iter, month == month_iter)
        if (nrow(current_fires) >= 5) {
          kde_sp <- sf::as_Spatial(current_fires); kde_sp$id <- "fires"
          sp_grid <- as(raster::raster(base_raster), "SpatialPixelsDataFrame")
          kde_result <- adehabitatHR::kernelUD(kde_sp[, "id"], h = kde_h, grid = sp_grid)
          kde_raster_raw <- terra::rast(adehabitatHR::estUDm2spixdf(kde_result))
          crs(kde_raster_raw) <- crs(base_raster)
          min_val <- global(kde_raster_raw, "min", na.rm=T)[1,1]; max_val <- global(kde_raster_raw, "max", na.rm=T)[1,1]
          kde_raster_norm <- if (max_val > min_val) (kde_raster_raw - min_val) / (max_val - min_val) else kde_raster_raw * 0
          kde_final <- terra::mask(kde_raster_norm, base_raster)
          names(kde_final) <- paste0("kde_", year_month_label)
          terra::writeRaster(kde_final, fire_density_path, overwrite = overwrite_files, datatype = 'FLT4S', gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9", "TILED=YES"))
          message("  Растр KDE успешно создан и сохранен.")
        } else {
          empty_raster <- base_raster * NA; names(empty_raster) <- "kde_empty"
          terra::writeRaster(empty_raster, fire_density_path, overwrite = overwrite_files, datatype = 'FLT4S', gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9", "TILED=YES"))
          message("  Недостаточно точек для KDE, создан пустой растр.")
        }
      } else { message("  Растр KDE уже существует, пропуск.") }
      
      # --- Температура ---
      mean_temp_path <- file.path(output_folder, paste0("mean_temp_", year_month_label, ".tif"))
      if (!file.exists(mean_temp_path) || overwrite_files) {
        
        layer_idx <- find_layer_in_nc(weather_files_map$t2m, "t2m", year_iter, month_iter)
        
        if (!is.null(layer_idx)) {
          # Загружаем весь стек и выбираем нужный слой
          temp_data_raw <- terra::rast(weather_files_map$t2m)
          temp_K <- temp_data_raw[[layer_idx]]
          
          ## >> ИСПРАВЛЕНИЕ <<
          ## Приводим имена переменных к единому стилю
          temp_C <- temp_K - 273.15
          temp_proj <- terra::project(temp_C, base_raster, method = "bilinear") # Используем ту же переменную, что и создали
          temp_final <- terra::mask(temp_proj, base_raster)
          names(temp_final) <- paste0("temp_", year_month_label)
          
          terra::writeRaster(temp_final, mean_temp_path, overwrite = overwrite_files, datatype = 'FLT4S', gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9", "TILED=YES"))
          message("  Растр температуры успешно создан и сохранен.")
          
        } else {
          empty_raster <- base_raster * NA; names(empty_raster) <- "temp_empty"
          terra::writeRaster(empty_raster, mean_temp_path, overwrite = overwrite_files, datatype = 'FLT4S', gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9", "TILED=YES"))
          message("  Данные по температуре не найдены, создан пустой растр.")
        }
      } else { 
        message("  Растр температуры уже существует, пропуск.")
      }
      
      # --- Осадки ---
      accum_precip_path <- file.path(output_folder, paste0("accum_precip_", year_month_label, ".tif"))
      if (!file.exists(accum_precip_path) || overwrite_files) {
        
        layer_idx <- find_layer_in_nc(weather_files_map$tp, "tp", year_iter, month_iter)
        
        if (!is.null(layer_idx)) {
          # Загружаем весь стек и выбираем нужный слой
          precip_data_raw <- terra::rast(weather_files_map$tp)
          precip_m <- precip_data_raw[[layer_idx]]
          
          ## >> ИСПРАВЛЕНИЕ <<
          ## Приводим имена переменных к единому стилю
          precip_mm <- precip_m * 1000
          precip_proj <- terra::project(precip_mm, base_raster, method = "bilinear") # Используем ту же переменную
          precip_final <- terra::mask(precip_proj, base_raster)
          precip_final[precip_final < 0] <- 0
          names(precip_final) <- paste0("precip_", year_month_label)
          
          terra::writeRaster(precip_final, accum_precip_path, overwrite = overwrite_files, datatype = 'FLT4S', gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9", "TILED=YES"))
          message("  Растр осадков успешно создан и сохранен.")
          
        } else {
          empty_raster <- base_raster * NA; names(empty_raster) <- "precip_empty"
          terra::writeRaster(empty_raster, accum_precip_path, overwrite = overwrite_files, datatype = 'FLT4S', gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9", "TILED=YES"))
          message("  Данные по осадкам не найдены, создан пустой растр.")
        }
      } else { 
        message("  Растр осадков уже существует, пропуск.")
      }
    }
  }
}

message("\n=======================================================")
message("Полная обработка и генерация ежемесячных растров завершена.")