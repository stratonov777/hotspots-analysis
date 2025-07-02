# =============================================================================
# 05_generate_lagged_rasters.R
# -----------------------------------------------------------------------------
# Назначение:
#   Создает растровые слои с временными лагами для климатических данных.
#   Для каждого месяца рассчитываются накопленные осадки и средняя
#   температура за предыдущие 3 и 6 месяцев.
#
# Автор: StratonovDD (адаптация и доработка: Партнер программиста)
# Дата: 02 июля 2025 г.
# =============================================================================

# --- 1. Загрузка библиотек ---
print("--- 1. Загрузка библиотек ---")
if (!require("terra")) install.packages("terra"); library(terra)
if (!require("lubridate")) install.packages("lubridate"); library(lubridate)
if (!require("here")) install.packages("here"); library(here)

# --- 2. Конфигурация ---
print("--- 2. Настройка параметров ---")

# Регионы для обработки
regions_to_process <- c(
  "Bashkortostan", "Chelyabinsk", "Khanty-Mansiy", "Sverdlovsk",
  "Tyumen", "Voronezh", "Yamal-Nenets", "Kurgan"
)

# Пути (используем относительные)
monthly_rasters_path <- here::here("data", "processed", "monthly_rasters")
output_lags_path     <- here::here("data", "processed", "lagged_rasters")
dir.create(output_lags_path, showWarnings = FALSE, recursive = TRUE)

# Параметры расчета
start_year <- 2000
end_year   <- 2023
lags_to_generate <- c(3, 6) # Оставляем только лаги в 3 и 6 месяцев
variables_to_process <- c("precip", "temp") # Переменные для обработки

# --- 3. Вспомогательная функция для расчета лага ---
calculate_and_save_lag <- function(region, base_ym, lag, var_type, monthly_path, output_path) {
  
  region_output_dir <- file.path(output_path, region)
  dir.create(region_output_dir, showWarnings = FALSE, recursive = TRUE)
  
  output_filename <- file.path(region_output_dir, sprintf("%s_lag%d_%s.tif", var_type, lag, base_ym))
  
  if (file.exists(output_filename)) {
    return()
  }
  
  base_date <- ymd(paste0(base_ym, "_01"), quiet = TRUE)
  required_dates <- base_date %m-% months(0:(lag - 1))
  required_yms <- format(required_dates, "%Y_%m")
  
  agg_fun <- if (var_type == "precip") sum else mean
  file_pattern <- if (var_type == "precip") "accum_precip_%s.tif" else "mean_temp_%s.tif"
  
  file_paths <- sapply(required_yms, function(ym) {
    year_part <- substr(ym, 1, 4)
    month_part <- substr(ym, 6, 7)
    file.path(monthly_path, region, year_part, month_part, sprintf(file_pattern, ym))
  })
  
  ## >> ИСПРАВЛЕНИЕ <<
  ## Убираем ошибочный вызов несуществующей функции 'file.paths'.
  ## Конструкция file_paths[file.exists(file_paths)] сама по себе возвращает нужный нам результат.
  existing_files <- file_paths[file.exists(file_paths)]
  
  if (length(existing_files) != lag) {
    return()
  }
  
  tryCatch({
    rast_stack <- terra::rast(existing_files)
    result_rast <- terra::app(rast_stack, fun = agg_fun, na.rm = TRUE)
    names(result_rast) <- basename(output_filename)
    
    terra::writeRaster(result_rast, output_filename, overwrite = TRUE, datatype='FLT4S', 
                       gdal=c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9", "TILED=YES"))
    
  }, error = function(e) {
    warning(paste("Ошибка при обработке", basename(output_filename), ":", e$message))
  })
}

# --- 4. Основной цикл для генерации ---
print("--- 4. Начало генерации растров с лагами ---")

# Создаем все комбинации год-месяц для итерации
all_year_months <- expand.grid(year = start_year:end_year, month = 1:12)
all_ym_labels <- paste(all_year_months$year, sprintf("%02d", all_year_months$month), sep = "_")

# Внешний цикл по регионам
for (region_iter in regions_to_process) {
  message(paste("\n======================================================="))
  message(paste("Обработка региона:", region_iter))
  
  # Внутренние циклы по переменным, датам и лагам
  for (var_iter in variables_to_process) {
    message(paste(" -> Переменная:", var_iter))
    
    # Используем `lapply` для наглядности и возможного распараллеливания в будущем
    lapply(all_ym_labels, function(ym_iter) {
      lapply(lags_to_generate, function(lag_iter) {
        calculate_and_save_lag(
          region = region_iter,
          base_ym = ym_iter,
          lag = lag_iter,
          var_type = var_iter,
          monthly_path = monthly_rasters_path,
          output_path = output_lags_path
        )
      })
    })
  }
}

print("\n--- Генерация всех растров с лагами завершена. ---")