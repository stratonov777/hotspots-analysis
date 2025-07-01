# =============================================================================
# check_netcdf_time.R (Версия 2.0 - с поддержкой секунд)
# =============================================================================
if (!require("ncdf4")) install.packages("ncdf4"); library(ncdf4)
if (!require("lubridate")) install.packages("lubridate"); library(lubridate)
if (!require("here")) install.packages("here"); library(here)

netcdf_file_to_check <- here::here("data", "raw", "ERA5_monthly_averaged_data_on_single_levels", "data_stream-moda_stepType-avgua.nc")
time_variable_name <- "valid_time"

print(paste("Проверяем файл:", netcdf_file_to_check))
nc_conn <- ncdf4::nc_open(netcdf_file_to_check)
time_raw_values <- ncdf4::ncvar_get(nc_conn, time_variable_name)
units_attribute <- ncdf4::ncatt_get(nc_conn, time_variable_name, "units")
ncdf4::nc_close(nc_conn)

print(paste("Атрибут 'units':", units_attribute$value))
origin_parts <- unlist(strsplit(trimws(units_attribute$value), " since "))
time_origin_str <- origin_parts[2]
time_origin <- lubridate::parse_date_time(time_origin_str, 
                                          orders = c("Ymd HMS z", "Ymd HMS", "Ymd HM z", "Ymd HM", "Ymd H z", "Ymd H", "Ymd z", "Ymd"), 
                                          tz = "UTC", quiet = TRUE)
print(paste("Распознанная дата начала отсчета:", time_origin))

## >> ИСПРАВЛЕНИЕ <<
## Добавлена проверка на "seconds"
if (grepl("second", origin_parts[1], ignore.case = TRUE)) {
  print("Единицы времени определены как 'секунды'.")
  calculated_dates <- time_origin + lubridate::dseconds(time_raw_values)
} else if (grepl("hour", origin_parts[1], ignore.case = TRUE)) {
  print("Единицы времени определены как 'часы'.")
  calculated_dates <- time_origin + lubridate::dhours(time_raw_values)
} else if (grepl("day", origin_parts[1], ignore.case = TRUE)) {
  print("Единицы времени определены как 'дни'.")
  calculated_dates <- time_origin + lubridate::ddays(time_raw_values)
} else {
  stop("Неподдерживаемые единицы времени.")
}

print("\n--- РЕЗУЛЬТАТ ДИАГНОСТИКИ ---")
print(paste("Первая дата в файле:", head(calculated_dates, 1)))
print(paste("Последняя дата в файле:", tail(calculated_dates, 1)))