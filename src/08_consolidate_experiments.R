# ===================================================================
# СКРИПТ: КОНСОЛИДАЦИЯ РЕЗУЛЬТАТОВ ЭКСПЕРИМЕНТОВ
# ===================================================================
#
# НАЗНАЧЕНИЕ:
# Этот скрипт автоматически находит все файлы с метриками
# (`model_comparison_metrics.csv`) внутри папок с экспериментами,
# собирает их в единую таблицу, добавляет столбец с названием
# эксперимента (именем папки) и сохраняет итоговый отчет
# в формате Excel.
#
# -------------------------------------------------------------------


# --- БЛОК 1: УСТАНОВКА И ЗАГРУЗКА БИБЛИОТЕК ---

# Список необходимых пакетов
required_packages <- c(
  "tidyverse", # Для dplyr, readr, purrr
  "writexl"    # Для сохранения в формат .xlsx
)

# Цикл для проверки, установки и загрузки
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}


# --- БЛОК 2: НАСТРОЙКА ПУТЕЙ ---

# Базовый путь к проекту
project_root <- "B:/YandexDisk/Projects/hotspots-analysis"

# Папка, где хранятся все папки с результатами экспериментов
experiments_base_path <- file.path(project_root, "results", "models")

# Путь и имя для итогового Excel-файла
output_excel_path <- file.path(project_root, "results", "all_experiments_summary.xlsx")


# --- БЛОК 3: ОСНОВНАЯ ЛОГИКА СБОРА И ОБЪЕДИНЕНИЯ ---

message("--- НАЧАЛО: Сбор результатов экспериментов ---")

# 1. Рекурсивно ищем все файлы с метриками во всех вложенных папках
message(paste("Поиск файлов 'model_comparison_metrics.csv' в папке:", experiments_base_path))
metric_files <- list.files(
  path = experiments_base_path,
  pattern = "model_comparison_metrics.csv",
  recursive = TRUE,
  full.names = TRUE
)

if (length(metric_files) == 0) {
  stop("Не найдено ни одного файла 'model_comparison_metrics.csv'. Проверьте путь и результаты экспериментов.")
}

message(paste("Найдено файлов с результатами:", length(metric_files)))

# 2. Создаем именованный вектор: имена - это названия папок, значения - полные пути
# Это позволит нам автоматически создать новый столбец с именем эксперимента
names(metric_files) <- basename(dirname(metric_files))

# 3. Читаем и объединяем все файлы в одну таблицу
# purrr::map_dfr - это элегантный способ прочитать все файлы и сразу склеить их.
# .id = "experiment_name" - создает новый столбец с именем "experiment_name"
# из имен вектора metric_files, которые мы задали на шаге выше.
message("Чтение и объединение всех файлов...")
summary_table <- map_dfr(metric_files, read_csv, col_types = cols(), .id = "experiment_name")

# 4. Обработка итоговой таблицы
# Переставляем столбцы для удобства и сортируем по качеству моделей
final_summary <- summary_table %>%
  select(experiment_name, model, rsq, rmse, mae) %>%
  arrange(desc(rsq))

message("Итоговая таблица успешно сформирована.")
print(head(final_summary, 10))


# --- БЛОК 4: СОХРАНЕНИЕ В EXCEL ---

message(paste("\nСохранение итогового отчета в:", output_excel_path))

# Используем пакет writexl для простого и быстрого сохранения
writexl::write_xlsx(final_summary, path = output_excel_path)

message("--- ГОТОВО! Отчет 'all_experiments_summary.xlsx' успешно создан. ---")