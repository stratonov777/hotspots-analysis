# ===================================================================
# СКРИПТ: ГЛАВНЫЙ КОНВЕЙЕР МОДЕЛИРОВАНИЯ ("MASTER MODELING PIPELINE")
# ВЕРСИЯ 4.1 - Финальная, с полным набором функций и комментариев
# ===================================================================
#
# НАЗНАЧЕНИЕ:
# Данный скрипт представляет собой универсальный и гибкий инструмент для
# проведения вычислительных экспериментов по моделированию. Он позволяет
# фильтровать данные по множеству критериев, гибко настраивать набор
# предикторов и моделей, а также автоматически сохранять все
# результаты в уникальную, самодокументируемую структуру папок.
#
# ПОРЯДОК РАБОТЫ:
# 1. Настраиваются все желаемые параметры в блоке `CONFIG` ("Пульт управления").
# 2. Запускается весь скрипт. Один запуск = один полный эксперимент.
#
# -------------------------------------------------------------------


# --- БЛОК 1: УСТАНОВКА И ЗАГРУЗКА БИБЛИОТЕК ---
# -------------------------------------------------------------------
# Список необходимых пакетов для работы конвейера
required_packages <- c(
  "tidymodels",
  "tidyverse",
  "randomForest",
  "xgboost",
  "vip",
  "glmnet",
  "kknn",
  "rpart",
  "earth",
  "Cubist",
  "kernlab",
  "brulee",
  "rules",
  "sf",
  "leaflet",
  # Необходим для создания интерактивной карты
  "mapedit"  # Необходим для редактирования карты
)

# Цикл для проверки, установки и загрузки пакетов
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg)
  library(pkg, character.only = TRUE)
}


# ===================================================================
# --- БЛОК 2: ПУЛЬТ УПРАВЛЕНИЯ (КОНФИГУРАЦИЯ ЭКСПЕРИМЕНТА) ---
# ===================================================================
# В данном блоке настраиваются все параметры для одного запуска.
# ESA WorldCover 2021 v200 имеет следующую классификацию:
# Value | Class Name                    | Description
# ------|-------------------------------|------------------------------------
# 10    | Trees                         | Деревья
# 20    | Shrubland                     | Кустарники
# 30    | Grassland                     | Травяной покров
# 40    | Cropland                      | Сельскохозяйственные земли
# 50    | Built-up                      | Застроенные территории
# 60    | Bare / sparse vegetation      | Голая почва / редкая растительность
# 70    | Snow and ice                  | Снег и лед (маловероятно в Воронежской области на уровне моря)
# 80    | Permanent water bodies        | Постоянные водные объекты
# 90    | Herbaceous wetland            | Травянистые водно-болотные угодья
# 95    | Mangroves                     | Мангры (неприменимо для Воронежской области)
# 100   | Moss and lichen               | Мхи и лишайники


CONFIG <- list(
  # --- 2.1. Основные настройки ---
  
  # Уникальное имя эксперимента. Если NULL, генерируется автоматически.
  experiment_name = NULL,
  
  path_input_csv = here::here(
    "data",
    "processed",
    "final_dataset",
    "ml_dataset_all_regions.csv"
  ),
  path_base_output = here::here("results", "models"),
  path_borders_dir = here::here("data", "raw", "borders"),
  
  
  # --- 2.2. Настройки фильтрации данных ---
  
  # Фильтр по региону. Оставьте NULL, чтобы использовать данные по всем регионам сразу.
  # Возможные значения: "Bashkortostan", "Chelyabinsk", "Khanty-Mansiy", "Sverdlovsk",
  #                    "Tyumen", "Voronezh", "Yamal-Nenets", "Kurgan"
  filter_region = "Voronezh",
  
  # Фильтр по типу земного покрова (указывается имя столбца, например, "lc_10").
  # Оставьте NULL, чтобы не фильтровать.
  filter_landcover_type = NULL,
  
  # Фильтр по месяцам. Оставьте NULL, чтобы не фильтровать.
  # Пример: c(4, 5, 6, 7, 8, 9) для теплого сезона.
  filter_months = 5,
  
  # Фильтр по годам. Оставьте NULL, чтобы не фильтровать.
  # Пример: 2015:2023. Полный диапазон: 2000:2023.
  filter_years = NULL,
  
  # Географическая фильтрация. Режимы: "none", "file", "interactive".
  filter_geo_mode = "none",
  path_shapefile_for_filter = NULL,
  # Путь к gpkg/shp для режима "file".
  
  
  # --- 2.3. Настройки предикторов ---
  
  # Режим выбора предикторов: "all" (все доступные) или "interactive" (выбор в окне).
  predictor_selection_mode = "all",
  
  # Список предикторов для исключения (сработает, если predictor_selection_mode = "all").
  # Скопируйте нужные имена из списка ниже. Пример: c("dem", "dist_roads").
  #
  # ПОЛНЫЙ СПИСОК ВОЗМОЖНЫХ ПРЕДИКТОРОВ:
  #   Временные: "month_sin", "month_cos"
  #   Климат: "temperature", "precipitation"
  #   Климатические лаги: "precip_lag3", "precip_lag6", "temp_lag3", "temp_lag6"
  #   Статические: "dem"
  #   Расстояния: "dist_roads", "dist_railways", "dist_water_a", "dist_waterways", "dist_buildings_a"
  #   Тип поверхности: "lc_10", "lc_20", "lc_30", "lc_40", "lc_50", "lc_60", "lc_80", "lc_90", "lc_100"
  predictors_to_exclude = NULL,
  
  # Предикторы по умолчанию для интерактивного режима.
  default_selected_predictors = c("temperature", "precipitation", "dem"),
  
  
  # --- 2.4. Настройки моделей ---
  
  # Режим выбора моделей: "all", "custom_list", "interactive".
  model_selection_mode = "all",
  
  # Список моделей для режима "custom_list" и как выбор по умолчанию в "interactive".
  # Возможные значения: "lm", "lasso", "ridge", "knn", "tree", "mars", "svm", "cubist", "rf", "xgb", "nn"
  models_to_run_custom = c("rf", "xgb", "lm"),
  
  # БАЗОВЫЕ (ЭТАЛОННЫЕ) ПАРАМЕТРЫ. Служат точкой отсчета. ЭТОТ БЛОК НЕ ТРОГАТЬ.
  default_model_specifications = list(
    lm = linear_reg() %>% set_engine("lm"),
    lasso = linear_reg(penalty = 0.01, mixture = 1) %>% set_engine("glmnet"),
    ridge = linear_reg(penalty = 0.01, mixture = 0) %>% set_engine("glmnet"),
    knn = nearest_neighbor(neighbors = 7) %>% set_engine("kknn") %>% set_mode("regression"),
    tree = decision_tree() %>% set_engine("rpart") %>% set_mode("regression"),
    mars = mars() %>% set_engine("earth") %>% set_mode("regression"),
    svm = svm_rbf() %>% set_engine("kernlab") %>% set_mode("regression"),
    cubist = cubist_rules() %>% set_engine("Cubist"),
    rf = rand_forest(trees = 150) %>% set_engine("randomForest", importance = TRUE) %>% set_mode("regression"),
    xgb = boost_tree(trees = 200, learn_rate = 0.05) %>% set_engine("xgboost") %>% set_mode("regression"),
    nn = mlp(
      hidden_units = 10,
      penalty = 0.01,
      epochs = 25
    ) %>% set_engine("brulee") %>% set_mode("regression")
  ),
  
  # ЭКСПЕРИМЕНТАЛЬНЫЕ ПАРАМЕТРЫ. Изменяйте гиперпараметры для экспериментов именно в этом списке.
  # Например, чтобы протестировать случайный лес с 500 деревьями, измените:
  # rf = rand_forest(trees = 500) %>% ...
  experimental_model_specifications = list(
    lm = linear_reg() %>% set_engine("lm"),
    lasso = linear_reg(penalty = 0.01, mixture = 1) %>% set_engine("glmnet"),
    ridge = linear_reg(penalty = 0.01, mixture = 0) %>% set_engine("glmnet"),
    knn = nearest_neighbor(neighbors = 7) %>% set_engine("kknn") %>% set_mode("regression"),
    tree = decision_tree() %>% set_engine("rpart") %>% set_mode("regression"),
    mars = mars() %>% set_engine("earth") %>% set_mode("regression"),
    svm = svm_rbf() %>% set_engine("kernlab") %>% set_mode("regression"),
    cubist = cubist_rules() %>% set_engine("Cubist"),
    rf = rand_forest(trees = 150) %>% set_engine("randomForest", importance = TRUE) %>% set_mode("regression"),
    xgb = boost_tree(trees = 200, learn_rate = 0.05) %>% set_engine("xgboost") %>% set_mode("regression"),
    nn = mlp(
      hidden_units = 10,
      penalty = 0.01,
      epochs = 25
    ) %>% set_engine("brulee") %>% set_mode("regression")
  )
)

# ===================================================================
# --- БЛОК 3: ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ---
# ===================================================================

# --- Функция для создания тега из измененных гиперпараметров ---
# Сравнивает списки default_ и experimental_ и создает тег, если есть отличия.
generate_hyperparameter_tag <- function(CONF) {
  default_specs <- CONF$default_model_specifications
  exp_specs <- CONF$experimental_model_specifications
  
  # Определяем, какие модели нужно проверить, в зависимости от режима
  models_to_check <- if (CONF$model_selection_mode == "all") {
    names(exp_specs)
  } else if (CONF$model_selection_mode == "custom_list") {
    CONF$models_to_run_custom
  } else {
    # Для интерактивного режима, на всякий случай, проверяем все
    names(exp_specs)
  }
  
  tags <- c()
  for (model_name in intersect(models_to_check, names(exp_specs))) {
    if (!model_name %in% names(default_specs))
      next
    
    default_args <- default_specs[[model_name]]$args
    exp_args <- exp_specs[[model_name]]$args
    
    # Сравниваем каждый параметр
    for (param_name in names(exp_args)) {
      default_val <- rlang::get_expr(default_args[[param_name]])
      exp_val <- rlang::get_expr(exp_args[[param_name]])
      
      if (!identical(default_val, exp_val)) {
        # Создаем тег вида "модель_параметр_значение", заменяя точки на 'p'
        exp_val_str <- gsub("\\.", "p", as.character(exp_val))
        tags <- c(tags,
                  paste(model_name, param_name, exp_val_str, sep = "_"))
      }
    }
  }
  
  if (length(tags) > 0)
    return(paste(tags, collapse = "__"))
  else
    return(NULL)
}


# --- Функция для генерации имени папки эксперимента (адаптированная) ---
generate_experiment_name <- function(CONF,
                                     row_count,
                                     selected_predictors,
                                     all_predictors) {
  if (!is.null(CONF$experiment_name) &&
      CONF$experiment_name != "")
    return(CONF$experiment_name)
  
  # Корректно обрабатываем один или несколько регионов
  region_tag <- paste(sort(CONF$filter_region), collapse = "-")
  parts <- c(region_tag, paste0(row_count, "rows"))
  
  # Начинаем имя папки с названия региона, чтобы сгруппировать результаты.
  parts <- c(CONF$filter_region, paste0(row_count, "rows"))
  
  # Добавляем теги для каждого фильтра
  if (!is.null(CONF$filter_landcover_type))
    parts <- c(parts, CONF$filter_landcover_type)
  if (!is.null(CONF$filter_months))
    parts <- c(parts, paste0("m_", paste(CONF$filter_months, collapse = "-")))
  if (!is.null(CONF$filter_years))
    parts <- c(parts, paste0("y_", paste(CONF$filter_years, collapse = "-")))
  if (CONF$filter_geo_mode == "file" &&
      !is.null(CONF$path_shapefile_for_filter) &&
      file.exists(CONF$path_shapefile_for_filter)) {
    parts <- c(parts, paste0("geo_", tools::file_path_sans_ext(
      basename(CONF$path_shapefile_for_filter)
    )))
  } else if (CONF$filter_geo_mode == "interactive") {
    parts <- c(parts, paste0("geo_interactive_", format(Sys.time(), "%Y%m%d%H%M")))
  }
  
  # Добавляем тег для набора предикторов
  abbreviations <- c(
    "month_sin" = "msin",
    "month_cos" = "mcos",
    "temperature" = "temp",
    "precipitation" = "prec",
    "precip_lag3" = "plag3",
    "precip_lag6" = "plag6",
    "temp_lag3" = "tlag3",
    "temp_lag6" = "tlag6",
    "dem" = "dem",
    "dist_roads" = "d_road",
    "dist_railways" = "d_rail",
    "dist_water_a" = "d_wtra",
    "dist_waterways" = "d_wtrw",
    "dist_buildings_a" = "d_bld",
    "lc_10" = "lc10",
    "lc_20" = "lc20",
    "lc_30" = "lc30",
    "lc_40" = "lc40",
    "lc_50" = "lc50",
    "lc_60" = "lc60",
    "lc_80" = "lc80",
    "lc_90" = "lc90",
    "lc_100" = "lc100"
  )
  if (length(selected_predictors) == length(all_predictors)) {
    predictor_tag <- "preds-all"
  } else {
    short_names <- abbreviations[selected_predictors]
    short_names <- ifelse(is.na(short_names), selected_predictors, short_names)
    predictor_tag <- paste0("preds_", paste(sort(short_names), collapse = "-"))
  }
  parts <- c(parts, predictor_tag)
  
  # Добавляем тег измененных гиперпараметров
  param_tag <- generate_hyperparameter_tag(CONF)
  if (!is.null(param_tag))
    parts <- c(parts, param_tag)
  
  # Собираем все части в одно имя
  return(paste(parts, collapse = "_"))
}

# --- Гаджет для интерактивного выбора предикторов ---
select_predictors_interactive <- function(all_choices, selected_by_default) {
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Выберите предикторы для моделирования"),
    miniUI::miniContentPanel(
      div(
        style = "padding-bottom: 10px;",
        actionButton("select_all", "Выбрать все"),
        actionButton("select_none", "Снять выделение"),
        actionButton("select_default", "Выбор по умолчанию")
      ),
      shiny::checkboxGroupInput(
        "predictors",
        label = "Доступные предикторы:",
        choices = all_choices,
        selected = selected_by_default
      )
    )
  )
  server <- function(input, output, session) {
    observeEvent(input$select_all, {
      updateCheckboxGroupInput(session, "predictors", selected = all_choices)
    })
    observeEvent(input$select_none, {
      updateCheckboxGroupInput(session, "predictors", selected = character(0))
    })
    observeEvent(input$select_default, {
      updateCheckboxGroupInput(session, "predictors", selected = selected_by_default)
    })
    observeEvent(input$done, {
      stopApp(input$predictors)
    })
  }
  runGadget(ui,
            server,
            viewer = dialogViewer("Выбор предикторов", width = 400, height = 700))
}

# --- Гаджет для интерактивного выбора моделей (исправленный) ---
select_models_interactive <- function(all_choices, selected_by_default) {
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Выберите модели для обучения"),
    miniUI::miniContentPanel(
      div(
        style = "padding-bottom: 10px;",
        actionButton("select_all_models", "Выбрать все"),
        actionButton("select_none_models", "Снять выделение"),
        actionButton("select_default_models", "Выбор по умолчанию")
      ),
      shiny::checkboxGroupInput(
        "models",
        label = "Доступные модели:",
        choices = all_choices,
        selected = selected_by_default
      )
    )
  )
  server <- function(input, output, session) {
    observeEvent(input$select_all_models, {
      updateCheckboxGroupInput(session, "models", selected = all_choices)
    })
    observeEvent(input$select_none_models, {
      updateCheckboxGroupInput(session, "models", selected = character(0))
    })
    # >> ИСПРАВЛЕНИЕ << Была опечатка в имени `input$select_default_models`
    observeEvent(input$select_default_models, {
      updateCheckboxGroupInput(session, "models", selected = selected_by_default)
    })
    observeEvent(input$done, {
      stopApp(input$models)
    })
  }
  runGadget(ui, server, viewer = dialogViewer("Выбор моделей", width = 400, height = 700))
}

# ===================================================================
# --- БЛОК 4: ОСНОВНАЯ ФУНКЦИЯ-КОНВЕЙЕР ---
# ===================================================================
run_modeling_experiment <- function(CONF) {
  # --- 4.1. Загрузка и фильтрация данных ---
  message("--- Этап 1: Загрузка и фильтрация данных ---")
  full_data <- read_csv(CONF$path_input_csv,
                        col_types = cols(),
                        progress = FALSE)
  
  # >> АДАПТАЦИЯ <<
  # Фильтруем по одному или нескольким регионам. Оператор %in% работает и с одним, и с вектором значений.
  if (!is.null(CONF$filter_region)) {
    data_to_process <- full_data %>% filter(region %in% CONF$filter_region)
    message(paste(
      "Выбраны регионы для анализа:",
      paste(CONF$filter_region, collapse = ", ")
    ))
  } else {
    data_to_process <- full_data
    message("Анализ будет проводиться по всем регионам вместе.")
  }
  
  # Географическая фильтрация
  sub_region_polygon <- NULL
  if (CONF$filter_geo_mode == "file") {
    if (!is.null(CONF$path_shapefile_for_filter) &&
        file.exists(CONF$path_shapefile_for_filter)) {
      sub_region_polygon <- st_read(CONF$path_shapefile_for_filter, quiet = TRUE)
    }
  } else if (CONF$filter_geo_mode == "interactive") {
    # Для интерактивной карты нам нужен базовый shp-файл только одного региона
    path_base_shp <- file.path(CONF$path_borders_dir,
                               paste0(CONF$filter_region[1], ".shp"))
    if (file.exists(path_base_shp)) {
      base_map_sf <- st_read(path_base_shp, quiet = TRUE) %>% st_transform(4326)
      # ... (остальная логика интерактивной карты)
    }
  }
  
  if (!is.null(sub_region_polygon)) {
    # >> АДАПТАЦИЯ << Используем нашу целевую проекцию ESRI:102025
    sub_region_polygon_proj <- st_transform(sub_region_polygon, crs = "ESRI:102025")
    points_sf <- st_as_sf(
      data_to_process,
      coords = c("x", "y"),
      crs = "ESRI:102025",
      remove = FALSE
    )
    points_in_zone <- st_filter(points_sf, sub_region_polygon_proj, .predicate = st_intersects)
    data_to_process <- as_tibble(points_in_zone) %>% select(-geometry)
  }
  
  # Остальные фильтры
  if (!is.null(CONF$filter_landcover_type))
    data_to_process <- data_to_process %>% filter(!!sym(CONF$filter_landcover_type) == 1)
  if (!is.null(CONF$filter_months))
    data_to_process <- data_to_process %>% filter(month %in% CONF$filter_months)
  if (!is.null(CONF$filter_years))
    data_to_process <- data_to_process %>% filter(year %in% CONF$filter_years)
  
  final_row_count <- nrow(data_to_process)
  if (final_row_count < 100) {
    warning("После фильтрации осталось < 100 строк. Эксперимент пропущен.")
    return(NULL)
  }
  message(paste(
    "ИТОГ ФИЛЬТРАЦИИ: Для анализа будет использовано",
    final_row_count,
    "строк."
  ))
  
  
  # --- 4.2. Определение предикторов ---
  message("\n--- Этап 2: Определение набора предикторов ---")
  # >> АДАПТАЦИЯ << Добавляем 'region' в список служебных столбцов
  non_predictor_cols <- c("region",
                          "point_id",
                          "x",
                          "y",
                          "period",
                          "year",
                          "month",
                          "fire_density")
  all_available_predictors <- setdiff(names(data_to_process), non_predictor_cols)
  
  selected_predictors <- if (CONF$predictor_selection_mode == "all") {
    setdiff(all_available_predictors, CONF$predictors_to_exclude)
  } else {
    # 'interactive' mode
    default_preds_exist <- intersect(CONF$default_selected_predictors,
                                     all_available_predictors)
    select_predictors_interactive(all_choices = all_available_predictors, selected_by_default = default_preds_exist)
  }
  if (is.null(selected_predictors))
    stop("Выбор предикторов был отменен.", call. = FALSE)
  
  
  # --- 4.3. Создание папки и документации эксперимента ---
  exp_name <- generate_experiment_name(CONF,
                                       final_row_count,
                                       selected_predictors,
                                       all_available_predictors)
  experiment_path <- file.path(CONF$path_base_output, exp_name)
  dir.create(experiment_path,
             showWarnings = FALSE,
             recursive = TRUE)
  
  config_text <- capture.output(dput(CONF))
  writeLines(config_text,
             file.path(experiment_path, "experiment_config.txt"))
  message(paste("\n--- ЗАПУСК ЭКСПЕРИМЕНТА:", exp_name, "---"))
  message(paste("Результаты будут сохранены в:", experiment_path))
  
  
  # --- 4.4. Подготовка данных для обучения ---
  message("\n--- Этап 3: Подготовка данных для обучения ---")
  data_for_modeling <- data_to_process %>% select(fire_density, all_of(selected_predictors))
  model_formula <- as.formula(paste("fire_density ~", paste(selected_predictors, collapse = " + ")))
  message("Итоговая формула для моделей:")
  print(model_formula)
  
  set.seed(123)
  data_split <- initial_split(data_for_modeling, prop = 0.80, strata = fire_density)
  train_data <- training(data_split)
  test_data  <- testing(data_split)
  main_recipe <- recipe(model_formula, data = train_data) %>%
    step_impute_median(all_numeric_predictors()) %>%
    step_normalize(all_numeric_predictors())
  
  
  # --- 4.5. Обучение моделей ---
  message("\n--- Этап 4: Обучение моделей ---")
  models_to_run <- if (CONF$model_selection_mode == "all") {
    names(CONF$experimental_model_specifications)
  } else if (CONF$model_selection_mode == "custom_list") {
    CONF$models_to_run_custom
  } else {
    # 'interactive'
    select_models_interactive(
      all_choices = names(CONF$experimental_model_specifications),
      selected_by_default = CONF$models_to_run_custom
    )
  }
  if (is.null(models_to_run) ||
      length(models_to_run) == 0)
    stop("Не выбрано ни одной модели для обучения.", call. = FALSE)
  
  models_to_train <- CONF$experimental_model_specifications[models_to_run]
  
  for (model_name in names(models_to_train)) {
    message(paste("Обучение модели:", model_name, "..."))
    tryCatch({
      wflow <- workflow() %>% add_recipe(main_recipe) %>% add_model(models_to_train[[model_name]])
      model_fit <- fit(wflow, data = train_data)
      saveRDS(model_fit, file = file.path(experiment_path, paste0(model_name, "_model_fit.rds")))
      message(paste("-> Модель", model_name, "обучена и сохранена."))
    }, error = function(e) {
      message(paste(
        "!!! ОШИБКА при обучении модели",
        model_name,
        ":",
        e$message,
        "!!!"
      ))
    })
  }
  
  # --- 4.6. Оценка и сохранение результатов ---
  message("\n--- Этап 5: Оценка и сохранение результатов ---")
  
  rds_files <- list.files(path = experiment_path,
                          pattern = "_model_fit\\.rds$",
                          full.names = TRUE)
  if (length(rds_files) == 0) {
    message("Не найдено обученных моделей для оценки. Завершение.")
    return(NULL)
  }
  
  all_metrics_list <- lapply(rds_files, function(file) {
    model_name <- str_remove(basename(file), "_model_fit.rds")
    model_fit <- readRDS(file)
    predict(model_fit, new_data = test_data) %>%
      bind_cols(test_data) %>%
      metrics(truth = fire_density, estimate = .pred) %>%
      mutate(model = model_name)
  })
  
  comparison_table <- bind_rows(all_metrics_list) %>%
    select(model, .metric, .estimate) %>%
    pivot_wider(names_from = .metric, values_from = .estimate) %>%
    arrange(desc(rsq))
  
  output_csv_path <- file.path(experiment_path, "model_comparison_metrics.csv")
  write_csv(comparison_table, output_csv_path)
  message(paste("Таблица с метриками сохранена в:", output_csv_path))
  print(comparison_table, n = Inf)
  
  models_for_vip <- intersect(c("xgb", "rf", "cubist", "mars", "lm", "lasso", "ridge"),
                              comparison_table$model)
  for (model_name in models_for_vip) {
    tryCatch({
      model_fit <- readRDS(file.path(experiment_path, paste0(model_name, "_model_fit.rds")))
      p <- model_fit %>% extract_fit_parsnip() %>% vip::vi() %>%
        slice_max(order_by = Importance, n = 20) %>%
        mutate(Variable = fct_reorder(Variable, Importance)) %>%
        ggplot(aes(x = Importance, y = Variable, fill = Importance)) +
        geom_col(show.legend = FALSE) + labs(title = paste("Важность признаков в модели", toupper(model_name)))
      
      output_plot_path <- file.path(experiment_path,
                                    paste0("feature_importance_", model_name, ".png"))
      ggsave(
        output_plot_path,
        plot = p,
        width = 10,
        height = 8,
        dpi = 300
      )
    }, error = function(e) {
    })
  }
  message("Графики важности признаков сохранены.")
  
  message(paste("\n--- ЭКСПЕРИМЕНТ", exp_name, "УСПЕШНО ЗАВЕРШЕН ---"))
}

# ===================================================================
# --- БЛОК 5: ЗАПУСК КОНВЕЙЕРА ---
# ===================================================================
# 1. Настройте все желаемые параметры в блоке `CONFIG` выше.
# 2. Запустите эту строку, чтобы выполнить весь эксперимент.

run_modeling_experiment(CONFIG)