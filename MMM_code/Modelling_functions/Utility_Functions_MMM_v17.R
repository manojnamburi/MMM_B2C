#-------------------------------------------------------------------------------#
#                                                                               #
#          A Collection of Utility Functions for Media Mix Modeling             #
#                                                                               #
#-------------------------------------------------------------------------------#

# Purpose
# -----------------------------
# To develop a collection of utility functions to facilitate media mix modeling:
# (1) `plot_varclust`:
#     - to group variables based on Pearson correlation
# (2) `make_pretest_data_MMM`
#     - to make a pretest data for better media mix model specifications
# (3) `make_stan_data_MMM`
#     - to make a stan data in list, which be consistent with a correspondng stan code
#
#
# Pre-Testing
# -----------------------------
# Unified pre-test functions that include
#   - All vs Geos
#   - Correlation by geo
#   - Estimates by geo
#   - Feature clustering
#   - Feature selection by geo
#   - Default controls: seasonality and time
#   - Estimates before/after default controls
#   - 
#
#
# MCMC Diagnostics
# -----------------------------
# Unified MCMC diagnostic functions that include
#   - rstan::check_divergences
#   - Divergence
#   - Treedepth
#   - Effective sample size
#   - rhat
#   - Energy
#   - Various plots
#   - 
#
#
# Model Evaluation/Validation
# -----------------------------
# Unified model evaluation/validation functions that include
#   - All vs Geos
#   - Posterior distribution
#   - Density/histogram/scatter plots
#   - Predictive checks
#   - R2/MAPE/RMSE/WAIC/LOO/Bayes factor
#   - In-sample vs out-of-sample prediction
#   - Media contribution
#   - Adstock, Hill curve, etc.
# 
# 
# References
# -----------------------------
#   - http://uc-r.github.io/model_selection#best
#   - https://uc-r.github.io/2017/03/03/clustering/
#   - https://mc-stan.org/rstan/articles/
#   - https://www.datascienceblog.net/post/machine-learning/probabilistic_programming/
#   - https://cran.r-project.org/web/packages/bayesplot/vignettes/visual-mcmc-diagnostics.html
#   - https://mc-stan.org/users/documentation/case-studies/divergences_and_bias.html
#   - https://www.weirdfishes.blog/blog/fitting-bayesian-models-with-stan-and-r/#run-diagnostics
#   - # https://cran.r-project.org/web/packages/bayesplot/vignettes/visual-mcmc-diagnostics.html
#   - 
#   - 




preprocess_MMM <- function(data_XyZ=NULL, data_seasonal=NULL, data_norm=NULL,
                           target_var=NULL,target_norm_var = NULL, media_names=NULL,
                           control_names=NULL, weather_names=NULL, compute_PCA = 'NO',
                           N_lag_target=1, N_lag_media=14, min_var_weather=1,subs = 'no',
                           DMA_Infos=FALSE, data_frequency = 'monthly',
                           y_percentile = 1, X_percentile = 1,
                           media_norm_var = NULL, media_group_norm_var = NULL, media_labels=NULL,
                           control_norm_across_DMA = F, control_norm_per_capita_var = NULL,
                           per_capita_norm_ts_var = F) {
  
  
  #---------------------------------------------------------------------#
  #  Preprocessing Procedure                                            #
  #---------------------------------------------------------------------#
  cat('\nNote: Preprocessing of MMM Raw Datasets\n')
  cat("1. Target, 'y'\n")
  cat(paste0(" - Normalize 'y' by '", target_norm_var, "'.\n"))
  cat(" - Transform Normalized 'y' by Max('y').\n")
  cat(" - Create Lagged 'y'.\n")
  cat(" - Create Seasonality.\n\n")
  
  cat("2. Media Variables, 'X'\n")
  cat(" - Normalize 'X' by `Normalization Variable` per Media Channel:")
  tb_media = if (is.null(media_labels)) {tibble("Media Channel" = media_names, "Normalization Variable" = media_norm_var)
  } else {tibble("Media Channel" = media_labels, "Normalization Variable" = media_norm_var)}
  print(knitr::kable(tb_media))
  cat("\n - Transform Normalized 'X' by MinMax('X').\n")
  cat(" - Create Lagged 'X'.\n\n")
  
  cat("3. Control Variables, 'Z'\n")
  cat(" - Convert 'weather' variables into N components/factors.\n")
  cat(" - Add the N 'weather' components to Z.\n")
  if(!is.null(control_norm_per_capita_var)){
  per_capita_norm_ts_var <- T
  lags          = seq(1, N_lag_target)
  lag_names     = paste("AR", formatC(lags, width = nchar(max(lags)), flag = "0"), sep = "")
  cat(" - Normalize few 'Z' by `Normalization Variable` per Control Variable (using the same normalization factor as target):")
  tb_control = tibble("Control Variable" = c(control_norm_per_capita_var,'seasonality',lag_names), "Normalization Variable" = target_norm_var)
  print(knitr::kable(tb_control))
  }else if(is.null(control_norm_per_capita_var) & per_capita_norm_ts_var == T){
    lag_names     = paste("AR", formatC(lags, width = nchar(max(lags)), flag = "0"), sep = "")
    lags          = seq(1, N_lag_target)
    cat(" - Normalize few 'Z' by `Normalization Variable` per Control Variable (using the same normalization factor as target):")
    tb_control = tibble("Control Variable" = c('seasonality',lag_names), "Normalization Variable" = target_norm_var)
    print(knitr::kable(tb_control))
  }
  cat(" - Transform 'Z' by MinMax('Z').\n")
  
  
  
  #---------------------------------------------------------------------#
  #  Check INPUTS                                                       #
  #---------------------------------------------------------------------#
  ## Check input data.
  if (is.null(data_XyZ)) stop("\n'data_XyZ' should be provided!\n")
  var_name  = c('brand', 'dma', 'period')
  var_false = !(var_name %in% names(data_XyZ))
  if (any(var_false)) {
    stop(paste0("\n'data_XyZ' should include '", var_name[var_false], "' variable!"))
  }
  
  if (is.null(data_seasonal)) stop("\n'data_seasonal' should be provided!\n")
  var_name  = c('brand', 'dma', 'period')
  var_false = !(var_name %in% names(data_seasonal))
  if (any(var_false)) {
    stop(paste0("\n'data_seasonal' should include '", var_name[var_false], "' variable!"))
  }
  
  if (is.null(data_norm)) stop("\n'data_norm' should be provided!\n")
  
  norm_var_name = c('dma', target_norm_var, media_norm_var) %>% unique()

  var_false = !(norm_var_name %in% names(data_norm))
  if (any(var_false)) {
    stop(paste0("\n'data_norm' should include '", norm_var_name[var_false], "' variable!"))
  }
  
  if (length(media_names) != length(media_norm_var)) {
    stop("\n'media_names' & `media_norm_var`should have the same length!\n")
  }

  if (length(media_names) != length(media_group_norm_var)) {
    stop("\n'media_names' & `media_group_norm_var`should have the same length!\n")
  }

  if (!is.null(media_labels) & length(media_names) != length(media_labels)) {
    stop("\n'media_names' & `media_labels`should have the same length!\n")
  }

  # Required packages
  required_packages = c('dplyr', 'tidyr','lubridate','purrr','rlang')
  for (i in 1:length(required_packages))
    suppressWarnings(suppressMessages(require(required_packages[i], character.only = TRUE)))
  
  
  ###### Based on data_frequency, determine weekly, monthly or quarterly
  freq      <- switch(data_frequency, "weekly" = 52, "monthly" = 12, "quarterly" = 4)
  freq_col  <- switch(data_frequency, "weekly" = 'week', "monthly" = 'month', "quarterly" = 'quarter')
  freq_func <- switch(data_frequency, "weekly" = "isoweek", "monthly" = "month", "quarterly" = "quarter")
  
  # lubridate_functions = setNames(freq_func, c(freq_col))
  
  
  
  
  
  
  #---------------------------------------------------------------------#
  #  Create DMA Dataframe                                               #
  #---------------------------------------------------------------------#
  ### Create `df_dma`.
  dma    = data_XyZ %>%
    dplyr::select(dma) %>%
    dplyr::arrange(dma) %>%
    dplyr::distinct() %>%
    dplyr::pull()
  
  df_dma = dma %>%
    data.frame() %>%
    dplyr::rename(dma    = '.') %>%
    dplyr::mutate(dma_id = row_number(),
                  dma    = as.character(dma)) %>%
    dplyr::select(dma_id, dma)
  
  if (DMA_Infos==TRUE){
    cat("\nNote: Make sure that correct 'dma_id' be used for STAN MMM prediction.\n")
    cat("      Please refer to the below 'dma_id-dma' matching table.\n\n")
    print(df_dma, row.names=FALSE)
  }
  
  
  
  #---------------------------------------------------------------------#
  #  Create `df_norm`                                                   #
  #---------------------------------------------------------------------#
  df_norm       = data_norm %>%
    dplyr::select(all_of(norm_var_name))
  
  
  
  #---------------------------------------------------------------------#
  #  Preprocessing: Target, y                                           #
  #---------------------------------------------------------------------#
  ### Create `df_target_norm_by_nonsub`.
  # Normalize 'y' by non-subscribers.
  default_names    = c('dma_id', 'dma', 'period', target_norm_var)
  
  df_target_norm_long  = data_XyZ %>%
    dplyr::left_join(df_dma, by=c('dma')) %>%
    dplyr::left_join(data_norm, by=c('dma')) %>%
    dplyr::select(all_of(default_names), all_of(target_var)) %>%
    dplyr::arrange(dma_id, dma, period) %>%
    tidyr::gather(key=y, value=y_value, -all_of(default_names)) %>%
    dplyr::mutate(y_norm = y_value/UQ(rlang::sym(target_norm_var))) %>%
    dplyr::select(dma_id, dma, period, y, y_norm) %>% 
    dplyr::as_tibble()
  
  df_target_norm_by_nonsub  = df_target_norm_long %>% 
    tidyr::spread(y, y_norm)
  
  
  ### Create `df_y_minmax`.
  # min/max('normalized y') per dma per y
  df_minmax_y1 = df_target_norm_long %>% 
    dplyr::group_by(dma_id, dma, y) %>%
    dplyr::summarise(min_1 = min(y_norm, na.rm=TRUE),
                     max_1 = max(y_norm, na.rm=TRUE))
  
  # min/max('normalized y') per dma across y
  df_minmax_y2 = df_target_norm_long %>% 
    dplyr::group_by(dma_id, dma) %>%
    dplyr::summarise(min_2 = min(y_norm, na.rm=TRUE),
                     max_2 = max(y_norm, na.rm=TRUE))
  
  # Use 99 percentile value for Max across DMAs!!!
  # min/max('normalized y') across dma per y
  df_minmax_y3 = df_target_norm_long %>% 
    dplyr::group_by(y) %>%
    dplyr::summarise(min_3 = min(y_norm, na.rm=TRUE),
                     max_3 = quantile(y_norm, y_percentile, na.rm = TRUE))
  # max_3 = max(y_norm, na.rm=TRUE))
  
  # min/max('normalized y') across dma across y
  df_minmax_y4 = df_target_norm_long %>% 
    dplyr::summarise(min_4 = min(y_norm, na.rm=TRUE),
                     max_4 = quantile(y_norm, y_percentile, na.rm = TRUE))
  # max_4 = max(y_norm, na.rm=TRUE))
  
  df_y_minmax      = df_minmax_y1 %>% 
    dplyr::inner_join(df_minmax_y2, by = c('dma_id', 'dma')) %>% 
    dplyr::inner_join(df_minmax_y3, by = 'y') %>% 
    tibble::add_column(df_minmax_y4)
  
  df_y_minmax_long = df_y_minmax %>% 
    tidyr::gather("key", "y_minmax", -dma_id, -dma, -y) %>% 
    tidyr::separate(key, c("stat", "method"), sep = "_") %>% 
    tidyr::spread(stat, y_minmax) %>% 
    dplyr::rename(y_max = max, y_min = min)
  
  
  ###### 4 different Normalizations
  # Note that y is normalized by dma subs only in Google papers!!!
  # See pp 3-4, 24 in Sun, Y., Wang, Y., Jin, Y., Chan, D. & Koehler, J. (2017). 
  #   Geo-level bayesian hierarchical media mix modeling. research.google.com.
  
  df_y_norm_max = df_target_norm_long %>% 
    dplyr::inner_join(df_y_minmax_long, by = c('dma_id', 'dma', 'y')) %>% 
    dplyr::mutate(y_value = y_norm / y_max,
                 y_value = ifelse(y_value >= 1, 1, y_value))
  
  # 1. per dma per target
  df_y_norm1 = df_y_norm_max %>% 
    dplyr::filter(method == 1) %>% 
    dplyr::select(dma_id:y, y_value) %>% 
    dplyr::mutate(y = paste0("norm_", y)) %>% 
    tidyr::spread(y, y_value)
  
  # 2. per dma across target
  df_y_norm2 = df_y_norm_max %>% 
    dplyr::filter(method == 2) %>% 
    dplyr::select(dma_id:y, y_value) %>% 
    dplyr::mutate(y = paste0("norm_", y)) %>% 
    tidyr::spread(y, y_value)
  
  # 3. across dma per target
  df_y_norm3 = df_y_norm_max %>% 
    dplyr::filter(method == 3) %>% 
    dplyr::select(dma_id:y, y_value) %>% 
    dplyr::mutate(y = paste0("norm_", y)) %>% 
    tidyr::spread(y, y_value)
  
  # 4. across dma across target
  df_y_norm4 = df_y_norm_max %>% 
    dplyr::filter(method == 4) %>% 
    dplyr::select(dma_id:y, y_value) %>% 
    dplyr::mutate(y = paste0("norm_", y)) %>% 
    tidyr::spread(y, y_value)
  
  
  ####### Target - Seasonality
  get_seasonality = function(df){
    # A custom utility function to get a series of monthly seasonalities
    date_start  = min(df$period)
    tmp_ts      = stats::ts(df$target, freq=freq, start=decimal_date(date_start))
    seasonality = stats::decompose(tmp_ts, type="additive")$seasonal
    return(as.numeric(seasonality))
  }
  
  # Compute monthly seasonality by dma and for each target variable and save it in `ls_seasonal`
  ls_seasonal <- list()
  
  for(trg in target_var){
    data_seasonal$target <- data_seasonal[[trg]]

    if(per_capita_norm_ts_var){
        ##When per capita norm var is set to True,
        ##Normalize AR and seasonality per capita according to "Geo-level Bayesian Hierarchical Media Mix Modeling"
        data_seasonal <- data_seasonal %>%
                         dplyr::left_join(data_norm, by=c('dma')) %>%
                         dplyr::mutate_at(.vars = vars('target'),.funs = ~./UQ(rlang::sym(target_norm_var)))
    }

    ls_seasonal[[trg]] = data_seasonal %>%
      dplyr::mutate_at(c('period'), list(freq_unit = rlang::as_function(freq_func))) %>%
      dplyr::rename(!!freq_col := freq_unit) %>%
      dplyr::filter(!!rlang::sym(freq_col)<=52) %>%   ##To exclude any weeks that might have value more than 52
      dplyr::left_join(df_dma, by=c('dma')) %>%
      dplyr::select(dma_id, dma, period, all_of(freq_col), target) %>%
      dplyr::arrange(dma_id, dma, period, !!rlang::sym(freq_col), target) %>%
      dplyr::group_by(dma_id, dma) %>%
      tidyr::nest() %>%
      dplyr::mutate(seasonality = purrr::map(data, get_seasonality)) %>%
      tidyr::unnest(cols = c(data, seasonality)) %>%
      dplyr::select(dma_id, dma, period, all_of(freq_col), target, seasonality) %>%
      dplyr::distinct(dma_id, dma, !!rlang::sym(freq_col), seasonality) %>%
      dplyr::arrange(dma_id, dma, !!rlang::sym(freq_col)) %>%
      dplyr::rename(!!paste('seasonality',trg,sep = '_') := seasonality)
  }
  
  #Join dataframes of all target variables into one dataframe
  df_seasonal = ls_seasonal %>%
    purrr::reduce(left_join, by = c('dma_id', 'dma', freq_col))
  
  
  ######## Target - Lagged variables
  ## Compute lagged variables for each target variable and save it in `ls_target_lag` as a list
  lags          = seq(1, N_lag_target)
  lag_names     = paste("AR", formatC(lags, width = nchar(max(lags)), flag = "0"), sep = "")
  lag_functions = setNames(paste("dplyr::lag(., ", lags, ")"), lag_names)
  
  ls_target_lag <- list()
  
  for(trg in target_var){
    var_name      = c(trg)
    ls_target_lag[[trg]] = data_seasonal %>%
      dplyr::mutate_at(c('period'), list(freq_unit = rlang::as_function(freq_func))) %>%
      dplyr::rename(!!freq_col := freq_unit) %>%
      dplyr::left_join(df_dma, by=c('dma')) %>%
      dplyr::left_join(data_norm, by=c('dma')) %>%
      dplyr::arrange(dma_id, dma, period) %>%
      dplyr::group_by(dma_id, dma) %>%
      dplyr::mutate_at(var_name, funs_(lag_functions)) %>%
      replace(., is.na(.), 0) %>%
      #dplyr::mutate_at(.vars = vars(lag_names),.funs = ~./UQ(rlang::sym(target_norm_var))) %>% 
      dplyr::select(dma_id, dma, period, all_of(freq_col), all_of(lag_names)) #%>%
      #dplyr::rename(!!paste(lag_names,trg,sep = '_') := all_of(lag_names))
  }
  
  #Join dataframes of all target variables into one final dataframe
  df_target_lag = ls_target_lag %>%
    purrr::reduce(left_join, by = c('dma_id', 'dma', 'period',freq_col))
  
  
  ### Create `df_Z_extra` with (1) seasonality and (2) lagged target by dma and period.
  #Join both Lagged variables and Seasonality dataframes into one single dataframe
  df_Z_extra <- df_target_lag %>% dplyr::left_join(df_seasonal, by=c('dma_id','dma',freq_col)) %>%
    dplyr::arrange(dma_id, dma, !!rlang::sym(freq_col)) %>% select(-all_of(freq_col))
  # df_Z_extra
  
  
  
  #---------------------------------------------------------------------#
  #  Preprocessing: Media Variables, X                                  #
  #---------------------------------------------------------------------#
  ### Create `df_media_norm_by_nonsub`.
  # Normalize 'X' by non-subscribers.
  
  media_group_df <- data.frame('X' = media_names,'X_group' = media_group_norm_var)

  default_names    = c('dma_id', 'period', c(norm_var_name))
  tmp_media_df     = data_XyZ %>%
                    dplyr::left_join(df_dma, by=c('dma')) %>%
                    dplyr::left_join(data_norm, by=c('dma')) %>%
                    dplyr::select(all_of(default_names), all_of(media_names)) %>%
                    dplyr::arrange(dma_id, dma, period) %>%
                    tidyr::gather(key=X, value=X_value, -all_of(default_names)) %>% 
                    dplyr::left_join(media_group_df, by = 'X') %>%
                    dplyr::as_tibble()
  
  tmp_media_ls     = list()
  
  for (i in 1:length(media_names)) {
    
    tmp_media_ls[[i]] = tmp_media_df %>% 
                        dplyr::filter(X == media_names[i]) %>% 
                        dplyr::mutate(X_norm = X_value/UQ(rlang::sym(media_norm_var[i]))) %>%
                        dplyr::select(dma_id, dma, period, X, X_norm, X_group)
  }
  
  df_media_norm_long = tmp_media_ls %>% 
                       purrr::reduce(bind_rows)
  
  df_media_norm_by_nonsub  = df_media_norm_long %>%
                             dplyr::select(-X_group) %>%
                             tidyr::spread(X, X_norm)
  
  
  ### Create `df_media_minmax`.
  # Compute min/max('normalized X') per dma per media
  df_minmax_media1  = df_media_norm_long %>%
    dplyr::group_by(dma_id, dma, X, X_group) %>%
    dplyr::summarise(min_1 = min(X_norm, na.rm=TRUE),
                     max_1 = max(X_norm, na.rm=TRUE))
  
  # Compute min/max('normalized X') per dma across media
  df_minmax_media2  = df_media_norm_long %>% 
    dplyr::group_by(dma_id, dma, X_group) %>%
    dplyr::summarise(min_2 = min(X_norm, na.rm=TRUE),
                     max_2 = max(X_norm, na.rm=TRUE))
  
  # Compute min/max('normalized X') across dma per media
  df_minmax_media3  = df_media_norm_long %>%
    dplyr::group_by(X,X_group) %>%
    dplyr::summarise(min_3 = min(X_norm, na.rm=TRUE),
                     max_3 = quantile(X_norm, X_percentile, na.rm = TRUE))
  
  # Compute min/max('normalized X') across dma across media
  df_minmax_media4  = df_media_norm_long %>%
    dplyr::group_by(X_group) %>% 
    dplyr::summarise(min_4 = min(X_norm, na.rm=TRUE),
                     max_4 = quantile(X_norm, X_percentile, na.rm = TRUE))
  
  df_media_minmax   = df_minmax_media1 %>% 
    dplyr::inner_join(df_minmax_media2, by = c('dma_id', 'dma','X_group')) %>% 
    dplyr::inner_join(df_minmax_media3, by = c('X','X_group')) %>% 
    dplyr::inner_join(df_minmax_media4, by = c('X_group')) %>%
    dplyr::select(-X_group)

  
  df_media_minmax_long = df_media_minmax %>% 
                        tidyr::gather("key", "X_minmax", -dma_id, -dma, -X) %>% 
                        tidyr::separate(key, c("stat", "method"), sep = "_") %>% 
                        tidyr::spread(stat, X_minmax) %>% 
                        dplyr::rename(X_max = max, X_min = min)
  
  
  ###### 4 different Normalizations
  df_media_norm_max = df_media_norm_long %>% 
    dplyr::inner_join(df_media_minmax_long, by = c('dma_id', 'dma', 'X')) %>% 
    dplyr::mutate(X_minmax = as.numeric((X_norm - X_min)/(X_max - X_min)),
                  X_minmax = ifelse(X_minmax >= 1, 1, X_minmax))
  
  # 1. per dma per media
  df_media_norm1 = df_media_norm_max %>% 
    dplyr::filter(method == 1) %>% 
    dplyr::select(dma_id:X, X_minmax) %>% 
    tidyr::spread(X, X_minmax)
  
  # 2. per dma across media
  df_media_norm2 = df_media_norm_max %>% 
    dplyr::filter(method == 2) %>% 
    dplyr::select(dma_id:X, X_minmax) %>% 
    tidyr::spread(X, X_minmax)

  # if(is.null(media_norm13_var)){
  #     df_media_norm2 = df_media_norm_max %>% 
  #       dplyr::filter(method == 2) %>% 
  #       dplyr::select(dma_id:X, X_minmax) %>% 
  #       tidyr::spread(X, X_minmax)
  # }else{
  #     df_media1 = df_media_norm_max %>% 
  #       dplyr::filter((method == 1) & (X %in% media_norm13_var))
  #     df_media2 = df_media_norm_max %>% 
  #       dplyr::filter((method == 2) & !(X %in% media_norm13_var))  
  #     df_media_norm2 = rbind(df_media1, df_media2) 
  #     df_media_norm2 = df_media_norm2 %>% 
  #         dplyr::select(dma_id:X, X_minmax) %>% 
  #         tidyr::spread(X, X_minmax)
  # }

  
  # 3. across dma per media
  df_media_norm3 = df_media_norm_max %>% 
    dplyr::filter(method == 3) %>% 
    dplyr::select(dma_id:X, X_minmax) %>% 
    tidyr::spread(X, X_minmax)
  
  # 4. across dma across media
  df_media_norm4 = df_media_norm_max %>% 
    dplyr::filter(method == 4) %>% 
    dplyr::select(dma_id:X, X_minmax) %>% 
    tidyr::spread(X, X_minmax)

  # if(is.null(media_norm13_var)){
  #     df_media_norm4 = df_media_norm_max %>% 
  #       dplyr::filter(method == 4) %>% 
  #       dplyr::select(dma_id:X, X_minmax) %>% 
  #       tidyr::spread(X, X_minmax)
  # }else{
  #     df_media3 = df_media_norm_max %>% 
  #       dplyr::filter((method == 3) & (X %in% media_norm13_var))
  #     df_media4 = df_media_norm_max %>% 
  #       dplyr::filter((method == 4) & !(X %in% media_norm13_var))  
  #     df_media_norm4 = rbind(df_media3, df_media4) 
  #     df_media_norm4 = df_media_norm4 %>% 
  #         dplyr::select(dma_id:X, X_minmax) %>% 
  #         tidyr::spread(X, X_minmax)
  # }
  
  
  ### Create media lag variables
  ## Compute lagged variables for each media variable and save it in `ls_media_lag` as a list
  ls_X_norm        = list(df_media_norm1, df_media_norm2, df_media_norm3, df_media_norm4)
  lags             = seq(0, N_lag_media)
  ls_media_lag     = list()
  df_X_norm_lag    = list()
  array_X_norm_lag = list()
  
  # Create a temporary array
  num_obs   = nrow(data_XyZ)
  num_media = length(media_names)
  num_lag   = N_lag_media + 1      # Including the current value
  
  for (i in 1:length(ls_X_norm)) {
    tmp_X = array(0, c(num_obs, num_media, num_lag))
    
    for(media in 1:length(media_names)){
      var_name      = c(media_names[media])
      
      lag_names     = paste("LAG", formatC(lags, width = nchar(max(lags)), flag = "0"), sep = "")
      lag_names1    = paste(media, formatC(lag_names, width = nchar(max(lags)), flag = "0"),  sep = "_")        
      lag_functions = setNames(paste("dplyr::lag(., ", lags, ")"), lag_names1)
      
      ls_media_lag[[media]] = ls_X_norm[[i]] %>%
        dplyr::select(dma, period, all_of(media_names)) %>%
        dplyr::mutate_at(c('period'), list(freq_unit = rlang::as_function(freq_func))) %>%
        dplyr::rename(!!freq_col := freq_unit) %>%
        dplyr::left_join(df_dma, by=c('dma')) %>%
        dplyr::arrange(dma_id, dma, period) %>%
        dplyr::group_by(dma_id, dma) %>%
        dplyr::mutate_at(var_name, funs_(lag_functions)) %>%
        replace(., is.na(.), 0) %>%
        dplyr::select(dma_id, dma, period, all_of(freq_col), all_of(lag_names1)) %>%
        dplyr::arrange(dma_id, dma, period)
      
      tmp_X[, media,] = ls_media_lag[[media]][lag_names1] %>% as.matrix()
    }
    
    #Join dataframes of all target variables into one dataframe
    df_X_norm_lag[[i]] = ls_media_lag %>%
      purrr::reduce(left_join, by = c('dma_id', 'dma', 'period', freq_col))
    array_X_norm_lag[[i]] = tmp_X
  }
  
  # lagged Media in df
  df_media_lag_norm1 = df_X_norm_lag[[1]]
  df_media_lag_norm2 = df_X_norm_lag[[2]]
  df_media_lag_norm3 = df_X_norm_lag[[3]]
  df_media_lag_norm4 = df_X_norm_lag[[4]]
  
  # lagged Media in array
  array_media_lag_norm1 = array_X_norm_lag[[1]]
  array_media_lag_norm2 = array_X_norm_lag[[2]]
  array_media_lag_norm3 = array_X_norm_lag[[3]]
  array_media_lag_norm4 = array_X_norm_lag[[4]]
  
  
  
  #---------------------------------------------------------------------#
  #  Preprocessing: Control Variables, Z                                #
  #---------------------------------------------------------------------#
  if (compute_PCA == 'YES'){
    ### Create `ls_pca`.
    ls_pca       = data_XyZ %>%
      dplyr::select(all_of(weather_names)) %>%
      stats::prcomp(, center=TRUE, scale=TRUE)
    N_component  = sum((ls_pca$sdev)^2 >= min_var_weather)
    Pct_variance = 100*sum((ls_pca$sdev[1:N_component])^2)/sum((ls_pca$sdev)^2)
    
    cat("\nNote: Principal Component Analysis of Weather Variables\n")
    cat(paste0("1. ", N_component, " components are selected by 'min_var_weather'=", min_var_weather, ".\n"))
    cat(paste0("2. ", f.digit2(Pct_variance), "% of the total variance are explained by the selected components.\n"))
    
    df_PCA        = ls_pca$x[, 1:N_component] %>%
      data.frame()
    names(df_PCA) = paste0("weather", seq(N_component))
    
    
    ####### Normalization of Control Variables
    # Create `df_control_minmax` which stores minmax values for all control variables
    default_names     = c('dma_id', 'dma', 'period')
    df_control  = data_XyZ %>%
      dplyr::left_join(df_dma, by=c('dma')) %>%
      dplyr::left_join(data_norm, by=c('dma')) %>%
      dplyr::select(all_of(default_names), all_of(control_names),target_norm_var) %>%
      dplyr::bind_cols(df_PCA, .id=NULL) %>%
      dplyr::left_join(df_Z_extra, by=c('dma_id', 'dma', 'period')) %>%
      dplyr::arrange(dma_id, dma, period) %>%
      dplyr::mutate_at(.vars = vars(control_norm_per_capita_var),.funs = ~./UQ(rlang::sym(target_norm_var))) %>% ##Normalize specific control variables by per capita according to "Geo-level Bayesian Hierarchical Media Mix Modeling"
      dplyr::select(-target_norm_var) %>%
      tidyr::gather(key=X, value=X_value, -all_of(default_names)) %>%
      dplyr::select(all_of(default_names), X, X_value) %>% 
      dplyr::as_tibble()
    
    if(control_norm_across_DMA){
      ##If specified as T, compute the minmax across geo by each control
      df_control_minmax = df_control %>%
                          dplyr::group_by(X) %>%
                          dplyr::summarise(X_min = min(X_value, na.rm=TRUE),
                                          X_max = max(X_value, na.rm=TRUE)) 
      join_cols_vec = c('X')
      
    }else{
      ##Default option - Compute minmax by control and geo
      df_control_minmax = df_control %>%
                          dplyr::group_by(dma_id, dma, X) %>%
                          dplyr::summarise(X_min = min(X_value, na.rm=TRUE),
                                          X_max = max(X_value, na.rm=TRUE))

      join_cols_vec = c('dma_id','dma','X')
    }
    # Create 'df_control_norm' which stores normalized values of all control variables
    df_control_norm = df_control %>%
      dplyr::left_join(df_control_minmax, by = join_cols_vec) %>%
      dplyr::mutate(X_minmax = as.numeric((X_value - X_min)/(X_max - X_min))) %>%
      dplyr::select(dma_id, dma, period, X, X_minmax) %>%
      tidyr::spread(X, X_minmax) %>%
      replace(is.na(.),0) %>%
      dplyr::arrange(dma_id, dma, period)
  } else {
    
    ls_pca = 'USER SELECTED - NO PCA COMPUTATION'
    N_component = 'USER SELECTED - NO PCA COMPUTATION'
    
    
    ####### Normalization of Control Variables
    # Create `df_control_minmax` which stores minmax values for all control variables
    default_names     = c('dma_id', 'dma', 'period')
    
    df_control  = data_XyZ %>%
      dplyr::left_join(df_dma, by=c('dma')) %>%
      dplyr::select(all_of(default_names), all_of(control_names)) %>%
      dplyr::left_join(df_Z_extra, by=c('dma_id', 'dma', 'period')) %>%
      dplyr::arrange(dma_id, dma, period) %>%
      tidyr::gather(key=X, value=X_value, -all_of(default_names)) %>%
      dplyr::select(all_of(default_names), X, X_value) %>% 
      dplyr::as_tibble()
    
    df_control_minmax = df_control %>%
      dplyr::group_by(dma_id, dma, X) %>%
      dplyr::summarise(X_min = min(X_value, na.rm=TRUE),
                       X_max = max(X_value, na.rm=TRUE))
    
    # Create 'df_control_norm' which stores normalized values of all control variables
    df_control_norm = df_control %>%
      dplyr::left_join(df_control_minmax, by = c('dma_id','dma','X')) %>%
      dplyr::mutate(X_minmax = as.numeric((X_value - X_min)/(X_max - X_min))) %>%
      dplyr::select(dma_id, dma, period, X, X_minmax) %>%
      tidyr::spread(X, X_minmax) %>%
      replace(is.na(.),0) %>%
      dplyr::arrange(dma_id, dma, period)
  }
  
  
  
  #---------------------------------------------------------------------#
  #  Preprocessing: Summary of Raw MMM Datasets                         #
  #---------------------------------------------------------------------#
  cat('\nNote: Summary of MMM Raw Datasets\n')
  cat("1. 'data_XyZ'\n")
  cat(paste0(" - Number of X      : ", length(media_names), " Media Variables\n"))
  if (compute_PCA == 'YES'){
    Z_count = length(control_names) + N_lag_target + N_component
  } else {
    Z_count = length(control_names) + N_lag_target
  }
  cat(paste0(" - Number of Z      : ", Z_count, " Control Variables\n"))
  cat(paste0(" - Study Period     : ", min(data_XyZ$period), " ~ ", max(data_XyZ$period), "\n"))
  cat(paste0(" - Number of ",freq_col,"s  : ", length(unique(data_XyZ$period))," ",freq_col,"s\n"))
  cat(paste0(" - Number of DMAs   : ", length(unique(data_XyZ$dma)), " DMAs\n"))
  
  cat("2. 'data_seasonal'\n")
  cat(" - Optimum\n")
  df_tmp = data_seasonal %>% filter(brand=="Optimum")
  cat(paste0("   - Study Period   : ", min(df_tmp$period), " ~ ", max(df_tmp$period), "\n"))
  cat(paste0("   - Number of ",freq_col,"s : ", length(unique(df_tmp$period))," ",freq_col,"s\n"))
  cat(" - Suddenlink\n")
  df_tmp = data_seasonal %>% filter(brand=="Suddenlink")
  cat(paste0("   - Study Period   : ", min(df_tmp$period), " ~ ", max(df_tmp$period), "\n"))
  cat(paste0("   - Number of ",freq_col,"s : ", length(unique(df_tmp$period))," ",freq_col,"s\n"))
  
  cat("3. 'data_norm'\n")
  cat(paste0(" - Number of DMAs   : ", length(unique(data_norm$dma)), " DMAs\n\n"))
  
  
  
  #---------------------------------------------------------------------#
  #  Preprocessing: Final Outcome                                       #
  #---------------------------------------------------------------------#
  ### Outcome Lists
  # Target, y
  target  = list(y_per_capita  = df_target_norm_by_nonsub,
                 y_minmax      = df_y_minmax,
                 y_minmax_long = df_y_minmax_long,
                 y_norm1       = df_y_norm1,
                 y_norm2       = df_y_norm2,
                 y_norm3       = df_y_norm3,
                 y_norm4       = df_y_norm4)
  
  # Media, X
  media   = list(X_per_capita  = df_media_norm_by_nonsub,
                 X_minmax      = df_media_minmax,
                 X_minmax_long = df_media_minmax_long,
                 X_norm1       = df_media_norm1,
                 X_norm2       = df_media_norm2,
                 X_norm3       = df_media_norm3,
                 X_norm4       = df_media_norm4,
                 X_norm1_lag   = df_media_lag_norm1,
                 X_norm2_lag   = df_media_lag_norm2,
                 X_norm3_lag   = df_media_lag_norm3,
                 X_norm4_lag   = df_media_lag_norm4,
                 X_norm1_array = array_media_lag_norm1,
                 X_norm2_array = array_media_lag_norm2,
                 X_norm3_array = array_media_lag_norm3,
                 X_norm4_array = array_media_lag_norm4)
  
  # Control, Z
  control = list(Z_per_capita  = df_control_norm,
                 Z_minmax      = df_control_minmax,
                 Z_extra       = df_Z_extra)
  
  # All Others
  extra   = list(df_seasonal   = df_seasonal,
                 df_norm       = df_norm,
                 df_dma        = df_dma,
                 ls_pca        = ls_pca,
                 var_media     = media_names,
                 var_control   = control_names,
                 var_weather   = weather_names,
                 N_lag_target  = N_lag_target,
                 N_lag_media   = N_lag_media,
                 N_component   = N_component,
                 target_norm_var = target_norm_var,
                 media_labels    = media_labels,
                 subs = subs)
  
  # Final Output
  pp_out  = list(target        = target,
                 media         = media,
                 control       = control,
                 extra         = extra)
  
  return(pp_out)
  
}





make_pretest_data_MMM <- function(pp_MMM_ls=NULL, target_norm_method=4, media_norm_method=4, var_target=NULL, var_control=NULL){
  # A utility function to make a pretest data, which will be examined
  #   to make better specifications of a media mix model
  # 
  # Note:
  # 1. `pp_MMM_ls` is the pre-processed media mix data in list.
  #   - All the required data is embedded in `pp_MMM_ls`.
  #   - All the required data is properly pre-processed via `preprocess_MMM`
  # 2. `target_norm_method` can choose one of the below normalization methods:
  #   - target_norm_method=1 --> per dma per target normalization
  #   - target_norm_method=2 --> per dma across target normalization
  #   - target_norm_method=3 --> across dma per target normalization
  #   - target_norm_method=4 --> across dma across target normalization
  # 3. `media_norm_method` can choose one of the below normalization methods:
  #   - media_norm_method=1 --> per dma per media normalization
  #   - media_norm_method=2 --> per dma across media normalization
  #   - media_norm_method=3 --> across dma per media normalization
  #   - media_norm_method=4 --> across dma across media normalization
  # 4. `var_target` selects a specific target variable of interest.
  # 5. `var_control` selects a specific set of control variables of interest.
  # 6. Media variables will be automatically selected from `pp_MMM_ls`.
  
  
  #---------------------------------------------------------------------#
  #  How to Convert Preproceed Media Mix Data Ready for STAN            #
  #---------------------------------------------------------------------#
  cat('\nNote: How to Make Pre-Test Data\n')
  cat("1. Use Preprocessed Media Mix Data\n")
  cat(" - Media mix data should be preprocessed by `preprocess_MMM()`.\n")
  cat("2. Choose a Target Normalization Method\n")
  if (target_norm_method == 1) {
    cat(" - target_norm_method=1 --> per dma per target normalization\n")
  } else if (target_norm_method == 2) {
    cat(" - target_norm_method=2 --> per dma across target normalization\n")
  } else if (target_norm_method == 3) {
    cat(" - target_norm_method=3 --> across dma per target normalization\n")
  } else if (target_norm_method == 4) {
    cat(" - target_norm_method=4 --> across dma across target normalization\n")
  }
  cat("3. Choose a Media Normalization Method\n")
  if (media_norm_method == 1) {
    cat(" - media_norm_method=1 --> per dma per media normalization\n")
  } else if (media_norm_method == 2) {
    cat(" - media_norm_method=2 --> per dma across media normalization\n")
  } else if (media_norm_method == 3) {
    cat(" - media_norm_method=3 --> across dma per media normalization\n")
  } else if (media_norm_method == 4) {
    cat(" - media_norm_method=4 --> across dma across media normalization\n")
  }
  
  
  #---------------------------------------------------------------------#
  #  Check INPUTS                                                       #
  #---------------------------------------------------------------------#
  ## Check input data.    
  if (is.null(pp_MMM_ls)) stop("\n'pp_MMM_ls' should be provided!\n")    
  if (is.null(target_norm_method)) stop("\n'target_norm_method' should be provided!\n")
  if (is.null(media_norm_method)) stop("\n'media_norm_method' should be provided!\n")
  if (is.null(var_target)) stop("\n'var_target' should be provided!\n")        
  if (is.null(var_control)) stop("\n'control_names' should be provided!\n")    
  
  
  #---------------------------------------------------------------------#
  #  Data                                                               #
  #---------------------------------------------------------------------#    
  # the response variable
  target_norm  = paste0('norm_', var_target)
  
  df_y         =  if (target_norm_method == 1) {
    pp_MMM_ls$target$y_norm1
  } else if (target_norm_method == 2) {
    pp_MMM_ls$target$y_norm2
  } else if (target_norm_method == 3) {
    pp_MMM_ls$target$y_norm3
  } else if (target_norm_method == 4) {
    pp_MMM_ls$target$y_norm4
  }
  
  df_y         =  df_y %>% 
    dplyr::arrange(dma_id, dma, period) %>% 
    dplyr::select(dma_id, dma, period, all_of(target_norm)) %>% 
    dplyr::rename(!!quo_name(var_target) := target_norm)
  
  # the media variables
  df_X_media   = if (media_norm_method == 1) {
    pp_MMM_ls$media$X_norm1
  } else if (media_norm_method == 2) {
    pp_MMM_ls$media$X_norm2
  } else if (media_norm_method == 3) {
    pp_MMM_ls$media$X_norm3
  } else if (media_norm_method == 4) {
    pp_MMM_ls$media$X_norm4
  }
  
  df_X_media   = df_X_media %>% 
    dplyr::arrange(dma_id, dma, period)
  
  # the control variables
  df_X_control = pp_MMM_ls$control$Z_per_capita %>% 
    dplyr::arrange(dma_id, dma, period) %>% 
    dplyr::select(dma_id, dma, period, all_of(var_control))   
  
  # the final pre-test data
  df_pretest = df_y %>% 
    dplyr::inner_join(df_X_media, by = c("dma_id", "dma", "period")) %>% 
    dplyr::inner_join(df_X_control, by = c("dma_id", "dma", "period"))
  
  
  #---------------------------------------------------------------------#
  #  Variable Names                                                     #
  #---------------------------------------------------------------------#    
  var_target  = names(df_y)[-c(1:3)]
  # var_media   = names(df_X_media)[-c(1:3)]
  var_media   = pp_MMM_ls$extra$var_media
  var_control = names(df_X_control)[-c(1:3)]  
  
  
  #---------------------------------------------------------------------#
  #  Pre-Test Data: Final Outcome                                       #
  #---------------------------------------------------------------------#
  pretest_out = list(
    df_pretest         = df_pretest,         # Data
    var_target         = var_target,         # Variable Names
    var_media          = var_media,
    var_control        = var_control,
    target_norm_method = target_norm_method,
    media_norm_method  = media_norm_method
  )  
  
}





pretest_MMM = function(pretest_df_ls=NULL) {
  # A utility function to explore a pretest data generated from `make_pretest_data_MMM` function
  # 
  # Note:
  # 1. `pretest_df_ls` is a list that includes:
  #   - a dataframe consisting of (1) target, (2) media, and (3) control features
  #   - target variable
  #   - media variables
  #   - control variables
  #   - normalization method
  
  
  #---------------------------------------------------------------------#
  #  How to Conduct a Pre-Testing before Model Specification            #
  #---------------------------------------------------------------------#
  cat('\nNote: How to Conduct a Pre-Testing\n')
  cat("1. Use Pre-Testing Media Mix Data\n")
  cat(" - Media mix data should be generated by `make_pretest_data_MMM`.\n")
  cat("\n2. Examine Pre-Testing Results\n")
  cat(" - Correlation Table/Plots to quantify the degree of association with Target\n")
  cat(" - Variable Clustering to visualize variable grouping\n")
  cat(" - DMA Plots to explore one-to-one relationship between each predictor and Target\n")
  cat(" - Media Plots to display the time series of each Media along with Target across DMAs\n")
  cat(" - Media Spending Plots to compare media spending between Media Channels and across DMAs for each Media\n")
  cat(" - Control Plots to display the time series of each Control along with Target across DMAs\n")
  
  
  #---------------------------------------------------------------------#
  #  Check INPUTS                                                       #
  #---------------------------------------------------------------------#
  ## Check input data.    
  if (is.null(pretest_df_ls)) stop("\n'pretest_df_ls' should be provided!\n")    
  
  
  #---------------------------------------------------------------------#
  #  Pre-Test Data and Variable Names                                   #
  #---------------------------------------------------------------------#    
  df_pretest         = pretest_df_ls$df_pretest
  var_target         = pretest_df_ls$var_target
  var_media          = pretest_df_ls$var_media
  var_control        = pretest_df_ls$var_control
  target_norm_method = pretest_df_ls$target_norm_method
  media_norm_method  = pretest_df_ls$media_norm_method
  
  
  #---------------------------------------------------------------------#
  #  Variable Clustering                                                #
  #---------------------------------------------------------------------#    
  varcluster_plots        = vector(mode = "list", length = 3)
  names(varcluster_plots) = c("control", "control_media", "control_media_target")
  varcluster_plots[[1]]   = plot_varcluster(df_pretest[var_control])
  varcluster_plots[[2]]   = plot_varcluster(df_pretest[c(var_control, var_media)])
  varcluster_plots[[3]]   = plot_varcluster(df_pretest[c(var_control, var_media, var_target)])
  
  
  #---------------------------------------------------------------------#
  #  DMA Plots: Target vs Predictor                                     #
  #---------------------------------------------------------------------#    
  df_pretest_long = df_pretest %>% 
    gather("variable", "value", -dma_id, -dma, -period, -all_of(var_target)) %>% 
    mutate(variable = factor(variable, levels = c(var_media, var_control)))
  
  # Create Plots for Each of DMAs  
  dma_ls    = df_pretest_long %>% distinct(dma) %>% pull(dma)
  dma_plots = vector(mode = "list", length = length(dma_ls))
  
  for (i in 1:length(dma_ls)) {
    dma_plots[[i]] = df_pretest_long %>% 
      dplyr::filter(dma == dma_ls[i]) %>% 
      ggplot(aes(x = period, y = !!as.name(var_target))) +
      geom_line(aes(linetype = "Target"), colour = 'red', size = 1.5) +
      geom_point(colour = 'green') + 
      geom_line(aes(y = value, linetype = "Predictor"), colour = 'blue', size = 1.0) +
      scale_linetype_manual(name = NULL, values = c(1, 1), 
                            guide = guide_legend(override.aes = list(color = c("blue", "red")))) + 
      facet_wrap(~ variable, scales = "fixed") + 
      ggtitle(paste0(dma_ls[i], " : ", var_target, " vs Predictor")) +        
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), legend.position = c(0.97, 1.00)) + 
      xlab("Time") + ylab("Target")
    
    names(dma_plots)[[i]] = make.names(dma_ls[i])
  }
  
  
  #---------------------------------------------------------------------#
  #  Plots by Variable Type                                             #
  #---------------------------------------------------------------------#    
  ### Media Plots across DMAs
  media_plots = vector(mode = "list", length = length(var_media))
  
  for (i in 1:length(var_media)) {
    media_plots[[i]] = df_pretest_long %>% 
      dplyr::filter(variable == var_media[i]) %>% 
      ggplot(aes(x = period, y = !!as.name(var_target))) +
      geom_line(aes(linetype = "Target"), colour = 'red', size = 1.5) +
      geom_point(colour = 'green') + 
      geom_line(aes(y = value, linetype = "Predictor"), colour = 'blue', size = 1.0) +
      scale_linetype_manual(name = NULL, values = c(1, 1), 
                            guide = guide_legend(override.aes = list(color = c("blue", "red")))) + 
      facet_wrap(~ dma, scales = "fixed") + 
      ggtitle(paste0(var_target, " vs ", var_media[i])) +        
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), legend.position = c(0.97, 1.00)) + 
      xlab("Time") + ylab("Target") + ylim(0, 1.1)
    
    names(media_plots)[[i]] = make.names(var_media[i])
  }
  
  ### Media Spend Plots across DMAs
  media_spend_plots = vector(mode = "list", length = length(var_media)+1)
  
  media_spend_all = df_pretest_long %>% 
    filter(variable %in% var_media) %>% 
    ggplot(aes(x = variable, y = value, fill = variable)) +
    geom_boxplot() + 
    ggtitle(paste0("Distributions of Normalized Media Mix Spendings")) +
    theme(plot.title      = element_text(hjust = 0.5, size = 20, face = "bold"),
          legend.position = "none",
          axis.text.x     = element_text(angle = 90, hjust = 1)) + 
    xlab("") + ylab("Normalized Spending")
  
  media_spend_plots[[1]]        = media_spend_all
  names(media_spend_plots)[[1]] = "All_Media"
  
  for (i in 1:length(var_media)) {
    media_spend_plots[[i+1]] = df_pretest_long %>% 
      dplyr::filter(variable == var_media[i]) %>% 
      dplyr::mutate(dma = reorder(dma, value, median)) %>% 
      ggplot(aes(x = dma, y = value, fill = dma)) +
      geom_boxplot() + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.1) +
      ggtitle(paste0( var_media[i], ": Normalized Spending across DMAs")) +
      theme(plot.title      = element_text(hjust = 0.5, size = 20, face = "bold"),
            legend.position = "none",
            axis.text.x     = element_text(angle = 90, hjust = 1)) + 
      xlab("") + ylab("Normalized Spending")
    
    names(media_spend_plots)[[i+1]] = make.names(var_media[i])
  }
  
  
  ### Control Plots across DMAs
  control_plots = vector(mode = "list", length = length(var_control))
  
  for (i in 1:length(var_control)) {
    control_plots[[i]] = df_pretest_long %>% 
      dplyr::filter(variable == var_control[i]) %>% 
      ggplot(aes(x = period, y = !!as.name(var_target))) +
      geom_line(aes(linetype = "Target"), colour = 'red', size = 1.5) +
      geom_point(colour = 'green') + 
      geom_line(aes(y = value, linetype = "Predictor"), colour = 'blue', size = 1.0) +
      scale_linetype_manual(name = NULL, values = c(1, 1), 
                            guide = guide_legend(override.aes = list(color = c("blue", "red")))) + 
      facet_wrap(~ dma, scales = "fixed") + 
      ggtitle(paste0(var_target, " vs ", var_control[i])) +        
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), legend.position = c(0.97, 1.00)) + 
      xlab("Time") + ylab("Target") + ylim(0, 1.1)
    
    names(control_plots)[[i]] = make.names(var_control[i])
  }
  
  
  #---------------------------------------------------------------------#
  #  DMA Correlation: Target vs Predictor                               #
  #---------------------------------------------------------------------#
  df_tmp = df_pretest %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(brand   = ifelse(dma_id==17, "Optimum", "Suddenlink"),
                  company = "Altice") %>% 
    dplyr::select(-dma_id, -period) %>% 
    dplyr::select(company, brand, everything())
  
  # Use a simple custom function, `correlate_quiet`.
  correlate_quiet = function(x) {corrr::correlate(x, quiet = TRUE)}
  
  # Correlation by Company  
  df_corr_company = df_tmp %>% 
    dplyr::select(-brand, -dma) %>% 
    dplyr::group_by(company) %>% 
    tidyr::nest() %>% 
    dplyr::mutate(data = purrr::map(data, purrr::compose(corrr::stretch, correlate_quiet))) %>% 
    tidyr::unnest() %>% 
    dplyr::filter(x == var_target, y != var_target) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(group = company, variable_name = y, corr = r)
  
  # Correlation by Brand  
  df_corr_brand = df_tmp %>% 
    dplyr::select(-company, -dma) %>% 
    dplyr::group_by(brand) %>% 
    tidyr::nest() %>% 
    dplyr::mutate(data = purrr::map(data, purrr::compose(corrr::stretch, correlate_quiet))) %>% 
    tidyr::unnest() %>% 
    dplyr::filter(x == var_target, y != var_target) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(group = brand, variable_name = y, corr = r)
  
  # Correlation by DMA
  df_corr_dma = df_tmp %>% 
    dplyr::select(-company, -brand) %>% 
    dplyr::group_by(dma) %>% 
    tidyr::nest() %>% 
    dplyr::mutate(data = purrr::map(data, purrr::compose(corrr::stretch, correlate_quiet))) %>% 
    tidyr::unnest() %>% 
    dplyr::filter(x == var_target, y != var_target) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(group = dma, variable_name = y, corr = r)
  
  df_corr = dplyr::bind_rows(df_corr_company, df_corr_brand, df_corr_dma) %>%
    dplyr::mutate(group = factor(group, 
                                 levels = c("Altice", "Optimum", "Suddenlink",
                                            df_tmp %>% distinct(dma) %>% pull())),
                  variable_name = factor(variable_name, levels = c(var_media, var_control))) %>% 
    dplyr::arrange(group, variable_name)
  
  # Correlation Table
  df_corr_stats = df_corr %>% 
    tidyr::spread(group, corr) %>% 
    dplyr::mutate_if(is.numeric, round, 3)
  
  
  ### Plots
  gg_theme1 = theme(plot.title      = element_text(hjust = 0.5, size = 20, face = "bold"),
                    legend.position = c(0.95, 0.9),
                    axis.text.x     = element_text(angle = 90, hjust = 1))
  
  gg_theme2 = theme(plot.title      = element_text(hjust = 0.5, size = 20, face = "bold"),
                    legend.position = "none",
                    axis.text.x     = element_text(angle = 90, hjust = 1))
  
  # Box Plots
  plot_corr_box = df_corr %>% 
    dplyr::filter(!(group %in% c("Altice", "Optimum", "Suddenlink"))) %>% 
    ggplot(aes(x = variable_name, y = corr, fill = variable_name)) +
    geom_boxplot() + 
    geom_hline(yintercept = 0, color='red', lwd = 1.5) + 
    ggtitle(paste0(var_target, ": Correlation Box Plots")) +        
    gg_theme2 + xlab("") + ylab("Correlation")
  
  # Bar Charts
  plot_corr_bar = df_corr %>% 
    dplyr::filter(group %in% c("Altice", "Optimum", "Suddenlink")) %>% 
    ggplot(aes(x = variable_name, y = corr, fill = group)) +
    geom_bar(position="dodge", stat="identity") + 
    ggtitle(paste0(var_target, ": Correlation Bar Chart")) +        
    gg_theme1 + xlab("") + ylab("Correlation")
  
  # Bar Charts by DMA
  dma_corr_plots = vector(mode = "list", length = length(dma_ls))
  
  for (i in 1:length(dma_ls)) {
    dma_corr_plots[[i]] = df_corr %>% 
      dplyr::filter(group == "Altice" | group == dma_ls[i]) %>% 
      ggplot(aes(x = variable_name, y = corr, fill = group)) +
      geom_bar(position="dodge", stat="identity") + 
      ggtitle(paste0(dma_ls[i], ": Correlation Bar Chart")) +        
      gg_theme1 + xlab("") + ylab("Correlation")
    
    names(dma_corr_plots)[[i]] = make.names(dma_ls[i])  
  }
  
  # Combined Charts by DMA
  dma_plots_combo = vector(mode = "list", length = length(dma_ls))
  
  for (i in 1:length(dma_ls)) {
    dma_plots_combo[[i]]        = ggpubr::ggarrange(dma_plots[[i]], dma_corr_plots[[i]])
    names(dma_plots_combo)[[i]] = make.names(dma_ls[i])  
  }
  
  
  #---------------------------------------------------------------------#
  #  Pre-Testing: Final Outcome                                         #
  #---------------------------------------------------------------------#
  pretest_out = list(
    correlation      = df_corr_stats,
    plot_corr_box    = plot_corr_box,
    plot_corr_bar    = plot_corr_bar,
    plot_varcluster  = varcluster_plots, 
    plot_dma_ts      = dma_plots,
    plot_dma_corr    = dma_corr_plots,
    plot_dma_combo   = dma_plots_combo,
    plot_media_ts    = media_plots,
    plot_media_spend = media_spend_plots,
    plot_control_ts  = control_plots
  )  
  
}





make_stan_data_MMM <- function(pp_MMM_ls=NULL, 
                               target_norm_method=4, media_norm_method=4, 
                               var_target=NULL,var_media = NULL, var_control=NULL, df_geo=NULL,
                               test_date=NULL, pooling_type="partial", media_priors=NULL){
  # A utility function to make a stan data in list, which should be consistent with a correspondng stan code
  # 
  # Note:
  # 1. `pp_MMM_ls` is the pre-processed media mix data in list.
  #   - All the required data is embedded in `pp_MMM_ls`.
  #   - All the required data is properly pre-processed via `preprocess_MMM`
  # 2. `target_norm_method` can choose one of the below normalization methods:
  #   - target_norm_method=1 --> per dma per target normalization
  #   - target_norm_method=2 --> per dma across target normalization
  #   - target_norm_method=3 --> across dma per target normalization
  #   - target_norm_method=4 --> across dma across target normalization
  # 3. `media_norm_method` can choose one of the below normalization methods:
  #   - media_norm_method=1 --> per dma per media normalization
  #   - media_norm_method=2 --> per dma across media normalization
  #   - media_norm_method=3 --> across dma per media normalization
  #   - media_norm_method=4 --> across dma across media normalization
  # 4. `var_target` selects a specific target variable of interest.
  # 5. `var_control` selects a specific set of control variables of interest.
  # 6. `df_geo` provides geo-level covariates, which accounts for the heterogeneity across geos.
  # 7. Media variables will be automatically selected from `pp_MMM_ls`.
  # 8. `test_date` splits the entire data into Train and Test datasets.
  # 9. `media_priors` specifies the priors of the media parameters.
  
  
  
  #---------------------------------------------------------------------#
  #  How to Convert Preproceed Media Mix Data Ready for STAN            #
  #---------------------------------------------------------------------#
  cat('\nNote: How to Make Stan Data\n')
  cat("1. Use Preprocessed Media Mix Data\n")
  cat(" - Media mix data should be preprocessed by `preprocess_MMM()`.\n")
  
  cat("\n2. Choose a Target Normalization Method\n")
  if (target_norm_method == 1) {
    cat(" - target_norm_method=1 --> per dma per target normalization\n")
  } else if (target_norm_method == 2) {
    cat(" - target_norm_method=2 --> per dma across target normalization\n")
  } else if (target_norm_method == 3) {
    cat(" - target_norm_method=3 --> across dma per target normalization\n")
  } else if (target_norm_method == 4) {
    cat(" - target_norm_method=4 --> across dma across target normalization\n")
  }
  
  cat("\n3. Choose a Media Normalization Method\n")
  if (media_norm_method == 1) {
    cat(" - media_norm_method=1 --> per dma per media normalization\n")
  } else if (media_norm_method == 2) {
    cat(" - media_norm_method=2 --> per dma across media normalization\n")
  } else if (media_norm_method == 3) {
    cat(" - media_norm_method=3 --> across dma per media normalization\n")
  } else if (media_norm_method == 4) {
    cat(" - media_norm_method=4 --> across dma across media normalization\n")
  }   
  
  cat("\n4. Create Train/Test Data\n")
  if (is.null(test_date)) {
    cat(" - The entire data will be used for model calibration as follows:")
  } else {cat(" - The entire data will be partitioned into Train & Test data as follows:")}
  
  data_all   = pp_MMM_ls$target$y_norm3 %>% 
    dplyr::summarise(Sample_Type = "All",
                     Sample_Size = n(),
                     Sample_Pct  = 100,
                     N_Period   = n_distinct(period),
                     First_Date = first(period),
                     Last_Date  = last(period))
  test_date = ifelse(is.null(test_date), data_all$Last_Date+7, test_date) %>% lubridate::as_date()
  
  data_train = pp_MMM_ls$target$y_norm3 %>% 
    dplyr::filter(period < test_date) %>% 
    dplyr::summarise(Sample_Type = "Train",
                     Sample_Size = n(),
                     Sample_Pct  = round(100*Sample_Size/data_all$Sample_Size),
                     N_Period   = n_distinct(period),
                     First_Date = first(period),
                     Last_Date  = last(period))
  
  data_test  = pp_MMM_ls$target$y_norm3 %>% 
    dplyr::filter(period >= test_date) %>% 
    dplyr::summarise(Sample_Type = "Test",
                     Sample_Size = n(),
                     Sample_Pct  = round(100*Sample_Size/data_all$Sample_Size),
                     N_Period   = n_distinct(period),
                     First_Date = first(period),
                     Last_Date  = last(period))
  
  df_train_test = dplyr::bind_rows(data_all, data_train, data_test) 
  df_train_test %>% knitr::kable() %>% print()

  all_media_var <- pp_MMM_ls$extra$var_media
  
  if(is.null(var_media)){
      var_media <- all_media_var
  }

  ##Get the media indexes to be used in the model
  media_index <- which(all_media_var %in% var_media)
  
  cat("\n5. Specify Priors for Media Parameters\n")
  if (is.null(pooling_type)) stop("\n'pooling_type' should be one of (partial, no, complete) pooling types!\n")
  
  if (is.null(media_priors)) {
    cat(" - The default priors will be used as follows:")
  } else {cat(" - The user-defined priors will be used as follows:")}
  
  if (pooling_type == "partial" && !is.null(media_priors)) {
    df_media = dplyr::tibble(Media_Mix    = var_media,
                             Pooling_Type = pooling_type,
                             Distribution = "log-normal",
                             Median       = media_priors$median %>% round(3),
                             STD          = media_priors$std,
                             Mean         = exp(log(Median)+(STD*STD/2)) %>% round(3),
                             Mode         = exp(log(Median)-(STD*STD)) %>% round(3)) %>% 
      dplyr::select(Media_Mix:Median, Mean, Mode, STD)
    
  } else if (pooling_type == "partial" && is.null(media_priors)) {
    df_media = dplyr::tibble(Media_Mix    = var_media,
                             Pooling_Type = pooling_type,
                             Distribution = "log-normal",   # See https://keisan.casio.com/exec/system/1180573214.
                             Median       = 0.2,
                             STD          = 1,
                             Mean         = exp(log(Median)+(STD*STD/2)) %>% round(3),
                             Mode         = exp(log(Median)-(STD*STD)) %>% round(3)) %>% 
      dplyr::select(Media_Mix:Median, Mean, Mode, STD)
  } else if (pooling_type != "partial" && !is.null(media_priors)) {
    df_media = dplyr::tibble(Media_Mix    = var_media,
                             Pooling_Type = pooling_type,
                             Distribution = "half-normal",  # See https://en.wikipedia.org/wiki/Half-normal_distribution.
                             STD          = media_priors$std,
                             Median       = STD*sqrt(2)*0.4769363 %>% round(3), # erfinv(0.5)
                             Mean         = STD*sqrt(2)/sqrt(pi) %>% round(3),
                             Mode         = 0) %>% 
      dplyr::select(Media_Mix:Distribution, Median, Mean, Mode, STD)
  } else if (pooling_type != "partial" && is.null(media_priors)) {
    df_media = dplyr::tibble(Media_Mix    = var_media,
                             Pooling_Type = pooling_type,
                             Distribution = "half-normal",  
                             STD          = 1,
                             Median       = STD*sqrt(2)*0.4769363 %>% round(3), # erfinv(0.5)
                             Mean         = STD*sqrt(2)/sqrt(pi) %>% round(3),
                             Mode         = 0) %>% 
      dplyr::select(Media_Mix:Distribution, Median, Mean, Mode, STD)
  }
  
  df_media %>% knitr::kable() %>% print()  
  
  
  
  #---------------------------------------------------------------------#
  #  Check INPUTS                                                       #
  #---------------------------------------------------------------------#
  ## Check input data.    
  if (is.null(pp_MMM_ls)) stop("\n'pp_MMM_ls' should be provided!\n")    
  if (is.null(target_norm_method)) stop("\n'target_norm_method' should be provided!\n")
  if (is.null(media_norm_method)) stop("\n'media_norm_method' should be provided!\n")
  if (is.null(var_target)) stop("\n'var_target' should be provided!\n")        
  if (is.null(var_control)) stop("\n'control_names' should be provided!\n")       
  
  
  
  #---------------------------------------------------------------------#
  #  Variable Names                                                     #
  #---------------------------------------------------------------------#
  fe_target   = var_target                                       # Target variable
  fe_media    = var_media                        # Media variables
  fe_control  = var_control                                      # Control variables
  
  
  
  #---------------------------------------------------------------------#
  #  Dimensions                                                         #
  #---------------------------------------------------------------------#
  num_obs_all   = data_all$Sample_Size                           # of Total observations
  num_obs_train = data_train$Sample_Size                         # of Train observations
  num_obs_test  = data_test$Sample_Size                          # of Test observations
  num_geo       = length(unique(pp_MMM_ls$extra$df_norm$dma))    # of geos
  num_media     = length(var_media)              # of media variables
  num_control   = length(var_control)                            # of control variables
  num_lag       = pp_MMM_ls$extra$N_lag_media +1                 # of max lags + current value  
  # of geo-level covariates + intercept
  if (is.null(df_geo)) {num_grp_var = 1} else {num_grp_var = 1 + ncol(df_geo)}
  
  
  
  #---------------------------------------------------------------------#
  #  Data                                                               #
  #---------------------------------------------------------------------#    
  # the response variable
  var_target  = paste0('norm_', var_target)
  
  df_y         =  if (target_norm_method == 1) {
    pp_MMM_ls$target$y_norm1
  } else if (target_norm_method == 2) {
    pp_MMM_ls$target$y_norm2
  } else if (target_norm_method == 3) {
    pp_MMM_ls$target$y_norm3
  } else if (target_norm_method == 4) {
    pp_MMM_ls$target$y_norm4
  }
  
  y_train      =  df_y %>% 
    dplyr::filter(period < test_date) %>% 
    dplyr::arrange(dma_id, dma, period) %>% 
    dplyr::pull(var_target)
  
  y_test       =  df_y %>% 
    dplyr::filter(period >= test_date) %>% 
    dplyr::arrange(dma_id, dma, period) %>% 
    dplyr::pull(var_target)
  
  
  # the media variables
  X_media     = if (media_norm_method == 1) {
    pp_MMM_ls$media$X_norm1_array
  } else if (media_norm_method == 2) {
    pp_MMM_ls$media$X_norm2_array
  } else if (media_norm_method == 3) {
    pp_MMM_ls$media$X_norm3_array
  } else if (media_norm_method == 4) {
    pp_MMM_ls$media$X_norm4_array
  }
  
  train_index = df_y %>% 
    tibble::rownames_to_column(var="index") %>% 
    dplyr::filter(period < test_date) %>% 
    dplyr::pull(index) %>% 
    as.numeric()
  
  X_media_train = X_media[train_index,media_index, ]
  X_media_test  = X_media[-train_index,media_index, ]
  
  
  # the control variables
  X_control_train = pp_MMM_ls$control$Z_per_capita %>% 
    dplyr::filter(period < test_date) %>% 
    dplyr::select(all_of(var_control)) %>% 
    as.matrix()
  
  X_control_test  = pp_MMM_ls$control$Z_per_capita %>% 
    dplyr::filter(period >= test_date) %>% 
    dplyr::select(all_of(var_control)) %>% 
    as.matrix()
  
  
  # the geo-level covariates
  if (is.null(df_geo)) {
    Z_geo = rep(1, num_geo) %>%
      as.matrix()
  } else {
    Z_geo = df_geo %>% 
      dplyr::mutate(intercept = 1) %>% 
      dplyr::select(intercept, everything()) %>% 
      as.matrix()        
  }    
  
  
  # the geo-id vector
  id_geo       = df_y %>% 
    dplyr::arrange(dma_id, dma, period) %>% 
    dplyr::pull(dma_id)                          
  
  id_geo_train = df_y %>% 
    dplyr::filter(period < test_date) %>% 
    dplyr::arrange(dma_id, dma, period) %>% 
    dplyr::pull(dma_id)                          
  
  id_geo_test  = df_y %>% 
    dplyr::filter(period >= test_date) %>% 
    dplyr::arrange(dma_id, dma, period) %>% 
    dplyr::pull(dma_id)                          
  
  
  
  #---------------------------------------------------------------------#
  #  DMA Information                                                    #
  #---------------------------------------------------------------------#
  df_DMA = df_y %>% 
    dplyr::distinct(DMA_Id = dma_id, DMA_Name = dma) %>% 
    as.data.frame()
  
  
  
  #---------------------------------------------------------------------#
  #  Priors                                                             #
  #---------------------------------------------------------------------#   
  prior_location = media_priors$median
  prior_spread   = media_priors$std   
  
  #   if (pooling_type == "partial") {
  #     prior_location = log(df_media$Median)
  #     prior_spread   = df_media$STD
  #   } else if (pooling_type != "partial") {
  #     prior_location = media_priors$median
  #     prior_spread   = media_priors$std
  #   }
  
  
  
  #---------------------------------------------------------------------#
  #  Stan Data: Final Outcome                                           #
  #---------------------------------------------------------------------#
  # Stan Data for Stan Code
  stan_train     = list(num_obs         = num_obs_train,     # Dimensions
                        num_geo         = num_geo, 
                        num_media       = num_media, 
                        num_control     = num_control, 
                        num_grp_var     = num_grp_var, 
                        num_lag         = num_lag, 
                        y               = y_train,           # Data
                        X_media         = X_media_train, 
                        X_control       = X_control_train,
                        Z_geo           = Z_geo,
                        id_geo          = id_geo_train,
                        prior_location  = prior_location,    # Priors
                        prior_spread    = prior_spread)
  
  stan_test      = list(num_obs        = num_obs_test,      # Dimensions
                        num_geo        = num_geo, 
                        num_media      = num_media, 
                        num_control    = num_control, 
                        num_grp_var    = num_grp_var, 
                        num_lag        = num_lag, 
                        y              = y_test,            # Data
                        X_media        = X_media_test, 
                        X_control      = X_control_test,
                        Z_geo          = Z_geo,
                        id_geo         = id_geo_test)
  
  stan_data      = list(train          = stan_train,         # Train Data
                        test           = stan_test,          # Test Data
                        DMA            = df_DMA)             # DMA Information
  
  
  # Variable Names for Further Analyses
  variable_names = list(var_target     = fe_target,          # Target variable
                        var_media      = fe_media,           # Media variables
                        var_control    = fe_control)         # Control variables
  
  
  # Normalization Methods for Further Analyses
  norm_methods   = list(target         = target_norm_method,  # Normalization method for Target variable
                        media          = media_norm_method)   # Normalization method for Media variables
  
  
  stan_out       = list(stan_data       = stan_data,
                        variable_names  = variable_names,
                        norm_methods    = norm_methods,
                        train_test_info = df_train_test,      # Train & Test data information
                        media_priors    = df_media)           # Priors for media parameters
  
  
}





diagnose_MCMC <- function(var_target=NULL, stanfit=NULL, chains=NULL, iter=NULL, warmup=NULL, max_treedepth=NULL) {
  # A utility function to diagnose the MCMC samples extracted from a given `stanfit` object
  # 
  # Note:
  # 1. `var_target` selects a specific target variable of interest.  
  # 2. `stanfit` is the fitted object from `rstan::stan`.
  # 3. `chains` is the total number of chains used in MCMC.
  # 4. `iter` is the total number of iterations in MCMC.
  # 5. `warmup` is the total number of iterations used in warmup period.
  
  #---------------------------------------------------------------------#
  #  Check INPUTS                                                       #
  #---------------------------------------------------------------------#
  ## Check input data.    
  if (is.null(var_target)) stop("\n'var_target' should be provided!\n")          
  if (class(stanfit)!="stanfit" ) stop("\n'stanfit' should be provided in stanfit object!\n")
  if (is.null(chains)) stop("\n'chains' should be provided!\n")        
  if (is.null(iter)) stop("\n'iter' should be provided!\n")
  if (is.null(warmup)) stop("\n'warmup' should be provided!\n")
  if (is.null(max_treedepth)) stop("\n'max_treedepth' should be provided!\n")      
  
  
  #---------------------------------------------------------------------#
  #  Data for Diagnostics                                               #
  #---------------------------------------------------------------------#
  df_summary <- summary(stanfit)$summary %>% 
    as.data.frame() %>% 
    dplyr::mutate(parameter = rownames(.)) %>% 
    dplyr::filter(!stringr::str_detect(parameter, "_raw|X_|mu_y|y_|weights")) %>%  # Remove Extra Parameters.
    dplyr::mutate(variable_type = ifelse(stringr::str_detect(parameter, "base"), "Base",
                                         ifelse(stringr::str_detect(parameter, "media"), "Media",
                                                ifelse(stringr::str_detect(parameter, "control"), "Control", "NA")))) %>% 
    dplyr::select(parameter, variable_type, everything()) %>% 
    dplyr::as_tibble()
  
  
  df_diagnostics <- get_sampler_params(stanfit, inc_warmup=TRUE) %>% 
    purrr::set_names(1:chains) %>% 
    purrr::map_df(as.data.frame, .id = "chain_id") %>% 
    dplyr::group_by(chain_id) %>% 
    dplyr::mutate(iter_num      = 1:length(chain_id),
                  warmup_period = ifelse(iter_num <= warmup, "Yes", "No"),
                  warmup_period = factor(warmup_period, levels = c("Yes", "No")))
  
  
  #---------------------------------------------------------------------#
  #  MCMC Diagnostics                                                   #
  #---------------------------------------------------------------------#
  df_stat <- df_diagnostics %>% 
    dplyr::filter(warmup_period != "Yes") %>% 
    dplyr::ungroup() %>% 
    dplyr::summarise(div_total = n(),
                     div_count = sum(divergent__),
                     div_pct   = 100*(div_count/div_total),
                     td_total  = n(),
                     td_count  = sum(treedepth__ == num_treedepth),
                     td_pct    = 100*(td_count/td_total))
  
  cat('\nMCMC Diagnostics\n')
  cat("------------------------------------------------------------\n")
  cat(paste0("0. Target: ", var_target), "\n\n")
  cat("1. Divergence\n")
  cat(paste0(" - The total number of iterations: ", f.integer(df_stat$div_total)), "\n")  
  cat(paste0(" - Iterations with divergences   : ", f.integer(df_stat$div_count)), "\n")
  cat(paste0(" - Divergence %                  : ", f.digit2(df_stat$div_pct), "%"), "\n\n")
  
  plot_divergence = df_diagnostics %>%
    group_by(warmup_period, chain_id) %>%
    summarise(divergent_pct = mean(divergent__ > 0)) %>%
    ggplot(aes(chain_id, divergent_pct, fill = warmup_period)) +
    geom_col(position = 'dodge', color = 'black') +
    geom_hline(aes(yintercept = 0.01), linetype = 3, colour = 'red', size = 1.5) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.01), name = "Divergence Percentage") +  
    ggtitle(paste0(var_target, " - MCMC Divergence: Before/After Warmups")) +
    theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), legend.position = c(0.94, 0.80)) + xlab("Chain")  
  
  # print(plot_divergence)
  
  
  cat("2. Treedepth\n")
  cat(paste0(" - The total number of iterations: ", f.integer(df_stat$div_total)), "\n")    
  cat(paste0(" - Iterations with max treedepth : ", f.integer(df_stat$td_count)), "\n")
  cat(paste0(" - Max treedepth %               : ", f.digit2(df_stat$td_pct), "%"), "\n\n")
  
  plot_treedepth = df_diagnostics %>%
    ggplot(aes(iter_num, treedepth__, color = chain_id)) +
    geom_line() + 
    geom_hline(aes(yintercept = num_treedepth, linetype = "Max Treedepth"), colour = 'red', size = 1.5) +
    geom_vline(aes(xintercept = num_warmups, linetype = "Warmup End"), colour = 'black', size = 1.5) +
    scale_linetype_manual(name = NULL, values = c(3, 3), 
                          guide = guide_legend(override.aes = list(color = c("red", "black")))) + 
    ggtitle(paste0(var_target, " - MCMC Treedepth: Before/After Warmups")) +
    theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), legend.position = c(0.94, 0.22)) + 
    xlab("Iteration") + ylab("Treedepth")
  
  # print(plot_treedepth)
  
  
  cat("3. n_eff, Effective Sample Size\n")
  para_total = df_summary %>% nrow()-1
  para_count = sum((df_summary$n_eff/df_stat$div_total) < 0.01)
  para_pct   = 100*(para_count/para_total)
  cat(paste0(" - The number of model parameters: ", f.integer(para_total)), "\n")
  cat(paste0(" - The evaluation criterion      : n_eff > 1% of iterations\n"))  
  cat(paste0(" - Parameters with the problem   : ", f.integer(para_count)), "\n")
  cat(paste0(" - Small n_eff parameter %       : ", f.digit2(para_pct) , "%"), "\n\n")
  
  max_n_eff <- (num_iterations-num_warmups) * num_chains
  min_n_eff <- (max_n_eff/1000) %>% floor()
  
  plot_n_eff = df_summary %>% 
    ggplot(aes(x = n_eff, color = variable_type, fill = variable_type)) + 
    geom_histogram(bins = 100, ) + 
    geom_vline(aes(xintercept = max_n_eff, linetype = "Max n_eff"), colour = 'black', size = 1.5) +
    geom_vline(aes(xintercept = min_n_eff, linetype = "Min n_eff"), colour = 'red', size = 1.5) +  
    scale_linetype_manual(name = NULL, values = c(3, 3), 
                          guide = guide_legend(override.aes = list(color = c("black", "red")))) + 
    ggtitle(paste0(var_target, " - MCMC Effective Sample Size by Variable Type")) +    
    theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), legend.position = c(0.95, 0.80)) + 
    xlab("Effective Sample Size") + ylab("Count")
  
  # print(plot_n_eff)
  
  
  cat("4. Rhat, Potential Scale Reduction Factor\n")
  para_total = df_summary %>% nrow()-1
  para_count = sum((df_summary$Rhat > 1.05))
  para_pct   = 100*(para_count/para_total)
  cat(paste0(" - The number of model parameters: ", f.integer(para_total)), "\n")
  cat(paste0(" - The evaluation criterion      : Rhat < 1.05\n"))    
  cat(paste0(" - Parameters with the problem   : ", f.integer(para_count)), "\n")
  cat(paste0(" - Poor mixing parameter %       : ", f.digit2(para_pct) , "%"), "\n\n")
  
  plot_Rhat = df_summary %>% 
    ggplot(aes(x = Rhat, color = variable_type, fill = variable_type)) + 
    geom_histogram(bins = 50, ) + 
    geom_vline(aes(xintercept = 1.05, linetype = "Rhat=1.05"), colour = 'black', size = 1.5) +
    geom_vline(aes(xintercept = 1.1, linetype = "Rhat=1.1, Not Mixed"), colour = 'red', size = 1.5) +  
    scale_linetype_manual(name = NULL, values = c(3, 3), 
                          guide = guide_legend(override.aes = list(color = c("black", "red")))) + 
    ggtitle(paste0(var_target, " - MCMC Rhat by Variable Type")) +        
    theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), legend.position = c(0.85, 0.78)) + 
    xlab("Rhat") + ylab("Count")
  
  # print(plot_Rhat)
  
  
  #---------------------------------------------------------------------#
  #  Diagnostic Plots                                                   #
  #---------------------------------------------------------------------#
  diagnotics_out = list(
    divergence = plot_divergence,            
    treedepth  = plot_treedepth, 
    n_eff      = plot_n_eff, 
    Rhat       = plot_Rhat
  )    
  
}





evaluate_MMM = function(df_XyZ=NULL, pp_MMM_ls=NULL, standata=NULL, stanfit=NULL, media_positive=FALSE, pooling_type_media="partial",pooling_type_control="partial") {
  # A utility function to evaluate a model fitted by a given Media Mix Model in Stan
  # 
  # Note:
  # 1. `df_XyZ` is the raw data in media mix modeling.  
  # 2. `pp_MMM_ls` is the pre-processed media mix data in list.
  #   - All the required data is embedded in `pp_MMM_ls`.
  #   - All the required data is properly pre-processed via `preprocess_MMM`
  # 3. `standata` is the data generated from `make_stan_data_MMM`.
  # 4. `stanfit` is the fitted object from `rstan::stan`.
  # 5. `media_positive` is set to `FALSE`.
  #   - If `media_positive=TRUE`, then exp(gamma_media) is used.
  # 6. `pooling_type_media` is set to `partial`.
  #   - If `pooling_type_media="partial"`, then 'pop_para_stat_media' is generated.
  
  
  
  #---------------------------------------------------------------------#
  #  Check INPUTS                                                       #
  #---------------------------------------------------------------------#
  ## Check input data.    
  if (is.null(df_XyZ)) stop("\n'df_XyZ' should be provided!\n")        
  if (is.null(pp_MMM_ls)) stop("\n'pp_MMM_ls' should be provided!\n")      
  if (is.null(standata)) stop("\n'standata' should be provided!\n")
  if (class(stanfit)!="stanfit" ) stop("\n'stanfit' should be provided in stanfit object!\n")
  
  
  
  #---------------------------------------------------------------------#
  #  Target                                                             #
  #---------------------------------------------------------------------#
  var_target     = standata$variable_names$var_target
  var_media      = standata$variable_names$var_media
  var_control    = standata$variable_names$var_control
  
  target_norm_method = standata$norm_methods$target
  media_norm_method  = standata$norm_methods$media
  
  df_y_norm = if (target_norm_method == 1) {
    pp_MMM_ls$target$y_norm1
  } else if (target_norm_method == 2) {
    pp_MMM_ls$target$y_norm2
  } else if (target_norm_method == 3) {
    pp_MMM_ls$target$y_norm3
  } else if (target_norm_method == 4) {
    pp_MMM_ls$target$y_norm4
  }
  
  df_y_norm_long = df_y_norm %>% 
    tidyr::gather("y_type", "y_value", -dma_id, -dma, -period) %>% 
    dplyr::mutate(y_type = stringr::str_remove(y_type, "norm_")) %>% 
    dplyr::arrange(dma_id, dma, period)
  
  df_y_max_long  = pp_MMM_ls$target$y_minmax_long %>% 
    dplyr::filter(method == target_norm_method) %>% 
    dplyr::select(dma_id, dma, y_type=y, y_max)
  
  df_norm        = pp_MMM_ls$extra$df_norm
  if (pp_MMM_ls$extra$subs == "no") {
    df_norm = dplyr::rename(df_norm, sub = non_sub)
  }
  
  df_target = df_y_norm_long %>% 
    dplyr::inner_join(df_y_max_long, by = c("dma_id", "dma", "y_type")) %>% 
    dplyr::inner_join(df_norm, by = "dma") %>% 
    dplyr::mutate(y_true = (y_value * y_max * sub) %>% round()) %>% 
    dplyr::arrange(dma_id, dma, period) %>% 
    dplyr::group_by(dma_id, y_type) %>% 
    dplyr::mutate(time = dplyr::row_number()) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(y_type == var_target)
  
  # Split df_target by Train/Test date
  train_start = standata$train_test_info$First_Date[2]
  train_end   = standata$train_test_info$Last_Date[2]
  test_start  = standata$train_test_info$First_Date[3]
  test_end    = standata$train_test_info$Last_Date[3]
  
  # Number of observations for Train/Test data
  train_num_obs = standata$stan_data$train$num_obs
  test_num_obs  = standata$stan_data$test$num_obs
  
  df_target_train = df_target %>% 
    dplyr::filter(period >= train_start, period <= train_end) %>% 
    dplyr::mutate(obs_id  = dplyr::row_number(),
                  company = "Altice",
                  brand   = ifelse(dma_id==17, "Optimum", "Suddenlink")) %>% 
    dplyr::select(obs_id, company, brand, dma_id:period, time, everything())
  
  df_target_test = df_target %>% 
    dplyr::filter(period >= test_start, period <= test_end) %>% 
    dplyr::mutate(obs_id  = dplyr::row_number(),
                  company = "Altice",
                  brand   = ifelse(dma_id==17, "Optimum", "Suddenlink")) %>% 
    dplyr::select(obs_id, company, brand, dma_id:period, time, everything())
  
  
  
  #---------------------------------------------------------------------#
  #  Supplementary Tables                                               #
  #---------------------------------------------------------------------#
  tb_media       = tibble(variable_name = var_media, variable_id = seq(1:length(var_media)))
  tb_control     = tibble(variable_name = c("intercept", var_control), variable_id = seq(0, length(var_control), by=1))
  tb_dma         = df_target %>% dplyr::distinct(dma_id, dma) %>% dplyr::as_tibble()
  key_statistics = tibble::lst(N, Mean, Median, StdDev, Min, Max, Range, 
                               q001, q0025, q005, q01, q015, q025, q075, q085, q09, q095, q0975, q099)
  # Update `var_control` by adding `intercept`.
  var_control    = tb_control$variable_name
  
  
  
  #---------------------------------------------------------------------#
  #  MCMC Draws                                                         #
  #---------------------------------------------------------------------#
  # MCMC Draws:
  #   1. Population-Level Parameters
  #   2. Geo-Level Parameters
  #   3. Fitted and Predicted Target
  #   4. Media and Control Contributions to Target
  #   5. Cost per Media (CPM)
  
  
  
  #---------------------------------------------------------------------#
  #  1. Population-Level Parameters                                     #
  #---------------------------------------------------------------------#
  ### 1. Adstock and Response Curves
  MCMC_alpha_K_S = stanfit %>% 
    tidybayes::gather_draws(alpha[variable_id], K[variable_id], S[variable_id]) %>% 
    tidyr::spread(.variable, .value) %>% 
    dplyr::inner_join(tb_media, by = "variable_id") %>% 
    dplyr::arrange(variable_id, .draw) %>% 
    dplyr::select(variable_id, variable_name, everything()) %>% 
    dplyr::mutate(variable_name = factor(variable_name, levels = c(tb_media$variable_name)))
  
  
  ### 2. Base, Media, and Control Parameters
  # Only partial pooling (or hierarchical Bayes) model has population-level parameters.
  pooling_type_media = standata$media_priors$Pooling_Type[1]
  
  if (pooling_type_media == "partial" & pooling_type_control == "partial") {
    MCMC_gamma_base = stanfit %>% 
      tidybayes::gather_draws(gamma_base[variable_id]) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(variable_id = 0) %>% 
      dplyr::select(variable_id, everything(), -.variable)
    
    if (media_positive == TRUE) {
      MCMC_gamma_media = stanfit %>% 
        tidybayes::gather_draws(exp_gamma_media[i, variable_id]) %>% 
        dplyr::ungroup() %>% 
        dplyr::select(-i, -.variable)  
    } else {
      MCMC_gamma_media = stanfit %>% 
        tidybayes::gather_draws(gamma_media[i, variable_id]) %>% 
        dplyr::ungroup() %>% 
        dplyr::select(-i, -.variable)  
    }
    
    MCMC_gamma_media = MCMC_gamma_media %>% 
      dplyr::inner_join(tb_media, by = "variable_id") %>% 
      dplyr::arrange(variable_id, .draw) %>% 
      dplyr::select(variable_id, variable_name, everything()) %>% 
      dplyr::mutate(variable_name = factor(variable_name, levels = c(tb_media$variable_name)))
    
    MCMC_gamma_control = stanfit %>% 
      tidybayes::gather_draws(gamma_control[i, variable_id]) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(-i, -.variable)
    
    MCMC_gamma_base_control = MCMC_gamma_base %>% 
      dplyr::bind_rows(MCMC_gamma_control) %>% 
      dplyr::inner_join(tb_control, by = "variable_id") %>% 
      dplyr::arrange(variable_id, .draw) %>% 
      dplyr::select(variable_id, variable_name, everything()) %>% 
      dplyr::mutate(variable_name = factor(variable_name, levels = c(tb_control$variable_name)))
  }else if(pooling_type_media == "partial" & pooling_type_control != "partial"){
    MCMC_gamma_base = stanfit %>% 
      tidybayes::gather_draws(gamma_base[variable_id]) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(variable_id = 0) %>% 
      dplyr::select(variable_id, everything(), -.variable)
    
    if (media_positive == TRUE) {
      MCMC_gamma_media = stanfit %>% 
        tidybayes::gather_draws(exp_gamma_media[i, variable_id]) %>% 
        dplyr::ungroup() %>% 
        dplyr::select(-i, -.variable)  
    } else {
      MCMC_gamma_media = stanfit %>% 
        tidybayes::gather_draws(gamma_media[i, variable_id]) %>% 
        dplyr::ungroup() %>% 
        dplyr::select(-i, -.variable)  
    }

    MCMC_gamma_media = MCMC_gamma_media %>% 
      dplyr::inner_join(tb_media, by = "variable_id") %>% 
      dplyr::arrange(variable_id, .draw) %>% 
      dplyr::select(variable_id, variable_name, everything()) %>% 
      dplyr::mutate(variable_name = factor(variable_name, levels = c(tb_media$variable_name)))
  }else if(pooling_type_media != "partial" & pooling_type_control == "partial"){
    MCMC_gamma_base = stanfit %>% 
      tidybayes::gather_draws(gamma_base[variable_id]) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(variable_id = 0) %>% 
      dplyr::select(variable_id, everything(), -.variable)

    MCMC_gamma_control = stanfit %>% 
      tidybayes::gather_draws(gamma_control[i, variable_id]) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(-i, -.variable)
    
    MCMC_gamma_base_control = MCMC_gamma_base %>% 
      dplyr::bind_rows(MCMC_gamma_control) %>% 
      dplyr::inner_join(tb_control, by = "variable_id") %>% 
      dplyr::arrange(variable_id, .draw) %>% 
      dplyr::select(variable_id, variable_name, everything()) %>% 
      dplyr::mutate(variable_name = factor(variable_name, levels = c(tb_control$variable_name)))
  }
  
  
  ### 3. Standard Deviations
  # Check how to handle sigma_media if (media_positive == TRUE)
  if (pooling_type_media == "partial" & pooling_type_control == "partial") {
    MCMC_std = stanfit %>% 
      tidybayes::gather_draws(sigma_y, sigma_base, sigma_media, sigma_control)
    MCMC_std_wide = MCMC_std %>% 
      tidyr::spread(.variable, .value)
  } else if(pooling_type_media == "partial" & pooling_type_control != "partial"){
    MCMC_std = stanfit %>% tidybayes::gather_draws(sigma_y,sigma_base,sigma_media)
    MCMC_std_wide = MCMC_std %>% 
      tidyr::spread(.variable, .value)
  } else if(pooling_type_media != "partial" & pooling_type_control == "partial"){
    MCMC_std = stanfit %>% 
      tidybayes::gather_draws(sigma_y, sigma_base, sigma_control)
    MCMC_std_wide = MCMC_std %>% 
      tidyr::spread(.variable, .value)
  }else{
    MCMC_std = stanfit %>% tidybayes::gather_draws(sigma_y)
    MCMC_std_wide = MCMC_std %>% 
      tidyr::spread(.variable, .value)
  }
  
  
  ### 4. Summary Statistics and Plots
  # alpha, K, S
  pop_para_stat_alpha_K_S = MCMC_alpha_K_S %>% 
    dplyr::group_by(variable=variable_name) %>%  
    dplyr::summarise_at(vars(alpha, K, S), key_statistics) %>% 
    tidyr::gather("metric", "value", -variable) %>% 
    tidyr::separate(metric, c("metric", "stat")) %>% 
    tidyr::spread(stat, value) %>% 
    dplyr::mutate(type     = "Adstock & Hill",
                  variable = as.character(variable)) %>% 
    dplyr::select(type, variable, metric, N, Mean, Median, StdDev, Min, Max, Range, everything())
  
  pop_para_plot_alpha_K_S = MCMC_alpha_K_S %>% 
    tidyr::gather("parameter", "value", -variable_id, -variable_name, -.chain, -.iteration, -.draw) %>% 
    ggplot(aes(x = value, y = parameter, color = parameter)) +
    tidybayes::stat_pointintervalh(point_interval = mean_qi, .width = c(.66, .95)) +
    tidybayes::stat_halfeyeh(point_interval = mean_qi, .width = c(.99, .8)) +
    facet_wrap(variable_name ~ parameter, scales = "free") +
    ggtitle(paste0(var_target, ": Adstock and Hill Curve Parameters")) +    
    theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
          legend.position = "none", axis.text.y=element_blank()) + 
    xlab("Posterior Distribution") + ylab("Adstock & Hill Curve Parameters")
  
  # gamma_media & gamma control
  if (pooling_type_media == "partial" & pooling_type_control == "partial") {
    # gamma_media
    pop_para_stat_media = MCMC_gamma_media %>% 
      dplyr::group_by(variable=variable_name) %>%  
      dplyr::summarise_at(vars(.value), key_statistics) %>%  
      dplyr::mutate(type     = "media variable",
                    metric   = NA,
                    variable = as.character(variable)) %>%
      dplyr::select(type, metric, everything())
    
    pop_para_plot_media = MCMC_gamma_media %>% 
      ggplot(aes(x = .value, y = variable_name, color = variable_name)) +
      tidybayes::stat_pointintervalh(point_interval = mean_qi, .width = c(.66, .95)) +
      tidybayes::stat_halfeyeh(point_interval = mean_qi, .width = c(.99, .8)) +
      facet_wrap(~ variable_name, scales = "free") + 
      ggtitle(paste0(var_target, ": Posterior Distribution of Media Channels")) +    
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
            legend.position = "none", axis.text.y=element_blank()) + 
      xlab("Posterior Distribution") + ylab("Media Channels")
    
    # gamma control
    pop_para_stat_control = MCMC_gamma_base_control %>% 
      dplyr::group_by(variable=variable_name) %>%  
      dplyr::summarise_at(vars(.value), key_statistics) %>% 
      dplyr::mutate(type     = "control variable",
                    metric   = NA,
                    variable = as.character(variable)) %>%
      dplyr::select(type, metric, everything())
    
    pop_para_plot_control = MCMC_gamma_base_control %>% 
      ggplot(aes(x = .value, y = variable_name, color = variable_name)) +
      tidybayes::stat_pointintervalh(point_interval = mean_qi, .width = c(.66, .95)) +
      tidybayes::stat_halfeyeh(point_interval = mean_qi, .width = c(.99, .8)) +
      facet_wrap(~ variable_name, scales = "free") + 
      ggtitle(paste0(var_target, ": Posterior Distribution of Control Variables")) +    
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
            legend.position = "none", axis.text.y=element_blank()) + 
      xlab("Posterior Distribution") + ylab("Control Variables")
  }else if(pooling_type_media == "partial" & pooling_type_control != "partial"){
    # gamma_media
    pop_para_stat_media = MCMC_gamma_media %>% 
      dplyr::group_by(variable=variable_name) %>%  
      dplyr::summarise_at(vars(.value), key_statistics) %>%  
      dplyr::mutate(type     = "media variable",
                    metric   = NA,
                    variable = as.character(variable)) %>%
      dplyr::select(type, metric, everything())
    
    pop_para_plot_media = MCMC_gamma_media %>% 
      ggplot(aes(x = .value, y = variable_name, color = variable_name)) +
      tidybayes::stat_pointintervalh(point_interval = mean_qi, .width = c(.66, .95)) +
      tidybayes::stat_halfeyeh(point_interval = mean_qi, .width = c(.99, .8)) +
      facet_wrap(~ variable_name, scales = "free") + 
      ggtitle(paste0(var_target, ": Posterior Distribution of Media Channels")) +    
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
            legend.position = "none", axis.text.y=element_blank()) + 
      xlab("Posterior Distribution") + ylab("Media Channels")
    

  }else if(pooling_type_media != "partial" & pooling_type_control == "partial"){
    # gamma control
    pop_para_stat_control = MCMC_gamma_base_control %>% 
      dplyr::group_by(variable=variable_name) %>%  
      dplyr::summarise_at(vars(.value), key_statistics) %>% 
      dplyr::mutate(type     = "control variable",
                    metric   = NA,
                    variable = as.character(variable)) %>%
      dplyr::select(type, metric, everything())
    
    pop_para_plot_control = MCMC_gamma_base_control %>% 
      ggplot(aes(x = .value, y = variable_name, color = variable_name)) +
      tidybayes::stat_pointintervalh(point_interval = mean_qi, .width = c(.66, .95)) +
      tidybayes::stat_halfeyeh(point_interval = mean_qi, .width = c(.99, .8)) +
      facet_wrap(~ variable_name, scales = "free") + 
      ggtitle(paste0(var_target, ": Posterior Distribution of Control Variables")) +    
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
            legend.position = "none", axis.text.y=element_blank()) + 
      xlab("Posterior Distribution") + ylab("Control Variables")

  }
  
  # Standard Deviation
  pop_para_stat_std = MCMC_std %>% 
    dplyr::group_by(variable=.variable) %>%  
    dplyr::summarise_at(vars(.value), key_statistics) %>%  
    dplyr::mutate(type     = "STD",
                  metric   = NA,
                  variable = as.character(variable)) %>%
    dplyr::select(type, metric, everything())
  
  pop_para_plot_std = MCMC_std %>% 
    ggplot(aes(x = .value, y = .variable, color = .variable)) +
    tidybayes::stat_pointintervalh(point_interval = mean_qi, .width = c(.66, .95)) +
    tidybayes::stat_halfeyeh(point_interval = mean_qi, .width = c(.99, .8)) +
    facet_wrap(~ .variable, scales = "free") + 
    ggtitle(paste0(var_target, ": Standard Deviation by Parameter")) +    
    theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
          legend.position = "none", axis.text.y=element_blank()) + 
    xlab("Posterior Distribution") + ylab("Standard Deviation")
  
  if (pooling_type_media == "partial" & pooling_type_control == "partial") {
    pop_para_stat_all = bind_rows(pop_para_stat_alpha_K_S, pop_para_stat_media, pop_para_stat_control, pop_para_stat_std)
  } else if(pooling_type_media == "partial" & pooling_type_control != "partial"){
    pop_para_stat_all = bind_rows(pop_para_stat_alpha_K_S, pop_para_stat_media, pop_para_stat_std)
  }else if(pooling_type_media != "partial" & pooling_type_control == "partial"){
    pop_para_stat_all = bind_rows(pop_para_stat_alpha_K_S, pop_para_stat_control, pop_para_stat_std)
  }else {
    pop_para_stat_all = bind_rows(pop_para_stat_alpha_K_S, pop_para_stat_std)
  }
  
  
  
  #---------------------------------------------------------------------#
  #  2. Geo-Level Parameters                                            #
  #---------------------------------------------------------------------#
  ### Geo-Level Parameters 
  MCMC_beta_media = stanfit %>% 
    tidybayes::gather_draws(beta_media[dma_id, variable_id]) %>% 
    dplyr::inner_join(tb_media, by = "variable_id") %>%
    dplyr::inner_join(tb_dma, by = "dma_id") %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(dma_id, dma, variable_id, .draw) %>% 
    dplyr::select(dma_id, dma, variable_id, variable_name, everything())
  
  MCMC_beta_base = stanfit %>% 
    tidybayes::gather_draws(beta_base[dma_id]) %>% 
    dplyr::mutate(variable_id = 0) %>% 
    dplyr::select(dma_id, variable_id, everything())
  
  MCMC_beta_control = stanfit %>% 
    tidybayes::gather_draws(beta_control[dma_id, variable_id])
  
  MCMC_beta_base_control = MCMC_beta_base %>% 
    dplyr::bind_rows(MCMC_beta_control) %>% 
    dplyr::inner_join(tb_control, by = "variable_id") %>%
    dplyr::inner_join(tb_dma, by = "dma_id") %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(dma_id, dma, variable_id, .draw) %>% 
    dplyr::select(dma_id, dma, variable_id, variable_name, everything())
  
  
  ### Summary Statistics
  geo_para_stat_media = MCMC_beta_media %>% 
    dplyr::group_by(dma, dma_id, variable=variable_name, variable_id) %>%  
    dplyr::summarise_at(vars(.value), key_statistics) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(type = "media variable") %>% 
    dplyr::select(dma, dma_id, type, variable, variable_id, N, Mean, Median, StdDev, Min, Max, Range, everything()) %>% 
    dplyr::arrange(dma_id, type, variable_id)
  
  geo_para_stat_control = MCMC_beta_base_control %>% 
    dplyr::group_by(dma, dma_id, variable=variable_name, variable_id) %>%  
    dplyr::summarise_at(vars(.value), key_statistics) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(type = "control variable") %>% 
    dplyr::select(dma, dma_id, type, variable, variable_id, N, Mean, Median, StdDev, Min, Max, Range, everything()) %>% 
    dplyr::arrange(dma_id, type, variable_id)
  
  geo_para_stat_all = geo_para_stat_media %>% 
    dplyr::bind_rows(geo_para_stat_control) %>% 
    dplyr::mutate(type = factor(type, levels = c("media variable", "control variable"))) %>% 
    dplyr::arrange(dma_id, type, variable_id)
  
  
  
  ### Summary Plots
  #----------------
  #  DMA Plots
  #----------------
  # Create Plots for Each of DMAs  
  dma_ls    = tb_dma$dma %>% as.vector()
  dma_plots = vector(mode = "list", length = length(dma_ls))
  MCMC_beta = dplyr::bind_rows(MCMC_beta_media %>% dplyr::mutate(type = "media variable"),
                               MCMC_beta_base_control %>% dplyr::mutate(type = "control variable")) %>% 
    dplyr::mutate(variable_name = factor(variable_name, 
                                         levels = c(tb_media$variable_name, tb_control$variable_name))) %>% 
    dplyr::arrange(dma_id, variable_name, .draw)
  
  for (i in 1:length(dma_ls)) {
    dma_plots[[i]] = MCMC_beta %>% 
      dplyr::filter(dma == dma_ls[i]) %>% 
      ggplot(aes(x = .value, y = variable_name, color = variable_name)) +
      tidybayes::stat_pointintervalh(point_interval = mean_qi, .width = c(.66, .95)) +
      tidybayes::stat_halfeyeh(point_interval = mean_qi, .width = c(.99, .8)) +
      facet_wrap(~ variable_name, scales = "free") + 
      ggtitle(paste0(var_target, " in ",dma_ls[i], ": Posterior Distribution of Media/Control Variables")) +    
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
            legend.position = "none", axis.text.y=element_blank()) + 
      xlab("Posterior Distribution") + ylab("Media/Control Variables")
    
    names(dma_plots)[[i]] = make.names(dma_ls[i])
  }
  
  #----------------
  #  Plots by Type
  #----------------
  ### Media Plots across DMAs
  media_plots = vector(mode = "list", length = length(var_media))
  
  for (i in 1:length(var_media)) {
    media_plots[[i]] = MCMC_beta_media %>% 
      dplyr::filter(variable_name == var_media[i]) %>% 
      ggplot(aes(x = .value, y = dma, color = dma)) +
      tidybayes::stat_pointintervalh(point_interval = mean_qi, .width = c(.66, .95)) +
      tidybayes::stat_halfeyeh(point_interval = mean_qi, .width = c(.99, .8)) +
      facet_wrap(~ dma, scales = "free") + 
      ggtitle(paste0(var_target, " vs ", var_media[i])) +        
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
            legend.position = "none", axis.text.y=element_blank()) + 
      xlab("Posterior Distribution") + ylab(paste0(var_media[i]))
    
    names(media_plots)[[i]] = make.names(var_media[i])
  }
  
  ### Control Plots across DMAs
  control_plots = vector(mode = "list", length = length(var_control))
  
  for (i in 1:length(var_control)) {
    control_plots[[i]] = MCMC_beta_base_control %>% 
      dplyr::filter(variable_name == var_control[i]) %>% 
      ggplot(aes(x = .value, y = dma, color = dma)) +
      tidybayes::stat_pointintervalh(point_interval = mean_qi, .width = c(.66, .95)) +
      tidybayes::stat_halfeyeh(point_interval = mean_qi, .width = c(.99, .8)) +
      facet_wrap(~ dma, scales = "free") + 
      ggtitle(paste0(var_target, " vs ", var_control[i])) +        
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
            legend.position = "none", axis.text.y=element_blank()) + 
      xlab("Posterior Distribution") + ylab(paste0(var_control[i]))
    
    names(control_plots)[[i]] = make.names(var_control[i])
  }
  
  
  
  #---------------------------------------------------------------------#
  #  3. Fitted and Predicted Target                                     #
  #---------------------------------------------------------------------#
  ### Train Data
  MCMC_y_fitted_train    = stanfit %>% 
    tidybayes::gather_draws(y_fitted[obs_id]) %>% 
    dplyr::inner_join(df_target_train, by = "obs_id") %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(.value_true = .value * y_max * sub)
  
  y_fitted_dma_train     = summarise_fitted_y(MCMC_draw = MCMC_y_fitted_train, group_variable = "dma")
  y_fitted_brand_train   = summarise_fitted_y(MCMC_draw = MCMC_y_fitted_train, group_variable = "brand")
  y_fitted_company_train = summarise_fitted_y(MCMC_draw = MCMC_y_fitted_train, group_variable = "company")
  
  y_fitted_stats_train  = dplyr::bind_rows(y_fitted_dma_train$MCMC_fit_metrics_stat, 
                                           y_fitted_brand_train$MCMC_fit_metrics_stat, 
                                           y_fitted_company_train$MCMC_fit_metrics_stat) %>% 
    dplyr::mutate(sample_type = "Train") %>% 
    dplyr::mutate(group  = factor(group, 
                                  levels = c("Altice", "Optimum", "Suddenlink",
                                             df_target %>% distinct(dma) %>% pull())),
                  metric = factor(metric, levels = c("r2", "mape", "rmse", "mae"))) %>%
    dplyr::select(group, metric, sample_type, everything()) %>% 
    dplyr::arrange(group, metric)
  
  ### Test Data
  if (test_num_obs > 0) {
    MCMC_y_fitted_test     = generate_MCMC_fitted_y(stanfit    = stanfit, 
                                                    standata   = standata$stan_data$test, 
                                                    targetdata = df_target_test)
    
    y_fitted_dma_test      = summarise_fitted_y(MCMC_draw = MCMC_y_fitted_test, group_variable = "dma")
    y_fitted_brand_test    = summarise_fitted_y(MCMC_draw = MCMC_y_fitted_test, group_variable = "brand")
    y_fitted_company_test  = summarise_fitted_y(MCMC_draw = MCMC_y_fitted_test, group_variable = "company")
    
    y_fitted_stats_test   = dplyr::bind_rows(y_fitted_dma_test$MCMC_fit_metrics_stat, 
                                             y_fitted_brand_test$MCMC_fit_metrics_stat, 
                                             y_fitted_company_test$MCMC_fit_metrics_stat) %>% 
      dplyr::mutate(sample_type = "Test") %>% 
      dplyr::mutate(group  = factor(group, 
                                    levels = c("Altice", "Optimum", "Suddenlink",
                                               df_target %>% distinct(dma) %>% pull())),
                    metric = factor(metric, levels = c("r2", "mape", "rmse", "mae"))) %>%
      dplyr::select(group, metric, sample_type, everything()) %>% 
      dplyr::arrange(group, metric)
  }
  
  ### Create a dataframe of fit metrics summary
  if (test_num_obs > 0) {
    y_fitted_stats        = dplyr::bind_rows(y_fitted_stats_train, y_fitted_stats_test) %>% 
      dplyr::arrange(group, metric)
  } else {
    y_fitted_stats        = y_fitted_stats_train
  }
  
  
  ### Create fit plots
  y_fitted_MCMC_fit_train = dplyr::bind_rows(y_fitted_dma_train$MCMC_fit_metrics,
                                             y_fitted_brand_train$MCMC_fit_metrics,
                                             y_fitted_company_train$MCMC_fit_metrics) %>% 
    dplyr::mutate(sample_type = "Train")%>% 
    dplyr::mutate(group = factor(group, 
                                 levels = c("Altice", "Optimum", "Suddenlink",
                                            df_target %>% distinct(dma) %>% pull())))
  
  if (test_num_obs > 0) {
    y_fitted_MCMC_fit_test  = dplyr::bind_rows(y_fitted_dma_test$MCMC_fit_metrics,
                                               y_fitted_brand_test$MCMC_fit_metrics,
                                               y_fitted_company_test$MCMC_fit_metrics)  %>% 
      dplyr::mutate(sample_type = "Test")%>% 
      dplyr::mutate(group = factor(group, 
                                   levels = c("Altice", "Optimum", "Suddenlink",
                                              df_target %>% distinct(dma) %>% pull())))
  }    
  
  if (test_num_obs > 0) {
    y_fitted_MCMC_fit       = dplyr::bind_rows(y_fitted_MCMC_fit_train, y_fitted_MCMC_fit_test)
  } else {
    y_fitted_MCMC_fit       = y_fitted_MCMC_fit_train
  }
  
  y_fitted_plot_r2 = y_fitted_MCMC_fit %>% 
    ggplot(aes(x = r2_true, y = fct_rev(group), color = group)) +
    tidybayes::stat_pointintervalh(point_interval = mean_qi, .width = c(.66, .95)) +
    tidybayes::stat_halfeyeh(point_interval = mean_qi, .width = c(.99, .8)) +
    ggtitle(paste0(var_target, " - R2: Altice, Footprints, and DMAs")) +    
    theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), legend.position = "none") + 
    xlab("R2") + ylab("Altice, Footprints, and DMAs") +
    facet_wrap(~sample_type)
  
  y_fitted_plot_mape = y_fitted_MCMC_fit %>% 
    ggplot(aes(x = mape_true, y = fct_rev(group), color = group)) +
    tidybayes::stat_pointintervalh(point_interval = mean_qi, .width = c(.66, .95)) +
    tidybayes::stat_halfeyeh(point_interval = mean_qi, .width = c(.99, .8)) +
    ggtitle(paste0(var_target, " - MAPE: Altice, Footprints, and DMAs")) +        
    theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), legend.position = "none") + 
    xlab("Mean Abs Pct Error") + ylab("Altice, Footprints, and DMAs") +
    facet_wrap(~sample_type)
  
  ### Create a dataframe of time series summary
  y_fitted_ts_train  = dplyr::bind_rows(y_fitted_dma_train$MCMC_fit_ts,
                                        y_fitted_brand_train$MCMC_fit_ts,
                                        y_fitted_company_train$MCMC_fit_ts) %>% 
    dplyr::mutate(sample_type = "Train") %>% 
    dplyr::mutate(group = factor(group, 
                                 levels = c("Altice", "Optimum", "Suddenlink",
                                            df_target %>% distinct(dma) %>% pull()))) %>% 
    dplyr::arrange(group, time)
  
  if (test_num_obs > 0) {
    y_fitted_ts_test     = dplyr::bind_rows(y_fitted_dma_test$MCMC_fit_ts,
                                            y_fitted_brand_test$MCMC_fit_ts,
                                            y_fitted_company_test$MCMC_fit_ts) %>% 
      dplyr::mutate(sample_type = "Test") %>% 
      dplyr::mutate(group = factor(group, 
                                   levels = c("Altice", "Optimum", "Suddenlink",
                                              df_target %>% distinct(dma) %>% pull()))) %>% 
      dplyr::arrange(group, time)
  }  
  
  if (test_num_obs > 0) {
    y_fitted_ts         = dplyr::bind_rows(y_fitted_ts_train, y_fitted_ts_test)
  } else {
    y_fitted_ts         = y_fitted_ts_train
  }
  
  ### Time Series Plots
  y_fitted_plot_ts_geo = y_fitted_ts %>% 
    dplyr::filter(!(group %in% c("Altice", "Optimum", "Suddenlink"))) %>%
    ggplot(aes(x = time, y = y_true)) +
    geom_line(aes(linetype = "True y"), colour = 'red', size = 1.5) +
    geom_point(colour = 'green') + 
    geom_line(aes(y = Mean, linetype = "Fitted y"), colour = 'blue', size = 1.0) +
    geom_ribbon(aes(ymin=q0025, ymax=q0975), linetype=2, alpha=0.1, fill = "blue") + 
    scale_linetype_manual(name = NULL, values = c(1, 1), 
                          guide = guide_legend(override.aes = list(color = c("blue", "red")))) + 
    facet_wrap(~ group, scales = "free") + 
    ggtitle(paste0(var_target, ": True vs Fitted y")) +        
    theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), legend.position = c(0.97, 1.0)) + 
    xlab("Time") + ylab("Sales")
  
  if (test_num_obs > 0) {
    y_fitted_plot_ts_geo = y_fitted_plot_ts_geo + 
      geom_vline(xintercept = y_fitted_ts_test$time %>% min(), color='black', lwd = 1.0)
  }
  
  y_fitted_plot_ts_brand = y_fitted_ts %>% 
    dplyr::filter(group %in% c("Altice", "Optimum", "Suddenlink")) %>%
    ggplot(aes(x = time, y = y_true, color = sample_type)) +
    geom_line(aes(linetype = "True y"), colour = 'red', size = 1.5) +
    geom_point(colour = 'green') + 
    geom_line(aes(y = Mean, linetype = "Fitted y"), colour = 'blue', size = 1.0) +
    geom_ribbon(aes(ymin=q0025, ymax=q0975), linetype=2, alpha=0.1, fill = "blue") + 
    scale_linetype_manual(name = NULL, values = c(1, 1), 
                          guide = guide_legend(override.aes = list(color = c("blue", "red")))) + 
    facet_wrap(~ group, scales = "free", nrow = 3) + 
    ggtitle(paste0(var_target, ": True vs Fitted y")) +        
    theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), legend.position = c(0.05, 0.95)) + 
    xlab("Time") + ylab("Sales") 
  
  if (test_num_obs > 0) {
    y_fitted_plot_ts_brand = y_fitted_plot_ts_brand +
      geom_vline(xintercept = y_fitted_ts_test$time %>% min(), color='black', lwd = 1.0)
  }
  
  
  
  #---------------------------------------------------------------------#
  #  4. Media Contribution to Target                                    #
  #---------------------------------------------------------------------#
  ### MCMC draws of X_Hill * beta_media 
  X_Hill_media_MCMC_raw = stanfit %>% 
    tidybayes::gather_draws(X_Hill[obs_id, variable_id]) %>% 
    dplyr::inner_join(df_target_train, by = "obs_id") %>% 
    dplyr::inner_join(MCMC_beta_media, by = c("dma_id", "dma","variable_id", ".draw")) %>% 
    dplyr::mutate(fitted = (.value.x * .value.y * sub * y_max))
  
  ### Use `summarise_fitted_media`
  MCMC_media_dma     = summarise_fitted_media(MCMC_media     = X_Hill_media_MCMC_raw,
                                              MCMC_y         = MCMC_y_fitted_train,
                                              group_variable = "dma")
  
  MCMC_media_brand   = summarise_fitted_media(MCMC_media     = X_Hill_media_MCMC_raw,
                                              MCMC_y         = MCMC_y_fitted_train,
                                              group_variable = "brand")
  
  MCMC_media_company = summarise_fitted_media(MCMC_media     = X_Hill_media_MCMC_raw,
                                              MCMC_y         = MCMC_y_fitted_train,
                                              group_variable = "company")
  
  
  ### Summary Statistics
  media_fitted_stat1 = dplyr::bind_rows(MCMC_media_company, MCMC_media_brand,  MCMC_media_dma) %>% 
    dplyr::mutate(group = factor(group, 
                                 levels = c("Altice", "Optimum", "Suddenlink",
                                            df_target %>% distinct(dma) %>% pull()))) %>% 
    dplyr::group_by(group, variable_id, variable_name) %>%  
    dplyr::summarise_at(vars(contribution_media), key_statistics) %>% 
    dplyr::ungroup()
  
  media_fitted_stat2 = dplyr::bind_rows(MCMC_media_company, MCMC_media_brand,  MCMC_media_dma) %>% 
    dplyr::filter(variable_id == 1) %>% 
    dplyr::mutate(group = factor(group, 
                                 levels = c("Altice", "Optimum", "Suddenlink",
                                            df_target %>% distinct(dma) %>% pull()))) %>% 
    dplyr::group_by(group, variable_id, variable_name) %>%  
    dplyr::summarise_at(vars(contribution_total), key_statistics) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(variable_id   = 0,
                  variable_name = "media_contribution_y")
  
  media_fitted_stats = dplyr::bind_rows(media_fitted_stat1, media_fitted_stat2) %>% 
    dplyr::arrange(group, variable_id)
  
  
  ### Summary Plots      
  media_fitted_plot_brand = dplyr::bind_rows(MCMC_media_company, MCMC_media_brand) %>% 
    dplyr::mutate(group         = factor(group, levels = c("Altice", "Optimum", "Suddenlink")),
                  variable_name = factor(variable_name, levels = c(var_media))) %>% 
    ggplot(aes(x = contribution_media, y = fct_rev(group), color = group)) +
    tidybayes::stat_pointintervalh(point_interval = mean_qi, .width = c(.66, .95)) +
    tidybayes::stat_halfeyeh(point_interval = mean_qi, .width = c(.99, .8)) +
    facet_wrap(~ variable_name, scales = "fixed") +
    ggtitle(paste0(var_target, ": Posterior Distribution of Media Contribution to y")) +    
    theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), legend.position = "none") + 
    xlab("Media Contribution to y") + ylab("Altice and Footprints")
  
  media_fitted_plot_dma = dplyr::bind_rows(MCMC_media_company, MCMC_media_brand, MCMC_media_dma) %>% 
    dplyr::mutate(group         = factor(group,
                                         levels = c("Altice", "Optimum", "Suddenlink", df_target %>% distinct(dma) %>% pull())),
                  variable_name = factor(variable_name, levels = c(var_media))) %>% 
    ggplot(aes(x = contribution_media, y = fct_rev(group), color = group)) +
    tidybayes::stat_pointintervalh(point_interval = mean_qi, .width = c(.66, .95)) +
    tidybayes::stat_halfeyeh(point_interval = mean_qi, .width = c(.99, .8)) +
    facet_wrap(~ variable_name, scales = "fixed") +
    ggtitle(paste0(var_target, ": Posterior Distribution of Media Contribution to y")) +    
    theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), legend.position = "none") + 
    xlab("Media Contribution to y") + ylab("Altice, Footprints, and DMAs")
  
  # Create Plots for Each of DMAs  
  dma_media_plots = vector(mode = "list", length = length(dma_ls))
  
  for (i in 1:length(dma_ls)) {
    dma_media_plots[[i]] = MCMC_media_dma %>% 
      dplyr::filter(group == dma_ls[i]) %>% 
      dplyr::mutate(variable_name = factor(variable_name, levels = c(var_media))) %>%       
      ggplot(aes(x = contribution_media, y = variable_name, color = variable_name)) +
      tidybayes::stat_pointintervalh(point_interval = mean_qi, .width = c(.66, .95)) +
      tidybayes::stat_halfeyeh(point_interval = mean_qi, .width = c(.99, .8)) +
      facet_wrap(~ variable_name, scales = "free") + 
      ggtitle(paste0(var_target, " in ",dma_ls[i], ": Posterior Distribution of Media Contribution to y")) +    
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
            legend.position = "none", axis.text.y=element_blank()) + 
      xlab("Posterior Distribution") + ylab("Media Contribution to y")
    
    names(dma_media_plots)[[i]] = make.names(dma_ls[i])
  }
  
  # Media Plots across DMAs
  media_contribution_plots = vector(mode = "list", length = length(var_media))
  
  for (i in 1:length(var_media)) {
    media_contribution_plots[[i]] = MCMC_media_dma %>% 
      dplyr::filter(variable_name == var_media[i]) %>% 
      ggplot(aes(x = contribution_media, y = group, color = group)) +
      tidybayes::stat_pointintervalh(point_interval = mean_qi, .width = c(.66, .95)) +
      tidybayes::stat_halfeyeh(point_interval = mean_qi, .width = c(.99, .8)) +
      facet_wrap(~ group, scales = "free") + 
      ggtitle(paste0(var_target, " vs ", var_media[i])) +        
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
            legend.position = "none", axis.text.y=element_blank()) + 
      xlab("Posterior Distribution of Media Contribution to y") + ylab(paste0(var_media[i]))
    
    names(media_contribution_plots)[[i]] = make.names(var_media[i])
  }
  
  
  
  #---------------------------------------------------------------------#
  #  5. Control Contribution to Target                                  #
  #---------------------------------------------------------------------#
  ### MCMC draws of X_control * beta_media 
  X_control   = pp_MMM_ls$control$Z_per_capita %>% 
    dplyr::arrange(dma_id, period) %>% 
    dplyr::group_by(dma_id) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(intercept = 1) %>% 
    dplyr::select(dma_id, dma, period, all_of(var_control)) %>% 
    tidyr::gather(key = "variable_name", value = ".value", -dma_id, -dma, -period) %>% 
    dplyr::mutate(variable_id = factor(variable_name, levels = c(var_control)) %>% as.numeric()-1)
  
  X_control_MCMC_raw = MCMC_beta_base_control %>% 
    dplyr::inner_join(X_control, by = c("dma_id", "dma","variable_id", "variable_name")) %>% 
    dplyr::inner_join(df_target_train, by = c("dma_id", "dma","period")) %>% 
    dplyr::mutate(fitted = (.value.x * .value.y * sub * y_max))
  
  ### Use `summarise_fitted_control`
  MCMC_control_dma     = summarise_fitted_control(MCMC_control   = X_control_MCMC_raw,
                                                  MCMC_y         = MCMC_y_fitted_train,
                                                  group_variable = "dma")
  
  MCMC_control_brand   = summarise_fitted_control(MCMC_control   = X_control_MCMC_raw,
                                                  MCMC_y         = MCMC_y_fitted_train,
                                                  group_variable = "brand")
  
  MCMC_control_company = summarise_fitted_control(MCMC_control   = X_control_MCMC_raw,
                                                  MCMC_y         = MCMC_y_fitted_train,
                                                  group_variable = "company")
  
  
  ### Summary Statistics
  control_fitted_stat1 = dplyr::bind_rows(MCMC_control_company, MCMC_control_brand,  MCMC_control_dma) %>% 
    dplyr::mutate(group = factor(group, 
                                 levels = c("Altice", "Optimum", "Suddenlink",
                                            df_target_train %>% distinct(dma) %>% pull()))) %>% 
    dplyr::group_by(group, variable_id, variable_name) %>%  
    dplyr::summarise_at(vars(contribution_control_abs), key_statistics) %>% 
    dplyr::ungroup()
  
  control_fitted_stat2 = dplyr::bind_rows(MCMC_control_company, MCMC_control_brand,  MCMC_control_dma) %>% 
    dplyr::filter(variable_id == 1) %>% 
    dplyr::mutate(group = factor(group, 
                                 levels = c("Altice", "Optimum", "Suddenlink",
                                            df_target_train %>% distinct(dma) %>% pull()))) %>% 
    dplyr::group_by(group, variable_id, variable_name) %>%  
    dplyr::summarise_at(vars(contribution_total), key_statistics) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(variable_id   = -1,
                  variable_name = "control_contribution_y")
  
  control_fitted_stats = dplyr::bind_rows(control_fitted_stat1, control_fitted_stat2) %>% 
    dplyr::arrange(group, variable_id) %>% 
    dplyr::mutate(variable_id = ifelse(variable_id==-1, NA, variable_id))
  
  
  ### Summary Plots      
  control_fitted_plot_brand = dplyr::bind_rows(MCMC_control_company, MCMC_control_brand) %>% 
    dplyr::mutate(group         = factor(group, levels = c("Altice", "Optimum", "Suddenlink")),
                  variable_name = factor(variable_name, levels = c(var_control))) %>% 
    ggplot(aes(x = contribution_control_abs, y = fct_rev(group), color = group)) +
    tidybayes::stat_pointintervalh(point_interval = mean_qi, .width = c(.66, .95)) +
    tidybayes::stat_halfeyeh(point_interval = mean_qi, .width = c(.99, .8)) +
    facet_wrap(~ variable_name, scales = "fixed", nrow = length(var_control)) +
    ggtitle(paste0(var_target, ": Posterior Distribution of Control Contribution to y")) +    
    theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), legend.position = "none") + 
    xlab("Control Contribution to y") + ylab("Altice and Footprints")
  
  control_fitted_plot_dma = dplyr::bind_rows(MCMC_control_company, MCMC_control_brand, MCMC_control_dma) %>% 
    dplyr::mutate(group         = factor(group,
                                         levels = c("Altice", "Optimum", "Suddenlink", df_target_train %>% distinct(dma) %>% pull())),
                  variable_name = factor(variable_name, levels = c(var_control))) %>% 
    ggplot(aes(x = contribution_control_abs, y = fct_rev(group), color = group)) +
    tidybayes::stat_pointintervalh(point_interval = mean_qi, .width = c(.66, .95)) +
    tidybayes::stat_halfeyeh(point_interval = mean_qi, .width = c(.99, .8)) +
    facet_wrap(~ variable_name, scales = "fixed", ncol = length(var_control)) +
    ggtitle(paste0(var_target, ": Posterior Distribution of Control Contribution to y")) +    
    theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), legend.position = "none") + 
    xlab("Control Contribution to y") + ylab("Altice, Footprints, and DMAs") 
  
  # Create Plots for Each of DMAs  
  dma_control_plots = vector(mode = "list", length = length(dma_ls))
  
  for (i in 1:length(dma_ls)) {
    dma_control_plots[[i]] = MCMC_control_dma %>% 
      dplyr::filter(group == dma_ls[i]) %>% 
      dplyr::mutate(variable_name = factor(variable_name, levels = c(var_control))) %>%     
      ggplot(aes(x = contribution_control_abs, y = variable_name, color = variable_name)) +
      tidybayes::stat_pointintervalh(point_interval = mean_qi, .width = c(.66, .95)) +
      tidybayes::stat_halfeyeh(point_interval = mean_qi, .width = c(.99, .8)) +
      facet_wrap(~ variable_name, scales = "free") + 
      ggtitle(paste0(var_target, " in ",dma_ls[i], ": Posterior Distribution of Control Contribution to y")) +    
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
            legend.position = "none", axis.text.y=element_blank()) + 
      xlab("Posterior Distribution") + ylab("Control Contribution to y")
    
    names(dma_control_plots)[[i]] = make.names(dma_ls[i])
  }
  
  # control Plots across DMAs
  control_contribution_plots = vector(mode = "list", length = length(var_control))
  
  for (i in 1:length(var_control)) {
    control_contribution_plots[[i]] = MCMC_control_dma %>% 
      dplyr::filter(variable_name == var_control[i]) %>% 
      ggplot(aes(x = contribution_control_abs, y = group, color = group)) +
      tidybayes::stat_pointintervalh(point_interval = mean_qi, .width = c(.66, .95)) +
      tidybayes::stat_halfeyeh(point_interval = mean_qi, .width = c(.99, .8)) +
      facet_wrap(~ group, scales = "free") + 
      ggtitle(paste0(var_target, " vs ", var_control[i])) +        
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
            legend.position = "none", axis.text.y=element_blank()) + 
      xlab("Posterior Distribution of Control Contribution to y") + ylab(paste0(var_control[i]))
    
    names(control_contribution_plots)[[i]] = make.names(var_control[i])
  }
  
  
  
  #---------------------------------------------------------------------#
  #  6. Cost per Media (CPM)                                            #
  #---------------------------------------------------------------------#
  ### Total Spending by Media
  df_spend_dma = df_XyZ %>% 
    dplyr::filter(period >= train_start, period <= train_end) %>% 
    dplyr::select(brand, dma, all_of(var_media)) %>% 
    tidyr::gather("variable_name", "spend", -brand, -dma) %>% 
    dplyr::group_by(group=dma, variable_name) %>% 
    dplyr::summarise(spend = sum(spend, na.rm = TRUE))
  
  df_spend_brand = df_XyZ %>% 
    dplyr::filter(period >= train_start, period <= train_end) %>% 
    dplyr::select(brand, dma, all_of(var_media)) %>% 
    tidyr::gather("variable_name", "spend", -brand, -dma) %>% 
    dplyr::group_by(group=brand, variable_name) %>% 
    dplyr::summarise(spend = sum(spend, na.rm = TRUE))
  
  df_spend_company = df_XyZ %>% 
    dplyr::filter(period >= train_start, period <= train_end) %>% 
    dplyr::select(brand, dma, all_of(var_media)) %>% 
    tidyr::gather("variable_name", "spend", -brand, -dma) %>% 
    dplyr::group_by(group="Altice", variable_name) %>% 
    dplyr::summarise(spend = sum(spend, na.rm = TRUE))  
  
  ### Cost per Media
  MCMC_spend_dma     = MCMC_media_dma %>% 
    dplyr::inner_join(df_spend_dma, by = c("group", "variable_name")) %>% 
    dplyr::mutate(CPM = spend/fitted_media)
  
  MCMC_spend_brand   = MCMC_media_brand %>% 
    dplyr::inner_join(df_spend_brand, by = c("group", "variable_name")) %>% 
    dplyr::mutate(CPM = spend/fitted_media)
  
  MCMC_spend_company = MCMC_media_company %>% 
    dplyr::inner_join(df_spend_company, by = c("group", "variable_name")) %>% 
    dplyr::mutate(CPM = spend/fitted_media)
  
  
  ### Summary Statistics
  spend_fitted_stats = dplyr::bind_rows(MCMC_spend_company, MCMC_spend_brand,  MCMC_spend_dma) %>% 
    dplyr::mutate(group = factor(group, 
                                 levels = c("Altice", "Optimum", "Suddenlink",
                                            df_target %>% distinct(dma) %>% pull()))) %>% 
    dplyr::group_by(group, variable_id, variable_name) %>%  
    dplyr::summarise_at(vars(CPM), key_statistics) %>% 
    dplyr::ungroup()
  
  
  ### Summary Plots      
  spend_fitted_plot_brand = dplyr::bind_rows(MCMC_spend_company, MCMC_spend_brand) %>% 
    dplyr::mutate(group         = factor(group, levels = c("Altice", "Optimum", "Suddenlink")),
                  variable_name = factor(variable_name, levels = c(var_media))) %>% 
    ggplot(aes(x = CPM, y = fct_rev(group), color = group)) +
    tidybayes::stat_pointintervalh(point_interval = mean_qi, .width = c(.66, .95)) +
    tidybayes::stat_halfeyeh(point_interval = mean_qi, .width = c(.99, .8)) +
    facet_wrap(~ variable_name, scales = "fixed", nrow = length(var_media)) +
    ggtitle(paste0(var_target, ": Posterior Distribution of Cost per Media")) +    
    theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), legend.position = "none") + 
    xlab("Cost per Media") + ylab("Altice and Footprints")
  
  spend_fitted_plot_dma = dplyr::bind_rows(MCMC_spend_company, MCMC_spend_brand, MCMC_spend_dma) %>% 
    dplyr::mutate(group         = factor(group,
                                         levels = c("Altice", "Optimum", "Suddenlink", df_target %>% distinct(dma) %>% pull())),
                  variable_name = factor(variable_name, levels = c(var_media))) %>% 
    ggplot(aes(x = CPM, y = fct_rev(group), color = group)) +
    tidybayes::stat_pointintervalh(point_interval = mean_qi, .width = c(.66, .95)) +
    tidybayes::stat_halfeyeh(point_interval = mean_qi, .width = c(.99, .8)) +
    facet_wrap(~ variable_name, scales = "fixed", ncol = length(var_media)) +
    ggtitle(paste0(var_target, ": Posterior Distribution of Cost per Media")) +    
    theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), legend.position = "none") + 
    xlab("Cost per Media") + ylab("Altice, Footprints, and DMAs") 
  
  # Create Plots for Each of DMAs  
  dma_spend_plots = vector(mode = "list", length = length(dma_ls))
  
  for (i in 1:length(dma_ls)) {
    dma_spend_plots[[i]] = MCMC_spend_dma %>% 
      dplyr::filter(group == dma_ls[i]) %>% 
      dplyr::mutate(variable_name = factor(variable_name, levels = c(var_media))) %>%       
      ggplot(aes(x = CPM, y = variable_name, color = variable_name)) +
      tidybayes::stat_pointintervalh(point_interval = mean_qi, .width = c(.66, .95)) +
      tidybayes::stat_halfeyeh(point_interval = mean_qi, .width = c(.99, .8)) +
      facet_wrap(~ variable_name, scales = "free") + 
      ggtitle(paste0(var_target, " in ",dma_ls[i], ": Posterior Distribution of Cost per Media")) +    
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
            legend.position = "none", axis.text.y=element_blank()) + 
      xlab("Posterior Distribution") + ylab("Cost per Media")
    
    names(dma_spend_plots)[[i]] = make.names(dma_ls[i])
  }
  
  # Media Plots across DMAs
  spend_contribution_plots = vector(mode = "list", length = length(var_media))
  
  for (i in 1:length(var_media)) {
    spend_contribution_plots[[i]] = MCMC_spend_dma %>% 
      dplyr::filter(variable_name == var_media[i]) %>% 
      ggplot(aes(x = CPM, y = group, color = group)) +
      tidybayes::stat_pointintervalh(point_interval = mean_qi, .width = c(.66, .95)) +
      tidybayes::stat_halfeyeh(point_interval = mean_qi, .width = c(.99, .8)) +
      facet_wrap(~ group, scales = "free") + 
      ggtitle(paste0(var_target, " vs ", var_media[i])) +        
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
            legend.position = "none", axis.text.y=element_blank()) + 
      xlab("Posterior Distribution of Cost per Media") + ylab(paste0(var_media[i]))
    
    names(spend_contribution_plots)[[i]] = make.names(var_media[i])
  }
  
  
  
  #---------------------------------------------------------------------#
  #  7. Spend-Contribution-Cost per Media                               #
  #---------------------------------------------------------------------#
  ### Total Spending Percent by Media
  # Compute Spend Share by Media
  df_spend_dma_tot = df_spend_dma %>% 
    dplyr::group_by(group) %>% 
    dplyr::summarise(spend_tot = sum(spend, na.rm = TRUE))
  
  df_spend_brand_tot = df_spend_brand %>% 
    dplyr::group_by(group) %>% 
    dplyr::summarise(spend_tot = sum(spend, na.rm = TRUE))
  
  df_spend_company_tot = df_spend_company %>% 
    dplyr::group_by(group) %>% 
    dplyr::summarise(spend_tot = sum(spend, na.rm = TRUE))
  
  # Spend %
  df_spend_dma_pct = df_spend_dma %>% 
    dplyr::inner_join(df_spend_dma_tot, by = "group") %>% 
    dplyr::mutate(spend_pct  = spend/spend_tot,
                  date_start = train_start,
                  date_end   = train_end) %>% 
    dplyr::select(group, media=variable_name, date_start, date_end, everything())
  
  df_spend_brand_pct = df_spend_brand %>% 
    dplyr::inner_join(df_spend_brand_tot, by = "group") %>% 
    dplyr::mutate(spend_pct  = spend/spend_tot,
                  date_start = train_start,
                  date_end   = train_end) %>% 
    dplyr::select(group, media=variable_name, date_start, date_end, everything())
  
  df_spend_company_pct = df_spend_company %>% 
    dplyr::inner_join(df_spend_company_tot, by = "group") %>% 
    dplyr::mutate(spend_pct  = spend/spend_tot,
                  date_start = train_start,
                  date_end   = train_end) %>% 
    dplyr::select(group, media=variable_name, date_start, date_end, everything())
  
  
  df_spend_pct = dplyr::bind_rows(df_spend_company_pct, df_spend_brand_pct, df_spend_dma_pct) %>% 
    dplyr::mutate(spend_pct = spend_pct * 100,
                  group     = factor(group, levels = c("Altice", "Optimum", "Suddenlink", tb_dma$dma)),
                  media     = factor(media, levels = var_media)) %>% 
    dplyr::arrange(group, media)
  
  
  ### Summary Statistics
  tmp_cont = media_fitted_stats %>% 
    dplyr::filter(variable_name != "media_contribution_y") %>% 
    dplyr::select(group, media=variable_name, cont_mean=Mean, cont_median=Median) %>% 
    dplyr::mutate(media = factor(media, levels = var_media))
  
  tmp_cost = spend_fitted_stats %>% 
    dplyr::select(group, media=variable_name, cost_mean=Mean, cost_median=Median) %>% 
    dplyr::mutate(media = factor(media, levels = var_media))
  
  spend_contribution_cost_stats = df_spend_pct %>% 
    dplyr::inner_join(tmp_cont, by = c("group", "media")) %>% 
    dplyr::inner_join(tmp_cost, by = c("group", "media"))
  
  
  ### Summary Plots  
  SCC_ls = levels(spend_contribution_cost_stats$group)
  
  # Media Spend - Contribution - CPGA (in Mean) 
  SCC_plots_mean = vector(mode = "list", length = length(SCC_ls))
  
  for (i in 1:length(SCC_ls)) {
    df_tmp = spend_contribution_cost_stats %>% 
      dplyr::filter(group == SCC_ls[i]) %>% 
      dplyr::select(group, media, spend_pct, contribution=cont_mean, cost=cost_mean) %>% 
      tidyr::gather("metric", "value", -group, -media) %>% 
      dplyr::mutate(value  = ifelse(metric == "cost", round(value, 0), round(value, 2)),
                    metric = factor(metric, 
                                    levels = c("spend_pct", "contribution", "cost"),
                                    labels = c("Media Spend", "Media Contribution", "CPGA ($)")))
    
    SCC_plots_mean[[i]] = df_tmp %>% 
      ggplot(aes(x=media, y=value, fill=metric)) + 
      geom_bar(stat="identity") +
      facet_wrap(~metric, scales=c("free_x")) + coord_flip() + 
      geom_text(aes(label=value), vjust=-0.3, size=3.5) + 
      ggtitle(paste0(SCC_ls[i], ": Media Spend - Contribution - CPGA (in Mean)")) +    
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
            legend.position = "none", axis.text.x=element_blank(),
            axis.ticks.x=element_blank()) + xlab("") + ylab("")
    
    names(SCC_plots_mean)[[i]] = make.names(SCC_ls[i])
  }
  
  # Media Spend - Contribution - CPGA (in Median) 
  SCC_plots_median = vector(mode = "list", length = length(SCC_ls))
  
  for (i in 1:length(SCC_ls)) {
    df_tmp = spend_contribution_cost_stats %>% 
      dplyr::filter(group == SCC_ls[i]) %>% 
      dplyr::select(group, media, spend_pct, contribution=cont_median, cost=cost_median) %>% 
      tidyr::gather("metric", "value", -group, -media) %>% 
      dplyr::mutate(value  = ifelse(metric == "cost", round(value, 0), round(value, 2)),
                    metric = factor(metric, 
                                    levels = c("spend_pct", "contribution", "cost"),
                                    labels = c("Media Spend", "Media Contribution", "CPGA ($)")))
    
    SCC_plots_median[[i]] = df_tmp %>% 
      ggplot(aes(x=media, y=value, fill=metric)) + 
      geom_bar(stat="identity") +
      facet_wrap(~metric, scales=c("free_x")) + coord_flip() + 
      geom_text(aes(label=value), vjust=-0.3, size=3.5) + 
      ggtitle(paste0(SCC_ls[i], ": Media Spend - Contribution - CPGA (in Median)")) +    
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
            legend.position = "none", axis.text.x=element_blank(),
            axis.ticks.x=element_blank()) + xlab("") + ylab("")
    
    names(SCC_plots_median)[[i]] = make.names(SCC_ls[i])
  }
  
  
  
  #---------------------------------------------------------------------#
  #  Model Evaluation: Final Outcome                                    #
  #---------------------------------------------------------------------#
  ### Outcome Lists
  if (pooling_type_media == "partial" & pooling_type_control == "partial") {
    pop_para_stat = list(alpha_K_S = pop_para_stat_alpha_K_S,
                         media     = pop_para_stat_media %>% dplyr::select(-metric),
                         control   = pop_para_stat_control %>% dplyr::select(-metric),
                         std       = pop_para_stat_std %>% dplyr::select(-metric),
                         all       = pop_para_stat_all)
  } else if(pooling_type_media == "partial" & pooling_type_control != "partial"){
    pop_para_stat = list(alpha_K_S = pop_para_stat_alpha_K_S,
                         media     = pop_para_stat_media %>% dplyr::select(-metric),
                         std       = pop_para_stat_std %>% dplyr::select(-metric),
                         all       = pop_para_stat_all)
  }
  else if(pooling_type_media != "partial" & pooling_type_control == "partial"){
    pop_para_stat = list(alpha_K_S = pop_para_stat_alpha_K_S,
                         control   = pop_para_stat_control %>% dplyr::select(-metric),
                         std       = pop_para_stat_std %>% dplyr::select(-metric),
                         all       = pop_para_stat_all)
  }else {
    pop_para_stat = list(alpha_K_S = pop_para_stat_alpha_K_S,
                         std       = pop_para_stat_std %>% dplyr::select(-metric),
                         all       = pop_para_stat_all)
  }
  
  if (pooling_type_media == "partial" & pooling_type_control == "partial") {
    pop_para_plot = list(alpha_K_S = pop_para_plot_alpha_K_S,
                         media     = pop_para_plot_media,
                         control   = pop_para_plot_control,
                         std       = pop_para_plot_std)
  }else if(pooling_type_media == "partial" & pooling_type_control != "partial"){
    pop_para_plot = list(alpha_K_S = pop_para_plot_alpha_K_S,
                         media     = pop_para_plot_media,
                         std       = pop_para_plot_std)
  }else if(pooling_type_media != "partial" & pooling_type_control == "partial"){
    pop_para_plot = list(alpha_K_S = pop_para_plot_alpha_K_S,
                         control   = pop_para_plot_control,
                         std       = pop_para_plot_std)
  }else {
    pop_para_plot = list(alpha_K_S = pop_para_plot_alpha_K_S,
                         std       = pop_para_plot_std)
  }
  
  geo_para_stat = list(media     = geo_para_stat_media %>% dplyr::select(-type),
                       control   = geo_para_stat_control %>% dplyr::select(-type),
                       all       = geo_para_stat_all)
  
  geo_para_plot = list(media     = media_plots,
                       control   = control_plots,
                       dma       = dma_plots)
  
  y_fitted_stat = list(all       = y_fitted_stats,
                       ts        = y_fitted_ts)
  
  y_fitted_plot = list(r2        = y_fitted_plot_r2,
                       mape      = y_fitted_plot_mape,
                       ts_geo    = y_fitted_plot_ts_geo,
                       ts_brand  = y_fitted_plot_ts_brand)
  
  media_C_stat = media_fitted_stats
  media_C_plot = list(by_brand   = media_fitted_plot_brand,
                      by_dma     = media_fitted_plot_dma,
                      dma        = dma_media_plots,
                      media      = media_contribution_plots)
  
  control_C_stat = control_fitted_stats
  control_C_plot = list(by_brand = control_fitted_plot_brand,
                        by_dma   = control_fitted_plot_dma,
                        dma      = dma_control_plots,
                        control  = control_contribution_plots)
  
  cost_stat      = spend_fitted_stats
  cost_plot      = list(by_brand = spend_fitted_plot_brand,
                        by_dma   = spend_fitted_plot_dma,
                        dma      = dma_spend_plots,
                        media    = spend_contribution_plots)
  
  media_SCC_stat = spend_contribution_cost_stats
  media_SCC_plot = list(mean     = SCC_plots_mean,
                        median   = SCC_plots_median)
  
  
  evaluation_out = list(
    pop_para_stat  = pop_para_stat,  # Population-Level Parameters
    pop_para_plot  = pop_para_plot, 
    geo_para_stat  = geo_para_stat,  # Geo-Level Parameters
    geo_para_plot  = geo_para_plot, 
    y_fitted_stat  = y_fitted_stat,  # Fitted y
    y_fitted_plot  = y_fitted_plot,
    media_C_stat   = media_C_stat,   # Media Contribution to y
    media_C_plot   = media_C_plot,
    control_C_stat = control_C_stat, # Control Contribution to y
    control_C_plot = control_C_plot,
    cost_stat      = cost_stat,      # Cost per Media
    cost_plot      = cost_plot,
    media_SCC_stat = media_SCC_stat, # Media Spend - Contribution - Cost
    media_SCC_plot = media_SCC_plot)
  
}





library(ggdendro)
# library(dendextend)
suppressPackageStartupMessages(library(dendextend))
plot_varcluster <- function(df=NULL, cluster_method="average") {
  # A utility function to group variables based on Pearson correlation
  # 
  # Note:
  # 1. df is assumed to include numerical features only.
  #   - All the numerical features are pre-standardized.
  #   - Correlation is used as the input to `hclust`.
  # 2. `hclust` can choose one of the below methods as `cluster_method`:
  #   - c(ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")
  # 3. Only `dendrogram` is returned.
  #   - Do not use `plot`, which cannot return an object.
  # 4. See the below link for more information.
  #   - https://cran.r-project.org/web/packages/dendextend/vignettes/dendextend.html
  
  plot_varcluster = df %>% 
    cor(use = "pairwise", method = "pearson") %>% 
    dist() %>% 
    hclust(method = cluster_method) %>% 
    as.dendrogram() %>%   
    set("branches_k_color", k=3) %>%   # See `dendextend`: pch=point type; cex=point size; col=point color.
    set("branches_lwd", 1.2) %>%
    set("labels_colors", "black") %>% 
    set("labels_cex", c(1, 1)) %>% 
    set("leaves_pch", 19) %>% 
    set("leaves_col", c("blue", "red")) %>% 
    as.ggdend() %>% 
    ggplot(horiz = TRUE, theme = NULL) +
    ggtitle("Variable Clustering Based on Pearson Correlation")+
    theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
  
  plot_varcluster
  
}





summarise_fitted_y = function(MCMC_draw=NULL, group_variable=NULL) {
  # A utility function to summarize MCMC draws of `y_fitted`
  # 
  # 1. `MCMC_draw` includes the MCMC draws of `y_fitted`.
  # 2. `group_variable` will be one of c("company", "brand", "dma")
  
  ### Raw MCMC Draws
  # https://stackoverflow.com/questions/29678435/how-to-pass-dynamic-column-names-in-dplyr-into-custom-function
  MCMC_fit_raw = MCMC_draw %>% 
    dplyr::group_by(.draw, group=!!as.name(group_variable), time) %>% 
    dplyr::summarise(.value_true     = sum(.value_true),
                     y_true          = sum(y_true)) %>% 
    dplyr::mutate(error_true         = .value_true - y_true,
                  error_abs_true     = abs(error_true),
                  error_abs_pct_true = 100 * (error_abs_true/y_true),
                  error_squared_true = error_true * error_true) %>% 
    dplyr::ungroup()
  
  ### MCMC Fit Metrics
  MCMC_fit_metrics = MCMC_fit_raw %>% 
    dplyr::group_by(.draw, group) %>% 
    dplyr::summarise(r2_true         = cor(y_true, .value_true)^2,
                     mae_true        = mean(error_abs_true),
                     mape_true       = mean(error_abs_pct_true), 
                     rmse_true       = sqrt(mean(error_squared_true))) %>% 
    dplyr::select(group, everything()) %>% 
    dplyr::ungroup()
  
  ### Summary Statistics of MCMC Fit Metrics
  key_statistics = tibble::lst(N, Mean, Median, StdDev, Min, Max, Range, 
                               q001, q0025, q005, q01, q015, q025, q075, q085, q09, q095, q0975, q099)
  
  MCMC_fit_metrics_stat = MCMC_fit_metrics %>% 
    dplyr::group_by(group) %>%  
    dplyr::summarise_at(vars(r2_true:rmse_true), key_statistics) %>% 
    tidyr::gather("metric", "value", -group) %>% 
    tidyr::separate(metric, c("metric", "true", "stat")) %>% 
    dplyr::select(-true) %>% 
    tidyr::spread(stat, value) %>% 
    dplyr::select(group, metric, N, Mean, Median, StdDev, Min, Max, Range, everything()) %>% 
    dplyr::ungroup()
  
  ### MCMC Fitted Time Series
  MCMC_fit_ts = MCMC_fit_raw %>% 
    dplyr::group_by(group, time, y_true) %>% 
    dplyr::summarise_at(vars(.value_true), key_statistics) %>% 
    dplyr::select(group, everything()) %>% 
    dplyr::ungroup()
  
  ### Final Outcomes for further Analyses
  summary_out = list(
    MCMC_fit_metrics      = MCMC_fit_metrics, 
    MCMC_fit_metrics_stat = MCMC_fit_metrics_stat, 
    MCMC_fit_ts           = MCMC_fit_ts
  )
  
}





summarise_fitted_media = function(MCMC_media=NULL, MCMC_y=NULL, group_variable=NULL) {
  # A utility function to summarize MCMC draws of `X_Hill` and `beta_media`
  # 
  # 1. `MCMC_media` includes the MCMC draws of `X_Hill` and `beta_media`.
  # 2. `MCMC_draw` includes the MCMC draws of `y_fitted`.
  # 3. `group_variable` will be one of c("company", "brand", "dma")
  
  ### Raw MCMC Draws
  MCMC_y_sum      = MCMC_y %>% 
    dplyr::group_by(group=!!as.name(group_variable), .draw) %>% 
    dplyr::summarise(fitted_y_sum = sum(.value_true, rm.na = TRUE)) %>% 
    dplyr::ungroup()
  
  MCMC_media_sum1 = MCMC_media %>% 
    dplyr::group_by(group=!!as.name(group_variable), variable_id, variable_name, .draw) %>% 
    dplyr::summarise(fitted_media = sum(fitted, rm.na = TRUE)) %>% 
    dplyr::inner_join(MCMC_y_sum, by = c("group", ".draw")) %>%
    dplyr::ungroup()
  
  MCMC_media_sum2 = MCMC_media_sum1 %>% 
    dplyr::group_by(group, .draw) %>% 
    dplyr::summarise(fitted_media_sum = sum(fitted_media)) %>% 
    dplyr::inner_join(MCMC_y_sum, by  = c("group", ".draw")) %>%
    dplyr::mutate(contribution_total  = 100 * (fitted_media_sum/fitted_y_sum)) %>%      
    dplyr::ungroup() %>% 
    dplyr::select(-fitted_y_sum)
  
  MCMC_media_sum  = MCMC_media_sum1 %>% 
    dplyr::inner_join(MCMC_media_sum2, by = c("group", ".draw")) %>%
    dplyr::mutate(contribution_media  = 100 * (fitted_media/fitted_media_sum)) %>%
    dplyr::ungroup() %>% 
    dplyr::arrange(group, .draw)
  
  MCMC_media_sum  
}





summarise_fitted_control = function(MCMC_control=NULL, MCMC_y=NULL, group_variable=NULL) {
  # A utility function to summarize MCMC draws of `X_control` and `beta_control`
  # 
  # 1. `MCMC_control` includes the MCMC draws of `X_control` and `beta_control`.
  # 2. `MCMC_draw` includes the MCMC draws of `y_fitted`.
  # 3. `group_variable` will be one of c("company", "brand", "dma")
  
  ### Raw MCMC Draws
  MCMC_y_sum      = MCMC_y %>% 
    dplyr::group_by(group=!!as.name(group_variable), .draw) %>% 
    dplyr::summarise(fitted_y_sum = sum(.value_true, rm.na = TRUE)) %>% 
    dplyr::ungroup()
  
  MCMC_control_sum1 = MCMC_control %>% 
    dplyr::group_by(group=!!as.name(group_variable), variable_id, variable_name, .draw) %>% 
    dplyr::summarise(fitted_control     = sum(fitted, rm.na = TRUE),
                     fitted_control_abs = sum(abs(fitted), rm.na = TRUE)) %>% 
    dplyr::inner_join(MCMC_y_sum, by = c("group", ".draw")) %>%
    dplyr::ungroup()
  
  MCMC_control_sum2 = MCMC_control_sum1 %>% 
    dplyr::group_by(group, .draw) %>% 
    dplyr::summarise(fitted_control_sum     = sum(fitted_control),
                     fitted_control_abs_sum = sum(fitted_control_abs)) %>% 
    dplyr::inner_join(MCMC_y_sum, by  = c("group", ".draw")) %>%
    dplyr::mutate(contribution_total  = 100 * (fitted_control_sum/fitted_y_sum)) %>%      
    dplyr::ungroup() %>% 
    dplyr::select(-fitted_y_sum)
  
  MCMC_control_sum  = MCMC_control_sum1 %>% 
    dplyr::inner_join(MCMC_control_sum2, by = c("group", ".draw")) %>%
    dplyr::mutate(contribution_control     = 100 * (fitted_control/fitted_control_sum),
                  contribution_control_abs = 100 * (fitted_control_abs/fitted_control_abs_sum)) %>%
    dplyr::ungroup() %>% 
    dplyr::arrange(group, .draw)
  
  MCMC_control_sum  
}





print_media_names = function(pp_MMM_ls=NULL,var_media = NULL) {
  # A utility function to print the names of selected media features
  
  if (is.null(pp_MMM_ls)) stop("\n'pp_MMM_ls' should be provided!\n")
  all_media_var = pp_MMM_ls$extra$var_media
  cat('\nNote: Selected Media Mix Variables for Modeling\n')
  if(is.null(var_media)){
      var_media = all_media_var
  }
  print(var_media)
}





generate_MCMC_fitted_y = function(stanfit=NULL, standata=NULL, targetdata=NULL) {
  # A utility function to generate MCMC draws of `y_fitted` for any given data
  # 
  # 1. `stanfit` includes the MCMC draws of a given model.
  # 2. `standata` provides a data consistent with `stanfit`.
  # 3. `targetdata` provides the key information on the target.
  
  
  #---------------------------------------------------------------------#
  #  Check INPUTS                                                       #
  #---------------------------------------------------------------------#
  ## Check STAN fit.
  if(class(stanfit)!="stanfit" ) {
    stop("STAN model fit should be provided, which should include the posterior draws of the model parameters.")
  }
  
  # Note: the below parameter names be consistent with the parameter names from a fitted STAN model.
  model_paras = c("alpha", "S", "K", "sigma_y", "beta_base", "beta_media", "beta_control")
  draws_paras = rstan::extract(stanfit, par=model_paras, permuted = TRUE)
  
  X_media   = standata$X_media
  X_control = standata$X_control
  id_geo    = standata$id_geo
  num_lag   = standata$num_lag
  
  
  
  #---------------------------------------------------------------------#
  #  Posterior Prediction                                               #
  #---------------------------------------------------------------------#
  # Preliminary Conditions
  N_obs       = dim(X_media)[1]    
  N_draws     = dim(draws_paras$beta_media)[1]
  N_media     = dim(draws_paras$beta_media)[3]
  
  # An empty matrix to contains N_draws prediction
  y_MCMC           = matrix(NA, nrow = N_draws*N_obs, ncol = 3)
  colnames(y_MCMC) = c('obs_id', '.draw', '.value')
  
  
  # Posterior Prediction
  for (i_obs in 1:N_obs){# Loop over the data
    # Note that beta_media and beta_control are estimated by DMA
    i_dma              = id_geo[i_obs]
    
    for (i_draws in 1:N_draws){# Loop over the draws
      
      # (0) Required Inputes to the Below Computations
      media_weight        = matrix(NA, nrow = N_media, ncol = num_lag)
      alpha_vector        = draws_paras$alpha[i_draws, ]
      K_vector            = draws_paras$K[i_draws, ]
      S_vector            = draws_paras$S[i_draws, ]
      beta_base           = draws_paras$beta_base[i_draws, i_dma]
      beta_media_vector   = draws_paras$beta_media[i_draws, i_dma, ]
      beta_control_vector = draws_paras$beta_control[i_draws, i_dma, ]
      y_sd                = draws_paras$sigma_y[i_draws]
      
      # (1) Compute Carryover Effect.
      media_adstock = c()
      for (i in 1:length(alpha_vector)){
        for (j in 1:num_lag){
          media_weight[i, j] = alpha_vector[i]^(j-1)
        }
      }
      for (i in 1:N_media){
        media_adstock[i] = (X_media[i_obs, i, ]%*%media_weight[i, ])/sum(media_weight[i, ])
      }
      
      # (2) Compute Shape Effect.
      media_Hill = c()
      for (i in 1:N_media){
        media_Hill[i] = 1/(1+(media_adstock[i]/K_vector[i])^(-S_vector[i]))
      }
      
      # (3) Compute Posterior Mean and Prediction per Each Draw.
      y_mu               = beta_base + (media_Hill%*%beta_media_vector) + (X_control[i_obs, ]%*%beta_control_vector)
      y_index            = (i_obs-1)*N_draws + i_draws
      y_MCMC[y_index, 1] = i_obs
      y_MCMC[y_index, 2] = i_draws
      y_MCMC[y_index, 3] = y_mu
    }
  }
  
  MCMC_y_fitted    = y_MCMC %>% 
    dplyr::as_tibble() %>% 
    dplyr::inner_join(targetdata, by = "obs_id") %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(.value_true = .value * y_max * sub)
  
  MCMC_y_fitted
}





generate_inits_PP <- function(standata=NULL){
  # A utility function to generate initial values for Parital Pooling
  # 
  # Note:
  # `standata` is the data generated from `make_stan_data_MMM`.
  
  
  #---------------------------------------------------------------------#
  #  Check INPUTS                                                       #
  #---------------------------------------------------------------------#
  ## Check input data.    
  if (is.null(standata)) stop("\n'standata' should be provided!\n")
  
  
  #---------------------------------------------------------------------#
  #  Dimensions                                                         #
  #---------------------------------------------------------------------#
  stan_ls     = standata$stan_data$train 
  num_obs     = stan_ls$num_obs           # of observations
  num_geo     = stan_ls$num_geo           # of geos
  num_media   = stan_ls$num_media         # of media variables
  num_control = stan_ls$num_control       # of control variables
  num_lag     = stan_ls$num_lag           # of max lags + current value  
  num_grp_var = stan_ls$num_grp_var       # of geo-level covariates + intercept
  media_lb    = stan_ls$prior_location[1] # Lower bound of media impact
  
  
  #---------------------------------------------------------------------#
  #  Initial Values                                                     #
  #---------------------------------------------------------------------#
  ### Initial values for partial pooling
  list(beta_base_raw    = rnorm(num_geo, 0, 1),
       beta_media_raw   = matrix(rnorm(num_geo*num_media, 0, 1), nrow = num_geo, ncol = num_media), 
       beta_control_raw = matrix(rnorm(num_geo*num_control, 0, 1), nrow = num_geo, ncol = num_control), 
       gamma_base       = exp(rnorm(num_grp_var, log(0.5), 0.5)), 
       gamma_media      = matrix(rnorm_bound(num_grp_var*num_media, log(0.3), 1, UB=0.3), nrow = num_grp_var, ncol = num_media), 
       gamma_control    = matrix(rnorm(num_grp_var*num_control, 0.5, 0.5), nrow = num_grp_var, ncol = num_control), 
       K                = runif(num_media, min = 0.3, max = 0.7),                
       S                = runif(num_media, min = 2.0, max = 3.0),
       alpha            = runif(num_media, min = 0.3, max = 0.7),
       sigma_y          = exp(rnorm(1, log(0.5), 0.5)),
       sigma_base       = exp(rnorm(1, log(1.0), 0.5)),
       sigma_media      = exp(rnorm(1, log(1.0), 1)),
       sigma_control    = exp(rnorm(1, log(1.0), 0.5)),
       beta_base        = exp(rnorm(num_geo, log(0.5), 0.25)),
       beta_media       = matrix(exp(rnorm(num_geo*num_media, log(0.3), 0.5)), nrow = num_geo, ncol = num_media), 
       beta_control     = matrix(rep(0, num_geo*num_control), nrow = num_geo, ncol = num_control),
       mu_y             = runif(num_obs, min = 0.2, max = 0.8),
       weights          = seq(1, 0, length.out = num_lag),
       X_Adstock        = runif(1, min = 0.2, max = 0.8), 
       X_Hill           = matrix(rbeta(num_obs*num_media, 2, 5), nrow = num_obs, ncol = num_media))
}





generate_inits_NP <- function(standata=NULL){
  # A utility function to generate initial values for Parital Pooling
  # 
  # Note:
  # `standata` is the data generated from `make_stan_data_MMM`.
  
  
  #---------------------------------------------------------------------#
  #  Check INPUTS                                                       #
  #---------------------------------------------------------------------#
  ## Check input data.    
  if (is.null(standata)) stop("\n'standata' should be provided!\n")
  
  
  #---------------------------------------------------------------------#
  #  Dimensions                                                         #
  #---------------------------------------------------------------------#
  stan_ls     = standata$stan_data$train 
  num_obs     = stan_ls$num_obs           # of observations
  num_geo     = stan_ls$num_geo           # of geos
  num_media   = stan_ls$num_media         # of media variables
  num_control = stan_ls$num_control       # of control variables
  num_lag     = stan_ls$num_lag           # of max lags + current value  
  num_grp_var = stan_ls$num_grp_var       # of geo-level covariates + intercept
  media_lb    = stan_ls$prior_location[1] # Lower bound of media impact
  
  
  #---------------------------------------------------------------------#
  #  Initial Values                                                     #
  #---------------------------------------------------------------------#
  ### Initial values for no pooling
  list(beta_base        = exp(rnorm(num_geo, log(0.5), 0.25)),
       beta_media_raw   = matrix(exp(rnorm(num_geo*num_media, log(0.1), 0.5)), nrow = num_geo, ncol = num_media), 
       beta_media       = matrix(exp(rnorm(num_geo*num_media, log(0.3), 0.5)), nrow = num_geo, ncol = num_media), 
       beta_control     = matrix(rep(0, num_geo*num_control), nrow = num_geo, ncol = num_control),
       K                = runif(num_media, min = 0.3, max = 0.7),                
       S                = runif(num_media, min = 2.0, max = 3.0),
       alpha            = runif(num_media, min = 0.3, max = 0.7),
       sigma_y          = exp(rnorm(1, log(0.5), 0.5)),
       mu_y             = runif(num_obs, min = 0.2, max = 0.8),
       weights          = seq(1, 0, length.out = num_lag),
       X_Adstock        = runif(1, min = 0.2, max = 0.8), 
       X_Hill           = matrix(rbeta(num_obs*num_media, 2, 5), nrow = num_obs, ncol = num_media))
}





rnorm_bound = function(n, mean = 0, sd = 1, LB=NULL, UB=NULL){
  # A custom function to generate random normal values with lower/upper bounds
  
  # Use `rnorm` as the base
  rvalues = rnorm(n, mean = mean, sd = sd)
  
  # Adjust Lower Bound
  LB      = ifelse(is.null(LB), -Inf, LB)
  rvalues = ifelse(rvalues <= LB, LB, rvalues)
  
  #Adjust Upper Bound
  UB      = ifelse(is.null(UB), Inf, UB)  
  rvalues = ifelse(rvalues >= UB, UB, rvalues)
  
  rvalues
}





figure <- function(width, heigth){
  options(repr.plot.width = width, repr.plot.height = heigth)
}





## Functions for Summary Statistics
N      <- function(x) {x      <- x[which(!is.na(x))]; N <- length(x)}
Mean   <- function(x) {Mean   <- mean(x, na.rm = TRUE)}
Median <- function(x) {Median <- median(x, na.rm = TRUE)}
StdDev <- function(x) {StdDev <- sd(x, na.rm = TRUE)}
Sum    <- function(x) {Sum    <- sum(x, na.rm = TRUE)}
Min    <- function(x) {Min    <- min(x, na.rm = TRUE)}
Max    <- function(x) {Max    <- max(x, na.rm = TRUE)}
Range  <- function(x) {Range  <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)}
q001   <- function(x) {q001   <- quantile(x, 0.01, na.rm = TRUE)}
q0025  <- function(x) {q0025  <- quantile(x, 0.025, na.rm = TRUE)}
q005   <- function(x) {q005   <- quantile(x, 0.05, na.rm = TRUE)}
q01    <- function(x) {q01    <- quantile(x, 0.1, na.rm = TRUE)}
q015   <- function(x) {q015   <- quantile(x, 0.15, na.rm = TRUE)}
q025   <- function(x) {q025   <- quantile(x, 0.25, na.rm = TRUE)}
q075   <- function(x) {q075   <- quantile(x, 0.75, na.rm = TRUE)}
q085   <- function(x) {q085   <- quantile(x, 0.85, na.rm = TRUE)}
q09    <- function(x) {q09    <- quantile(x, 0.9, na.rm = TRUE)}
q095   <- function(x) {q095   <- quantile(x, 0.95, na.rm = TRUE)}
q0975  <- function(x) {q0975  <- quantile(x, 0.975, na.rm = TRUE)}
q099   <- function(x) {q099   <- quantile(x, 0.99, na.rm = TRUE)}

q020   <- function(x) {q020   <- quantile(x, 0.20, na.rm = TRUE)}
q030   <- function(x) {q030   <- quantile(x, 0.30, na.rm = TRUE)}
q035   <- function(x) {q035   <- quantile(x, 0.35, na.rm = TRUE)}
q040   <- function(x) {q040   <- quantile(x, 0.40, na.rm = TRUE)}
q045   <- function(x) {q045   <- quantile(x, 0.45, na.rm = TRUE)}

q055   <- function(x) {q055   <- quantile(x, 0.55, na.rm = TRUE)}
q060   <- function(x) {q060   <- quantile(x, 0.60, na.rm = TRUE)}
q065   <- function(x) {q065   <- quantile(x, 0.65, na.rm = TRUE)}
q070   <- function(x) {q070   <- quantile(x, 0.70, na.rm = TRUE)}
q080   <- function(x) {q080   <- quantile(x, 0.80, na.rm = TRUE)}


## Functions for Formatting Numbers
f.dollar  <- function(x, ...) {paste0("$", formatC(as.numeric(x), format="f", digits=0, big.mark=","))}
f.integer <- function(x, ...) {paste0(formatC(as.numeric(x), format="f", digits=0, big.mark=","))}
f.digit1  <- function(x, ...) {paste0(formatC(as.numeric(x), format="f", digits=1, big.mark=","))}
f.digit2  <- function(x, ...) {paste0(formatC(as.numeric(x), format="f", digits=2, big.mark=","))}
f.digit3  <- function(x, ...) {paste0(formatC(as.numeric(x), format="f", digits=3, big.mark=","))}
f.digit4  <- function(x, ...) {paste0(formatC(as.numeric(x), format="f", digits=4, big.mark=","))}
f.digit5  <- function(x, ...) {paste0(formatC(as.numeric(x), format="f", digits=5, big.mark=","))}