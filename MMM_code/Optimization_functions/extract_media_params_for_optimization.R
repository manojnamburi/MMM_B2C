library(tidyverse)
#####################################################
#   Function to prepare Optimizer data
#####################################################
make_optim_data_mmm <- function(
                           data_XyZ,
                           list_pop_param_df,
                           list_geo_param_df,
                           target_names_vec,
                           list_preprocessed_data,
                           target_norm_cols,
                           media_norm_type = 4,
                           target_norm_type = 4,
                           statistic = 'Mean',
                           pooling_type = 'No',
                           var_infos = T){
  
  ##From the preprocessed data extract the relevant dataframes
  media_norm        = paste0('df_media_norm',media_norm_type)
  
  all_target_max_df = list_preprocessed_data$target$y_minmax_long
  norm_control_df   = list_preprocessed_data$control$Z_per_capita
  prospect_df       = list_preprocessed_data$extra$df_norm
  media_min_max_df  = list_preprocessed_data$media$X_minmax_long
  
    
    
  
  output_list       = list()
  
  idx = 1
  ## Extract target specific elements needed for optimization from preprocessing and the model result objects
  for(target_name in target_names_vec)
  {
    temp_list             = list()
    ##The column used to per capita normalize the target
    target_norm_col       = target_norm_cols[idx]
    pop_model_results_df  = list_pop_param_df[[target_name]][[1]]
    
    ###If the pooling is complete pooling, the geo level attributes have to be obtained from the population parameters
    if(pooling_type == 'Complete'){
      common_params_df = pop_model_results_df %>%
                         dplyr::filter(grepl("variable",type))
      
      dma_df = data.frame('dma' = rep(geo_names_vec,each = dim(common_params_df)[1]))
      
      geo_model_results_df = cbind(dma_df,common_params_df)
    }else{
      geo_model_results_df  = list_geo_param_df[[target_name]][[1]]
    }
    ## Extract the channel names,DMA names and control names used in the model from the beta parameters at geo level
    media_names_vec   =  geo_model_results_df %>% 
                          dplyr::filter(type == 'media variable') %>% 
                          dplyr::select(variable) %>% 
                          dplyr::distinct() %>% 
                          dplyr::pull()
    
    geo_names_vec     = geo_model_results_df %>% 
                          dplyr::select(dma) %>% 
                          dplyr::distinct() %>% 
                          dplyr::arrange(dma) %>%
                          dplyr::pull()
    
    ##To ensure its locale independent
    geo_names_vec = stringr::str_sort(geo_names_vec,locale = 'UTF-8')
    
    if(var_infos){
      cat(sprintf('DMA names:\n%s',paste(c(1:length(geo_names_vec)),geo_names_vec,collapse = '\n')))
      cat("\n")
      cat(sprintf('media names:\n%s',paste(c(1:length(media_names_vec)),media_names_vec,collapse = '\n')))
    }
    
    prospect_df     = prospect_df %>%
                      dplyr::arrange(match(dma,geo_names_vec))  
    
    control_names_vec = geo_model_results_df %>% 
                          dplyr::filter(type == 'control variable') %>% 
                          dplyr::select(variable) %>% 
                          dplyr::distinct() %>% 
                          dplyr::pull()
    
    n_geo             = length(geo_names_vec)
    n_media           = length(media_names_vec)
    n_control         = length(control_names_vec)
    n_target          = length(target_names_vec)
    
   
    
    ## Subset the actual data by only the media used in the model to reduce size of object
    final_data_cols   = c('period','dma',media_names_vec)
    data_XyZ          = data_XyZ %>%
                          dplyr::select(final_data_cols)
    
    ## Remove spend and cost words from the media_names and create human readable names for Visualizations
    spend_cols_vec               = media_names_vec
    clean_media_names_vec         = sapply(media_names_vec,FUN = function(x){gsub('(_spend|_cost)','',x)})
    readable_media_names          = sapply(clean_media_names_vec,Caps)
    names(readable_media_names)  = clean_media_names_vec
    names(clean_media_names_vec) = media_names_vec
    
    ## Extract from the model results
    ## Extract the K and S parameters by media
    common_params_df    = pop_model_results_df %>% 
                          dplyr::filter((variable %in% media_names_vec) & 
                                        (metric %in% c('K','S','alpha'))) %>% 
                          dplyr::select(variable,metric,!!rlang::sym(statistic)) %>% 
                          tidyr::spread(key = metric,value = !!rlang::sym(statistic)) %>%
                          dplyr::rename('media' = 'variable') %>%
                          dplyr::mutate(media = recode(media,!!!clean_media_names_vec)) %>%  ## Recoding the parameters to remove the spend and cost from media names
                          dplyr::arrange(match(media,as.character(clean_media_names_vec)))
    
    ## Extract geo level parameters from geo_results_df
    beta_media_matrix   = geo_model_results_df %>%
                          dplyr::arrange(match(dma,geo_names_vec)) %>%
                          dplyr::filter(type == 'media variable') %>% 
                          dplyr::select(!!rlang::sym(statistic)) %>% 
                          dplyr::pull() %>%
                          matrix(data = .,ncol = n_media,nrow = n_geo,byrow = T,dimnames = list(NULL,as.character(clean_media_names_vec))) 
    
    beta_control_matrix = geo_model_results_df %>%
                          dplyr::arrange(match(dma,geo_names_vec)) %>%
                          dplyr::filter(type == 'control variable') %>% 
                          dplyr::select(!!rlang::sym(statistic)) %>% 
                          dplyr::pull() %>%
                          matrix(data = .,ncol = n_control,nrow = n_geo,byrow = T,dimnames = list(NULL,as.character(control_names_vec)))
    
    ## Extract from preprocessed data
    y_max_vec           = all_target_max_df %>% 
                          dplyr::filter(y == target_name, method == target_norm_type) %>%
                          dplyr::ungroup() %>%
                          dplyr::arrange(match(dma,geo_names_vec)) %>%
                          dplyr::select(y_max) %>%
                          dplyr::pull()
      
    target_capita_norm_col     = prospect_df %>%
                                 dplyr::select(!!rlang::sym(target_norm_col)) %>%
                                 dplyr::pull()
      
    
    ## Extract the vectors from the preprocessing results
    min_value_vec       = media_min_max_df %>%
                          dplyr::filter(X %in% media_names_vec) %>%
                          dplyr::arrange(match(dma,geo_names_vec),match(X,media_names_vec)) %>%
                          dplyr::filter(method == media_norm_type) %>%
                          dplyr::select(X_min) %>%
                          dplyr::pull()

    max_value_vec       = media_min_max_df %>%
                          dplyr::filter(X %in% media_names_vec) %>%
                          dplyr::arrange(match(dma,geo_names_vec),match(X,media_names_vec)) %>%
                          dplyr::filter(method == media_norm_type) %>%
                          dplyr::select(X_max) %>%
                          dplyr::pull()
    
    ## Convert the extracted min and max vector to matrix
    # if(length(min_value_vec) == n_geo){
    #   min_value_mat = matrix(rep(min_value_vec,(n_geo*n_media)/length(min_value_vec)),
    #                          ncol  = n_media,
    #                          nrow  = n_geo,
    #                          byrow = F)
    # }else if(length(min_value_vec) == n_media){
    #   min_value_mat = matrix(rep(min_value_vec,(n_geo*n_media)/length(min_value_vec)),
    #                          ncol  = n_media,
    #                          nrow  = n_geo,
    #                          byrow = T)
    # }else if(length(min_value_vec) == n_geo*n_media){
    min_value_mat = matrix(min_value_vec,
                            #  (n_geo*n_media)/length(min_value_vec),
                            #  ncol  = n_media,
                             nrow  = n_geo,
                             byrow = T)
    # }else{
    #   min_value_mat = matrix(min_value_vec,
    #                          (n_geo*n_media)/length(min_value_vec),
    #                          ncol  = n_media,
    #                          nrow  = n_geo,
    #                          byrow = T)
    # }
    colnames(min_value_mat) <- as.character(clean_media_names_vec)
    

    # if(length(max_value_vec) == n_geo){
    #   max_value_mat = matrix(rep(max_value_vec,(n_geo*n_media)/length(max_value_vec)),
    #                          ncol  = n_media,
    #                          nrow  = n_geo,
    #                          byrow = F)
    # }else if(length(max_value_vec) == n_media){
    #   max_value_mat = matrix(rep(max_value_vec,(n_geo*n_media)/length(max_value_vec)),
    #                          ncol  = n_media,
    #                          nrow  = n_geo,
    #                          byrow = T)
    # }else if(length(max_value_vec) == n_geo*n_media){
    max_value_mat = matrix(max_value_vec,
                            #  (n_geo*n_media)/length(max_value_vec),
                            #  ncol  = n_media,
                             nrow  = n_geo,
                             byrow = T)
    # }else{
    #   max_value_mat = matrix(max_value_vec,
    #                          (n_geo*n_media)/length(max_value_vec),
    #                          ncol  = n_media,
    #                          nrow  = n_geo,
    #                          byrow = T)
    # }
    colnames(max_value_mat) <- as.character(clean_media_names_vec)

   

    control_matrix  = norm_control_df %>%
                        dplyr::mutate('intercept' = 1) %>%                   ##Adding the intercept term
                        dplyr::select(control_names_vec) %>%
                        as.matrix(.)
    
    media_vars_list = list(spend_cols_vec = spend_cols_vec,
                           media_names_vec = as.character(clean_media_names_vec),
                           common_params_df = common_params_df,
                           beta_media_mat = beta_media_matrix,
                           min_media_mat = min_value_mat,
                           max_media_mat = max_value_mat,
                           cleaned_media_names_vec = readable_media_names)
    
    control_vars_list = list(control_names_vec = control_names_vec,
                             control_matrix    = control_matrix,
                             beta_control_mat  = beta_control_matrix)
    
    other_vars_list = list(data_XyZ = data_XyZ,
                           dma_names_vec = geo_names_vec,
                           prospect_df       = prospect_df
                           )
    
    target_vars_list = list(target_max_vec = y_max_vec,
                            target_capita_norm_col = target_capita_norm_col)
    
    temp_list = list('media' = media_vars_list,
                     'control' = control_vars_list,
                     'target' = target_vars_list,
                     'other' = other_vars_list)
    ##Append the temp_list to the final output_list
    output_list[[target_name]] = temp_list
  }##End extraction
  

  return(output_list)
  
}