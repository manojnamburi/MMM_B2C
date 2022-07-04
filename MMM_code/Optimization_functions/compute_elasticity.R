##Function to get the media target value for a given spend rolled up to quarterly level
get_target_footprint <- function(spend)
{
    ###Get the media GA for the given spend matrix
    res <- gross_add_weekly(solution = spend,
                 min_media_mat = min_media_mat,
                 max_media_mat = max_media_mat,
                 ratio_dma = spend_activity_ratio_dma_df,
                 y_max = target_max_vec,
                 prospect_mat_media = prospect_mat_media,
                 beta_media_df = beta_media_mat,
                 K = K,
                 S=S)
    
    ##Roll up the media GA to quarterly level
    quarterly_res <- data.frame(nbr_period_q*res) %>%
                     mutate(Total = rowSums(.),
                            dma = dma_names_vec) %>%
                     select(dma,everything()) 
    
    ##Subset Optimum and Suddenlink results along with total altice results
     opt_res <- quarterly_res %>%
               filter(dma%in%optimum_dma) %>%
               select(-dma) %>%
               gather(key = media,value = 'Media_GA') %>%
               mutate_at(.vars = vars('Media_GA'),.funs = ~format(round(.),big.mark = ','))
    
    sdl_res <- quarterly_res %>%
               filter(!(dma%in%optimum_dma)) %>%
               summarize_at(.vars = c(media_names_vec,'Total'),.funs = sum) %>%
               gather(key = media,value = 'Media_GA') %>%
               mutate_at(.vars = vars('Media_GA'),.funs = ~format(round(.),big.mark = ','))

    altice_res <- quarterly_res %>%
                  summarize_at(.vars = c(media_names_vec,'Total'),.funs = sum) %>%
                  gather(key = media,value = 'Media_GA') %>%
               mutate_at(.vars = vars('Media_GA'),.funs = ~format(round(.),big.mark = ','))
    
    

    
return(list('Optimum' = opt_res,
            'Suddenlink' = sdl_res,
            'Altice' = altice_res))    
}


computeElasticity <- function(model_params,
                              mean_qy_spend,
                              spend_change = 0.1,
                              display_order = media_names_vec){
    
    
    
    ##Extract the params needed to compute the elasticity
    beta_media_mat <<- model_params$media_params$beta_media_mat
    K <<- model_params$media_params$K
    S <<- model_params$media_params$S
    spend_activity_ratio_dma_df <<- model_params$media_params$spend_activity_ratio_dma_df
    min_media_mat <<- model_params$media_params$min_media_mat
    max_media_mat <<- model_params$media_params$max_media_mat
    media_names_vec <<- model_params$media_params$media_names_vec
    cleaned_names_list <<- model_params$media_params$cleaned_names_list
    prospect_mat_media <<- model_params$media_params$prospect_mat_media
    media_spend_cols <<- model_params$media_params$spend_cols_vec

    ##--------------------------------Control parameters ----------------------------------
    beta_control_mat <<- model_params$control_params$beta_control_mat
    control_mat <<- model_params$control_params$control_mat
    control_names_vec <<- model_params$control_params$control_names_vec

    ##-------------------------------Target parameters ------------------------------------
    target_max_vec <<- model_params$target_params$target_max_vec
    target_var <<- model_params$target_params$target_var
    target_capita_norm_col <<- model_params$target_params$target_capita_norm_col

    ##--------------------------------Other parameters -------------------------------------
    nbr_period_q <<- model_params$other_params$nbr_period_q
    #prospect_vec <<- model_params$other_params$prospect_vec
    dma_names_vec <<- model_params$other_params$dma_names_vec
    optimum_dma <<- model_params$other_params$optimum_dma
    
    ##Compute the spend matrix
    mean_qy_spend_mat <- mean_qy_spend %>%
                         arrange(dma,dma_names_vec) %>%
                         rename_at(.vars = media_spend_cols, .funs = ~media_names_vec) %>%
                         select(media_names_vec) %>%
                         as.matrix(.)
    
    ##Get the long version for output by footprint
    mean_spend_long_opt <- mean_qy_spend %>%
                               mutate(Total = rowSums(.[media_spend_cols])) %>%
                               filter((dma%in%optimum_dma)) %>%
                               summarize_at(.vars = c(media_spend_cols,'Total'),.funs=~nbr_period_q*sum(.)) %>%
                               rename_at(.vars = media_spend_cols, .funs = ~media_names_vec) %>%
                               gather(key = media,value = mean_spend) %>%
                               mutate_at(.vars = vars(mean_spend),.funs = ~round(.,0)) %>%
                               arrange(match(media,display_order))
    
     mean_spend_long_sdl <- mean_qy_spend %>%
                               mutate(Total = rowSums(.[media_spend_cols])) %>%
                               filter(!(dma%in%optimum_dma)) %>%
                               summarize_at(.vars = c(media_spend_cols,'Total'),.funs=~nbr_period_q*sum(.)) %>%
                               rename_at(.vars = media_spend_cols, .funs = ~media_names_vec) %>%
                               gather(key = media,value = mean_spend) %>%
                               mutate_at(.vars = vars(mean_spend),.funs = ~round(.,0)) %>%
                               arrange(match(media,display_order))
    
    ##Compute the media GA from the model results
    mean_quarterly_trg <- get_target_footprint(mean_qy_spend_mat)
    
    opt_trg <- mean_quarterly_trg$Optimum
    sdl_trg <- mean_quarterly_trg$Suddenlink
    
    
    ##Join the spend and target
    opt_spend_trg <- mean_spend_long_opt %>%
                     dplyr::left_join(opt_trg, by = 'media') %>%
                     dplyr::ungroup() %>%
                     dplyr::select(mean_spend,Media_GA)
    
    sdl_spend_trg <- mean_spend_long_sdl %>%
                     dplyr::left_join(sdl_trg, by = 'media') %>%
                     dplyr::ungroup() %>%
                     dplyr::select(mean_spend,Media_GA)
    
    media_output_list = list()
    media_output_list[['Actual']] = list('OPT' = opt_spend_trg,
                                       'SDL' = sdl_spend_trg)
    
    for(change in spend_change){
        list_name <- paste('change',change,sep = '_')
        change_list <- list()
        
        ##Compute the incremental and decremental spend matrices
        increment <- 1 + change
        decrement <- 1 - change

        mean_p_spend_mat <- increment*mean_qy_spend_mat 
        mean_b_spend_mat <- decrement*mean_qy_spend_mat 

        ##Get the media target for the incremental and decremental spends
        mean_p_target = get_target_footprint(mean_p_spend_mat)
        mean_b_target = get_target_footprint(mean_b_spend_mat)
        
        

        ##Get the spend 
        change_spend_opt <-    mean_spend_long_opt %>%
                               mutate('mean_spend_up' = round(increment*mean_spend,0),
                                     'mean_spend_down' = round(decrement*mean_spend,0))
        
        mean_spend_opt_up <- change_spend_opt %>%
                             dplyr::select(media,mean_spend_up)
        
        
        mean_spend_opt_down <- change_spend_opt %>%
                               dplyr::select(media,mean_spend_down)


        change_spend_sdl <-    mean_spend_long_sdl%>%
                               mutate('mean_spend_up' = round(increment*mean_spend,0),
                                     'mean_spend_down' = round(decrement*mean_spend,0)) 
        
        mean_spend_sdl_up <- change_spend_sdl %>%
                             dplyr::select(media,mean_spend_up)
        
        
        mean_spend_sdl_down <- change_spend_sdl %>%
                               dplyr::select(media,mean_spend_down)
        
    
        ##Increased spend and GA
        incr_opt_spend_ga <- mean_spend_opt_up %>%
                             dplyr::left_join(mean_p_target$Optimum, by = 'media') %>%
                             dplyr::ungroup() %>%
                             dplyr::select(mean_spend_up,Media_GA)
        
        incr_sdl_spend_ga <- mean_spend_sdl_up %>%
                             dplyr::left_join(mean_p_target$Suddenlink, by = 'media') %>%
                             dplyr::ungroup() %>%
                             dplyr::select(mean_spend_up,Media_GA)
        
        ##Decreased spend and GA
        decr_opt_spend_ga <- mean_spend_opt_down %>%
                             dplyr::left_join(mean_b_target$Optimum, by = 'media') %>%
                             dplyr::ungroup() %>%
                             dplyr::select(mean_spend_down,Media_GA)
        
        
        decr_sdl_spend_ga <- mean_spend_sdl_down %>%
                             dplyr::left_join(mean_b_target$Suddenlink, by = 'media') %>%
                             dplyr::ungroup() %>%
                             dplyr::select(mean_spend_down,Media_GA)
        
        
        
     media_output_list[[list_name]] = list('Increased' = list('OPT' = incr_opt_spend_ga,
                                                            'SDL' = incr_sdl_spend_ga),
                                         'Decreased' = list('OPT' = decr_opt_spend_ga,
                                                            'SDL' = decr_sdl_spend_ga))   
    }
   
    
    ##Compute the control GA averaging across the latest of each quarters
    control_ga_Q1 = gross_add_control(Z = control_mat,
                                  beta_control_df = beta_control_mat,
                                  control_names = control_names_vec,
                                  data_XyZ = data_XyZ,
                                  input_period = 'Q1',
                                  y_max = target_max_vec,
                                  target_capita_norm_col = target_capita_norm_col)

    control_ga_Q2 = gross_add_control(Z = control_mat,
                                      beta_control_df = beta_control_mat,
                                      control_names = control_names_vec,
                                      data_XyZ = data_XyZ,
                                      input_period = 'Q2',
                                      y_max = target_max_vec,
                                      target_capita_norm_col = target_capita_norm_col)

    control_ga_Q3 = gross_add_control(Z = control_mat,
                                      beta_control_df = beta_control_mat,
                                      control_names = control_names_vec,
                                      data_XyZ = data_XyZ,
                                      input_period = 'Q3',
                                      y_max = target_max_vec,
                                      target_capita_norm_col = target_capita_norm_col)

    control_ga_Q4 = gross_add_control(Z = control_mat,
                                      beta_control_df = beta_control_mat,
                                      control_names = control_names_vec,
                                      data_XyZ = data_XyZ,
                                      input_period = 'Q4',
                                      y_max = target_max_vec,
                                      target_capita_norm_col = target_capita_norm_col)
    
    
    ##Compute the cumulative baseline (control) target by footprint
    control_ga_sum_opt <- sum(control_ga_Q1[var_id_opt,]+control_ga_Q2[var_id_opt,]+control_ga_Q3[var_id_opt,]+control_ga_Q4[var_id_opt,])
    mean_baseline_trg_opt <- round(control_ga_sum_opt/4)

    control_ga_sum_sdl <- sum(control_ga_Q1[-var_id_opt,]+control_ga_Q2[-var_id_opt,]+control_ga_Q3[-var_id_opt,]+control_ga_Q4[-var_id_opt,])
    mean_baseline_trg_sdl <- round(control_ga_sum_sdl/4)
    
    cat('Order of media:\n')
    cat(paste(c(1:(length(display_order)+1)),c(display_order,'total'),collapse = '\n', sep ='.'))
    
    return(list('Control' = list('East' = mean_baseline_trg_opt,
                            'West' = mean_baseline_trg_sdl),
           'Media' = media_output_list))
    
    
    
}