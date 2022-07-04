##Function to get the media target value for a given spend rolled up to quarterly level
get_target <- function(spend)
{
    ###Get the media GA for the given spend matrix
    res <- gross_add_weekly(solution = spend,
                 min_media_mat = min_media_mat,
                 max_media_mat = max_media_mat,
                 ratio_dma = spend_activity_ratio_dma_df,
                 y_max = target_max_vec,
                 prospect_mat_media = prospect_mat_media,
                 beta_media_df = beta_media_mat,
                 target_capita_norm_col = target_capita_norm_col,
                 K = K,
                 S=S)
    
    media_spend_names_only <- media_names_vec[!(media_names_vec %in% activity_based_media)]
    #cat(paste(media_spend_names_only,collapse=','))
    
    ## Roll up the media GA to quarterly level by using mean spend weeks
    quarterly_res <- data.frame(n_weeks_mat*res) %>%
                     # mutate_at(.vars = vars(media_names_vec),.funs = ~round(.)) %>%
                     mutate(spend_media_total = rowSums(.[media_spend_names_only]),
                            Total = rowSums(.),
                            dma = dma_names_vec) %>%
                     select(dma,everything())
    
  
    ## DMA level GA - includes Optimum also
    dma_res <- quarterly_res %>%
                    gather(key = media,value = 'Media_GA', -dma) %>%
              #       mutate_at(.vars = vars('Media_GA'),.funs = ~format(round(.))) %>%
                    mutate(Media_GA = as.numeric(Media_GA))

                
    ## Suddenlink level GA
    sdl_res <- quarterly_res %>%
                   filter(!(dma%in%optimum_dma)) %>%
                   summarize_at(.vars = c(media_names_vec,'spend_media_total','Total'),.funs = sum) %>%
                   gather(key = media,value = 'Media_GA') %>%
              #      mutate_at(.vars = vars('Media_GA'),.funs = ~format(round(.))) %>%
                   mutate(Media_GA = as.numeric(Media_GA))
    
    ## Altice level GA
    altice_res <- quarterly_res %>%
                      summarize_at(.vars = c(media_names_vec,'spend_media_total','Total'),.funs = sum) %>%
                      gather(key = media,value = 'Media_GA') %>%
                     #  mutate_at(.vars = vars('Media_GA'),.funs = ~format(round(.))) %>%
                      mutate(Media_GA = as.numeric(Media_GA))

   ##Optimum level GA
   optim_res <- quarterly_res %>%
                filter((dma%in%optimum_dma)) %>%
                summarize_at(.vars = c(media_names_vec,'spend_media_total','Total'),.funs = sum) %>%
                gather(key = media,value = 'Media_GA') %>%
              #      mutate_at(.vars = vars('Media_GA'),.funs = ~format(round(.))) %>%
                mutate(Media_GA = as.numeric(Media_GA))
    
    
    return(list('DMA' = dma_res,
                'Suddenlink' = sdl_res,
                'Altice' = altice_res,
                'Optimum' = optim_res))    
}


##Compute Elasticity & Media Contribution at Footprint level & DMA level
compute_elasticity_and_contribution <- function(model_params,
                              mean_qy_spend,
                              mean_weekly_spend,
                              data_XyZ,
                              spend_change = 0.1,
                              display_order = media_names_vec,
                              activity_based_media = NULL){
    
    
    
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
    n_weeks_mat <<- model_params$media_params$n_weeks_mat

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
    
    activity_based_media <<- activity_based_media
    
    media_spend_cols_only <<- media_spend_cols[!(media_spend_cols %in% activity_based_media)]
    
    
    ##Compute the spend matrix from input quarterly mean spend
    mean_qy_spend_mat <- mean_weekly_spend %>%
                            arrange(dma,dma_names_vec) %>%
                            rename_at(.vars = media_spend_cols, .funs = ~media_names_vec) %>%
                            select(media_names_vec) %>%
                            as.matrix(.)
    
    ##Get the long format of spend by DMA
     mean_spend_long_dma <- mean_qy_spend %>%
                                   mutate(spend_media_total = rowSums(.[media_spend_cols_only]),
                                          Total = rowSums(.[media_spend_cols])) %>%
                                   #mutate_at(.vars = c(media_spend_cols,'spend_media_total','Total'),.funs=~n_weeks_mat*(.)) %>%
                                   rename_at(.vars = media_spend_cols, .funs = ~media_names_vec) %>%
                                   gather(key = media,value = mean_spend, -dma) %>%
                                   mutate_at(.vars = vars(mean_spend),.funs = ~round(.,0)) %>%
                                   arrange(dma, match(media,display_order))
    
    ##Get the long format of spend for SDL
     mean_spend_long_sdl <- mean_qy_spend %>%
                                   mutate(spend_media_total = rowSums(.[media_spend_cols_only]),
                                                 Total = rowSums(.[media_spend_cols])) %>%
                                   filter(!(dma%in%optimum_dma)) %>%
                                   summarize_at(.vars = c(media_spend_cols,'spend_media_total','Total'),.funs=~sum(.)) %>%
                                   rename_at(.vars = media_spend_cols, .funs = ~media_names_vec) %>%
                                   gather(key = media,value = mean_spend) %>%
                                   mutate_at(.vars = vars(mean_spend),.funs = ~round(.,0)) %>%
                                   arrange(match(media,display_order))
    
    ##Get the long format of spend for Altice
     mean_spend_long_altice <- mean_qy_spend %>%
                                   mutate(spend_media_total = rowSums(.[media_spend_cols_only]),
                                          Total = rowSums(.[media_spend_cols])) %>%
                                   summarize_at(.vars = c(media_spend_cols,'spend_media_total','Total'),.funs=~sum(.)) %>%
                                   rename_at(.vars = media_spend_cols, .funs = ~media_names_vec) %>%
                                   gather(key = media,value = mean_spend) %>%
                                   mutate_at(.vars = vars(mean_spend),.funs = ~round(.,0)) %>%
                                   arrange(match(media,display_order))
    
    ##Get the long format of spend for Optimum
    mean_spend_long_opt <- mean_qy_spend %>%
                                   mutate(spend_media_total = rowSums(.[media_spend_cols_only]),
                                          Total = rowSums(.[media_spend_cols])) %>%
                                   filter((dma%in%optimum_dma)) %>%
                                   summarize_at(.vars = c(media_spend_cols,'spend_media_total','Total'),.funs=~sum(.)) %>%
                                   rename_at(.vars = media_spend_cols, .funs = ~media_names_vec) %>%
                                   gather(key = media,value = mean_spend) %>%
                                   mutate_at(.vars = vars(mean_spend),.funs = ~round(.,0)) %>%
                                   arrange(match(media,display_order))
    
    ##Compute the media GA from the model results
    mean_quarterly_trg <- get_target(mean_qy_spend_mat)
    
    dma_trg <- mean_quarterly_trg$DMA
    sdl_trg <- mean_quarterly_trg$Suddenlink
    altice_trg <- mean_quarterly_trg$Altice
    optim_trg <- mean_quarterly_trg$Optimum
    
    ##Join the spend and target 
    dma_spend_trg <- mean_spend_long_dma %>%
                         dplyr::left_join(dma_trg, by = c('dma','media')) %>%
                         dplyr::ungroup()
    
    sdl_spend_trg <- mean_spend_long_sdl %>%
                         dplyr::left_join(sdl_trg, by = 'media') %>%
                         dplyr::ungroup() %>%
                         dplyr::mutate(dma = 'SUDDENLINK') %>%
                         dplyr::select(dma, everything())
    
    altice_spend_trg <- mean_spend_long_altice %>%
                         dplyr::left_join(altice_trg, by = 'media') %>%
                         dplyr::ungroup() %>%
                         dplyr::mutate(dma = 'ALTICE') %>%
                         dplyr::select(dma, everything())
    
    opt_spend_trg <- mean_spend_long_opt %>%
                       dplyr::left_join(optim_trg, by = 'media') %>%
                       dplyr::ungroup() %>%
                       dplyr::mutate(dma = 'OPTIMUM') %>%
                       dplyr::select(dma, everything())
    
    for(change in spend_change){
        list_name <- paste('change',change,sep = '_')
        
        ##Compute the incremental and decremental spend matrices
        increment <- 1 + change
        decrement <- 1 - change

        mean_p_spend_mat <- increment*mean_qy_spend_mat 
        mean_b_spend_mat <- decrement*mean_qy_spend_mat 

        ##Get the media target for the incremental and decremental spends
        mean_p_target = get_target(mean_p_spend_mat)
        mean_b_target = get_target(mean_b_spend_mat)
        
        
        ##Get the spend for DMA       
        change_spend_dma    <-  mean_spend_long_dma%>% 
                                    mutate('mean_spend_up' =  round(increment*mean_spend,0), #increment*mean_spend,
                                           'mean_spend_down' = round(decrement*mean_spend,0))  #decrement*mean_spend)
        mean_spend_dma_up   <- change_spend_dma %>%
                                    dplyr::select(dma, media,mean_spend_up)
        mean_spend_dma_down <- change_spend_dma %>%
                                    dplyr::select(dma, media,mean_spend_down)
        
        ##Get the spend for Suddenlink
        change_spend_sdl    <-  mean_spend_long_sdl%>%
                                    mutate('mean_spend_up' = round(increment*mean_spend,0),
                                           'mean_spend_down' = round(decrement*mean_spend,0)) 
        mean_spend_sdl_up   <- change_spend_sdl %>%
                                    dplyr::select(media,mean_spend_up)
        mean_spend_sdl_down <- change_spend_sdl %>%
                                    dplyr::select(media,mean_spend_down)
        
        ##Get the spend for Altice
        change_spend_altice    <-  mean_spend_long_altice%>%
                                        mutate('mean_spend_up' = round(increment*mean_spend,0),
                                               'mean_spend_down' = round(decrement*mean_spend,0)) 
        mean_spend_altice_up   <- change_spend_altice %>%
                                       dplyr::select(media,mean_spend_up)
        mean_spend_altice_down <- change_spend_altice %>%
                                       dplyr::select(media,mean_spend_down)
        
        ##Get the spend for Optimum
        change_spend_opt    <-  mean_spend_long_opt %>%
                                        mutate('mean_spend_up' = round(increment*mean_spend,0),
                                               'mean_spend_down' = round(decrement*mean_spend,0)) 
        mean_spend_opt_up   <- change_spend_opt %>%
                                       dplyr::select(media,mean_spend_up)
        mean_spend_opt_down <- change_spend_opt %>%
                                       dplyr::select(media,mean_spend_down)

    
        ##Increased & Decreased spend and GA at DMA level    
        incr_dma_spend_ga <- mean_spend_dma_up %>%
                             dplyr::left_join(mean_p_target$DMA, by = c('dma','media')) %>%
                             dplyr::ungroup() %>%
                             dplyr::select(dma, media, mean_spend_up,Media_GA) %>%
                             dplyr::mutate(mean_spend_up = as.numeric(mean_spend_up),
                                           Media_GA = as.numeric(Media_GA)) %>%
                             dplyr::rename(!!paste('mean_spend_up',change,sep = '_') := mean_spend_up,
                                           !!paste('Media_GA_up',change,sep = '_') := Media_GA) 
        
        decr_dma_spend_ga <- mean_spend_dma_down %>%
                             dplyr::left_join(mean_b_target$DMA, by = c('dma','media')) %>%
                             dplyr::ungroup() %>%
                             dplyr::select(dma, media, mean_spend_down,Media_GA) %>%
                             dplyr::mutate(mean_spend_down = as.numeric(mean_spend_down),
                                           Media_GA = as.numeric(Media_GA)) %>%
                             dplyr::rename(!!paste('mean_spend_down',change,sep = '_') := mean_spend_down,
                                           !!paste('Media_GA_down',change,sep = '_') := Media_GA)
        
        ##Increased & Decreased spend and GA at Suddenlink level 
        incr_sdl_spend_ga <- mean_spend_sdl_up %>%
                             dplyr::left_join(mean_p_target$Suddenlink, by = 'media') %>%
                             dplyr::ungroup() %>%
                             dplyr::mutate(dma = 'SUDDENLINK') %>%
                             dplyr::select(dma, media, mean_spend_up,Media_GA) %>%
                             dplyr::mutate(mean_spend_up = as.numeric(mean_spend_up),
                                           Media_GA = as.numeric(Media_GA)) %>%
                             dplyr::rename(!!paste('mean_spend_up',change,sep = '_') := mean_spend_up,
                                           !!paste('Media_GA_up',change,sep = '_') := Media_GA)
        
        decr_sdl_spend_ga <- mean_spend_sdl_down %>%
                             dplyr::left_join(mean_b_target$Suddenlink, by = 'media') %>%
                             dplyr::ungroup() %>%
                             dplyr::mutate(dma = 'SUDDENLINK') %>%
                             dplyr::select(dma, media, mean_spend_down,Media_GA) %>%
                             dplyr::mutate(mean_spend_down = as.numeric(mean_spend_down),
                                           Media_GA = as.numeric(Media_GA)) %>%
                             dplyr::rename(!!paste('mean_spend_down',change,sep = '_') := mean_spend_down,
                                           !!paste('Media_GA_down',change,sep = '_') := Media_GA)
        
        ##Increased & Decreased spend and GA at Altice level 
        incr_altice_spend_ga <- mean_spend_altice_up %>%
                             dplyr::left_join(mean_p_target$Altice, by = 'media') %>%
                             dplyr::ungroup() %>%
                             dplyr::mutate(dma = 'ALTICE') %>%
                             dplyr::select(dma, media, mean_spend_up,Media_GA) %>%
                             dplyr::mutate(mean_spend_up = as.numeric(mean_spend_up),
                                           Media_GA = as.numeric(Media_GA)) %>%
                             dplyr::rename(!!paste('mean_spend_up',change,sep = '_') := mean_spend_up,
                                           !!paste('Media_GA_up',change,sep = '_') := Media_GA)
        
        decr_altice_spend_ga <- mean_spend_altice_down %>%
                             dplyr::left_join(mean_b_target$Altice, by = 'media') %>%
                             dplyr::ungroup() %>%
                             dplyr::mutate(dma = 'ALTICE') %>%
                             dplyr::select(dma, media, mean_spend_down,Media_GA) %>%
                             dplyr::mutate(mean_spend_down = as.numeric(mean_spend_down),
                                           Media_GA = as.numeric(Media_GA)) %>%
                             dplyr::rename(!!paste('mean_spend_down',change,sep = '_') := mean_spend_down,
                                           !!paste('Media_GA_down',change,sep = '_') := Media_GA) 
        
        ##Increased & Decreased spend and GA at Optimum level 
        incr_opt_spend_ga <- mean_spend_opt_up %>%
                             dplyr::left_join(mean_p_target$Optimum, by = 'media') %>%
                             dplyr::ungroup() %>%
                             dplyr::mutate(dma = 'OPTIMUM') %>%
                             dplyr::select(dma, media, mean_spend_up,Media_GA) %>%
                             dplyr::mutate(mean_spend_up = as.numeric(mean_spend_up),
                                           Media_GA = as.numeric(Media_GA)) %>%
                             dplyr::rename(!!paste('mean_spend_up',change,sep = '_') := mean_spend_up,
                                           !!paste('Media_GA_up',change,sep = '_') := Media_GA)
        
        decr_opt_spend_ga <- mean_spend_opt_down %>%
                             dplyr::left_join(mean_b_target$Optimum, by = 'media') %>%
                             dplyr::ungroup() %>%
                             dplyr::mutate(dma = 'OPTIMUM') %>%
                             dplyr::select(dma, media, mean_spend_down,Media_GA) %>%
                             dplyr::mutate(mean_spend_down = as.numeric(mean_spend_down),
                                           Media_GA = as.numeric(Media_GA)) %>%
                             dplyr::rename(!!paste('mean_spend_down',change,sep = '_') := mean_spend_down,
                                           !!paste('Media_GA_down',change,sep = '_') := Media_GA)
        
        ##Add Incr and Decr dataframes to Actual dataframe - DMA level
        dma_spend_trg <- dma_spend_trg %>%
                             dplyr::left_join(incr_dma_spend_ga, by = c('dma','media')) %>%
                             dplyr::left_join(decr_dma_spend_ga, by = c('dma','media')) %>%
                             dplyr::ungroup()
        
        ##Add Incr and Decr dataframes to Actual dataframe - Suddenlink level
        sdl_spend_trg <- sdl_spend_trg %>%
                             dplyr::left_join(incr_sdl_spend_ga, by = c('dma','media')) %>%
                             dplyr::left_join(decr_sdl_spend_ga, by = c('dma','media')) %>%
                             dplyr::ungroup()
        
        ##Add Incr and Decr dataframes to Actual dataframe - Altice level
        altice_spend_trg <- altice_spend_trg %>%
                             dplyr::left_join(incr_altice_spend_ga, by = c('dma','media')) %>%
                             dplyr::left_join(decr_altice_spend_ga, by = c('dma','media')) %>%
                             dplyr::ungroup()

        ##Add Incr and Decr dataframes to Actual dataframe - Optimum level
        opt_spend_trg <- opt_spend_trg %>%
                             dplyr::left_join(incr_opt_spend_ga, by = c('dma','media')) %>%
                             dplyr::left_join(decr_opt_spend_ga, by = c('dma','media')) %>%
                             dplyr::ungroup()
         
    }
   
    ## Combine DMA level, Suddenlink level, Altice level and Optimum level dataframes into 1 single dataframe
    final_spend_trg <-  altice_spend_trg %>% rbind(sdl_spend_trg,
                                                   opt_spend_trg,
                                                   dma_spend_trg) 
    
    ##Compute the control GA averaging across the latest of each quarters
    control_ga_Q1 <- gross_add_control(Z = control_mat,
                                      beta_control_df = beta_control_mat,
                                      control_names = control_names_vec,
                                      data_XyZ = data_XyZ,
                                      input_period = 'Q1',
                                      y_max = target_max_vec,
                                      target_capita_norm_col = target_capita_norm_col)

    control_ga_Q2 <- gross_add_control(Z = control_mat,
                                      beta_control_df = beta_control_mat,
                                      control_names = control_names_vec,
                                      data_XyZ = data_XyZ,
                                      input_period = 'Q2',
                                      y_max = target_max_vec,
                                      target_capita_norm_col = target_capita_norm_col)

    control_ga_Q3 <- gross_add_control(Z = control_mat,
                                      beta_control_df = beta_control_mat,
                                      control_names = control_names_vec,
                                      data_XyZ = data_XyZ,
                                      input_period = 'Q3',
                                      y_max = target_max_vec,
                                      target_capita_norm_col = target_capita_norm_col)

    control_ga_Q4 <- gross_add_control(Z = control_mat,
                                      beta_control_df = beta_control_mat,
                                      control_names = control_names_vec,
                                      data_XyZ = data_XyZ,
                                      input_period = 'Q4',
                                      y_max = target_max_vec,
                                      target_capita_norm_col = target_capita_norm_col)
    
    
    ##Compute the cumulative baseline (control) target by DMA, Suddenlink, Altice and Optimum
    control_ga_sum_dma    <- rowSums(control_ga_Q1+control_ga_Q2+control_ga_Q3+control_ga_Q4)
    mean_baseline_trg_dma <- round(control_ga_sum_dma/4)
    control_ga_dma        <- data.frame(dma = dma_names_vec, control_ga = mean_baseline_trg_dma)
    
    control_ga_sum_sdl    <- sum(control_ga_Q1[-var_id_opt,]+control_ga_Q2[-var_id_opt,]+control_ga_Q3[-var_id_opt,]+control_ga_Q4[-var_id_opt,])
    mean_baseline_trg_sdl <- round(control_ga_sum_sdl/4)
    control_ga_sdl        <- data.frame(dma = 'SUDDENLINK', control_ga = mean_baseline_trg_sdl)
    
    control_ga_sum_altice    <- sum(control_ga_Q1+control_ga_Q2+control_ga_Q3+control_ga_Q4)
    mean_baseline_trg_altice <- round(control_ga_sum_altice/4)
    control_ga_altice        <- data.frame(dma = 'ALTICE', control_ga = mean_baseline_trg_altice)

    control_ga_sum_opt    <- sum(control_ga_Q1[var_id_opt,]+control_ga_Q2[var_id_opt,]+control_ga_Q3[var_id_opt,]+control_ga_Q4[var_id_opt,])
    mean_baseline_trg_opt <- round(control_ga_sum_opt/4)
    control_ga_opt        <- data.frame(dma = 'OPTIMUM', control_ga = mean_baseline_trg_opt)
    
    ## Combine DMA level, Suddenlink level, Altice level dataframes into 1 single dataframe
    final_control_ga <-  control_ga_altice %>% rbind(control_ga_sdl,
                                                     control_ga_opt,
                                                   control_ga_dma)
    
    final_spend_trg_controlga <- final_spend_trg %>%
                                     dplyr::left_join(final_control_ga, by = c('dma')) %>%
                                     dplyr::group_by(dma) %>%
                                     dplyr::mutate(Media_contribution = Media_GA*100/Media_GA[media == "Total"],
                                                   Media_contribution = ifelse(media == 'Total', Media_GA*100/(Media_GA+control_ga), Media_contribution)) %>%
                                     dplyr::ungroup() %>%
                                     dplyr::mutate_at(.vars = vars('Media_contribution'),.funs = ~round(.,2))
    
    Elasticity_list <- list()
    desired_order <- final_control_ga$dma
    
    Media_contribution <- final_spend_trg_controlga %>%
                                    dplyr::filter(media != 'spend_media_total') %>%
                                    dplyr::select(dma, media, Media_contribution) %>%
                                    dplyr::mutate(dma = factor(dma,levels = desired_order)) %>%
                                    tidyr::spread(media, Media_contribution) %>%
                                    dplyr::select(dma, display_order, Total)
                        
    
    for(change in spend_change){
        
         list_name_up <- paste('Elasticity_up',change,sep = '_')
         list_name_down <- paste('Elasticity_down',change,sep = '_')
        
         var_mean_spend_up <-  paste('mean_spend_up',change,sep='_')
         var_Media_GA_up <-  paste('Media_GA_up',change,sep='_')  
         var_Elasticity_up <- paste('Elasticity_up',change,sep='_')
        
         var_mean_spend_down <-  paste('mean_spend_down',change,sep='_')
         var_Media_GA_down <-  paste('Media_GA_down',change,sep='_')  
         var_Elasticity_down <- paste('Elasticity_down',change,sep='_')

         final_spend_trg_controlga <- final_spend_trg_controlga %>% 
               group_by(dma) %>% 
               mutate(
                      Spend_change_pct_up = ifelse(media %in% activity_based_media,
                                              (UQ(rlang::sym(var_mean_spend_up)) - mean_spend)*100/mean_spend,
                                              (UQ(rlang::sym(var_mean_spend_up)) - mean_spend)*100/mean_spend[media == "spend_media_total"]),
                      GA_change_pct_up    = (UQ(rlang::sym(var_Media_GA_up)) - Media_GA)*100/(Media_GA[media == 'Total'] + control_ga[media == 'Total']),
                      Total_Elasticity_up = GA_change_pct_up/Spend_change_pct_up,
                      
                      Spend_change_pct_down = ifelse(media %in% activity_based_media,
                                                 (UQ(rlang::sym(var_mean_spend_down)) - mean_spend)*100/mean_spend,
                                                 (UQ(rlang::sym(var_mean_spend_down)) - mean_spend)*100/mean_spend[media == "spend_media_total"]),
                      GA_change_pct_down    = (UQ(rlang::sym(var_Media_GA_down)) - Media_GA)*100/(Media_GA[media == 'Total'] + control_ga[media == 'Total']),
                      Total_Elasticity_down = GA_change_pct_down/Spend_change_pct_down) %>% 
               ungroup() %>%
               mutate_at(.vars = vars(Spend_change_pct_up, GA_change_pct_up, Total_Elasticity_up,
                                      Spend_change_pct_down, GA_change_pct_down, Total_Elasticity_down),
                         .funs = ~round(.,2)) %>%
               rename(!!paste('Spend_change_pct_up',change,sep='_')   := Spend_change_pct_up,
                      !!paste('GA_change_pct_up',change,sep='_')      := GA_change_pct_up,
                      !!paste('Elasticity_up',change,sep='_')         := Total_Elasticity_up,
                      !!paste('Spend_change_pct_down',change,sep='_') := Spend_change_pct_down,
                      !!paste('GA_change_pct_down',change,sep='_')    := GA_change_pct_down,
                      !!paste('Elasticity_down',change,sep='_')       := Total_Elasticity_down) %>%
               select(dma, media, mean_spend, Media_GA, Media_contribution, everything(), -starts_with('Spend_change'), -starts_with('GA_change'))

        
          Elasticity_list[[list_name_up]] <- list(final_spend_trg_controlga %>%
                                                dplyr::filter(media != 'Total') %>%
                                                dplyr::select('dma', 'media', var_Elasticity_up) %>%
                                                dplyr::mutate(dma = factor(dma,levels = desired_order)) %>%
                                                tidyr::spread(media, !!paste('Elasticity_up',change,sep='_')) %>%
                                                dplyr::select(dma, display_order, spend_media_total)
                                               )
        
          Elasticity_list[[list_name_down]] <- list(final_spend_trg_controlga %>%
                                                dplyr::filter(media != 'Total') %>%
                                                dplyr::select('dma', 'media', var_Elasticity_down) %>%
                                                dplyr::mutate(dma = factor(dma,levels = desired_order)) %>%
                                                tidyr::spread(media, !!paste('Elasticity_down',change,sep='_')) %>%
                                                dplyr::select(dma, display_order, spend_media_total)
                                               )
    }
    
#     final_spend_trg_controlga <- final_spend_trg_controlga %>% filter(media != 'Total')
    
    
    cat('Order of media:\n')
    cat(paste(c(1:(length(display_order)+1)),c(display_order,'total'),collapse = '\n', sep ='.'))
    
    return(list('Control_GA' = final_control_ga,
                'All_metrics_detailed' = final_spend_trg_controlga,
                'Elasticity' =  Elasticity_list,
                'Media_contribution' = Media_contribution))
    
    
    
}


###Using the entire posterior distribution of K,S,alpha and beta, compute elasticity and contribution
summarize_draws <- function(draw_df,
                            summary_col){
    
    key_statistics = tibble::lst(N, Mean, Median, StdDev, Min, Max, Range, 
                               q001, q0025, q005, q01, q015, q025, q075, q085, q09, q095, q0975, q099)
    
    return_df <- draw_df %>%
                 dplyr::group_by(dma,media) %>%
                 dplyr::summarize_at(.vars = vars(summary_col),.funs = key_statistics) %>%
                 dplyr::arrange(dma,match(media,media_names_vec))
    
    return(return_df)
}

##Compute the elasticity and contribution for each draw of the sample
compute_elas_contrib_draw <- function(MCMC_beta_media,
                                      MCMC_beta_base_control,
                                      MCMC_alpha_K_S,
                                      model_params,
                                      mean_qy_spend,
                                      mean_weekly_spend,
                                      data_XyZ,
                                      activity_based_media = NULL,
                                      draw,
                                      spend_change = 0.1,
                                      type = 'up'){
     model_params_draw <- model_params
    dma_names_vec <- model_params$other_params$dma_names_vec
    media_names_vec <- model_params$media_params$media_names_vec
    control_names_vec <- model_params$control_params$control_names_vec
    var_media <- model_params$media_params$spend_cols_vec
    
    
    n_media = length(media_names_vec)
    n_geo = length(dma_names_vec)
    n_control = length(control_names_vec)
    
    ##Extract the media parameters by draw
    beta_media_mat_draw <- MCMC_beta_media %>%
                      dplyr::filter(.draw == draw) %>%
                      dplyr::arrange(match(dma,dma_names_vec)) %>%
                       dplyr::select(.value) %>% 
                       dplyr::pull() %>%
                       matrix(data = .,ncol = n_media,nrow = n_geo,byrow = T,dimnames = list(NULL,as.character(var_media)))
    
    K_draw <- MCMC_alpha_K_S %>%
         dplyr::filter(.draw == draw) %>%
         dplyr::arrange(match(variable_name,var_media)) %>%
         dplyr::select(K) %>%
         dplyr::pull()
    
    S_draw <- MCMC_alpha_K_S %>%
         filter(.draw == draw) %>%
         arrange(match(variable_name,var_media)) %>%
         select(S) %>%
         pull()
    
    
    
    #beta_media_mat_list <- append(beta_media_mat_list,list(beta_media_mat_draw))
    ##Create a list of beta control matrices
    beta_control_mat_draw <- MCMC_beta_base_control %>%
                      filter(.draw == draw) %>%
                      dplyr::arrange(match(dma,dma_names_vec)) %>%
                       dplyr::select(.value) %>% 
                       dplyr::pull() %>%
                       matrix(data = .,ncol = n_control,nrow = n_geo,byrow = T,dimnames = list(NULL,as.character(control_names_vec)))
    
    #beta_control_mat_list <- append(beta_control_mat_list,list(beta_control_mat_draw))
    
    model_params_draw$media_params$K <- K_draw
    model_params_draw$media_params$S <- S_draw
    model_params_draw$media_params$beta_media_mat <- beta_media_mat_draw
    model_params_draw$control_params$beta_control_mat <- beta_control_mat_draw
    
    res_draw <- compute_elasticity_and_contribution(model_params = model_params_draw,
                                              mean_qy_spend = mean_qy_spend,
                                            mean_weekly_spend = mean_weekly_spend,
                                           data_XyZ = data_XyZ,
                                             spend_change = spend_change,
                                            activity_based_media = activity_based_media)
    
    elas_df_name_down <- paste('Elasticity_down',spend_change,sep='_')
    elas_df_name_up <- paste('Elasticity_up',spend_change,sep='_')
    
    elas_df_draw_down <- data.frame(res_draw$Elasticity[[elas_df_name_down]]) %>%
                         gather(key = media,value = elasticity,-dma) %>%
                         mutate(.draw = draw)
    
    elas_df_draw_up <- data.frame(res_draw$Elasticity[[elas_df_name_up]]) %>%
                         gather(key = media,value = elasticity,-dma) %>%
                         mutate(.draw = draw)
    
    contrib_df_draw <- data.frame(res_draw$Media_contribution) %>%
                         gather(key = media,value = contribution,-dma) %>%
                         mutate(.draw = draw)
    
    if(type == 'up'){
        return(elas_df_draw_up)
    }else if(type == 'down'){
        return(elas_df_draw_down)
    }else{
        return(contrib_df_draw)
    }  
}

##Compute the posterior distribution of elasticity and contribution using all the samples of the parameters
##Requires the use of doparallel and doSnow packages for parallel running. Takes 8x times without parallel computing
compute_elas_contrib_dist <- function(MCMC_beta_media,
                                      MCMC_beta_base_control,
                                      MCMC_alpha_K_S,
                                      model_params,
                                      mean_qy_spend,
                                      mean_weekly_spend,
                                      data_XyZ,
                                      activity_based_media = NULL,
                                      run_in_parallel = TRUE,
                                      n_cores = NULL,
                                      spend_change = 0.1){
    
    ##Adding the function to summarize each draw
   
    
    output_list <- list()
    output_list_change <- list()
    
    n_draws <- MCMC_beta_media %>%
              dplyr::select(.draw) %>%
              dplyr::distinct() %>%
              dplyr::pull()
    
    
      if(run_in_parallel){

        system_cores = parallel::detectCores()[1]
        
        if(is.null(n_cores)){
            cores = system_cores - 1
        }else{
            cores = n_cores
            if(n_cores >= system_cores){
                warning("Cores greater than or equal to system cores, defaulting to system cores -1 for safe run \n")
            }
        }
        
        cat(sprintf('Setting up the cluster with %s cores\n',cores))
        cl <- makeCluster(cores) 
        registerDoSNOW(cl)
          
        
        
        ##Compute the elasticity when the media spend is increased
        for(change in spend_change){
            up_df_name = paste('Elasticity_up',change,sep = '_')
            down_df_name = paste('Elasticity_down',change,sep = '_')
            contrib_df_name = 'Media_Contribution'
            list_name = paste('Change',scales::percent(change),sep='_')
            ##DFs to hold the distributions of elasticity up,down and media contributions
            elas_df_down <- data.frame(matrix(nrow = 0,ncol = 13))
            elas_df_up <- data.frame(matrix(nrow = 0,ncol = 13))
            contrib_df <- data.frame(matrix(nrow = 0,ncol = 13))
            
            iterations <- length(n_draws)
            cat(sprintf('Starting the computation of Elasticity when spending is +%s mean spend\n',scales::percent(change)))
            pb <- txtProgressBar(max = iterations, style = 3)
            progress <- function(n) setTxtProgressBar(pb, n)
            opts <- list(progress = progress)
           
            elas_df_up <- foreach(draw = 1:length(n_draws),
                                  .combine = rbind,
                                  .packages = c("magrittr","tidyverse"),
                                  .options.snow = opts,
                                  .export = ls(.GlobalEnv)) %dopar% {
                           elas_df_draw <- compute_elas_contrib_draw(MCMC_alpha_K_S = MCMC_alpha_K_S,
                                              MCMC_beta_base_control = MCMC_beta_base_control,
                                              MCMC_beta_media = MCMC_beta_media,
                                              model_params = model_params,
                                              mean_qy_spend = mean_qy_spend,
                                              mean_weekly_spend = mean_weekly_spend,
                                              data_XyZ = data_XyZ,
                                              draw = draw,
                                              spend_change = change,
                                              type = 'up')
    
                             elas_df_draw
                            }
            close(pb)
            cat(sprintf('Starting the computation of Elasticity when spending is -%s mean spend\n',scales::percent(change)))
            pb <- txtProgressBar(max = iterations, style = 3)
            progress <- function(n) setTxtProgressBar(pb, n)
            opts <- list(progress = progress)
            
             elas_df_down <- foreach(draw = 1:iterations,
                                  .combine = rbind,
                                  .packages = c("magrittr","tidyverse"),
                                  .options.snow = opts,
                                  .export = ls(.GlobalEnv)) %dopar% {
                           elas_df_draw <- compute_elas_contrib_draw(MCMC_alpha_K_S = MCMC_alpha_K_S,
                                              MCMC_beta_base_control = MCMC_beta_base_control,
                                              MCMC_beta_media = MCMC_beta_media,
                                              model_params = model_params,
                                              mean_qy_spend = mean_qy_spend,
                                              mean_weekly_spend = mean_weekly_spend,
                                              data_XyZ = data_XyZ,
                                              draw = draw,
                                              spend_change = change,
                                              type = 'down')
    
                             elas_df_draw
                            }
            close(pb)
            cat('Starting the computation of Media Contribution at mean spend\n')
            pb <- txtProgressBar(max = iterations, style = 3)
             progress <- function(n) setTxtProgressBar(pb, n)
            opts <- list(progress = progress)
            
             contrib_df <- foreach(draw = 1:length(n_draws),
                                  .combine = rbind,
                                  .packages = c("magrittr","tidyverse"),
                                  .options.snow = opts,
                                  .export = ls(.GlobalEnv)) %dopar% {
                           elas_df_draw <- compute_elas_contrib_draw(MCMC_alpha_K_S = MCMC_alpha_K_S,
                                              MCMC_beta_base_control = MCMC_beta_base_control,
                                              MCMC_beta_media = MCMC_beta_media,
                                              model_params = model_params,
                                              mean_qy_spend = mean_qy_spend,
                                              mean_weekly_spend = mean_weekly_spend,
                                              data_XyZ = data_XyZ,
                                              draw = draw,
                                              spend_change = change,
                                              type = 'contrib')
    
                             elas_df_draw
                            }
            close(pb)
            ##Summarize the dfs
            elas_df_up_sum <- summarize_draws(elas_df_up,'elasticity')
            elas_df_down_sum <- summarize_draws(elas_df_down,'elasticity')
            contrib_df_sum <- summarize_draws(contrib_df,'contribution')
            
            
            
            output_list_change = list(up = elas_df_up_sum,
                                      down = elas_df_down_sum,
                                      contrib = contrib_df_sum)
            names(output_list_change) <- c(up_df_name,down_df_name,contrib_df_name)
            temp_list = list(output_list_change)
            names(temp_list) <- list_name
            output_list <- append(output_list,temp_list)
            
        }
     stopCluster(cl)
    }else{
           for(change in spend_change){
            up_df_name = paste('Elasticity_up',change,sep = '_')
            down_df_name = paste('Elasticity_down',change,sep = '_')
            contrib_df_name = 'Media_Contribution'
            list_name = paste('Change',scales::percent(change),sep='_')
          
            elas_df_down <- data.frame(matrix(nrow = 0,ncol = 13))
            elas_df_up <- data.frame(matrix(nrow = 0,ncol = 13))
            contrib_df <- data.frame(matrix(nrow = 0,ncol = 13))
         
            for(draw in n_draws){
    
                  cat('\n')
                  cat(sprintf('Running the loop for %s draw\n',draw))

                  elas_df_draw_down <- compute_elas_contrib_draw(MCMC_alpha_K_S = MCMC_alpha_K_S,
                                              MCMC_beta_base_control = MCMC_beta_base_control,
                                              MCMC_beta_media = MCMC_beta_media,
                                              model_params = model_params,
                                              mean_qy_spend = mean_qy_spend,
                                              mean_weekly_spend = mean_weekly_spend,
                                              data_XyZ = data_XyZ,
                                              draw = draw,
                                              spend_change = change,
                                              type = 'down')
                  elas_df_draw_up <- compute_elas_contrib_draw(MCMC_alpha_K_S = MCMC_alpha_K_S,
                                              MCMC_beta_base_control = MCMC_beta_base_control,
                                              MCMC_beta_media = MCMC_beta_media,
                                              model_params = model_params,
                                              mean_qy_spend = mean_qy_spend,
                                              mean_weekly_spend = mean_weekly_spend,
                                              data_XyZ = data_XyZ,
                                              draw = draw,
                                              spend_change = change,
                                              type = 'up')
                  contrib_df_draw <- compute_elas_contrib_draw(MCMC_alpha_K_S = MCMC_alpha_K_S,
                                              MCMC_beta_base_control = MCMC_beta_base_control,
                                              MCMC_beta_media = MCMC_beta_media,
                                              model_params = model_params,
                                              mean_qy_spend = mean_qy_spend,
                                              mean_weekly_spend = mean_weekly_spend,
                                              data_XyZ = data_XyZ,
                                              draw = draw,
                                              spend_change = change,
                                              type = 'contrib')


                  elas_df_down <- rbind(elas_df_down,elas_df_draw_down)
                  elas_df_up <- rbind(elas_df_up,elas_df_draw_up)
                  contrib_df <- rbind(contrib_df,contrib_df_draw)
                }
                 ##Summarize the dfs
                  elas_df_up_sum <- summarize_draws(elas_df_up,'elasticity')
                  elas_df_down_sum <- summarize_draws(elas_df_down,'elasticity')
                  contrib_df_sum <- summarize_draws(contrib_df,'contribution')
            
            
            
                  output_list_change = list(up = elas_df_up_sum,
                                           down = elas_df_down_sum,
                                           contrib = contrib_df_sum)
                  names(output_list_change) <- c(up_df_name,down_df_name,contrib_df_name)
                  temp_list = list(output_list_change)
                  names(temp_list) <- list_name
                  output_list <- append(output_list,temp_list)
          }
      } 
    return(output_list)
}