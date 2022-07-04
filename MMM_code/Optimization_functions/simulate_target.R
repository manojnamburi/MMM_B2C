get_simulated_ga <- function(OPT_media_budgets_vec,
                             SDL_media_budget_vec,
                             mean_spend_data,
                             var_period,
                             by_mean = F){
  common_cols <- c('dma',media_names_vec,target_var)

  ##Default outputs for finalained media
  final_target_by_dma <- rep(0,length(dma_names_vec))
  final_budget_df <- data.frame(matrix(nrow = length(dma_names_vec),ncol = length(common_cols)-1))
  final_target_df <- data.frame(matrix(nrow = length(dma_names_vec),ncol = length(common_cols)-1))

  colnames(final_budget_df) <- c(media_names_vec,target_var)
  colnames(final_target_df) <- c(media_names_vec,target_var)

  ##Remove all the NAs to add to the final_df in case finalaint vector is null
  final_budget_df[is.na(final_budget_df)] <- 0
  final_target_df[is.na(final_target_df)] <- 0

  period_budget_mat <- 0*beta_media_mat


  

    ##Assuming that the input provided for the finalained budget is at the quarterly level,
    ##Converting it to the level of the data - weekly or monthly
    budget_period_OPT <- OPT_media_budgets_vec/nbr_period_q
    budget_period_SDL <- SDL_media_budget_vec/nbr_period_q

    mean_spend_data_sdl <- mean_spend_data[-var_id_opt,]

    ##Compute the share of spend for all media by DMA and media
    share_spend_sdl_mat <- mean_spend_data_sdl %>%
                           dplyr::select(media_names_vec) %>%
                           dplyr::mutate_at(.vars = dplyr::vars(media_names_vec),.funs = ~./sum(.)) %>%
                           dplyr::select(media_names_vec) %>%
                           as.matrix()


    period_budget_mat[var_id_opt,media_names_vec] <- budget_period_OPT
    period_budget_mat[-var_id_opt,media_names_vec] <- sweep(share_spend_sdl_mat, MARGIN=2, budget_period_SDL, `*`)

    ##Compute the target value for the finalained media
    period_target_mat <- gross_add_weekly(solution = period_budget_mat,
                                          min_media_mat = min_media_mat,
                                          max_media_mat = max_media_mat,
                                          ratio_dma = spend_activity_ratio_dma_df,
                                          y_max = target_max_vec,
                                          prospect_mat_media = prospect_mat_media,
                                          beta_media_df = beta_media_mat,K,S
    )
    
    final_budget_mat <- nbr_period_q*period_budget_mat
    final_target_mat <- nbr_period_q*period_target_mat #Scaling the weekly/monthly to quarterly
    
    final_target_by_dma <- rowSums(final_target_mat)
    
    final_budget_df[media_names_vec] <- final_budget_mat
    final_budget_df[target_var] <- final_target_by_dma

    final_target_df[media_names_vec] <- final_target_mat
    final_target_df[target_var] <- final_target_by_dma
    
     control_target_mat <- gross_add_control(control_mat,
                                          beta_control_df = beta_control_mat,
                                          control_names = control_names_vec,
                                          data_XyZ = data_XyZ,
                                          input_period = var_period,
                                          y_max = target_max_vec,
                                          target_capita_norm_col = target_capita_norm_col,
                                          by_mean = by_mean,
                                          nbr_period_q = nbr_period_q)
    
    media_target_vec <- final_target_df[[target_var]]
  ##Add the control GA to the media GA
  target_by_dma <- media_target_vec + rowSums(control_target_mat)
  names(target_by_dma) <- dma_names_vec

  ##Creating a dataframe for Total GA (Control + Media)
  tga_df <- data.frame('dma' = dma_names_vec,'TGA' = as.numeric(target_by_dma))
  tga_df <- addFootprintData(tga_df,metric_cols = 'TGA')
    
  control_ga_df <- data.frame('dma' = dma_names_vec, 'control_target' = rowSums(control_target_mat))
  
    
   return(list('budget' = final_budget_df,
               'target_by_media' = final_target_df,
               'target_by_dma' = tga_df,
               'control_target' = control_ga_df))
}