##########################################################################################
###Optimizer function
##########################################################################################
optimizer <- function(budget_input,
                      var_footprint,
                      mean_spend_data,
                      var_id_constraint,
                      var_id_opt,
                      dma_media_excl_list,
                      optimization_lb_mat,
                      optimization_ub_mat,
                      var_spend_share_sdl,
                      flight_media_vec,
                      print_nloptr_log,
                      print_dma_opt,
                      var_n_flight_period,
                      flight_media_share,
                      prev_opt_solution = NULL,
                      ftol_rel = ftol_rel,
                      xtol_rel = xtol_rel){
  #'Get the optimized weekly budget spending 
  #'
  #'@description
  #'Function that outputs the optimized weekly budget spending based on the input budget
  #'for each footprint and input constraint. Objective is to maximize the weekly GA.
  #'
  #'@details
  #'Get the optimized budget spend for each dma based on the input quarterly budget specified 
  #'through the simulator. Takes the budgets as inputs along with other inputs and calculates 
  #'the optimal media mix by channel and scenario. The share of the spend by DMA is calculated based on the 
  #'mean of spend data across the entire modelling period. Leverages nloptr package and COBYLA method currently for 
  #'calculation of the optimal spend mix.
  #'The starting point for the optimizer is set to the lower bound of the spending. The lower bound
  #'is set currently at +30% and -30% while the UI limits are set at +20% and -20% of the average spending 
  #'by DMA and media.
  #'Based on the number of flight media specified, the scenarios are generated. If there are n flight media,
  #'the total number of scenarios are 2^n. 
  #'
  #'
  #'@return
  #'List of weekly and quarterly Gross Adds and spend by DMA and media by scenario. 
  #'
  #'@param budget_opt Input quarterly budget for Optimum 
  #'@param budget_sdl Input quarterly budget for Suddenlink 
  #'@param mean_spend_data_case Mean spend data for the SUDDENLINK DMA needed to calculate the share of spend by scenario
  #'@param var_id_constraint IDs of the media that are constrained i.e not included in the budget calculation or optimization across DMA
  #'@param var_id_opt ID/s of the OPTIMUM DMAs
  #'@param dma_media_excl_list List of any medias by DMA that need to be excluded like non-TV DMAs
  #'@param optimization_lb_mat The lower bound of optimization by media and DMA
  #'@param optimization_ub_mat The upper bound of optimization by media and DMA
  #'@param var_spend_share_sdl Any specific share of spend by DMA that should be used for Suddenlink DMA budget allocation
  #'@param flight_media_vec Vector of medias that need to be flighted i.e produce optimization results by including and excluding them at the same input budget level
  #'
  #'
  
  ##Set up the default media_list
  media_list = c(1:nbr_media)
  output_list = list()
  lb_opt = optimization_lb_mat
  ub_opt = optimization_ub_mat
  
  ##Index of flight media to obtain the DMA wise media optimization list
  flight_media_vec = media_names_vec[media_names_vec%in%flight_media_vec]
    
  ##Share of spend of non-flight media
  non_flight_media_share = 1- flight_media_share
  
  
  ##Update the media_list to exclude the constraint media
  media_list <- media_list[!media_list%in%var_id_constraint]
  
  ####Exclude the medias that need not be optimized.
  media_list_dma = c(1:nbr_dma) %>% 
    purrr::map(~media_list)
  
  
  
  ##Exclude specific media from DMAs like TV from certain DMAs
  
  id_media_by_dma_excl = which(media_names_vec%in%names(dma_media_excl_list))
    
    
  ##Checks to see if any mean_spend_data is zero which is not already captured by exclude media
   #if(any(mean_spend_data[id_opt_case,] == 0))
   #{
       #zero_means <- data.frame(which(mean_spend_data == 0,arr.ind = T))
       #prob_media <- zero_means$col
       #prob_dma <- zero_means$row
       
       #missing_media <- zero_means %in%id_media_by_dma_excl
       
       #if(
       #stop(cat(sprintf("Zero means found for %s ",paste(,,sep = ':',collapse = ',\n'))))
       
   #}
  
  for(id in id_media_by_dma_excl){
    
    dma_list = dma_media_excl_list[[media_names_vec[id]]]
    media_list_dma[dma_list] = lapply(dma_list,function(x) media_list_dma[[x]][!media_list_dma[[x]]%in%id])
      
  }
  
  cat(sprintf("Running optimization for: %s and input budget: %s\n",var_footprint,format_money(budget_input,0)))
  #Final result object initialization
  quarterly_media_mix = 0*beta_media_mat
  ##Default input with no flight media
  all_flight_media_on = 0*beta_media_mat
  
  ##Get default share_spend_case when there are no flight media
  ##Subset of media_names after excluding both flight and constraint media
  ##Default lb and ub by media
  
  media_names_constraint = media_names_vec[media_list]
  #cat(media_names_constraint)
  
  ##Subsetting the mean_spend_data by case
  mean_spend_data_case = mean_spend_data[var_id_opt,]
                                      
  share_spend_case = mean_spend_data_case %>%  
                    dplyr::select(media_names_constraint) %>%      ##Select the media that are remaining after excluding the constraint media
                    dplyr::mutate(spend = rowSums(.)) %>%     ##Get the total spend by DMA
                    dplyr::mutate_at(.vars = vars(spend),.funs=~./sum(spend)) %>% ##Get the share by dividing DMA total with total SDL spend
                    dplyr::select(spend) %>%                  ##Extract the share vector
                    dplyr::pull()
  
  
  lb_df = lb_opt %>%
          dplyr::select(media_names_constraint) %>%
          dplyr::mutate('DMA_lb' = rowSums(.)) %>%
          dplyr::select('DMA_lb')
  
  ub_df = ub_opt %>%
          dplyr::select(media_names_constraint) %>%
          dplyr::mutate('DMA_ub' = rowSums(.)) %>%
          dplyr::select('DMA_ub')
  
  ##Initial combination of the entire data,share of spend and upper and lower bounds by DMA
  media_list_combinations = list('all_on' = media_list_dma)
  share_spend_case_list = list(share_spend_case)
  lb_dma_list = list(lb_df)
  ub_dma_list = list(ub_df)
  
  
  if(length(flight_media_vec)>0){
    ##Get all combinations of the flight media
    
    id_float_media = which(media_names_vec %in% flight_media_vec)
    ##Assuming when flight media are on, they are all on - Commenting out the combination generator - 11/02/2020
#     if(length(id_float_media)>1){
#       ##Generate combinations of media taken n at a time. n = c(1:length(id_flight_media))
#       cat(sprintf('Generating all the possible combinations of flight media. Number of combinations = %s\n',2^length(flight_media_vec)))
#       #possible_combinations = c(1:length(id_float_media)) %>% map(~combn(id_float_media,.,simplify = F))
#       possible_combinations = list()
#       for(i in c(1:length(id_float_media))){
#           temp <- combn(id_float_media,i,simplify = F)
#           possible_combinations <- append(possible_combinations,temp)
#           }
#     }else{
      possible_combinations = list(id_float_media)
    #}
    #print(cat('Possible_combinations:'))
    #print(length(possible_combinations))
    ##All the possible combinations of the flight media
    for(l in possible_combinations){
      # media_list_combinations = c(media_list_combinations,
      #                             lapply(l,function(x){
      #                               x<-unlist(x);
      #                               lapply(media_list_dma,
      #                                      function(x2)
      #                                        x2[!x2%in%x])
      #                             }
      #                             )
      # )
      ##Remove the excluded media from the default media_list to generate the excluded combination
      media_exclude_comb = media_list_dma %>%
                           purrr::map(~.[!.%in%l])
      
      #print(cat("\n"))
      ##Compute the share of spend,lb and ub by DMA by excluding the flight media
      flight_media = media_names_vec[l]
      media_names_subset = media_names_constraint[!(media_names_constraint%in%flight_media)]
      share_spend_case = mean_spend_data_case %>%  
                        dplyr::select(media_names_subset) %>%      ##Select the media that are remaining after excluding the constraint media
                        dplyr::mutate(spend = rowSums(.)) %>%     ##Get the total spend by DMA
                        dplyr::mutate_at(.vars = vars(spend),.funs=~./sum(spend)) %>% ##Get the share by dividing DMA total with total SDL spend
                        
                        dplyr::select(spend) %>%                  ##Extract the share vector
                        dplyr::pull()
      share_spend_case_list = c(share_spend_case_list,list(share_spend_case))
      
      lb_df = lb_opt %>%
              dplyr::select(media_names_subset) %>%
              dplyr::mutate('DMA_lb' = rowSums(.)) %>%
              dplyr::select('DMA_lb')
      
      lb_dma_list = c(lb_dma_list,list(lb_df))
      
      ub_df = ub_opt %>%
              dplyr::select(media_names_subset) %>%
              dplyr::mutate('DMA_ub' = rowSums(.)) %>%
              dplyr::select('DMA_ub')
      
      
      ub_dma_list = c(ub_dma_list,list(ub_df))
      
      media_excl_comb_list = list(media_exclude_comb)
      names(media_excl_comb_list) = paste(media_names_vec[l],'off',sep = '_',collapse = '_')
      
      ##Add the excluded media list to the entire list of media list combinations
      media_list_combinations = append(media_list_combinations,media_excl_comb_list)
    }
    
  }
  
  iter_count = c(1:length(media_list_combinations))
                                      
  #cat(sprintf('########### Iter Count: ###############%s\n',iter_count))

  ##Overwrite the computed share spend list with the manually entered list in case user specifies it as input
  if(!is.null(var_spend_share_sdl))
  {
    share_spend_case_list = list(rep(var_spend_share_sdl,length(share_spend_case_list)))
  }
  
  ##Compute the weekly budget by case - All on and Flight off
  
  budget_input_vec = rep(0,length(iter_count))
  
  flight_budget = (flight_media_share*budget_input)/var_n_flight_period
  non_flight_budget = (non_flight_media_share*budget_input)/nbr_period_q
                                      
  flight_on_budget = flight_budget + non_flight_budget
  flight_off_budget = non_flight_budget

  cat(sprintf('Budget when flight media on:%s\n',format_money(flight_on_budget,0)))
  cat(sprintf('Budget when flight media off:%s\n',format_money(flight_off_budget,0)))
                                      
  budget_input_vec = c(flight_on_budget,flight_off_budget)
  
  for(iter in iter_count){
    ##Set the budget for each DMA by using SDL share of spend
    budget = rep(0,nbr_dma)
    #budget[var_id_opt] = budget_input 
    
    budget[var_id_opt] = budget_input_vec[iter]*share_spend_case_list[[iter]]
    
    media_to_optimize = media_list_combinations[[iter]]
    name_iter_list = names(media_list_combinations[iter])
    ###Default dataframe for all cases
    weekly_optimized_budget = as.data.frame(0*beta_media_mat)
    weekly_optimized_budget['Input_DMA_Budget'] = budget
    
    ##Set up the previous optimization solution mat
    if(!is.null(prev_opt_solution)){
      prev_solution_df = prev_opt_solution[[name_iter_list]][['Weekly_optimized_df']]
      prev_solution_mat <- prev_solution_df %>%
                           as.matrix()
    }else{
      prev_solution_mat = NULL
    }


    cat(sprintf('Running the optimizer when %s \n',gsub(pattern = '_',
                                                            replacement = ' ',
                                                            x = name_iter_list)))
    
    # ##Setup the names of different iterations based on the flight media
    # if(iter==1){
    #   ###The first iteration is when all the flight media are kept on
    #   name_iter_list = 'all_flight_on'
    #   cat('Running the optimizer when all flight media are on\n')
    #   
    # }else if((length(flight_media_vec)>0) &(iter == max(iter_count))){
    #   ##The last iteration is when all flight media are turned off
    #   name_iter_list = 'all_flight_off'
    #   cat('Running the optimizer when all flight media are off\n')
    # }else if(length(flight_media_vec) >0 & (iter != max(iter_count))){
    #   media_off = flight_media_vec[iter-1]
    #   cat(sprintf('Running the optimizer when %s media is off\n',media_off))
    #   
    #   name_iter_list = paste(media_off,'off',sep='_')
    # }
    
    
    cat(sprintf("###Optimization starts for scenario no.%s###########\n",iter))
    count_dma = 0
    for(dma in dma_names_vec[var_id_opt]){
      count_dma = count_dma + 1
      idxdma = which(dma == dma_names_vec)
      media_list_dma = media_to_optimize[[idxdma]]
      
      if(!is.null(prev_solution_mat)){
        prev_solution_dma = prev_solution_mat[idxdma,media_list_dma]
      }else{
        prev_solution_dma = NULL
      }


      weekly_optimized_budget_dma = optimize_MMM_weekly(dma_budget = budget[idxdma],
                                                        idxdma = idxdma,
                                                        media_list_dma = media_list_dma,
                                                        lb_opt_dma = as.numeric(lb_opt[idxdma,media_list_dma]),
                                                        ub_opt_dma = as.numeric(ub_opt[idxdma,media_list_dma]),
                                                        ratio_dma_opt = as.numeric(spend_activity_ratio_dma_df[idxdma,media_list_dma]),
                                                        print_nloptr_log = print_nloptr_log,
                                                        print_dma_opt = print_dma_opt,
                                                        prev_solution_dma = prev_solution_dma,
                                                        ftol_rel = ftol_rel,
                                                        xtol_rel = xtol_rel)
      
      weekly_optimized_budget[idxdma,media_list_dma] = weekly_optimized_budget_dma
      
      
    }
    cat(sprintf("#####Optimization ran for %s DMA########\n",count_dma))
    
    optimized_ga = gross_add_weekly(weekly_optimized_budget,
                                    min_media_mat = min_media_mat,
                                    max_media_mat = max_media_mat,
                                    ratio_dma = spend_activity_ratio_dma_df,
                                    prospect_mat_media = prospect_mat_media,
                                    y_max = target_max_vec,
                                    beta_media_df = beta_media_mat,
                                    K,S,
                                    target_capita_norm_col
                                    )
    
    dma_weekly_ga = ceiling(apply(optimized_ga,sum,MARGIN = 1))    
   
    weekly_optimized_budget = data.frame(weekly_optimized_budget,target_var = dma_weekly_ga)
    names(weekly_optimized_budget)[names(weekly_optimized_budget) == 'target_var'] <- target_var
    
    optimized_ga = data.frame(optimized_ga,target_var = dma_weekly_ga)
    names(optimized_ga)[names(optimized_ga) == 'target_var'] <- target_var
    ##Append the lower and upper bound by DMA to the output
    weekly_optimized_budget = data.frame(weekly_optimized_budget,
                                         'DMA_lb' = lb_dma_list[[iter]]$DMA_lb,
                                         'DMA_ub' = ub_dma_list[[iter]]$DMA_ub)
    
    iter_list = list('Weekly_optimized_df' = weekly_optimized_budget,
                     'Weekly_optimized_ga' = optimized_ga,
                     "Media_list" = media_to_optimize)
    
    
    
    output_list[[name_iter_list]] <- iter_list
  }
  output_list[['share_spend_case_list']] <- share_spend_case_list
  return(output_list)
}