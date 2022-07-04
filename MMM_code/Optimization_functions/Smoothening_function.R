#source('alt_optimizer.R')
smoothen_eval <- function(weekly_spend_df,
                          weekly_target_df,
                          media_list,
                          optimization_lb_mat,
                          optimization_ub_mat,
                          id_opt_case,
                          print_nloptr_log = print_nloptr_log,
                          print_dma_opt = print_dma_opt,
                          nsmooth = 4)
{
  #'Smoothen evaluation curve
  #'
  #'@description
  #'Function to take the evaluation results from optimizer and smoothen out the sudden dips in GA
  #'at various spend levels
  #'
  #'@details
  #'This is a stripped down version of the optimizer code in the server.R
  #'The code is by DMA level and there are no iterations.
  #'It computes the lagged GA by spend and sees if the difference is continuously increasing
  #'After identifying the problematic points, it picks the last point where the difference > 0 
  #'and uses that as the starting point for the optimization of the problematic point
  #'
  #'@param weekly_spend_df Output from the test_case function which has GA at various spend levels
  #'@param constraint Whether the spending is constrained on DM
  #'@param is_opt Is the smoothening being done for OPT?
  #'@param dates Period for which the smoothening is being done for
  #'@param is_dm_slider Is the input for the DM slider?
  #'
  #'@return Smoothened weekly_spend_df
  #'
  #'
  #'
  
  lb_opt = optimization_lb_mat
  ub_opt = optimization_ub_mat
  weekly_spend_df_alt <- weekly_spend_df
  weekly_target_df_alt <- weekly_target_df
  idxdma_list <- c(1:length(dma_names_vec[id_opt_case]))
  iteration_count <- 1
  repeat{
    cat(sprintf('In iteration:%d',iteration_count))
   
    for(idxdma in idxdma_list){
      DMA = dma_names_vec[id_opt_case][idxdma]
     
      dma_res <- weekly_spend_df_alt %>% 
                 dplyr::filter(dma == DMA) %>%
                 dplyr::mutate('lagged_TGA' = lag(!!rlang::sym(target_var))) %>%
                 dplyr::mutate('diff_TGA' = !!rlang::sym(target_var) - lagged_TGA) %>%
                 tidyr::replace_na(list(diff_TGA = 0))
      
      #dma_res$lagged_TGA <- dplyr::lag(dma_res[target_var])
      #dma_res$diff_TGA <- dma_res[target_var] - dma_res$lagged_TGA
      #dma_res$diff_TGA[is.na(dma_res$diff_TGA)] <- 0
      correct_points <- which(dma_res$diff_TGA>=0)
      problem_points <- which(round(dma_res$diff_TGA)<0)
      prev_best_solution_points <- sapply(problem_points,FUN = function(x){max(correct_points[correct_points<x])})
      
      
      
      ##Determine what is the spend variable used based on the scenario. 
      ##is_dm_slider determines if we are trying to smoothen dm weekly_spend
      ##is_opt determines if we are smoothening the OPT budget

      
      if(length(problem_points)>0){
        
        prev_best_solution_budgets <- dma_res[prev_best_solution_points,'Input_quarterly_budget']
        for(i in c(1:length(problem_points)))
        {
          corrected_weekly_spend = 0*beta_media_mat
          colnames(corrected_weekly_spend) <- media_names_vec
          ##Reference: https://stackoverflow.com/questions/54919400/dplyr-filter-using-dynamic-column-name-and-dynamic-value/54919698#54919698
          ##Using dynamic name of the spend_variable to filter
          prev_best_solution <- dma_res%>%filter(Input_quarterly_budget==prev_best_solution_budgets[i])%>%select(media_names_vec)
          

          dma_budget = dma_res$Input_DMA_Budget[problem_points[i]]
          media_list_dma = media_list[[idxdma]]
          
         
          id_dma <-  which(dma_names_vec==DMA)
          new_res <- optimize_MMM_weekly(dma_budget = dma_budget,
                                         idxdma = id_dma,
                                         media_list_dma = media_list_dma,
                                         lb_opt_dma = as.numeric(lb_opt[id_dma,media_list_dma]),
                                         ub_opt_dma = as.numeric(ub_opt[id_dma,media_list_dma]),
                                         ratio_dma_opt = as.numeric(spend_activity_ratio_dma_df[id_dma,media_list_dma]),
                                         print_nloptr_log = print_nloptr_log,
                                         print_dma_opt = print_dma_opt,
                                         prev_solution_dma = as.numeric(prev_best_solution[media_list_dma])
          )
          ##Assign the corrected spend of the respective DMA to the matrix
          corrected_weekly_spend[id_dma,media_list_dma] <- new_res
          optimized_ga = gross_add_weekly(corrected_weekly_spend,
                                          min_media_mat = min_media_mat,
                                          max_media_mat = max_media_mat,
                                          ratio_dma = spend_activity_ratio_dma_df,
                                          prospect_mat_media = prospect_mat_media,
                                          y_max = target_max_vec,
                                          beta_media_df = beta_media_mat,K,S,
                                          target_capita_norm_col = target_capita_norm_col)
          
          dma_weekly_ga = apply(optimized_ga,sum,MARGIN = 1)
          
          corrected_ga <- dma_weekly_ga
          
          ##Apply the correction to the dma_res object
          dma_res[problem_points[i],target_var] <- corrected_ga[id_dma]
          dma_res[problem_points[i],media_names_vec] <- corrected_weekly_spend[id_dma,media_names_vec]
          
          ##Apply the correction to the target as well
          weekly_target_df_alt[problem_points[i],media_names_vec] <- optimized_ga[id_dma,media_names_vec]
          weekly_target_df_alt[problem_points[i],target_var] <- corrected_ga[id_dma]
        }
      }##Close if
      else{
        ##Do nothing as no correction needs to be applied
      }
      ##Drop the intermediate columns used to compute the problematic points
      dma_res <- dma_res %>% select(colnames(weekly_spend_df_alt))
      ##Assign the corrected df back to the weekly_spend_df6
      weekly_spend_df_alt[weekly_spend_df_alt$dma==DMA,] <- dma_res
      
    }
    if(iteration_count >= nsmooth || length(problem_points)==0 ){
      break
    }
    iteration_count <- iteration_count + 1
  }
  return(list('corrected_spend_df' = weekly_spend_df_alt,
              'corrected_target_df' = weekly_target_df_alt))
}
