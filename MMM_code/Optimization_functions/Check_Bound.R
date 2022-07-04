#'Check if the media and DMA budgets are bound
#'
#'@description
#'Function to check if the media and DMA optimized budget are within the optimized range
#'
#'@param check_df Dataframe containing the flags set in the simulator function for the media and DMA bounds
#'@return
#'List containing the issue points when the media and DMA are not bounded within the optimization range
#'
#'@details
#'This function is used in the eval_optim function to check if there are any budget points and any DMA budgets
#'for which the budgets are within the optimization bounds.
#'
#'This function is used for both before and after smoothening.
#'
#'At each simulated budget, the flags generated from the simulate_optimization for bound checks are evaluated.
#'If any issues are found (flags set as 0), the issue points are returned for further investigation and fixing.
#'
check_bound <- function(check_df){
  ##Check to see if any of the media flags are set to 0 which indicates unbounded
  check_res <- check_df%>%
    dplyr::filter_at(.vars = vars(media_names_vec),any_vars(. ==0))
  media_unbound_res <- data.frame()
  dma_unbound_res <- data.frame()
  if(nrow(check_res)!=0){
    cat('Unbounded media found for the DMA and input budget:\n')
    media_unbound_res <- check_res %>%
      dplyr::select(iteration_num,dma,Input_quarterly_budget,media_names_vec)

    media_unbound_res%>%
      knitr::kable() %>%
      print()
  }else{
    cat('All media are within the optimization bounds!\n')
  }
  ##Check if all DMA are within the optimization bounds
  check_res <- check_df%>%
    dplyr::filter_at(.vars = vars(DMA_bound),any_vars(. ==0))

  if(nrow(check_res)!=0){
    cat('Unbounded DMA found for input budget:\n')
    dma_unbound_res <- check_res %>%
      dplyr::select(iteration_num,dma,Input_quarterly_budget,media_names_vec,DMA_bound)

    dma_unbound_res %>%
      knitr::kable() %>%
      print()
  }else{
    cat('All DMA are within the optimization bounds!\n')
  }
  return(list('DMA_unbound' = dma_unbound_res,
              'Media_unbound' = media_unbound_res))
}
