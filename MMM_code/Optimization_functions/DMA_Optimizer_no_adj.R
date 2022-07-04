
###############################################################################################################################################
# cobyla All DMAs
###############################################################################################################################################
gross_add_cobyla <- function(media, idxdma, media_list){
  #'
  #'Compute the Weekly Media Gross Adds for the DMA
  #'
  #'@description
  #'Function to compute the Media Gross Adds
  #'
  #'@details
  #'Compute the weekly gross adds for given weekly spend. Uses the shape
  #'parameters K and S obtained from the model to compute the media gross adds. Used to
  #'compute the objective function for the optimization
  #'
  #'@return 
  #'Returns the media gross adds for a given DMA
  #'
  #'@param media The media matrix of the given DMA
  #'@param idxdma The id of the DMA for which the optimization is being run
  #'@param media_list List of media to optimized
  #'
  
  
  media_GA = c(rep(0, length(media_list)))
  
  idx = 0
  for(idxmedia in media_list){
    idx = idx +1
    #normalize the media to evaluate the Hill  
    dma = dma_names_vec[idxdma]
    channel = media_names_vec[idxmedia]
    media_norm = normalize_x_dma(x=media[idx],min_media_mat,max_media_mat,prospect_mat_media,media = channel,dma = dma)
    if(media_norm <=0){
      media_GA[idx]=0
    } else {
      media_GA[idx] = beta_media_mat[[idxdma,idxmedia]]*
        hill(media_norm, K = K[[channel]], S = S[[channel]])*
        target_max_vec[idxdma]*prospect_mat_media[[idxdma,idxmedia]]
    }
    
  }#idxmedia
  media_GA = as.numeric(media_GA)
  #names(media_GA) <- media_names_prospect
  media_GA
  
}

#objective function for idx period pred time
eval_f0_opt <- function(spend, B, dma, ratio, media_list){
  #'
  #'Objective function of the optimization
  #'
  #'@description 
  #'Function to specify the objective function of the optimization
  #'
  #'@details
  #'This function specifies the objective of the optimization which is to maximize the weekly
  #'Gross Adds. We use the weekly budget to compute the media GA and the objective is to maximize
  #'the media Gross Adds
  #'
  #'@return
  #'Returns the media Gross Adds for the DMA. Negative of the Gross Adds is computed as nloptr 
  #'minimizes the objective function by default.
  #'
  #'@param spend Weekly spend of DMA
  #'@param B Quarterly budget allotted to DMA
  #'@param ratio Ratio of activity to spend
  #'@param constraint Direct Mail constraint variable
  #'@param media_list List of media to be optimized
  #'
  #'@references 
  #'nloptr package: https://cran.r-project.org/web/packages/nloptr/nloptr.pdf
  #' 
  media = spend*ratio
  ga = sum(gross_add_cobyla(media, idxdma=dma, media_list))
  #message(ga)
  return(- ga )
}

#constraint function
eval_g0_opt <- function(spend, B, dma, ratio, media_list){
  #'Constraint function to the optimizer
  #'
  #'@description
  #'Function to specify the budget constraint of the optimization
  #'
  #'@details
  #'Function that defines the budget constraint of the optimizer. 
  #'Constraint: Total weekly spend*number of weeks in the period = Input budget
  #'This is specified by the DMA. Currently as we are using the COBYLA algorithm
  #'under the 'nloptr' package. This does not support equality constraint, so, the
  #'equality constraint is broken down into two inequality constraints to ensure that
  #'the sum(weekly spend)*number of weeks = budget[dma]
  #'
  #'@return 
  #'Returns a vector of two opposite inequalities that translate to the constraint of the budget
  #'
  #'@param spend Weekly spend of DMA
  #'@param B Quarterly budget allotted to DMA
  #'@param ratio Ratio of activity to spend
  #'@param constraint Direct Mail constraint variable
  #'@param media_list List of media that need to be optimized
  #'
  #'@references 
  #'nloptr package: https://cran.r-project.org/web/packages/nloptr/nloptr.pdf
  #'
  eq_constraint = c(0,0)
  eq_constraint[1] = sum(spend)*nbr_period_q - B
  eq_constraint[2] = B - sum(spend)*nbr_period_q
  #message(spend)
  #message(contraint)
  return(  eq_constraint)
}

optimize_MMM_weekly_no_adj <- function(idxdma,
                                dma_budget,
                                media_list_dma,
                                lb_opt_dma,
                                ub_opt_dma,
                                ratio_dma_opt,
                                print_nloptr_log = print_nloptr_log,
                                print_dma_opt = print_dma_opt,
                                prev_solution_dma = NULL,
                                ftol_rel = 1e-10,
                                xtol_rel = 1e-10){
  #'Get the optimized weekly budget spending by DMA 
  #'
  #'@description
  #'Function that outputs the optimized weekly budget spending based on the input budget
  #'for each DMA. Objective is to maximize the weekly GA.
  #'
  #'@details
  #'Get the optimized budget spend for each dma based on the input quarterly budget specified 
  #'through the optimizer. Takes the budget and list of media by DMA as inputs and calculates 
  #'the optimal media mix by channel.Leverages nloptr package and COBYLA method currently for 
  #'calculation of the optimal spend mix.
  #'The starting point for the optimizer is set to the lower bound of the spending. The lower bound
  #'is computed and inputed in from the optimizer.
  #'For smoothening, the starting point is set to the previous solution point rather than the lower bound.
  #'
  #'
  #'@return
  #'Optimized budget vector by DMA
  #'
  #'@param idxdma ID of the DMA for which the optimization is being run for
  #'@param dma_budget Input quarterly budget for the DMA
  #'@param media_list_dma List of the media that need to be optimized for the DMA
  #'@param lb_opt_dma Lower bound of optimization for the DMA by media
  #'@param ub_opt_dma Upper bound of optimization for the DMA by media
  #'@param ratio_dma_opt Spend/activity ratio by DMA for activity based models to convert activity metrics to spend
  #'@param prev_solution_dma Solution of the previous budget point. Previous budget point is current budget - step_size. Used for smoothening
  #'@param ftol_rel Tolerance level set for the function according to nloptr optimization
  #'@param xtol_rel Tolerance level set for x according to nloptr optimization
  
  
  
  ##Set up the parameters you need for the optimization
  media_opt_dma = media_list_dma
  dma_name = dma_names_vec[idxdma]
  x0_opt = lb_opt_dma
  ##Initialize the starting point of optimization as the lower bound
  if(is.null(prev_solution_dma)==F){
    x0_opt = prev_solution_dma
  }
  
  
  
  cobyla_opt = nloptr(
    ratio = ratio_dma_opt,
    dma = idxdma,
    x0 = x0_opt,
    eval_f= eval_f0_opt,
    lb = lb_opt_dma,
    ub = ub_opt_dma,
    eval_g_ineq = eval_g0_opt,
    media_list = media_opt_dma,
    B = dma_budget,
    opts = list("algorithm"="NLOPT_LN_COBYLA", 
                ftol_rel = ftol_rel,
                xtol_rel = xtol_rel, 
                maxeval = 1e4
    )
  )
  if(print_nloptr_log){
    cat(paste('NLOPTR fit for',dma_name))
    cat("\n")
    cat(print(cobyla_opt))
  }
  
  weekly_optimized_solution_dma = cobyla_opt$solution
  if(print_dma_opt){
    cat(paste(dma_name,
              " Budget ", format_money(dma_budget,0),
              " Solution" , format_money(sum(weekly_optimized_solution_dma)*nbr_period_q,0), 
              sep = " "))
    cat('\n')
    flush.console()
  }
  
  # ##Adjusting the solution if the input budget doesn't match the optimized budget
  # if(round(sum(weekly_optimized_solution_dma)*nbr_period_q) != round(dma_budget)){
  #   cat('Budget mismatch has occurred. Adjusting the optimized solution using reweighting\n')
  #   ##If the allocated budget is at min or max by media level, don't adjust those media in the respective cases.
  #   ##Reweight only the ones that can be adjusted
  #   ##When removing budget, dont remove from already min media
  #   ##When adding budget, dont add to already max media
    
  #   ##Difference rounded to nearest dollar to reduce rounding issue adjustments
  #   budget_diff <- (dma_budget - sum(weekly_optimized_solution_dma)*nbr_period_q)/nbr_period_q
  #   #cat(sprintf('Budget Dif:%s\n',budget_diff))
    
  #   diff_w_lb <- weekly_optimized_solution_dma - lb_opt_dma
  #   diff_w_ub <- ub_opt_dma - weekly_optimized_solution_dma
    
  #   ratio_budget_vec <- rep(0,length(weekly_optimized_solution_dma))
  #   adjustable_media <- NULL
  #   ##Media available for adjustment
  #   if(budget_diff>0){
  #     adjustable_media <- which(diff_w_ub>0)
  #     ratio_budget_vec[adjustable_media] <- diff_w_ub[adjustable_media]/sum(diff_w_ub[adjustable_media])
  #   }else if(budget_diff <0){
  #     adjustable_media <- which(diff_w_lb>0)
  #     ratio_budget_vec[adjustable_media] <- diff_w_lb[adjustable_media]/sum(diff_w_lb[adjustable_media])
  #   }else{
  #     ##No difference in optimized and input budgets, no adjustment needed
  #     adjustable_media <- NULL
  #     ratio_budget_vec <- NULL
  #     return(weekly_optimized_solution_dma)
  #   }
  #   #adjustable_media <- ifelse(budget_diff>0,which(diff_w_ub>0),which(diff_w_lb>0))
  #   #cat(sprintf('Adjustable Media: %s\n',paste0(adjustable_media,sep=',')))
  #   if(!is.null(adjustable_media)){
  #     adj_factor <- rep(0,length(weekly_optimized_solution_dma))
      
  #     ##Compute the ratio only for the adjustable media
  #     #ratio_budget_vec <- diff_w_lb[adjustable_media]/sum(diff_w_lb[adjustable_media])
      
  #     adj_factor[adjustable_media] <- budget_diff*ratio_budget_vec[adjustable_media]
  #     #cat(sprintf('Adjustment factor: %s\n',paste0(adj_factor,sep=',')))
  #     weekly_optimized_solution_dma <- weekly_optimized_solution_dma + adj_factor
  #     cat(sprintf('Output budget after adjustment:%s',format_money(sum(weekly_optimized_solution_dma)*nbr_period_q,0)))
      
  #   }else{
  #     cat('Cannot perform adjustment. All media are at extremes (min/max)\n')
  #   }
    
  # }
  return(weekly_optimized_solution_dma)
}
