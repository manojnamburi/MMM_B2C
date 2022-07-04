#'Evaluate Optimized Mix
#'
#'@description
#'Function to evaluate the simulator/optimizer output.
#'
#'
#'@param sim_res Result from the simulator function
#'@param constr_step_size_vec Vector specifying the step size for constraint media 
#'@return
#'List containing issue points for further investigation
#'
#'@details
#'This function is used to evaluate the results from the simulator function.
#'The main checks the function performs on the simulation results for every input budget are:
#'
#'1. If the media and the DMA budgets are in the bounded region of the optimization
#'
#'2. Whether the Optimization constraint of input budget being the same as the optimized budget has been fulfilled
#'
#'3. If the target is monotonically increasing with the budget
#'
#'For each scenario, the above checks are performed for both before and after smoothening at each input budget level.
#'The corrective measures to be taken to fix the issues if found are suggested at the end.
#'At the start of the function, summary of all the inputs given to the simulator are provided.
#'
source('Check_Bound.R')
eval_optim <- function(sim_res,constr_step_size_vec = NULL,var_n_flight_period = NULL){
  ##Use this function to evaluate the simulator/optimization results. Pass the simulator generated object to this function
  input_params_list <- sim_res$Input_Params
  ##Extract all the parameters from the list
  var_specific_input_budget <- input_params_list$var_specific_input_budget 
  data_XyZ <- input_params_list$data_XyZ                                                
  var_spend_period <- input_params_list$var_spend_period                                    
  var_footprint <- input_params_list$var_footprint                                  
  constraint_media_vec <- input_params_list$constraint_media_vec                                          
  flight_media_vec <- input_params_list$flight_media_vec                              
  var_optimization_lb <- input_params_list$var_optimization_lb
  var_optimization_ub <- input_params_list$var_optimization_ub
  var_lb_budget_sim <- input_params_list$var_lb_budget_sim
  var_ub_budget_sim <- input_params_list$var_ub_budget_sim
  optimization_lb_dma_list <- input_params_list$optimization_lb_dma_list                                              
  optimization_ub_dma_list <- input_params_list$optimization_ub_dma_list                                             
  var_id_opt <- input_params_list$var_id_opt                                             
  var_step_size <- input_params_list$var_step_size                                                                         
  var_exclude_zero_media <- input_params_list$var_exclude_zero_media                              
  dma_media_excl_list <- input_params_list$dma_media_excl_list
  var_spend_share_sdl <- input_params_list$var_spend_share_sdl
  var_do_smoothen <- input_params_list$var_do_smoothen
  mean_spend_data <- sim_res$Mean_Spend_Data
  spend_media_vec <- input_params_list$spend_media_vec
  non_spend_media_vec <- media_names_vec[which(!(media_names_vec%in%spend_media_vec))]
#   var_n_flight_period <- input_params_list$var_n_flight_period
#   special_media_vec <- input_params_list$special_media_vec
#   var_n_special_period <- input_params_list$var_n_special_period
  
  ##Indexes of both spend media and non-spend media
  spend_media_idx <- which(media_names_vec %in% spend_media_vec)
  non_spend_media_idx <- which(media_names_vec %in% non_spend_media_vec)
  

  ##Checks for flight media and var_n_flight_period
  if(!is.null(flight_media_vec)){
    if(is.null(var_n_flight_period)){
      stop(cat('Please specify var_n_flight_period for the number of flight periods for flight media\n'))
    }
  }
  
  

  ##Setting up the case id variable based on the footprint for which the simulator is being evaluated
  if(var_footprint == 'OPT'){
    var_id_opt_case = var_id_opt
   }else{
    var_id_opt_case = -var_id_opt
   }
  
  ##Extract all the budget, target and check lists along with the graphs
  before_smoothening_budget_list <- sim_res$Without_smoothening_output$budget
  before_smoothening_target_list <- sim_res$Without_smoothening_output$target
  before_smoothening_check_list <- sim_res$Without_smoothening_output$check
  before_smoothening_sim_graphs <- sim_res$Without_smoothening_graphs
  
  post_smoothening_budget_list <- sim_res$Post_smoothening_output$budget
  post_smoothening_target_list <- sim_res$Post_smoothening_output$target
  post_smoothening_check_list <- sim_res$Post_smoothening_output$check
  post_smoothening_sim_graphs <- sim_res$Post_smoothening_graphs
  
  str_zero_weeks <- 'no'
  if(!is.null(var_exclude_zero_media)){
    str_zero_weeks <- paste(cleaned_names_list[var_exclude_zero_media],collapse = ',')
  }
  
  str_qy <- paste(gsub('_',' ',var_spend_period),collapse = ',')
  
  ##Assign default empty strings if the vectors are NULL
  if(is.null(constraint_media_vec)){str_constraint_media <- ''}else{str_constraint_media <- paste(cleaned_names_list[constraint_media_vec],sep=',')}
  if(is.null(flight_media_vec)){str_flight_media <- ''}else{str_flight_media <- paste(cleaned_names_list[flight_media_vec],sep=',')}
  if(is.null(flight_media_vec)){str_n_flight_period <- ''}else{str_n_flight_period <- paste(cleaned_names_list[flight_media_vec],var_n_flight_period,sep=':',collapse = ',')}
  #if(is.null(special_media_vec)){str_special_media <- ''}else{str_special_media <- paste(cleaned_names_list[special_media_vec],sep=',')}
  #if(is.null(special_media_vec)){str_n_special_period <- ''}else{str_n_special_period <- paste(cleaned_names_list[special_media_vec],var_n_special_period,sep=':',collapse = ',')}
  
  
  start_budget_opt <- min(before_smoothening_budget_list$all_on['Input_quarterly_budget'])
  end_budget_opt <- max(before_smoothening_budget_list$all_on['Input_quarterly_budget'])
  
  cat('Summary of the simulation:\n')
  ##Run for
  cat(sprintf('1. Evaluating the simulation results for: %s\n',var_footprint))
  ##Chosen optimization and simulation limits
  cat(sprintf('2. Optimization bounds chosen by %s zero spend weeks in mean calculated from %s periods:\n\tUpper = %s\n\tLower = %s\n',str_zero_weeks,str_qy,scales::label_percent()(var_optimization_ub-1),scales::label_percent()(var_optimization_lb-1)))
  cat(sprintf('3. Simulation bounds chosen:\n\tUpper = %s\n\tLower = %s\n',scales::label_percent()(var_ub_budget_sim-1),scales::label_percent()(var_lb_budget_sim-1)))
  ##Constrained media and flight media details
  cat(sprintf('4. Media constrained (Not included in Optimization): %s\n',str_constraint_media))
  cat(sprintf('5. Media flighted (Turned on and off): %s\n',str_flight_media))
  cat(sprintf('5.a. The number of periods of flight by media: %s\n',str_n_flight_period))
  #cat(sprintf('5.b. Special media (Not treated on for all weeks): %s\n',str_special_media))
  #cat(sprintf('The number of periods on by media: %s\n',str_n_special_period))
  ##Budget limits of the run/budget run
  if(!is.null(var_specific_input_budget)){
    cat(sprintf('6. Budget the simulator ran for: %s\n',format_money(var_specific_input_budget,0)))
  }else{
    cat(sprintf('6. Budget limits the simulator ran for: Start: %s    End: %s   Increments:%s\n',
                format_money(start_budget_opt,0),
                format_money(end_budget_opt,0),
                format_money(var_step_size,0)))
  }
  ##Special conditions if set - DMAs excluded by media, Special upper bounds set
  cat('7. Special conditions:\n')
  if(!is.null(dma_media_excl_list)){
    cat('DMAs excluded from optimization by media due to zero spending:\n')
    id_media_by_dma_excl = which(media_names_vec%in%names(dma_media_excl_list))
    
    for(idm in id_media_by_dma_excl){
      media_excl <- cleaned_names_list[media_names_vec[idm]]
      dma_vec <- dma_names_vec[dma_media_excl_list[[media_names_vec[idm]]]]
      str_dma <- paste(dma_vec,collapse = ',')
      cat(sprintf('For %s, DMA excluded: %s\n',media_excl,str_dma))
    }
  }
  ##If any special lower/upper bounds by media and DMA are set
  if(!is.null(optimization_lb_dma_list)){
    cat('Specific lower bounds set by DMA and media:\n')
    id_media_lb <- which(media_names_vec %in% names(optimization_lb_dma_list))
    
    for(idm in id_media_lb){
      media_lb <- cleaned_names_list[media_names_vec[idm]]
      lb_vec <- optimization_lb_dma_list[[media_names_vec[idm]]]
      dma_vec <- dma_names_vec[as.integer(names(lb_vec))]
      lb_vec_fmt <- format_money(lb_vec,0)
      cat(sprintf('For %s,specific lower bound set by DMA: %s\n',media_lb,paste(dma_vec,lb_vec_fmt,sep =':',collapse = ',')))
      
    }
  }
  if(!is.null(optimization_ub_dma_list)){
    cat('Specific lower bounds set by DMA and media:\n')
    id_media_ub <- which(media_names_vec %in% names(optimization_ub_dma_list))
    
    for(idm in id_media_ub){
      media_ub <- cleaned_names_list[media_names_vec[idm]]
      ub_vec <- optimization_ub_dma_list[[media_names_vec[idm]]]
      dma_vec <- dma_names_vec[as.integer(names(ub_vec))]
      ub_vec_fmt <- format_money(ub_vec,0)
      cat(sprintf('For %s,specific lower bound set by DMA: %s\n',media_ub,paste(dma_vec,ub_vec_fmt,sep =':',collapse = ',')))
      
    }
  }
  if(!is.null(var_spend_share_sdl)){
    cat('Using specific share spend sdl entered by user\n')
  }
  cat(rep('-',60))
  cat('\n')
  sim_scenarios <- names(before_smoothening_budget_list)
  cat('Evaluation starts\n')
  cat('Issues evaluated:\n')
  cat('1.DMA and media bounds\n')
  cat('2.Checks to see if input budget and optimized budget are same\n')
  cat('3.Checks to see if target is monotonically increasing with budget\n')
  cat('4.Checks to see if constrained target value is monotonically increasing with media (Not for specific simulated result)\n')
  cat('Cases of simulation based on flight media:\n')
  cat(paste(sapply(sim_scenarios,FUN = Caps,USE.NAMES = F),collapse = ',\n'))
  cat('\n')
  cat(rep('-',60))
  cat('\n')
  
  ##Before Smoothening
  bs_bound_check_res <- list()
  bs_unmatch_res <- list()
  bs_target_non_incr <- list()
  bs_target_non_incr_q <- list()
  
  ps_bound_check_res <- list()
  ps_unmatch_res <- list()
  ps_target_non_incr <- list()
  ps_target_non_incr_q <- list()
  
  for(scenario in sim_scenarios){
    cat(sprintf('For %s:\n',Caps(scenario)))
    check_df <- before_smoothening_check_list[[scenario]]
    budget_df <- before_smoothening_budget_list[[scenario]]
     
    ##------------------------------Checks for bounds - DMA and by media---------------------------------------------------------
    ##Use the check_bound function to see if all media and DMA are within the optimization bounds
    cat(rep('-',60))
      cat('\n')
    cat('\t\t1. DMA and Media Bound Check - Before Smoothening\n')
    cat(rep('-',60))
      cat('\n')
    bs_bound_check_res[[scenario]] <- check_bound(check_df)
    
    
    ##------------------------------Checks if Optimization objective was met---------------------------------------------------------
    ##Checks to see if input_budget by DMA matches the optimizated_budget
    cat(rep('-',60))
      cat('\n')
    cat('\t\t2. Constraint Check (Input budget = Optimized budget) - Before Smoothening\n')
    cat(rep('-',60))
      cat('\n')
    check_df <- budget_df %>% 
      dplyr::filter(DMA_budget_mismatch == 1) %>%
      dplyr::select(-Simulated_for)
    unmatch_res <- data.frame()
    if(nrow(check_df)!=0){
      cat('Optimized and input DMA budget mismatch found for the DMA and input budget:\n')
      unmatch_res <- check_df %>%
        dplyr::select(iteration_num,dma,Input_quarterly_budget,Input_DMA_Budget,dma_optimized_budget) 
      unmatch_res %>%
        knitr::kable() %>%
        print()
    }else{
      cat('All DMAs have input budget the same as optimized budget for all the simulated budgets!\n')
    }
    bs_unmatch_res[[scenario]] <- unmatch_res
    
    ##-----------------------------Check if the target value is monotonically increasing---------------------------------------------
    cat(rep('-',60))
    cat('\n')
   cat('\t\t3. Target Monotonicity Check - Before Smoothening\n')
    cat(rep('-',60))
      cat('\n')
    ##Extract the target_vec
    target_vec <- budget_df %>% 
      dplyr::group_by(Input_quarterly_budget) %>% 
      dplyr::summarize_at(.vars = vars(target_var),.funs = ~round(sum(.))) %>% 
      dplyr::select(target_var) %>% 
      dplyr::pull()
    ##Get the points at which the target_vec is not monotonically increasing and also near points to see the issue.
    target_not_incr <- which(!target_vec == cummax(target_vec))
    lower_points <- target_not_incr - 1
    upper_points <- target_not_incr + 1
    check_points <- sort(c(target_not_incr,lower_points,upper_points))
    
    target_not_incr_df <- budget_df %>%
      dplyr::filter(iteration_num %in% check_points)
    
    if(nrow(target_not_incr_df)!=0){
      cat('Some points found where target is not monotonically increasing:\n')
      target_not_incr_df %>%
        dplyr::group_by(iteration_num,Input_quarterly_budget) %>%
        dplyr::summarize_at(.vars = vars(Input_DMA_Budget,dma_optimized_budget,target_var), .funs = sum) %>%
        dplyr::select(iteration_num,Input_quarterly_budget,Input_DMA_Budget,dma_optimized_budget,target_var) %>%
        knitr::kable() %>%
        print()
      
    }else{
      cat('Target monotonically increasing for all budget values!\n')
    }
    bs_target_non_incr[[scenario]] <- target_not_incr_df
    
      
    ##If there is constrained media present, check for the monotonicity for each constrained media if step-size not zero
    
    if(!is.null(constraint_media_vec) & is.null(var_specific_input_budget)){
        cat(rep('-',60))
        cat('\n')
        cat('\t\t4. Target Monotonicity Check - Constrained media\n')
        cat(rep('-',60))
        cat('\n')
        if(length(constr_step_size_vec)!=length(constraint_media_vec))
          {
              stop(cat('Please specify step sizes for each constrained media\n'))
          } 
        names(constr_step_size_vec) <- constraint_media_vec
        ##IDs of the constrained media
        id_constr_vec <- which(media_names_vec%in%constraint_media_vec)
        
        ##Setup the share of spend for SDL DMA
        if(var_footprint == 'SDL'){
            mean_spend_data_sdl <- data.frame(mean_spend_data[-var_id_opt,])
    
            ##For spend media, calculate it based on overall share of spend and for other non-spend media use individual shares
            share_spend_sdl_mat <- 0*mean_spend_data_sdl

            ##Spend share for all spend media
            share_spend_sdl_vec <- mean_spend_data_sdl %>%
                                     dplyr::select(spend_media_vec) %>%
                                     dplyr::mutate(spend = rowSums(.)) %>%     ##Get the total spend by DMA
                                     dplyr::mutate_at(.vars = dplyr::vars(spend),.funs = ~./sum(spend)) %>% ##Get the share by dividing DMA total with total SDL spend
                                     dplyr::select(spend) %>%                  ##Extract the share vector
                                     dplyr::pull()

             share_spend_sdl_mat[,spend_media_idx] <- share_spend_sdl_vec

            ##Compute the share of spend for all media by DMA and media
            share_non_spend_sdl_mat <- mean_spend_data_sdl %>%
                                   dplyr::select(non_spend_media_vec) %>%
                                   dplyr::mutate_at(.vars = dplyr::vars(non_spend_media_vec),.funs = ~./sum(.)) %>%
                                   dplyr::select(non_spend_media_vec) %>%
                                   as.matrix()

            #cat(dim(share_spend_sdl_mat))

            share_spend_sdl_mat[,non_spend_media_idx] <- share_non_spend_sdl_mat
      }else{
            share_spend_sdl_mat <- c(1)
      }
        
       ##For each constraint media verify if the corresponding target value is monotonically increasing
        for(id_constr in id_constr_vec){
            var_constr_step_size <- constr_step_size_vec[media_names_vec[id_constr]]
            if(var_constr_step_size == 0){
                cat(sprintf('%s is being skipped from monotonicity check as step-size is zero \n',media_names_vec[id_constr]))
                next
                }
            cat(sprintf('Checking the target monotonicity for %s:\n',cleaned_names_list[media_names_vec[id_constr]]))
        ##Setup the minimum and maximum bounds of running for each constrained media
                min_constr_val <-  plyr::round_any(sum(mean_spend_data[var_id_opt_case,id_constr]*var_optimization_lb, na.rm = TRUE)*nbr_period_q, var_constr_step_size,f = ceiling)
                max_constr_val <-  plyr::round_any(sum(mean_spend_data[var_id_opt_case,id_constr]*var_optimization_ub, na.rm = TRUE)*nbr_period_q, var_constr_step_size,f = floor)
                ##For display, format either by $ or ,
                start_val = ifelse(media_names_vec[id_constr]%in%spend_media_vec,
                                   format_money(min_constr_val,0),
                                   format(min_constr_val,big.mark = ','))
                end_val = ifelse(media_names_vec[id_constr]%in%spend_media_vec,
                                 format_money(max_constr_val,0),
                                 format(max_constr_val,big.mark = ','))
                incr_val = ifelse(media_names_vec[id_constr]%in%spend_media_vec,
                                 format_money(var_constr_step_size,0),
                                 format(var_constr_step_size,big.mark = ','))
                                   
                cat(sprintf('Target monotonicity check will run from %s to %s with increments of %s\n',start_val,end_val,incr_val))
                constr_media_range <- seq(from = min_constr_val, to = max_constr_val , by = var_constr_step_size )
                output_df <- data.frame(matrix(nrow=0,ncol = 3))
                colnames(output_df) <- c('Iteration_Num','Constrained_Budget','Quarterly_Target')
                idx = 1
                ##Get the target value at the quarterly level for each of the input media value
                for(constr_media in constr_media_range){

                    constr_media_period <- constr_media/nbr_period_q
                    constr_budget_mat <- 0*beta_media_mat

                    if(var_footprint == 'OPT'){
                        constr_budget_mat[var_id_opt,id_constr] <- constr_media_period
                    }else{
                        constr_budget_mat[-var_id_opt,id_constr] <- share_spend_sdl_mat[,id_constr]*constr_media_period
                    }
                    ##Compute the target value for the constrained media
                    constr_target_mat <- gross_add_weekly(solution = constr_budget_mat,
                                                          min_media_mat = min_media_mat,
                                                          max_media_mat = max_media_mat,
                                                          ratio_dma = spend_activity_ratio_dma_df,
                                                          y_max = target_max_vec,
                                                          prospect_mat_media = prospect_mat_media,
                                                          beta_media_df = beta_media_mat,
                                                          target_capita_norm_col = target_capita_norm_col,
                                                          K,S
                    )

                    quarterly_target <- nbr_period_q*sum(constr_target_mat[var_id_opt_case,id_constr])
                    temp_df <- data.frame('Iteration_Num' = idx,
                                          'Constrained_Budget' = constr_media,
                                          'Quarterly_Target' = quarterly_target)
                    idx = idx+1
                    output_df = rbind(output_df,temp_df)
            }
       target_vec <- output_df %>% 
                    dplyr::group_by(Constrained_Budget) %>% 
                    dplyr::summarize_at(.vars = vars(Quarterly_Target),.funs = ~round(sum(.))) %>% 
                    dplyr::select(Quarterly_Target) %>% 
                    dplyr::pull()
      ##Get the points at which the target_vec is not monotonically increasing and also near points to see the issue.
      target_not_incr <- which(!target_vec == cummax(target_vec))
      lower_points <- target_not_incr - 1
      upper_points <- target_not_incr + 1
      check_points <- sort(c(target_not_incr,lower_points,upper_points))
      
      target_not_incr_df <- output_df %>%
        dplyr::filter(Iteration_Num %in% check_points)
      
      if(nrow(target_not_incr_df)!=0){
        cat('Some points found where target is not monotonically increasing:\n')
        target_not_incr_df %>%
          dplyr::group_by(iteration_num,Input_quarterly_budget) %>%
        dplyr::summarize_at(.vars = vars(Input_DMA_Budget,dma_optimized_budget,target_var), .funs = sum) %>%
        dplyr::select(iteration_num,Input_quarterly_budget,Input_DMA_Budget,dma_optimized_budget,target_var) %>%
          knitr::kable() %>%
          print()
}else{
        cat('Target monotonically increasing for all budget values!\n')
      }
    }
   }   

    ##------------------------------Plot the graphs to show the stability of the simulator--------------------------------------------
    cat('Plot of the simulation budget vs target variable before smoothening:\n')
    print(before_smoothening_sim_graphs[[scenario]])
    flush.console()
    cat(rep('-',60))
    cat('\n')
  }
  ##------------------------------Checks to see if the target is monotonically increasing at the quarterly level--------------------
    cat(rep('-',60))
    cat('\n')
    cat('\t\t3.a. Target Monotonicity Check (Quarterly)- Before Smoothening\n')
    cat(rep('-',60))
    cat('\n')
    ##Get the target dataframe for all on scenario which is present even when there is no flight media scenario
    all_on_target_df <- before_smoothening_target_list[['all_on']]
    all_on_target_df <- all_on_target_df %>%
                        dplyr::group_by(iteration_num,Input_quarterly_budget) %>%
                        dplyr::summarize_at(.vars = vars(target_var), .funs = ~round(sum(.)))
                        
    ##By default roll the weekly level results to quarterly for all-on scenario and add the flight media target value if present
     quarterly_target_df <- all_on_target_df %>%
                            dplyr::mutate_at(.vars = vars(target_var), .funs = ~.*nbr_period_q) %>%
                            dplyr::select(iteration_num,Input_quarterly_budget,'Quarterly_Target' = target_var)
     

     df_types <- sim_scenarios
     join_str <- sapply(df_types,FUN = function(x){paste0('_',x)},USE.NAMES = F)
     

     if(!is.null(flight_media_vec)){
       flight_df_name <- sim_scenarios[2]
       flight_off_target_df <- before_smoothening_target_list[[flight_df_name]]
       flight_off_target_df <- flight_off_target_df %>%
                        dplyr::group_by(iteration_num,Input_quarterly_budget) %>%
                        dplyr::summarize_at(.vars = vars(target_var), .funs = ~round(sum(.)))

       
       quarterly_target_df <- all_on_target_df %>%
                              dplyr::left_join(flight_off_target_df, by =c('iteration_num','Input_quarterly_budget'), suffix = join_str) %>%
                              dplyr::mutate_at(.vars = vars(matches(df_types[1])),.funs = ~.*var_n_flight_period) %>%
                              dplyr::mutate_at(.vars = vars(matches(df_types[2])), .funs = ~.*(nbr_period_q-var_n_flight_period)) %>%
                              tidyr::gather(key = type,value = value,-Input_quarterly_budget,-iteration_num) %>%
                              dplyr::group_by(iteration_num,Input_quarterly_budget) %>%
                              dplyr::summarize(Quarterly_Target = sum(value)) %>%
                              dplyr::select(iteration_num,Input_quarterly_budget,Quarterly_Target)
     }

     ##Check to see if the quarterly target is monotonically increasing or not
      target_vec <- quarterly_target_df %>%
                  dplyr::select(Quarterly_Target) %>% 
                  dplyr::pull()
      ##Get the points at which the target_vec is not monotonically increasing and also near points to see the issue.
      target_not_incr <- which(!target_vec == cummax(target_vec))
      lower_points <- target_not_incr - 1
      upper_points <- target_not_incr + 1
      check_points <- sort(c(target_not_incr,lower_points,upper_points))
      
      target_not_incr_df <- quarterly_target_df %>%
                            dplyr::filter(iteration_num %in% check_points)
      
      if(nrow(target_not_incr_df)!=0){
        cat('Some points found where target is not monotonically increasing at the quarterly level:\n')
        target_not_incr_df %>%
        dplyr::select(iteration_num,Input_quarterly_budget,Quarterly_Target) %>%
          knitr::kable() %>%
          print()
      }else{
        cat('Target monotonically increasing for all budget values!\n')
      }
      bs_target_non_incr_q[['Quarterly']] <- target_not_incr_df

  cat(rep('-',60))
      cat('\n')
  cat('Done with all checks before smoothening\n')
  cat(rep('-',60))
  cat('\n')
  
  
  ##After Smoothening
  if(var_do_smoothen){
    
    for(scenario in sim_scenarios){
      cat(sprintf('For %s:\n',Caps(scenario)))
      check_df <- post_smoothening_check_list[[scenario]]
      budget_df <- post_smoothening_budget_list[[scenario]]
      ##------------------------------Checks for bounds - DMA and by media---------------------------------------------------------
      ##Use the check_bound function to see if all media and DMA are within the optimization bounds
      cat(rep('-',60))
      cat('\n')
      cat('\t\t1. DMA and Media Bound Check - After Smoothening\n')
      cat(rep('-',60))
      cat('\n')
      ps_bound_check_res <- check_bound(check_df)
      
      ##------------------------------Checks if Optimization objective was met---------------------------------------------------------
      ##Checks to see if input_budget by DMA matches the optimizated_budget
      cat(rep('-',60))
      cat('\n')
      cat('\t\t2. Constraint Check (Input budget = Optimized budget) - After Smoothening\n')
      cat(rep('-',60))
      cat('\n')
      
      
        check_df <- budget_df %>% 
        dplyr::filter(DMA_budget_mismatch_ps == 1) %>%
        dplyr::select(-Simulated_for)
      
      unmatch_res <- data.frame()
      
      if(nrow(check_df)!=0){
        cat('Optimized and input DMA budget mismatch found for the DMA and input budget:\n')
        unmatch_res <- check_df %>%
          dplyr::select(iteration_num,dma,Input_quarterly_budget,Input_DMA_Budget,dma_optimized_budget,dma_optimized_budget_ps) 
        
        unmatch_res %>%
          knitr::kable() %>%
          print()
      }else{
        cat('All DMAs have input budget the same as optimized budget for all the simulated budgets!\n')
      }
      ps_unmatch_res[[scenario]] <- unmatch_res
      
      ##-----------------------------Check if the target value is monotonically increasing---------------------------------------------
      
        
      cat(rep('-',60))
      cat('\n')
      cat('\t\t3. Target Monotonicity Check - After Smoothening\n')
      cat(rep('-',60))
      cat('\n')
      
        
      ##Extract the target_vec
      target_vec <- budget_df %>% 
        dplyr::group_by(Input_quarterly_budget) %>% 
        dplyr::summarize_at(.vars = vars(target_var),.funs = ~round(sum(.))) %>% 
        dplyr::select(target_var) %>% 
        dplyr::pull()
      ##Get the points at which the target_vec is not monotonically increasing and also near points to see the issue.
      target_not_incr <- which(!target_vec == cummax(target_vec))
      lower_points <- target_not_incr - 1
      upper_points <- target_not_incr + 1
      check_points <- sort(c(target_not_incr,lower_points,upper_points))
      
      target_not_incr_df <- budget_df %>%
        dplyr::filter(iteration_num %in% check_points)
      
      if(nrow(target_not_incr_df)!=0){
        cat('Some points found where target is not monotonically increasing:\n')
        target_not_incr_df %>%
         dplyr::group_by(iteration_num,Input_quarterly_budget) %>%
        dplyr::summarize_at(.vars = vars(Input_DMA_Budget,dma_optimized_budget,target_var), .funs = sum) %>%
        dplyr::select(iteration_num,Input_quarterly_budget,Input_DMA_Budget,dma_optimized_budget,target_var) %>%
          knitr::kable() %>%
          print()
        
      }else{
        cat('Target monotonically increasing for all budget values!\n')
      }
      ps_target_non_incr[[scenario]] <- target_not_incr_df
        
      
      
      
      ##------------------------------Plot the graphs to show the stability of the simulator--------------------------------------------
      cat('Plot of the simulation budget vs target variable after smoothening:\n')
      print(post_smoothening_sim_graphs[[scenario]])
      flush.console()
      cat(rep('-',60))
      cat('\n')
    }
      ##------------------------------Checks to see if the target is monotonically increasing at the quarterly level--------------------
    cat(rep('-',60))
    cat('\n')
    cat('\t\t3.a. Target Monotonicity Check (Quarterly)- After Smoothening\n')
    cat(rep('-',60))
    cat('\n')
    ##Get the target dataframe for all on scenario which is present even when there is no flight media scenario
    all_on_target_df <- post_smoothening_target_list[['all_on']]
    all_on_target_df <- all_on_target_df %>%
                        dplyr::group_by(iteration_num,Input_quarterly_budget) %>%
                        dplyr::summarize_at(.vars = vars(target_var), .funs = ~round(sum(.)))
                        
    ##By default roll the weekly level results to quarterly for all-on scenario and add the flight media target value if present
     quarterly_target_df <- all_on_target_df %>%
                            dplyr::mutate_at(.vars = vars(target_var), .funs = ~.*nbr_period_q) %>%
                            dplyr::select(iteration_num,Input_quarterly_budget,'Quarterly_Target' = target_var)
     

     df_types <- sim_scenarios
     join_str <- sapply(df_types,FUN = function(x){paste0('_',x)},USE.NAMES = F)
     

     if(!is.null(flight_media_vec)){
       flight_df_name <- sim_scenarios[2]
       flight_off_target_df <- post_smoothening_target_list[[flight_df_name]]
       flight_off_target_df <- flight_off_target_df %>%
                        dplyr::group_by(iteration_num,Input_quarterly_budget) %>%
                        dplyr::summarize_at(.vars = vars(target_var), .funs = ~round(sum(.)))

       
       quarterly_target_df <- all_on_target_df %>%
                              dplyr::left_join(flight_off_target_df, by =c('iteration_num','Input_quarterly_budget'), suffix = join_str) %>%
                              dplyr::mutate_at(.vars = vars(matches(df_types[1])),.funs = ~.*var_n_flight_period) %>%
                              dplyr::mutate_at(.vars = vars(matches(df_types[2])), .funs = ~.*(nbr_period_q-var_n_flight_period)) %>%
                              tidyr::gather(key = type,value = value,-Input_quarterly_budget,-iteration_num) %>%
                              dplyr::group_by(iteration_num,Input_quarterly_budget) %>%
                              dplyr::summarize(Quarterly_Target = sum(value)) %>%
                              dplyr::select(iteration_num,Input_quarterly_budget,Quarterly_Target)
     }

     ##Check to see if the quarterly target is monotonically increasing or not
      target_vec <- quarterly_target_df %>%
                  dplyr::select(Quarterly_Target) %>% 
                  dplyr::pull()
      ##Get the points at which the target_vec is not monotonically increasing and also near points to see the issue.
      target_not_incr <- which(!target_vec == cummax(target_vec))
      lower_points <- target_not_incr - 1
      upper_points <- target_not_incr + 1
      check_points <- sort(c(target_not_incr,lower_points,upper_points))
      
      target_not_incr_df <- quarterly_target_df %>%
                            dplyr::filter(iteration_num %in% check_points)
      
      if(nrow(target_not_incr_df)!=0){
        cat('Some points found where target is not monotonically increasing at the quarterly level:\n')
        target_not_incr_df %>%
        dplyr::select(iteration_num,Input_quarterly_budget,Quarterly_Target) %>%
          knitr::kable() %>%
          print()
      }else{
        cat('Target monotonically increasing for all budget values!\n')
      }
      ps_target_non_incr_q[['Quarterly']] <- target_not_incr_df

  cat(rep('-',60))
  cat('\n')
  cat('Done with all checks after smoothening\n')
  cat(rep('-',60))
  cat('\n')
  }##After Smoothening checks done
  
  
  ##---------------------------------Corrective actions--------------------------------------------------------------------------------
  cat(rep('-',60))
  cat('\n')
  cat('Corrective steps to take to fix the issues found in the simulator\n')
  cat(rep('-',60))
  cat('\n')
  
  cat("1. If there are instances where the optimized budget doesn't fall in the bounds before smoothening,\n re-run the simulator for the problematic budget and set print_nloptr_log to T\n to see if optimizer converged or any other nloptr related issues.\n")
  cat("NLOPTR reference: https://cran.r-project.org/web/packages/nloptr/vignettes/nloptr.pdf\n")
  
  cat("2. If there are instances where the optimized budget is not the same as input DMA budget,\n investigate if the input budget has exceeded or is lower than the ub and lb for the DMA.\n If so, try re-running the optimizer with wider range of opt_lb and opt_ub.\n")
  cat("Other potential issues can be wrongly excluding some media from optimization for specific\n DMA even when the mean spend for that media and DMA is not zero.\n")
  
  cat("2. If after smoothening the input budget doesnt match the post smoothening budget,\n avoid using smoothening for this case or re-run with smaller var_n_smoothen.\n")
  
  cat("3. If the target is not monotonically increasing, try using the smoothened results.\n If still not fixed, use larger step_size for better results. When simulator shows no results for the var_n_smoothen,\n try increasing the number of smoothening.\n")
  
  cat('Returning all the issue points for investigation\n')
  
  output_list <- list('before_smoothening' = list('type1_issue' = bs_bound_check_res,
                                                  'type2_issue' = bs_unmatch_res,
                                                  'type3_issue' = bs_target_non_incr,
                                                  'type3_issue_q' = bs_target_non_incr_q),
                      'after_smoothening' = list('type1_issue' = ps_bound_check_res,
                                                 'type2_issue' = ps_unmatch_res,
                                                 'type3_issue' = ps_target_non_incr,
                                                 'type3_issue_q' = ps_target_non_incr_q))
}