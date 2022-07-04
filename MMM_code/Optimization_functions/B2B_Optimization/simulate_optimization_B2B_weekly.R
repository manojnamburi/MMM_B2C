##Load the dependent functions
source('Optimization_functions/Optimization_function.R')
source('Optimization_functions/DMA_Optimizer.R')
#'Simulate optimization scenarios at various budget levels
#' 
#'@description
#'Function to simulate the optimizer results in various scenarios 
#' 
#'@details 
#'Function to simulate the optimizer at different budget levels and for various input scenarios
#'Ability to generate all the possible combinations of flight_media and also include/exclude certain
#'media from budget calculations. Also has an input to experiment with various share of spend levels for SDL
#'footprint. Also outputs the optimization before and after smoothening. 
#'Smoothening refers to ironing out kinks in the plot of spend vs target It identifies spend levels where
#'the resultant target level is lower at a step_size higher than current spend level and uses the current spend level
#'mix as the input to the optimizer.
#' 
#' In B2B, the key assumption that irrespective of the number of media being optimized, the weekly budget is the same is not true as flight media contribute to much higher percentage
#' of total spending. So, we use the share of spend by flight media and number of weeks they are on instead of that assumption.
#' 
#' The optimization happens by scenario and at weekly level
#' 
#'For var_approach, there are two possible options - 'Mean' and 'MinMax'
#'When using 'Mean' approach, all the quarters specified under var_spend_period are considered and quarterly mean is computed by dma which
#'is used to compute the lower and upper bounds of optimization. Use 'All' for considering all quarters
#'
#'When using 'MinMax' approach, first, quarterly mean is computed across different quarters ('Q1','Q2','Q3','Q4') by DMA or limited to the quarter 
#'specified in var_spend_period. Then min of all the quarterly mins and max of all quarterly max are computed. These are used for the lb and ub computation.
#'This provides a wider range of optimization by media. Use 'All' for considering all quarters
#'
#'Note: var_spend_period  is specified as 'QQ_YYYY' format 
#'Example: var_approach = 'Mean' var_spend_period = c('Q1_2019','Q2_2019','Q4_2019') 
#'         var_approach = 'MinMax' var_spend_period = c('Q1_2019','Q2_2019','Q4_2019') 
#'
#'Formula used for determining the optimization limits: (Based on the flight_media_vec and constraint_media_vec scenarios)
#'all_on_min_q_budget <- plyr::round_any(sum(mean_spend_data[id_opt_case,-id_constraint]*var_lb_budget_sim, na.rm = TRUE)*nbr_period_q, var_step_size,f = ceiling)
#'all_on_max_q_budget <- plyr::round_any(sum(mean_spend_data[id_opt_case,-id_constraint]*var_ub_budget_sim, na.rm = TRUE)*nbr_period_q, var_step_size,f = floor)
#'flight_off_min_q_budget <- plyr::round_any(sum(mean_spend_data[id_opt_case,-id_excl]*var_lb_budget_sim, na.rm = TRUE)*nbr_period_q, var_step_size,f = ceiling)
#'flight_off_max_q_budget <- plyr::round_any(sum(mean_spend_data[id_opt_case,-id_excl]*var_ub_budget_sim, na.rm = TRUE)*nbr_period_q, var_step_size,f = floor)
#'Here, mean_spend_data is calculated based on the inputs exclude zero weeks and the input spend period.
#'The id_excl is a vector of both constrained and flight media.
#'The id_constraint is a vector of only the constrained media.
#'nbr_period_q is a global parameter which indicates the number of periods in a quarter. If it is weekly, it is 13. If monthly it is 3.
#'
#' 
#'@return List containing the evaluation curves as well as optimized budget and target for the specified input budget limits/specific input budget.
#'
#'
#'@param data_XyZ Input dataframe which contains the media,target and control information the model was trained on. Needs to contain the columns 'period' for time column and 'dma' for geos.
#'@param control_mat Matrix of normalized control variables used in the model.
#'@param specific_input_budget Optional. If you want the simulator to run for just one input budget and not for a range, specify this parameter.
#'@param var_approach Whether to use mean for the specified quarters or min/max of quarters. See details for more info. Defaults to 'mean'
#'@param var_spend_period Spend period for which the mean spend values by DMA and media will be calculated. Defaults to 'All'. If specifying a quarter, please use the format 'Qq_YYYY' where q is the quarter number.
#'@param var_footprint For which footprint the simulator needs to run. 'OPT' or 'SDL' are the accepted values.
#'@param constraint_media_vec Vector of media that needs to be excluded from the simulation across all the geos/dmas.
#'@param flight_media_vec Vector of media that need to be flighted i.e compute the mix for when this media is both included as well as excluded for the same input budget level.
#'@param var_optimization_lb Lower limit for the Optimization. Defaults to 0.8. See details for more information. 
#'@param var_optimization_ub Upper limit for Optimization. Defaults to 1.2. See details for more information.
#'@param optimization_lb_dma_list List of any specific lower bounds for optimization by media. Format of input - list(media_name <- Named vector of bounds. Names are dma ids.)
#'@param optimization_ub_dma_list List of any specific upper bounds for optimization by media. Format of input - list(media_name <- Named vector of bounds. Names are dma ids.)
#'@param var_id_opt All the Optimum DMA ids.
#'@param var_step_size Increments that will be used to calculate the starting and ending of the simulation runs.               
#'@param var_exclude_zero_weeks Flag to indicate if zero weeks will be excluded when calculating the mean spend by media and DMA. Defaults to T.
#'@param dma_media_excl_list List of any medias which need to be removed from optimization for specific dmas.
#'@param var_spend_share_sdl Optional.Share of spend by Suddenlink DMAs. 
#'@param var_do_smoothen Optional. If the evaluation curve of the simulator is not smooth and has kinks, can help correct the kinks by using previous value as the starting point for optimization.
#'@param var_n_smoothen If smoothening is chosen, how many times should the process be repeated. Default is 4.
#'@param print_nloptr_log Print the NLOPTR optimization log. Default is F. (Better used with a specific budget)
#'@param print_dma_opt Print the Optimization outcome by DMA. Default is T.
#'@param n_flight_period Number of periods of flight media on. Needs to be the same length as flight_media_vec. Default is NULL.
#'@param spend_media_vec Vector of names of spend media. Default is media_names_vec.

##Simulator function
simulator <- function(data_XyZ,                                                ##Input data which contains spend columns
                      var_specific_input_budget = NULL,
                      var_approach = 'Mean',
                      var_spend_period = 'All',                                    ##Subset period for x0.usa
                      var_footprint = 'OPT',                                       ##Which footprint to simulate the optimization
                      constraint_media_vec = NULL,                                 ##Constraint media. Use empty list for no constraint
                      flight_media_vec = c(),                                      ##Which media need to be flighted
                      var_optimization_lb = .7,                                    ##Optimization upper and lower bounds              
                      var_optimization_ub = 1.3,
                      optimization_lb_dma_list = NULL,                             ##Specific lowerbound by DMA (DM ub)
                      optimization_ub_dma_list = NULL,                             ##Specific upperbound by DMA (DM ub)
                      var_id_opt = 17,                                             ##OPT DMA id
                      var_step_size = 100000,                                      ##Step increment for the budget                         
                      var_exclude_zero_media = NULL,                               ##For which media zero periods should be excluded
                      dma_media_excl_list = c(),                                   ##Non-TV DMAs.
                      var_spend_share_sdl = NULL,
                      var_do_smoothen = T,                                          ##Does smoothening of the evaluation curve be applied
                      var_n_smoothen = 4,                                          ##Number of smoothening to be applied
                      print_nloptr_log = F,                                        ##Print NLOPT log
                      print_dma_opt = T,                                            ##Print DMA Optimization log
                      var_n_flight_period = NULL,  
                      #n_flight_period = NULL,                                    ##Number of periods of flight media
                      spend_media_vec = media_names_vec,                            ##Names of spend media
                      flight_media_share = NULL,
                      var_cold_start = T,
                      ftol_rel = 1e-10,
                      xtol_rel = 1e-10                                            ##Share of spend of flight media
                      ){
 
  #start_logging()
  dma_list <- data_XyZ %>%
             dplyr::select(dma) %>%
             dplyr::distinct() %>%
             dplyr::arrange(match(dma,dma_names_vec)) %>%
             dplyr::pull()

  
##----------------------------------Input Checks-----------------------------------------------------------------
  if(!all(c('period','dma')%in%colnames(data_XyZ))){stop(cat('Please ensure the data_XyZ contains the period and dma column. Period column is the column which specifies the date of each row.\n'))}
  if(!all(spend_cols_vec %in% colnames(data_XyZ))){stop('Missing some spend columns. Please check and enter data again\n')}
  if(!all((constraint_media_vec%in%media_names_vec)))stop('Wrong media set in the constraint parameter\n')
  if(!var_footprint%in%c('SDL','OPT','Both'))stop("Select proper footprint - 'SDL' or 'OPT' or 'Both'\n")
  if(!is.null(var_spend_share_sdl)) {
     if((length(var_spend_share_sdl)!= (length(dma_list)-length(id_opt))) 
     ){
       stop(cat('Please check if the share of spend is specified for all SDL DMA\n'))
     }
    if(sum(var_spend_share_sdl!=1)){stop(cat('Please make sure the share of spend add up to 1.\n'))}
  }
  if((constraint_media_vec=='')||is.null(constraint_media_vec)){constraint_media_vec<-c()}
  #if((var_lb_budget_sim < var_optimization_lb)){stop(cat('Optimization lower bound cannot be greater than simulator lower bound'))}
  #if((var_ub_budget_sim > var_optimization_ub)){stop(cat('Optimization upper bound cannot be lower than simulator upper bound'))}
  #if((var_lb_budget_sim > var_ub_budget_sim)||(var_optimization_lb > var_optimization_ub)){stop(cat('Please make sure the upper bounds are greater than the lower bounds'))}
  if(!isTRUE(all.equal(dma_list,dma_names_vec))){stop(cat('Please ensure the dma_names_vec and dma_list have the same DMAs\n'))}
  ##To ensure the correct media parameters are being used.
  if(is.null(names(K)))
  {
    warning(cat("Using the media_names_vec for naming the K as the vector does'nt have any names. Please ensure the order of K is the same as media_names_vec for correct results.\n"))
    names(K) <<- media_names_vec  
  }else{
    if(!isTRUE(all.equal(names(K),as.character(media_names_vec)))){stop(cat('Please ensure the media of K and media_names_vec have the same media and in the same order\n'))}
    K <<- K[media_names_vec]
  }
    
  
  if(is.null(names(S)))
  {
    warning(cat("Using the media_names_vec for naming the S as the vector does'nt have any names. Please ensure the order of S is the same as media_names_vec for correct results.\n"))
    names(S) <<- media_names_vec  
  }else{
    if(!isTRUE(all.equal(names(S),as.character(media_names_vec)))){stop(cat('Please ensure the media of S and media_names_vec have the same media and in the same order\n'))}
    S <<- S[media_names_vec]
  }
  ##For matrices of beta, ensuring their column names are same as the media_names_vec and also in the same order
  if(is.null(colnames(beta_media_mat)))
  {
    warning(cat("Using the media_names_vec for naming the columns of beta_media_mat. Please ensure the columns are in the order of the media_names_vec for accurate results."))
    colnames(beta_media_mat) <<- media_names_vec
  }else{
    if(!isTRUE(all.equal(colnames(beta_media_mat),as.character(media_names_vec)))){stop(cat('Please ensure the columns in beta and media_names_vec have the same media and in the same order\n'))}
    beta_media_mat <<- beta_media_mat[,media_names_vec]
  }
    ##Checks for spend_media_vec which should be a subset of the media_names_vec
  if(!all(spend_media_vec %in% media_names_vec)){
     stop(cat(sprintf('Unknown media entered in spend media vec. Wrong media are: %s',paste(spend_media_vec[which(!(spend_media_vec%in%media_names_vec))],collapse = ','))))
    }
     ##Assuming for all the other media (except flight media) the number of spending periods (weeks/months) is nbr_period_q and for flight media, we use n_flight_period
    period_vec <- rep(nbr_period_q,length(media_names_vec))
    names(period_vec) <- media_names_vec
    
#     if(!is.null(flight_media_vec)){
#         ##If flight_media_vec is specified,check if flight media vector and n_flight_period have the same dimensions
#         if(length(flight_media_vec)!=length(n_flight_period)){
#             cat('Length of flight media and n_flight_period not same\n')
#             warning(cat(sprintf('Using default value %s for n_flight_period for all flight media',nbr_period_q)))
#             n_flight_period <- rep(nbr_period_q,length(flight_media_vec))
#             }
#         names(n_flight_period) <- flight_media_vec
#         ##For extreme values - Special cases
#         ##For 0 weeks, treat the media as constraint media as no weeks for spending are selected
#         if(any(n_flight_period==0)){
#             complete_excl_media <- names(n_flight_period)[which(n_flight_period == 0)]
#             constraint_media_vec <- c(contraint_media_vec,complete_excl_media)
#             }
#         ##If any flight media are to be spend for all periods, then the scenario of them being off doesnt exist and can be removed
#         if(any(n_flight_period == nbr_period_q)){
#              complete_on_media <- names(n_flight_period)[which(n_flight_period == nbr_period_q)]
#              flight_media_vec <- flight_media_vec[!(complete_on_media %in% flight_media_vec)]
#         }
#             period_vec[flight_media_vec] <- n_flight_period
            
#             }
  ##Assign the number of weeks to all the flight media equally
  period_vec[flight_media_vec] <- var_n_flight_period
  var_n_off_period <- nbr_period_q - var_n_flight_period
  week_vec <- c(var_n_flight_period,var_n_off_period)
  
##--------------------------------------------Main Code section------------------------------------------------
  cat(paste0(rep('*',20)))
  cat('\n')
  cat('Starting the simulator\n')
  cat('Running the simulator for:\n')
  cat(sprintf('Footprint: %s\n',var_footprint))
  cat(sprintf('Media constrained(not included in budget calculation/optimization): %s\n',paste(constraint_media_vec,collapse=',')))
  cat('Adding the quarter columns to the input dataframe\n')
  #cat('Columns in the data_XyZ:\n')
  #cat(paste(c(1:dim(data_XyZ)[2]),colnames(data_XyZ),sep<-'_',collapse <- '\n'))
  #cat('\n')
  data_XyZ <- get_quarters(data_XyZ)
#   if(is.null(var_rounding_num)){
#       rounding_digits <- str_count(as.character(var_step_size),'0')
#   }else{
#       if(var_rounding_num > var_step_size){
#           cat('Rounding number cannot be greater than step-size.Will cause wrong warning messages. Using step-size to round\n')
#           var_rounding_num = var_step_size
#           }
#       rounding_digits <- str_count(as.character(var_rounding_num),'0')
#   }
  #cat('Columns after adding the quarter columns:\n')
  #cat(paste(c(1:dim(data_XyZ)[2]),colnames(data_XyZ),sep<-'_',collapse <- '\n'))
  #cat('\n')
  cat('Number of periods by quarters:\n')
  qy_counts <- data_XyZ %>% 
              dplyr::group_by(qy) %>% 
              dplyr::select(period) %>% 
              dplyr::distinct() %>% 
              dplyr::count(n()) %>%
              dplyr::select(qy,n)
  
  write.table(qy_counts,row.names = F)
  
  ##Get the id of the constraint media
  id_constraint <- if(length(constraint_media_vec)==0){(length(media_names_vec)+1)}else{which(media_names_vec %in% constraint_media_vec)}
  
  ##Appending the flight media into the constraint_media to get the exclude media list for share of spend and
  ##Range of spend calculation
  exclude_media_vec <- c(constraint_media_vec,flight_media_vec)
  ##Check to see if the media parameters match the input media names.
  if((length(K)!=length(media_names_vec))||length(S)!=length(media_names_vec)){stop(cat('The media parameters length is not the same as the input media list. Please check K and S parameters.\n'))}
  
  ##Assign all quarters to spend_period if we want to get mean spending across all periods
  if(var_spend_period=='All'){
    all_quarters <- data_XyZ %>%
                   dplyr::select(qy) %>%
                   dplyr::distinct() %>%
                   dplyr::pull()
    
    var_spend_period <- all_quarters
  }
  ##Get the mean spend data by the selected quarter
   mean_quarterly_data <- data_XyZ %>% 
                      dplyr::filter(qy %in% var_spend_period) %>%
                      dplyr::select(qy,dma,spend_cols_vec) %>%
                      dplyr::group_by(qy,dma) %>%
                      dplyr::summarize_at(.vars = vars(spend_cols_vec),.funs = sum) %>%
                      dplyr::group_by(dma) %>%
                      dplyr::summarize_at(.vars = vars(spend_cols_vec),.funs = exclude_zero_spend_weeks) %>%
                      dplyr::ungroup() %>% 
                      dplyr::select(spend_cols_vec) %>% 
                      dplyr::rename_at(spend_cols_vec,~media_names_vec) %>%
                      as.matrix()

    ##Get the mean weekly spend for the selected quarters
    mean_weekly_data <- data_XyZ %>% 
                      dplyr::filter(qy %in% var_spend_period) %>%
                      dplyr::select(period,dma,spend_cols_vec) %>%
                      dplyr::group_by(period,dma) %>%
                      dplyr::summarize_at(.vars = vars(spend_cols_vec),.funs = sum) %>%
                      dplyr::group_by(dma) %>%
                      dplyr::summarize_at(.vars = vars(spend_cols_vec),.funs = exclude_zero_spend_weeks) %>%
                      dplyr::ungroup() %>% 
                      dplyr::select(spend_cols_vec) %>% 
                      dplyr::rename_at(spend_cols_vec,~media_names_vec) %>%
                      as.matrix()


    
    #mean_spend_data <- sweep(mean_quarterly_data,MARGIN = 2,period_vec,'/')
    mean_spend_data <- data.frame(mean_weekly_data)

    #cat(paste('period_vec',period_vec))
  
  if((dim(mean_spend_data)[2]!=length(media_names_vec))||(all(colnames(mean_spend_data) != media_names_vec))){
    stop(cat('Make sure the spend data and media list have the same names and size.\n'))
  }
  
  ###Calculate the share of spend based on the exclude_media
  ##Assign an index greater than the number of media in case the constraint_media is empty. Otherwise assign the respective
  ##indices of the exclude_media to id_excl
  id_excl <- if(length(exclude_media_vec)==0){(length(media_names_vec)+1)}else{which(media_names_vec %in% exclude_media_vec)}
  
  
  
  ##Override the mean spend data if any special conditions are specified using optimization_dma_list param
  if(!is.null(optimization_ub_dma_list)){
    ##Extract the media,dma and corresponding limits
    ##Assuming the specific input values by DMA and media as input, we overwrite the mean spend data to obtain these when multiplied by the var_optimization parameter
    id_media <- which(media_names_vec%in%names(optimization_ub_dma_list))
    idx <- 0
    for(l in optimization_ub_dma_list){
      idx <- idx + 1
      ub_vec_dma_names <- names(l)
      if(names(l)=='Suddenlink'){
        #cat(rep('*',30))
        #cat('\n')
        #cat('Code is working!\n')
        ##DMAs not including the Optimum DMA
        dma_vec <- seq(1:length(dma_names_vec))
        dma_vec <- dma_vec[!(dma_vec%in%var_id_opt)]
        ##Compute the scaling factor of input limit and upper bound of footprint to be used to rescale the UB for all DMA
        unscaled_ub_dma_vec <- mean_spend_data[dma_vec,id_media[idx]]*var_optimization_ub
        ub_media <- sum(unscaled_ub_dma_vec)
        scaling_fac <- as.numeric(l)/ub_media
        ub_vec <- unscaled_ub_dma_vec*scaling_fac


      }else if(names(l)=='Altice'){
        dma_vec <- seq(1:length(dma_names_vec))
        ##Compute the scaling factor of input limit and upper bound of footprint to be used to rescale the UB for all DMA
        unscaled_ub_dma_vec <- mean_spend_data[dma_vec,id_media[idx]]*var_optimization_ub
        ub_media <- sum(unscaled_ub_dma_vec)
        scaling_fac <- as.numeric(l)/ub_media
        ub_vec <- unscaled_ub_dma_vec*scaling_fac

      }else{
        dma_vec <- as.numeric(ub_vec_dma_names)
        ub_vec <- as.numeric(l)
      }
      ##Update the mean spend data to be later used to compute the lower and upper bounds of optimization
      mean_spend_data[dma_vec,id_media[idx]] <- ub_vec/var_optimization_ub
    }
    
  }
  if(!is.null(optimization_lb_dma_list)){
    ##Extract the media,dma and corresponding limits
    
    id_media <- which(media_names_vec%in%names(optimization_lb_dma_list))
    idx <- 0
    for(l in optimization_lb_dma_list){
      idx <- idx + 1
      lb_vec_dma_names <- names(l)
      if(names(l)=='Suddenlink'){
        #cat(rep('*',30))
        #cat('\n')
        #cat('Code is working!\n')
        ##DMAs not including the Optimum DMA
        dma_vec <- seq(1:length(dma_names_vec))
        dma_vec <- dma_vec[!(dma_vec%in%var_id_opt)]
        ##Compute the scaling factor of input limit and lower bound of footprint to be used to rescale the UB for all DMA
        unscaled_lb_dma_vec <- mean_spend_data[dma_vec,id_media[idx]]*var_optimization_lb
        lb_media <- sum(unscaled_lb_dma_vec)
        scaling_fac <- as.numeric(l)/lb_media
        lb_vec <- unscaled_lb_dma_vec*scaling_fac


      }else if(names(l)=='Altice'){
        dma_vec <- seq(1:length(dma_names_vec))
        ##Compute the scaling factor of input limit and lower bound of footprint to be used to rescale the UB for all DMA
        unscaled_lb_dma_vec <- mean_spend_data[dma_vec,id_media[idx]]*var_optimization_lb
        lb_media <- sum(unscaled_lb_dma_vec)
        scaling_fac <- as.numeric(l)/lb_media
        lb_vec <- unscaled_lb_dma_vec*scaling_fac

      }else{
        dma_vec <- as.numeric(lb_vec_dma_names)
        lb_vec <- as.numeric(l)
      }
      ##Update the mean spend data to be later used to compute the lower and upper bounds of optimization
      mean_spend_data[dma_vec,id_media[idx]] <- lb_vec/var_optimization_lb
    }
    
  }
  ##Compute the mean spend data for SUDDENLINK to calculate the share of spend by DMA and flight_media
  mean_spend_data_sdl <- mean_spend_data[-var_id_opt,-id_constraint]
  ##Lower and upper bounds of optimization

  ##Lower and upper bounds of optimization
  if(var_approach == 'Mean'){
  optimization_lb_mat <- var_optimization_lb * mean_spend_data
  optimization_ub_mat <- var_optimization_ub * mean_spend_data
  }else{
    ##Using the minmax approach to get the optimization bounds by dma and media - weekly level
    mean_quarterly_specific_data <- data_XyZ %>% 
                                    dplyr::filter(qy %in% var_spend_period) %>%
                                    dplyr::select(quarter,period,dma,spend_cols_vec) %>%
                                    dplyr::group_by(quarter,period,dma) %>%
                                    dplyr::summarize_at(.vars = vars(spend_cols_vec),.funs = sum) %>%
                                    dplyr::group_by(quarter, dma) %>%
                                    dplyr::summarize_at(.vars = vars(spend_cols_vec),.funs = exclude_zero_spend_weeks) %>%
                                    dplyr::ungroup() %>% 
                                    dplyr::select(quarter, dma, spend_cols_vec) %>% 
                                    dplyr::rename_at(spend_cols_vec,~media_names_vec) 
    ##Obtain the min of all means and max of all max by DMA
    min_max_df <- mean_quarterly_specific_data %>%
                  dplyr::group_by(dma) %>%
                  dplyr::summarize_at(.vars = vars(media_names_vec), .funs = list('min' = ~min(.[. != 0]),'max' = ~max(.[. != 0]) )) %>%
                  dplyr::mutate_at(.vars = vars(matches(media_names_vec)),.funs = ~ifelse(is.infinite(.),0,.))

    min_weekly_mat <- min_max_df %>%
               dplyr::select(dma,matches('min')) %>%
               dplyr::rename_at(.vars = vars(-dma), .funs = ~gsub('_min',replacement = '',.)) %>%
               dplyr::select(media_names_vec) %>%
               as.matrix()

    max_weekly_mat <- min_max_df %>%
               dplyr::select(dma,matches('max')) %>%
               dplyr::rename_at(.vars = vars(-dma), .funs = ~gsub('_max',replacement = '',.)) %>%
               dplyr::select(media_names_vec) %>%
               as.matrix()
    ##Alternate approach to setting the lower and upper bounds. Use ratio of the min and max to mean instead of the default lb and ub
    mean_weekly_spend <- sum(mean_spend_data[id_opt_case,-id_constraint])
    min_weekly_spend <- var_optimization_lb*sum(min_weekly_mat[id_opt_case,-id_constraint])
    max_weekly_spend <- var_optimization_ub*sum(max_weekly_mat[id_opt_case,-id_constraint])
    var_optimization_lb <- round(min_weekly_spend/mean_weekly_spend,2)
    var_optimization_ub <- round(max_weekly_spend/mean_weekly_spend,2)
    cat('Overriding the bounds by using the ratio of min of min of all media and max of max of all media to mean spend data\n')
    cat(sprintf('Revised Lower bound:%f\n',var_optimization_lb))
    cat(sprintf('Revised Upper bound:%f\n',var_optimization_ub))
    ##Convert the quarterly matrices to weekly as the optimization happens at the weekly level
    optimization_lb_mat <- var_optimization_lb * mean_spend_data
    optimization_ub_mat <- var_optimization_ub * mean_spend_data

    ##For the exclude_zero media use the weekly exclude zero media as the bounds
    # optimization_lb_mat[,var_exclude_zero_media]  <- var_optimization_lb*mean_spend_data[,var_exclude_zero_media]
    # optimization_ub_mat[,var_exclude_zero_media]  <- var_optimization_ub*mean_spend_data[,var_exclude_zero_media]

    optimization_lb_mat <- as.data.frame(optimization_lb_mat)
    optimization_ub_mat <- as.data.frame(optimization_ub_mat)

    ##Override the optimization_lb_mat and optimization_ub_mat if override lists are specified
    if(!is.null(optimization_ub_dma_list)){
    ##Extract the media,dma and corresponding limits
    ##Assuming the specific input values by DMA and media as input, we overwrite the mean spend data to obtain these when multiplied by the var_optimization parameter
    id_media <- which(media_names_vec%in%names(optimization_ub_dma_list))
    idx <- 0
    for(l in optimization_ub_dma_list){
      idx <- idx + 1
      dma_vec <- as.numeric(names(l))
      ub_vec <- as.numeric(l)
      optimization_ub_mat[dma_vec,id_media[idx]] <- ub_vec
    }
    
  }
  if(!is.null(optimization_lb_dma_list)){
    ##Extract the media,dma and corresponding limits
    
    id_media <- which(media_names_vec%in%names(optimization_lb_dma_list))
    idx <- 0
    for(l in optimization_lb_dma_list){
      idx <- idx + 1
      dma_vec <- as.numeric(names(l))
      lb_vec <- as.numeric(l)
      optimization_lb_mat[dma_vec,id_media[idx]] <- lb_vec
    }
    
   }

  }
  ##Create dataframes of the optimization matrices to be joined for limits check later
  opt_lb_df <- data.frame('dma' = dma_names_vec,optimization_lb_mat)
  opt_ub_df <- data.frame('dma' = dma_names_vec,optimization_ub_mat)
  
  
  
  ##Setup the input parameters to the optimizer
 
  if(var_footprint == 'OPT'){
    
    #budget_to_set <- 'Input_OPT_budget'
    id_opt_case <- 17
  }else{
    
    #budget_to_set <- 'Input_SDL_budget'
    id_opt_case <- -17
  }
  
  
  ##Unrounded Optimization bounds - Weekly - By case
  ##When all on
  unrounded_opt_lb_on <- sum(optimization_lb_mat[id_opt_case,-id_constraint], na.rm = TRUE)
  unrounded_opt_ub_on <- sum(optimization_ub_mat[id_opt_case,-id_constraint], na.rm = TRUE)
  ##When flight media off
  unrounded_opt_lb_off <- sum(optimization_lb_mat[id_opt_case,-id_excl], na.rm = TRUE)
  unrounded_opt_ub_off <- sum(optimization_ub_mat[id_opt_case,-id_excl], na.rm = TRUE)
    
  if(is.null(flight_media_share)){
      flight_media_share = 1 - (unrounded_opt_lb_off/unrounded_opt_lb_on)
      }
 cat(sprintf('#############################Flight media share: %s',flight_media_share))
   
  ###Limits for each budget based on the number of flight weeks
  
  quarterly_flight_on_lb <- plyr::round_any(unrounded_opt_lb_on*var_n_flight_period, var_step_size,f = ceiling)
  
  quarterly_flight_on_ub <- plyr::round_any(unrounded_opt_ub_on*var_n_flight_period, var_step_size,f = floor)
  
  quarterly_flight_off_lb <- plyr::round_any(unrounded_opt_lb_off*(nbr_period_q - var_n_flight_period), var_step_size,f = ceiling)
    
  quarterly_flight_off_ub <- plyr::round_any(unrounded_opt_ub_off*(nbr_period_q - var_n_flight_period), var_step_size,f = floor)
    
  quarterly_combined_lb <- quarterly_flight_on_lb + quarterly_flight_off_lb
  quarterly_combined_ub <- quarterly_flight_on_ub + quarterly_flight_off_ub

  
  start_budget <- quarterly_combined_lb 
  end_budget <-   quarterly_combined_ub
  
  
  ##If the specific input budget is used, ensure it lies within the optimization limits in order to avoid issues
  if(!is.null(var_specific_input_budget)){
    if((start_budget <= var_specific_input_budget) & (var_specific_input_budget <= end_budget)){
      input_budget_vec <- var_specific_input_budget
        var_step_size = 1 ##As it is for specific budget, step_size doesnt make any sense
      var_do_smoothen <- F ##Disable smoothening even if the input is given as it is not applicable for a specific point
      cat(sprintf('The simulator for %s footprint will run for %s budget for 1 iterations.\n',var_footprint,format_money(var_specific_input_budget)))
      rounding_digits = 0
    }else{
      stop(cat(sprintf('Please ensure the input budget for %s footprint lies between %s and %s for proper optimization. Or change the input optimization limits',var_footprint,start_budget,end_budget)))
    }
  }else{
    
     ##To ensure that the rounded simulator budgets fall within optimization range even when lb_sim = lb_opt and ub_sim = ub_opt, check the border cases
    
  ##The lower budget is determined by excluding just the constrained media and the upper budget is determined by excluding both flight and constrained media  
      
#      if(start_budget < unrounded_opt_lb)
#   {
#       cat('The starting budget of simulation is lower than the optimization lower bound. Adjusting the start budget by one step-size\n')
#       start_budget <- start_budget + var_step_size
#   }
#   if(end_budget > unrounded_opt_ub)
#   {
#       cat('The ending budget of simulation is higher than the optimization lower bound. Adjusting the end budget by one step-size\n')
#       end_budget <- end_budget - var_step_size
#   }
    input_budget_vec <- seq(from = start_budget,to = end_budget,by=var_step_size)
    cat(sprintf('The simulator for %s footprint will run from %s to %s budget for %s iterations.\n',var_footprint,format_money(min(input_budget_vec),0),format_money(max(input_budget_vec),0),length(input_budget_vec)))
    
  }
  
    
 
  
  ##Set up the output
  ##Create the final output dataframes to store results across different budgets
  
  
  common_cols <- c('Simulated_for','iteration_num','dma',"Input_quarterly_budget")
  
  ##As the same weekly budget is not being used for on and off, adding the column of weekly budget to the result df
  budget_cols <- c(common_cols,"Input_DMA_Budget",
                  "DMA_lb","DMA_ub",
                  "dma_optimized_budget",media_names_vec,target_var,
                  "DMA_budget_mismatch")
  
  target_cols <- c(common_cols,media_names_vec,target_var)
    
  check_cols <- c(common_cols,media_names_vec,"DMA_bound")
  
  common_budget_df <- as.data.frame(matrix(nrow = 0,ncol = length(budget_cols)))
  colnames(common_budget_df) <- budget_cols
  
  common_target_df <- as.data.frame(matrix(nrow = 0,ncol = length(target_cols)))
  colnames(common_target_df) <- target_cols
    
  bound_check_df <- as.data.frame(matrix(nrow = 0,ncol = length(check_cols)))
  colnames(bound_check_df) <- check_cols
    
  ##Total number of possible scenarios
  total_scenarios <- 2#^length(flight_media_vec)
  
  before_smoothening_budget_list <- replicate(total_scenarios,common_budget_df,simplify = F)
  before_smoothening_target_list <- replicate(total_scenarios,common_target_df,simplify = F)
  before_smoothening_check_list <-  replicate(total_scenarios,bound_check_df,simplify = F)
    
  list_of_media_lists <- list()
  ##Loop starts for the optimization
  ##Set progress bar for the loop
  #pb <- txtProgressBar(min <- 0, max <- length(input), initial <- 0) 
  
  ##Initialize prev_opt_solution to default value
  prev_opt_solution <- NULL

  for(idx in c(1:length(input_budget_vec))){
  #setTxtProgressBar(pb,idx)
  cat(rep('*',50))
  cat('\n')
  cat(sprintf('Running the optimizer for iteration:%s\n',idx))
  input_budget = input_budget_vec[idx]


  #write.table(as.data.frame(do.call(cbind, input)),file = 'test_input.csv')
  ###Run the optimizer code
    optimized_solution <- optimizer(budget_input = input_budget,
                                    var_footprint = var_footprint,
                                    mean_spend_data = mean_spend_data,
                                    var_id_constraint = id_constraint,
                                    var_id_opt = id_opt_case,
                                    flight_media_vec = flight_media_vec, 
                                    dma_media_excl_list = dma_media_excl_list,
                                    optimization_lb_mat = optimization_lb_mat,
                                    optimization_ub_mat = optimization_ub_mat,
                                    var_spend_share_sdl = var_spend_share_sdl, 
                                    print_nloptr_log = print_nloptr_log,
                                    print_dma_opt = print_dma_opt,
                                    flight_media_share = flight_media_share,
                                    var_n_flight_period = var_n_flight_period,
                                    prev_opt_solution = prev_opt_solution,
                                    ftol_rel = ftol_rel,
                                    xtol_rel = xtol_rel
                          )
    

    if(!var_cold_start){
        ##Assuming hot start setting previous solution as the starting point
        prev_opt_solution = optimized_solution
    }
    
    ##Set up the output dataframes
    ##Common dataframe across all the scenarios
    temp <- data.frame("Simulated_for"=var_footprint,
                       "iteration_num" = idx,
                       "dma" = dma_names_vec[id_opt_case],
                       "Input_quarterly_budget" = input_budget,
                      stringsAsFactors = F
    )
    ##Remove the shared spend list from the optimized solution after storing it
    share_spend_case_list <- optimized_solution[['share_spend_case_list']]
    optimized_solution[['share_spend_case_list']] <- NULL
    
    all_scenarios <- names(optimized_solution)
    names(share_spend_case_list) <- all_scenarios
    
   
    #df_names <- all_scenarios %>% purrr::map(~paste('Weekly_Optimized_budget',.,'df',sep<-'_'))
    
    ##Assign the names of the scenarios to the output list and media lists.
    names(before_smoothening_budget_list) <- all_scenarios
    names(before_smoothening_target_list) <- all_scenarios
    names(before_smoothening_check_list)  <- all_scenarios
    ###Test if in each of the case, the total optimized spend is the same as input spend
    for(scenario in all_scenarios){
      
      #output_df_name <- paste('Weekly_Optimized_budget',scenario,'df',sep<-'_')
      optimized_budget <- optimized_solution[[scenario]][['Weekly_optimized_df']]
      optimized_target <- optimized_solution[[scenario]][['Weekly_optimized_ga']]
      media_list <- optimized_solution[[scenario]][['Media_list']][id_opt_case]
      
      ##Check for each scenario if the total allotted budget adds up to the input weekly budget
      total_optimized_budget <- plyr::round_any(sum(optimized_budget[id_opt_case,media_names_vec]),1,f = round)
      input_weekly_budget <- plyr::round_any(sum(optimized_budget[id_opt_case,'Input_DMA_Budget']),1,f = round)
      #optimized_sdl_budget <- round(sum(optimized_budget[-var_id_opt,media_names_vec])*nbr_period_q,-rounding_num)
      if(input_weekly_budget!=total_optimized_budget){
        warning(cat(sprintf('The total weekly input %s budget does not match the optimized budget for input budget level:%s and scenario:%s',var_footprint,input_budget,scenario)))
         cat(sprintf('Input budget: %s\n',input_budget))
         cat(sprintf('Optimized budget: %s\n',total_optimized_budget))
        }
      
     
      output_df <- cbind.data.frame(temp,optimized_budget[id_opt_case,])
      output_target_df <- cbind.data.frame(temp,optimized_target[id_opt_case,])
      output_check_df <- cbind.data.frame(temp,optimized_budget[id_opt_case,])
      ###Add the flag for identifying DMAs where the input budget and the optimized budget are mismatching
      output_df <- output_df  %>% 
               dplyr::mutate(dma_optimized_budget = rowSums(select(.,media_names_vec)))%>%
               dplyr::mutate('DMA_budget_mismatch' = ifelse((round(dma_optimized_budget) - round(Input_DMA_Budget))==0,0,1)) %>%
               dplyr::distinct()
      
      ##Check to ensure the optimized budgets for each media are within the guard-rails set by the user
      rename_cols <- c('media','media_lb','media_ub')
      temp_df <- output_check_df
      ##For each media, check if the optimized budget lies within the guard-rails set by the user
      for(m in media_names_vec)
      {      ##If the media is a constraint media or if the scenario is flight media off, then, set the check to 2 as we aren't optimizing this media
             if(m%in%constraint_media_vec| grepl(m,scenario))
             {
                 output_check_df[,m] <- 2
                 next
             
             }
             select_cols <- c(m,paste(m,'lb',sep = '_'),paste(m,'ub',sep = '_'))
             media_df <- temp_df %>%
                         dplyr::left_join(opt_lb_df,by = 'dma', suffix = c('','_lb')) %>%
                         dplyr::left_join(opt_ub_df,by = 'dma',suffix = c('','_ub')) %>%
                         dplyr::select(select_cols) %>%
                         dplyr::rename_all(~rename_cols) %>%
                         dplyr::mutate(media = ifelse((round(media,2)>=round(media_lb,2))&(round(media,2)<=round(media_ub,2)),1,0)) %>%
                         dplyr::select(media) %>%
                         dplyr::rename_at(.vars = vars(media),.funs = ~m)
            
           output_check_df[,m] <- media_df
           ##For each DMA check if the optimized DMA budget lies within the ub and lb of the DMA budget
           output_check_df <- output_check_df %>%
                              dplyr::mutate(DMA_bound = ifelse((round(Input_DMA_Budget,2)>=round(DMA_lb,2))&(round(Input_DMA_Budget,2)<=round(DMA_ub,2)),1,0))  
                        }
        
      output_check_df  <- output_check_df %>% 
                          dplyr::select(check_cols)
      
      
      output_df <- output_df %>% 
                   dplyr::select(budget_cols)
      before_smoothening_budget_list[[scenario]] <- rbind(before_smoothening_budget_list[[scenario]],output_df)
      before_smoothening_target_list[[scenario]] <- rbind(before_smoothening_target_list[[scenario]],output_target_df)
      before_smoothening_check_list[[scenario]] <- rbind(before_smoothening_check_list[[scenario]],output_check_df)
      list_of_media_lists[[scenario]] <- media_list
    } 
    
  }
  var_names_scenarios <- names(before_smoothening_budget_list)
 ###Add plots to visualize the total weekly_ga distribution
  bs_output_plot_list <- list()
  var_count_scenario <- 0
  cat('Generating the output plots to show the stability of optimization.\n')
 for(data in before_smoothening_budget_list){
   #write.table(head(data),row.names <- F)
   var_count_scenario <- var_count_scenario + 1
   n_weeks <- week_vec[var_count_scenario]
   plot_data <- data %>%
                dplyr::group_by(Input_quarterly_budget) %>%
                dplyr::select(Input_DMA_Budget,target_var) %>%
                dplyr::summarise_all(sum) %>%  ##For SDL, add up all the individual DMA level budgets by grouping by the quarterly budget
                dplyr::mutate_at(.vars = vars(Input_DMA_Budget,target_var),.funs = ~n_weeks*.) %>% ##Calculate the total spending and target by weeks
                dplyr::mutate(CPMT = Input_DMA_Budget/(!!rlang::sym(target_var)),
                              min_CPMT = Input_quarterly_budget[which.min(CPMT)]) 
   
   scenario <- all_scenarios[var_count_scenario]
   
   #write.table(head(plot_data),row.names <- F)
   colors <- c('Target' = 'blue','CPT' = 'red')
     
     coeff <- max(plot_data$CPMT)/max(plot_data[[target_var]])
     
   a <-  ggplot(plot_data, aes(x=Input_quarterly_budget)) +
         geom_line( aes(y=!!rlang::sym(target_var), color = 'Target'), size=2) + 
         geom_line( aes(y=CPMT / coeff, color = 'CPT'), size=2) +

          scale_y_continuous(

            # Features of the first axis
            name = 'Quarterly Target',

            # Add a second axis and specify its features
            sec.axis = sec_axis(~.*coeff, name="Cost per Media Target($)")
          ) +
          geom_vline(xintercept = plot_data$min_CPMT,color = 'yellow',linetype = 'dashed') +
        dark_theme_gray() + 
                ggtitle(sprintf('%s - %s Budget vs Target vs CPT',var_footprint,Caps(scenario))) +
                labs(subtitle = sprintf("Minimum Cost per Media at %s",format_money(plot_data$min_CPMT,0))) +
                theme(plot.title = element_text(hjust = 0.5),
                     legend.title = element_blank(),
                     legend.position = 'top',
                     plot.subtitle = element_text(size = rel(1),hjust = 0.5))
   
   bs_output_plot_list[[var_names_scenarios[var_count_scenario]]] <- a
 }
 ##Show the quarterly results for the best and worst case scenarios
  # 
  # quarterly_ga_df <- data_frame('budget' <- input_budget)
  # 
  # 
  # dma_target_var <- paste('weekly_dma_media',var_target_name,sep <- '_')
  # 
  # cat('Computing the best and worst case TGA by DMA for the quarter level.\n')
  # cat('Assuming atleast one week for each scenario\n')
  # cat('The actual results will fall between the best and worst case scenarios\n')
  # cat(sprintf('Using the %s quarter for calculation of control GA',var.dates))
  # control.ga.quarter <- gross.add.control(Z <- control.mat,
  #                                        beta.control.df <- beta.control.mat,
  #                                        control.names <- control.names.vec,
  #                                        data.XyZ <- data.XyZ,
  #                                        input.period <- var.dates,
  #                                        prospect.vec <- prospect.df[[subs.col]],y.max <- target.max.vec,
  #                                        by.mean <- !var.use.latest.quarter)
  # 
  # 
  # 
  # ##Add the control GA to the quarterly df
  # quarterly.ga.df['control.ga'] <- sum(control.ga.quarter[id.opt.case]) 
  # 
  # ##Generating the possible combinations of weeks with one week for atleast one scenario.
  # period.vec <- c(rep(1,(total.scenarios-1)),nbr_period_q - (total.scenarios-1))
  # list.of.week.permutations <- permn(period.vec)
  # 
  # ##Extract the target variable from the dataframes in the before.smoothening list summing it by input budget
  # target.list <- lapply(before.smoothening.budget.list, 
  #                      function(x) {
  #                        x%>% 
  #                        group_by_at(.vars <- budget.to.set) %>% 
  #                        select(dma.target.var) %>% 
  #                        summarize_all(sum) %>% 
  #                        ungroup() %>% 
  #                        select(dma.target.var) %>% 
  #                        pull() })
  # 
  # possible.outcomes.target <- list()
  # ##Generate the possible total GA df for each of the weeks combinations
  # i <- 0
  # for(permutation in list.of.week.permutations){
  #   ##Compute the permuted multiplication of the week permutation with the target list
  #   i <- i + 1
  #   target.list1 <- Map('*',permutation,target.list)
  #   possible.outcomes.target[[i]] <- Reduce('+',target.list1)
  # }
  # ##Whichever combination has the highest target value,
  # ##will be assigned to the best case and the least one assigned to worse case
  # rank.order <- rank(unlist(lapply(possible.outcomes.target,max)))
  # ##As rank ranges from 1 to length(list), the one which has the highest target 
  # ##will also have the highest rank
  # best.rank <- length(possible.outcomes.target)
  # best.scenario <- which(rank.order == best.rank)
  # worst.scenario <- which(rank.order == 1)
  # ##Extracting the outcome which has the highest rank and corresponding outcome with the 
  # ##lowest rank
  # cat('Best case scenario:')
  # cat(paste(all.scenarios,list.of.week.permutations[[best.scenario]]),sep <- ' ',collapse <- '')
  # best.target <- possible.outcomes.target[]
  # worst.target <- possible.outcomes.target[which(rank.order == 1)]
##--------------------------------------Smoothening(If var_do_smoothen == T)---------------------------------
  post_smoothening_budget_list <- list()
  post_smoothening_target_list <- list()
  post_smoothening_check_list <- list()
  ps_output_plot_list <- list()
  if(var_do_smoothen){
   source('Optimization_functions/Smoothening_function.R')
  
   var_count_scenario <- 0
   for(scenario in var_names_scenarios){
     var_count_scenario <- var_count_scenario + 1
     budget_data <- before_smoothening_budget_list[[scenario]]
     # write.table(head(budget_data),row.names <- F)
     target_data <- before_smoothening_target_list[[scenario]]
     #write.table(head(target_data),row.names <- F)
     smoothened_data_res <- smoothen_eval(weekly_spend_df = budget_data,
                                         weekly_target_df = target_data,
                                     media_list = list_of_media_lists[[var_count_scenario]],
                                     optimization_lb_mat,
                                     optimization_ub_mat,
                                     id_opt_case,
                                     print_nloptr_log = print_nloptr_log,
                                     print_dma_opt = print_dma_opt,
                                     nsmooth = var_n_smoothen
                                        )
     
     smoothened_data <- smoothened_data_res[['corrected_spend_df']]
     corrected_target_df <- smoothened_data_res[['corrected_target_df']]
     ##Update DMA_budget_mismatch flag to check if smoothening caused any mismatch between input and optimized budget
     smoothened_data <- smoothened_data %>% 
                       dplyr::mutate(dma_optimized_budget_ps = rowSums(select(.,media_names_vec)))%>%
                       dplyr::mutate('DMA_budget_mismatch_ps' = ifelse((round(dma_optimized_budget_ps) - round(Input_DMA_Budget))==0,0,1)) %>%
                       dplyr::distinct() 
    ##Ensure that the smoothened data also obeys the optimization constraints
     output_check_df <- smoothened_data_res[['corrected_spend_df']]
       
      ##Check to ensure the optimized budgets for each media are within the guard-rails set by the user
      rename_cols <- c('media','media_lb','media_ub')
      temp_df <- output_check_df
      ##For each media, check if the optimized budget lies within the guard-rails set by the user

      for(m in media_names_vec)
      {
             if(m%in%constraint_media_vec | grepl(m,scenario))
             {
                 output_check_df[,m] <- 2
                 next
             
             }
             select_cols <- c(m,paste(m,'lb',sep = '_'),paste(m,'ub',sep = '_'))
             media_df <- temp_df %>%
                         dplyr::left_join(opt_lb_df,by = 'dma', suffix = c('','_lb')) %>%
                         dplyr::left_join(opt_ub_df,by = 'dma',suffix = c('','_ub')) %>%
                         dplyr::select(select_cols) %>%
                         dplyr::rename_all(~rename_cols) %>%
                         dplyr::mutate(media = ifelse((round(media,2)>=round(media_lb,2))&(round(media,2)<=round(media_ub,2)),1,0)) %>%
                         dplyr::select(media) %>%
                         dplyr::rename_at(.vars = vars(media),.funs = ~m)
            
           output_check_df[,m] <- media_df
           ##For each DMA check if the optimized DMA budget lies within the ub and lb of the DMA budget
           output_check_df <- output_check_df %>%
                              dplyr::mutate(DMA_bound = ifelse((round(Input_DMA_Budget,2)>=round(DMA_lb,2))&(round(Input_DMA_Budget,2)<=round(DMA_ub,2)),1,0))  
       }
        
      output_check_df  <- output_check_df %>% 
                          dplyr::select(check_cols)
      
       
       
    post_smoothening_budget_list[[var_names_scenarios[var_count_scenario]]] <- smoothened_data
    post_smoothening_target_list[[var_names_scenarios[var_count_scenario]]] <- corrected_target_df
    post_smoothening_check_list[[scenario]] <- rbind(post_smoothening_check_list[[scenario]],output_check_df)
     }
   
 }

  var_count_scenario <- 0
  for(data in post_smoothening_budget_list){

    var_count_scenario <- var_count_scenario + 1
    scenario <- all_scenarios[var_count_scenario]
    n_weeks <- week_vec[var_count_scenario]
     plot_data <- data %>%
                dplyr::group_by(Input_quarterly_budget) %>%
                dplyr::select(Input_DMA_Budget,target_var) %>%
                dplyr::summarise_all(sum) %>%   ##For SDL, add up all the individual DMA level budgets by grouping by the quarterly budget
                dplyr::mutate_at(.vars = vars(Input_DMA_Budget,target_var),.funs = ~n_weeks*.) %>%    ##Calculate the total spending and target by weeks
                dplyr::mutate(CPMT = Input_DMA_Budget/(!!rlang::sym(target_var)),
                              min_CPMT = Input_quarterly_budget[which.min(CPMT)])
      
    
    colors <- c('Target' = 'blue','CPT' = 'red')
    coeff <- max(plot_data$CPMT)/max(plot_data[[target_var]])
    a <- ggplot(plot_data, aes(x=Input_quarterly_budget)) +
         geom_line( aes(y=!!rlang::sym(target_var), color = 'Target'), size=2) + 
         geom_line( aes(y=CPMT / coeff, color = 'CPT'), size=2) +

          scale_y_continuous(

            # Features of the first axis
            name = 'Quarterly Target',

            # Add a second axis and specify its features
            sec.axis = sec_axis(~.*coeff, name="Cost per Media Target($)")
          ) +
          geom_vline(xintercept = plot_data$min_CPMT,color = 'yellow',linetype = 'dashed') +
        dark_theme_gray() + 
                ggtitle(sprintf('%s - %s Budget vs Target vs CPT',var_footprint,Caps(scenario))) +
                labs(subtitle = sprintf("Minimum Cost per Media at %s",format_money(plot_data$min_CPMT,0))) +
                theme(plot.title = element_text(hjust = 0.5),
                     legend.title = element_blank(),
                     legend.position = 'top',
                     plot.subtitle = element_text(size = rel(1),hjust = 0.5))
    
    ps_output_plot_list[[var_names_scenarios[var_count_scenario]]] <- a
  }

   #Save the input params to be later used in the evaluate function
  input_params_list =   list('var_specific_input_budget' =   var_specific_input_budget ,
                        'data_XyZ' = data_XyZ,                                                
                        'var_spend_period' = var_spend_period,                                    
                        'var_footprint' = var_footprint,                                  
                        'constraint_media_vec' = constraint_media_vec,                                          
                        'flight_media_vec' = flight_media_vec,                              
                        'var_optimization_lb' = var_optimization_lb,
                        'var_optimization_ub' = var_optimization_ub,
                        #'var_lb_budget_sim' = var_lb_budget_sim,
                        #'var_ub_budget_sim' = var_ub_budget_sim,
                        'optimization_lb_dma_list' = optimization_lb_dma_list,                                              
                        'optimization_ub_dma_list' = optimization_ub_dma_list,
                        'var_n_flight_period' = var_n_flight_period,
                        'var_id_opt' = var_id_opt,                                             
                        'var_step_size' = var_step_size,                                                                         
                        'var_exclude_zero_media' = var_exclude_zero_media,                                  
                        'dma_media_excl_list' = dma_media_excl_list,
                        'var_spend_share_sdl' = var_spend_share_sdl,
                        'var_do_smoothen' = var_do_smoothen,
                         'spend_media_vec' = spend_media_vec)

##------------------------------------------Final output------------------------------------------------------
  res <- list('Without_smoothening_output' = list('budget' = before_smoothening_budget_list,
                                                  'target' = before_smoothening_target_list,
                                                  'check' = before_smoothening_check_list),
              'Without_smoothening_graphs' = bs_output_plot_list,
              'Post_smoothening_output' = list('budget' = post_smoothening_budget_list,
                                               'target' = post_smoothening_target_list,
                                               'check' = post_smoothening_check_list),
              'Post_smoothening_graphs' = ps_output_plot_list,
              'Input_Params' = input_params_list,
              'Mean_Spend_Data' = mean_spend_data,
              'LB_data' = opt_lb_df,
              'UB_data' = opt_ub_df,
              'share_spend_case_list' = share_spend_case_list,
              'mean_quarterly_data' = mean_quarterly_data,
              'mean_spend_data' = mean_spend_data,
              # 'mean_quarterly_specific_data' = mean_quarterly_specific_data,
              # 'mean_quarterly_data_Q1' = mean_quarterly_data_Q1,
              # 'mean_quarterly_data_Q2' = mean_quarterly_data_Q2,
              # 'mean_quarterly_data_Q3' = mean_quarterly_data_Q3,
              # 'mean_quarterly_data_Q4' = mean_quarterly_data_Q4,
              # 'mean_spend_data_Q1'     = mean_spend_data_Q1,
              # 'mean_spend_data_Q2'     = mean_spend_data_Q2,
              # 'mean_spend_data_Q3'     = mean_spend_data_Q3,
              # 'mean_spend_data_Q4'     = mean_spend_data_Q4,
              # 'optimization_lb_mat_Q1' = optimization_lb_mat_Q1,
              # 'optimization_lb_mat_Q2' = optimization_lb_mat_Q2,
              # 'optimization_lb_mat_Q3' = optimization_lb_mat_Q3,
              # 'optimization_lb_mat_Q4' = optimization_lb_mat_Q4,
              # 'optimization_ub_mat_Q1' = optimization_ub_mat_Q1,
              # 'optimization_ub_mat_Q2' = optimization_ub_mat_Q2,
              # 'optimization_ub_mat_Q3' = optimization_ub_mat_Q3,
              # 'optimization_ub_mat_Q4' = optimization_ub_mat_Q4,
              # 'opt_lb_df_all' = opt_lb_df_all,
              # 'opt_ub_df_all' = opt_ub_df_all,
              # 'opt_lb_df' = opt_lb_df,
              # 'opt_ub_df' = opt_ub_df,
              'optimization_lb_mat' = optimization_lb_mat,
              'optimization_ub_mat' = optimization_ub_mat
              )
  #stop_logging()
  return(res)
}