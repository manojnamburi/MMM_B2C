##Source the dependencies
#source('common_functions_mmm.R')
#'Get Optimized Quarterly Mix
#'
#'@description
#'Function to obtain the quarterly mix at given budget levels for given scenario
#'
#'@param input_quarterly_budget_opt Input quarterly Optimum budget
#'@param input_quarterly_budget_sdl Input quarterly Suddenlink budget
#'@param sim_results_opt Optimum results from the simulator
#'@param sim_results_sdl Suddenlink results from the simulator
#'@param control_beta_mat Beta estimates of the control variables
#'@param control_mat Actual,normalized values of the control variables from the preprocessing function
#'@param control_names_vec Names of the control variables used in the model. Has to be subset of the column names in control_mat
#'@param target_capita_norm_col Vector for denormalization of target value. 'subs' or 'non-subs'
#'@param flight_media_vec Vector of the flight media. Currently supports single flight media. Default is NULL
#'@param const_media_vec Vector of constraint media. Use this if these media were excluded from the optimization across all DMA
#'@param constr_media_budget_OPT If there is a fixed Optimum budget of the constrained media to be added to the final optimal mix
#'@param constr_media_budget_SDL If there is a fixed Suddenlink budget of the constrained media to be added to the final optimal mix
#'@param var_n_flight_period Number of periods (weeks/months) of the flight media to be considered as 'On'
#'@param mean_spend_data Mean of spend data across all the weeks by DMA
#'@param var_period The quarter for which the optimized mix is being planned for. Uses the latest quarter data from the control data to calculate the target value of control
#'
#'@return
#'Formatted output list containing: \cr
#'Footprint metrics - Spend, target, CPMTGA, TGA, CPTGA \cr
#'DMA metrics - Spend, target, CPMTGA, TGA, CPTGA \cr
#'
#'Unformatted raw output list containing: \cr
#'Quarterly budget mix by DMA and media \cr
#'Quarterly target values by DMA and media \cr
#'Target value across media by DMA \cr
#'
#'
#'@details
#'This function is used to combine the results obtained from the simulator for each footprint into one unified mix
#'at the quarterly level.
#'
#'The simulator outputs the optimized budget mix by DMA at the period level (weekly/monthly) which need to be combined
#'into results at the quarterly level to be shared to the stakeholders. This function combines the results from the simulator
#'(output lists from the simulator) and generates the mix for a particular input Quarterly budget by Footprint.
#'
#'This accounts for media that have been flighted and combines the on and off results from the simulator to get the final mix at the quarterly level.
#'
get_quarterly_mix <- function(input_quarterly_budget_opt,
                              input_quarterly_budget_sdl,
                              sim_results_opt,
                              sim_results_sdl,
                              control_beta_mat,
                              control_mat,
                              control_names_vec,
                              target_capita_norm_col,
                              flight_media_vec_OPT = NULL,
                              flight_media_vec_SDL = NULL,
                              constr_media_vec_OPT = NULL,
                              constr_media_vec_SDL = NULL,
                              constr_media_budget_OPT = NULL,
                              constr_media_budget_SDL = NULL,
                              var_n_flight_period_OPT = NULL,
                              var_n_flight_period_SDL = NULL,
                              mean_spend_data = NULL,
                              spend_media_vec = NULL,
                              by_mean = F,
                              var_period = 'Q1'){

  #cat('Initiating the final result objects for target and budget\n')
  ##Final result dfs by combining the OPT and SDL on and off dfs
  all_on_budget_df <- data.frame('dma' = dma_names_vec)
  all_on_target_df <- data.frame('dma' = dma_names_vec)

  ##Default var_n_flight_period if flight is not specified
  var_n_flight_period <- rep(nbr_period_q,length(dma_names_vec))

  ##Generate the quarterly results for the selected budget and flight weeks.

  names(var_n_flight_period_OPT) <- flight_media_vec_OPT
  names(var_n_flight_period_SDL) <- flight_media_vec_SDL
  ##If the var_n_flight_period is nbr_period_q for any flight media, remove them from the flight media
  if(any(var_n_flight_period_OPT == nbr_period_q)){
    exclude_flight_media <- names(var_n_flight_period_OPT)[which(var_n_flight_period_OPT == nbr_period_q)]
    flight_media_vec_OPT <- flight_media_vec_OPT[!(exclude_flight_media %in% flight_media_vec_OPT)]
  }

  if(any(var_n_flight_period_SDL == nbr_period_q)){
    exclude_flight_media <- names(var_n_flight_period_SDL)[which(var_n_flight_period_SDL == nbr_period_q)]
    flight_media_vec_SDL <- flight_media_vec_SDL[!(exclude_flight_media %in% flight_media_vec_SDL)]
  }

  if(((length(sim_results_opt[['budget']])-1)!=length(flight_media_vec_OPT))||(((length(sim_results_sdl[['budget']])-1)!=length(flight_media_vec_SDL)))){stop(cat('Please ensure you have entered the possible week combination values for the flight scenarios.'))}




  ##Checks for constraint budget vec by footprint
  if(!is.null(constr_media_vec_OPT)){
    if(length(constr_media_budget_OPT)!=length(constr_media_vec_OPT)){
      stop(cat("Please enter the OPT budget for all the constrained media"))
    }
  }

  if(!is.null(constr_media_vec_SDL)){
    if(length(constr_media_budget_SDL)!=length(constr_media_vec_SDL)){
      stop(cat("Please enter the SDL budget for all the constrained media"))
    }
  }

  ##If spend media are not explicitly specified, assuming all media are spend based
  if(is.null(spend_media_vec)){
    spend_media_vec = media_names_vec
  }

  ##Ensuring all spend media are part of the media names vec
  if(!all(spend_media_vec %in% media_names_vec)){
    stop(cat(sprintf('Unknown media entered in spend media vec. Wrong media are: %s',paste(spend_media_vec[which(!(spend_media_vec%in%media_names_vec))],collapse = ','))))
  }

  ##Extract all non-spend media to be formatted seperately
  non_spend_media_vec <- media_names_vec[which(!(media_names_vec%in%spend_media_vec))]

  ##Indexes of both spend media and non-spend media
  spend_media_idx <- which(media_names_vec %in% spend_media_vec)
  non_spend_media_idx <- which(media_names_vec %in% non_spend_media_vec)

  ##Extract the weekly budget from the result objects and generate a ndma*nmedia matrix for
  ##computing the quarterly metrics
  ##Optimum budget df
  opt_all_on_budget_df <- sim_results_opt[['budget']][['all_on']]
  ##Target dfs
  opt_all_on_target_df <- sim_results_opt[['target']][['all_on']]
  ##SDL budget df
  sdl_all_on_budget_df <- sim_results_sdl[['budget']][['all_on']]

  ##SDL Target df
  sdl_all_on_target_df <- sim_results_sdl[['target']][['all_on']]



  common_cols <- c('dma',media_names_vec,target_var)

  ###Compute the constrained media target value
  constr_media_index_OPT <- which(media_names_vec %in% constr_media_vec_OPT)
  constr_media_index_SDL <- which(media_names_vec %in% constr_media_vec_SDL)
  if(length(constr_media_index_OPT)!=length(constr_media_vec_OPT)){
    stop(cat(sprintf('Please ensure the constraint media in OPT is one of:%s',paste(media_names_vec,collapse=','))))
  }
  if(length(constr_media_index_SDL)!=length(constr_media_vec_SDL)){
    stop(cat(sprintf('Please ensure the constraint media in SDL is one of:%s',paste(media_names_vec,collapse=','))))
  }

  ##Default outputs for constrained media
  constr_target_by_dma <- rep(0,length(dma_names_vec))
  constr_budget_df <- data.frame(matrix(nrow = length(dma_names_vec),ncol = length(common_cols)-1))
  constr_target_df <- data.frame(matrix(nrow = length(dma_names_vec),ncol = length(common_cols)-1))

  colnames(constr_budget_df) <- c(media_names_vec,target_var)
  colnames(constr_target_df) <- c(media_names_vec,target_var)

  ##Remove all the NAs to add to the final_df in case constraint vector is null
  constr_budget_df[is.na(constr_budget_df)] <- 0
  constr_target_df[is.na(constr_target_df)] <- 0

  constr_budget_mat <- 0*beta_media_mat


  if(!is.null(constr_media_vec_OPT)||!is.null(constr_media_vec_SDL)){

    ##Assuming that the input provided for the constrained budget is at the quarterly level,
    ##Converting it to the level of the data - weekly or monthly
    constr_budget_period_OPT <- constr_media_budget_OPT/nbr_period_q
    constr_budget_period_SDL <- constr_media_budget_SDL/nbr_period_q

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

    constr_budget_mat[var_id_opt,constr_media_vec_OPT] <- constr_budget_period_OPT
    constr_budget_mat[-var_id_opt,constr_media_vec_SDL] <- sweep(as.matrix(share_spend_sdl_mat[,constr_media_index_SDL]), MARGIN=2, constr_budget_period_SDL, `*`)

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



    constr_target_by_dma <- rowSums(constr_target_mat)

    if(!(length(constr_target_by_dma)!=dim(constr_budget_mat)[2]))
    {
      stop(cat('Error in merging the constrained target to constrained budget. Check dimensions.'))
    }

    constr_budget_df[media_names_vec] <- constr_budget_mat
    constr_budget_df[target_var] <- constr_target_by_dma

    constr_target_df[media_names_vec] <- constr_target_mat
    constr_target_df[target_var] <- constr_target_by_dma

    #constr_budget_mat <- cbind(constr_budget_mat,constr_target_by_dma)
    #constr_target_mat <- cbind(constr_target_mat,constr_target_by_dma)


    names(constr_target_by_dma) <- dma_names_vec

  }

  ##Check if the input budget is infact part of the result dataframes to avoid errors
  if(!(input_quarterly_budget_opt%in%opt_all_on_budget_df[['Input_quarterly_budget']])){
    stop(cat(paste('Please ensure the input budget is part of the Optimum result set. Possible values for input budget:',paste0(opt_all_on_budget_df[['Input_quarterly_budget']],collapse = ','))))
  }else if(!(input_quarterly_budget_sdl%in%sdl_all_on_budget_df[['Input_quarterly_budget']])){
    stop(cat(paste('Please ensure the input budget is part of the Suddenlink result set. Possible values for input budget:',paste0(sdl_all_on_budget_df[['Input_quarterly_budget']],collapse = ','))))
  }

  ##Get the simulator results for the input optimum and suddenlink budget

  ##Combine the selected OPT and SDL dfs for the selected input budget
  ##Budget - on
  selected_opt_on_budget_df <- opt_all_on_budget_df %>%
    dplyr::filter(Input_quarterly_budget == input_quarterly_budget_opt) %>%
    dplyr::select(common_cols)

  selected_sdl_on_budget_df <- sdl_all_on_budget_df %>%
    dplyr::filter(Input_quarterly_budget == input_quarterly_budget_sdl) %>%
    dplyr::select(common_cols)

  temp_df <- rbind(selected_opt_on_budget_df,selected_sdl_on_budget_df)


  #cat('Combining the Optimum and Suddenlink budget dataframes for the selected input budget values when all flight media are on\n')
  all_on_budget_df <- all_on_budget_df %>%
    dplyr::left_join(temp_df, by ='dma') %>%
    dplyr::select(-dma)

  ###Combine the budget df with constrained budget
  all_on_budget_df <- all_on_budget_df + constr_budget_df



  ##Target - on
  selected_opt_on_target_df <- opt_all_on_target_df %>%
    dplyr::filter(Input_quarterly_budget == input_quarterly_budget_opt) %>%
    dplyr::select(common_cols)

  selected_sdl_on_target_df <- sdl_all_on_target_df %>%
    dplyr::filter(Input_quarterly_budget == input_quarterly_budget_sdl) %>%
    dplyr::select(common_cols)

  temp_df <- rbind(selected_opt_on_target_df,selected_sdl_on_target_df)
  #cat('Combining the Optimum and Suddenlink target dataframes for the selected input budget values when all flight media are on\n')

  all_on_target_df <- all_on_target_df %>%
    dplyr::left_join(temp_df,by = 'dma') %>%
    dplyr::select(-dma)

  all_on_target_df <- all_on_target_df + constr_target_df

  ##When the flight media vector is empty assume all the possible weeks are flight weeks unless there are any special cases where a particular media needs to have different value for the number of periods
  if(is.null(flight_media_vec_OPT)&&is.null(flight_media_vec_SDL)){
    quarterly_mix_df <- nbr_period_q*all_on_budget_df
    quarterly_target_df <- nbr_period_q*all_on_target_df
  }

  ##Budget - off
  if(!is.null(flight_media_vec_OPT)||!is.null(flight_media_vec_SDL)){
    var_n_flight_period <- rep(nbr_period_q,length(dma_names_vec))

    ##Final combined result dfs
    all_off_budget_df <- data.frame('dma' = dma_names_vec)
    all_off_target_df <- data.frame('dma' = dma_names_vec)
    ##Default off dataframes
    dummy_df_opt <- selected_opt_on_budget_df %>%
      dplyr::mutate_at(.vars = vars(-'dma'),.funs = ~0*.)

    dummy_df_sdl <- selected_sdl_on_budget_df %>%
      dplyr::mutate_at(.vars = vars(-'dma'),.funs = ~0*.)

    selected_opt_off_budget_df <- dummy_df_opt

    selected_sdl_off_budget_df <- dummy_df_sdl

    selected_opt_off_target_df <- dummy_df_opt

    selected_sdl_off_target_df <- dummy_df_sdl

    ##If OPT has any flight media
    if(!is.null(flight_media_vec_OPT)){
      var_n_flight_period[var_id_opt] <- var_n_flight_period_OPT

      list_name <- paste(flight_media_vec_OPT,'off',sep = '_',collapse = '_')
      opt_all_off_budget_df <- sim_results_opt[['budget']][[list_name]]
      opt_all_off_target_df <- sim_results_opt[['target']][[list_name]]

      selected_opt_off_budget_df <- opt_all_off_budget_df %>%
        dplyr::filter(Input_quarterly_budget == input_quarterly_budget_opt) %>%
        dplyr::select(common_cols)

      selected_opt_off_target_df <- opt_all_off_target_df %>%
        dplyr::filter(Input_quarterly_budget == input_quarterly_budget_opt) %>%
        dplyr::select(common_cols)

    }
    ##If SDL has any flight media
    if(!is.null(flight_media_vec_SDL)){
      var_n_flight_period[-var_id_opt] <- var_n_flight_period_SDL

      list_name <- paste(flight_media_vec_SDL,'off',sep = '_',collapse = '_')
      sdl_all_off_budget_df <- sim_results_sdl[['budget']][[list_name]]
      sdl_all_off_target_df <- sim_results_sdl[['target']][[list_name]]

      selected_sdl_off_budget_df <- sdl_all_off_budget_df %>%
        dplyr::filter(Input_quarterly_budget == input_quarterly_budget_sdl) %>%
        dplyr::select(common_cols)


      selected_sdl_off_target_df <- sdl_all_off_target_df %>%
        dplyr::filter(Input_quarterly_budget == input_quarterly_budget_sdl) %>%
        dplyr::select(common_cols)

    }


    temp_df <- rbind(selected_opt_off_budget_df,selected_sdl_off_budget_df)
    #cat('Combining the Optimum and Suddenlink budget dataframes for the selected input budget values when all flight media are off\n')
    all_off_budget_df <- all_off_budget_df %>%
      dplyr::left_join(temp_df,by = 'dma')%>%
      dplyr::select(-dma)

    all_off_budget_df <- all_off_budget_df + constr_budget_df

    ##Target - off

    temp_df <- rbind(selected_opt_off_target_df,selected_sdl_off_target_df)
    #cat('Combining the Optimum and Suddenlink target dataframes for the selected input budget values when all flight media are off\n')
    all_off_target_df <- all_off_target_df %>%
      dplyr::left_join(temp_df,by='dma')%>%
      dplyr::select(-dma)

    all_off_target_df <- all_off_target_df + constr_target_df

    ##Get the quarterly mix for the selected budget levels
    quarterly_mix_df <- var_n_flight_period*all_on_budget_df + (nbr_period_q - var_n_flight_period)*all_off_budget_df
    quarterly_target_df <- var_n_flight_period*all_on_target_df + (nbr_period_q - var_n_flight_period)*all_off_target_df


  }

  ##Subset the cleaned names to only the spend media
  cleaned_spend_names <- cleaned_names_list[which(media_names_vec %in% spend_media_vec)]

  ##Obtain the display order based on final spending by media
  display_order <- quarterly_mix_df %>%
    dplyr::select(media_names_vec) %>%
    dplyr::rename_at(.vars = vars(media_names_vec), ~as.character(cleaned_names_list)) %>%
    dplyr::summarize_all(.funs = sum) %>%
    tidyr::gather(key = media,value = spend) %>%
    dplyr::arrange(desc(spend)) %>%
    dplyr::select(media) %>%
    dplyr::pull()

  ##Display order for spend media only
  display_spend_only <- display_order[(display_order %in% cleaned_spend_names)]

  #cat(sprintf('Computing the control target values for the period %s\n',var_period))
  ##Compute the quarterly control target
  control_target_mat <- gross_add_control(control_mat,
                                          beta_control_df = beta_control_mat,
                                          control_names = control_names_vec,
                                          data_XyZ = data_XyZ,
                                          input_period = var_period,
                                          y_max = target_max_vec,
                                          target_capita_norm_col = target_capita_norm_col,
                                          by_mean = by_mean,
                                          nbr_period_q = nbr_period_q)

  media_target_vec <- quarterly_target_df[[target_var]]

  #cat(sprintf('Total Media target for the period %s:%s\n',var_period,format(round(sum(media_target_vec)),big.mark <- ',')))
  #cat(sprintf('Optimum Media target for the period %s:%s\n',var_period,format(round(sum(media_target_vec[var_id_opt])),big.mark <- ',')))
  #cat(sprintf('Suddenlink Media target for the period %s:%s\n',var_period,format(round(sum(media_target_vec[-var_id_opt])),big.mark <- ',')))

  total_target_period <- format(round((sum(media_target_vec) + sum(control_target_mat))),big.mark = ',')
  total_target_opt <- format(round((sum(media_target_vec[var_id_opt]) + sum(control_target_mat[var_id_opt,]))),big.mark = ',')
  total_target_sdl <- format(round((sum(media_target_vec[-var_id_opt]) + sum(control_target_mat[-var_id_opt,]))),big.mark = ',')

  total_spend_period <- format_money(sum(quarterly_mix_df[,media_names_vec]))
  total_spend_opt <- format_money(sum(quarterly_mix_df[var_id_opt,media_names_vec]))
  total_spend_sdl <- format_money(sum(quarterly_mix_df[-var_id_opt,media_names_vec]))

  #cat(sprintf('Total target for the period %s:%s\n',var_period,total_target_period))
  #cat(sprintf('Optimum target for the period %s:%s\n',var_period,total_target_opt))
  #cat(sprintf('Suddenlink target for the period %s:%s\n',var_period,total_target_sdl))
  req_cols <- c('Total',media_names_vec)
  req_cols_spend <- c('Total',spend_media_vec)
  req_cols_fmt <- c('Total',display_order)
  req_cols_fmt_spend <- c('Total', display_spend_only)

  ##Adding only spend columns up
  period_spend_df <- quarterly_mix_df %>%
    dplyr::mutate('Total' = rowSums(.[spend_media_vec])) %>%
    dplyr::mutate('dma' = dma_names_vec) %>%
    dplyr::select(c('dma',req_cols))

  period_target_df <- quarterly_target_df %>%
    dplyr::mutate('Total' = rowSums(.[media_names_vec])) %>%
    dplyr::mutate('dma' = dma_names_vec) %>%
    dplyr::select(c('dma',req_cols))


  ##Add footprint level metrics
  period_spend_df1 <- addFootprintData(period_spend_df,req_cols)

  period_target_df1 <- addFootprintData(period_target_df,req_cols)

  ##Compute the CPTMGA only for the spend media
  period_spend_long <- period_spend_df1 %>%
    dplyr::select(-non_spend_media_vec) %>%
    tidyr::gather(key = media, value = spend, -c('dma'))

  #print(period_spend_long)

  fprint_spend_df <-   period_spend_df1 %>%
    dplyr::filter(!(dma %in% dma_names_vec)) %>%
    dplyr::rename('Footprint' = 'dma') %>%
    dplyr::arrange(factor(Footprint, levels = c('Altice USA','Optimum','Suddenlink'))) %>%
    dplyr::mutate_at(.vars = vars(req_cols),.funs = ~./1000) %>%
    dplyr::mutate_at(.vars = vars(req_cols_spend),.funs = format_money) %>%         ##Format money for spend columns
    dplyr::mutate_at(.vars = vars(c(non_spend_media_vec)),.funs = function(x){format(round(x),big.mark = ',')}) %>%  ##Format other media
    dplyr::rename_at(.vars = vars(media_names_vec), ~as.character(cleaned_names_list)) %>%
    dplyr::select(c('Footprint',req_cols_fmt)) %>%
    dplyr::ungroup()



  period_target_long <- period_target_df1 %>%
    dplyr::select(-non_spend_media_vec) %>%
    tidyr::gather(key = media,value = target, -c('dma'))

  fprint_target_df <- period_target_df1 %>%
    dplyr::filter(!(dma %in% dma_names_vec)) %>%
    dplyr::rename('Footprint' = 'dma') %>%
    dplyr::arrange(factor(Footprint, levels = c('Altice USA','Optimum','Suddenlink'))) %>%
    dplyr::mutate_at(.vars = vars(req_cols),.funs = function(x){format(round(x),big.mark = ',')}) %>%
    dplyr::rename_at(.vars = vars(media_names_vec), ~as.character(cleaned_names_list)) %>%
    dplyr::select(c('Footprint',req_cols_fmt)) %>%
    dplyr::ungroup()


  cpmtga_df <- period_spend_long %>%
    dplyr::left_join(period_target_long, by = c('dma','media')) %>%
    tidyr::replace_na(list(target = 0)) %>%
    dplyr::select(dma,media,spend,target) %>%
    dplyr::group_by(dma,media) %>%
    dplyr::summarize_all(list(sum)) %>%
    dplyr::mutate(cpmga = spend/target,
                  cpmga = format_money(cpmga),
                  cpmga = ifelse(cpmga == '$NaN','Zero spend',cpmga)) %>%
    dplyr::select(dma,media,cpmga) %>%
    tidyr::spread(key = media,value = cpmga) %>%
    dplyr::select(c('dma',req_cols_spend)) %>%
    dplyr::rename_at(.vars = vars(spend_media_vec), ~as.character(cleaned_spend_names)) %>%
    dplyr::rename('DMA' = 'dma') %>%
    dplyr::select(c('DMA',req_cols_fmt_spend)) %>%
    dplyr::ungroup()

  fprint_cpmtga_df <-    cpmtga_df %>%
    dplyr::filter(!(DMA %in% dma_names_vec)) %>%
    dplyr::rename('Footprint' = 'DMA') %>%
    dplyr::arrange(factor(Footprint, levels = c('Altice USA','Optimum','Suddenlink'))) %>%
    dplyr::select(c('Footprint',req_cols_fmt_spend)) %>%
    dplyr::ungroup()


  ##Add the control GA to the media GA
  target_by_dma <- media_target_vec + rowSums(control_target_mat)
  names(target_by_dma) <- dma_names_vec

  ##Creating a dataframe for Total GA (Control + Media)
  tga_df <- data.frame('dma' = dma_names_vec,'TGA' = as.numeric(target_by_dma))
  tga_df <- addFootprintData(tga_df,metric_cols = 'TGA')

  #print(tga_df)


  cptga_df <- period_spend_long %>%
    dplyr::filter(media == 'Total') %>%
    dplyr::left_join(tga_df, by = c('dma')) %>%
    tidyr::replace_na(list(TGA = 0)) %>%
    dplyr::select(dma,spend,TGA) %>%
    dplyr::group_by(dma) %>%
    dplyr::mutate(cptga = spend/TGA,
                  cptga = format_money(cptga,0),
                  cptga = ifelse(cptga == '$NaN','Zero spend',cptga)) %>%
    dplyr::select(dma,cptga) %>%
    dplyr::rename('DMA' = 'dma') %>%
    dplyr::ungroup()

  fprint_tga_df <- tga_df %>%
    dplyr::filter(!(dma %in% dma_names_vec)) %>%
    dplyr::rename('Footprint' = 'dma') %>%
    dplyr::arrange(factor(Footprint, levels = c('Altice USA','Optimum','Suddenlink'))) %>%
    dplyr::mutate_at(.vars = vars('TGA'),.funs = function(x){format(round(x),big.mark = ',')}) %>%
    dplyr::ungroup()


  fprint_cptga_df <-    cptga_df %>%
    dplyr::filter(!(DMA %in% dma_names_vec)) %>%
    dplyr::rename('Footprint' = 'DMA') %>%
    dplyr::arrange(factor(Footprint, levels = c('Altice USA','Optimum','Suddenlink'))) %>%
    dplyr::ungroup()



  summary_df <- data.frame('Footprint' = c('USA','Optimum','Suddenlink'),
                           'Quarterly Spend [$]' = c(total_spend_period,total_spend_opt,total_spend_sdl),
                           'Quarterly target' = c(total_target_period,total_target_opt,total_target_sdl))

  dma_quart_budget <- period_spend_df %>%
    dplyr::mutate_at(.vars = vars(req_cols),.funs = ~./1000) %>%
    dplyr::mutate_at(.vars = vars(req_cols_spend),.funs = format_money) %>%
    dplyr::mutate_at(.vars = vars(c(non_spend_media_vec)),.funs = function(x){format(round(x),big.mark = ',')}) %>%  ##Format other media
    dplyr::rename_at(.vars = vars(media_names_vec), ~as.character(cleaned_names_list)) %>%
    dplyr::rename('DMA' = 'dma') %>%
    dplyr::select(c('DMA',req_cols_fmt)) %>%
    dplyr::ungroup()

  dma_quart_target <- period_target_df %>%
    dplyr::filter(dma %in% dma_names_vec) %>%
    dplyr::mutate_at(.vars = vars(req_cols),.funs = function(x){format(round(x),big.mark = ',')}) %>%
    dplyr::rename_at(.vars = vars(media_names_vec), ~as.character(cleaned_names_list)) %>%
    dplyr::rename('DMA' = 'dma') %>%
    dplyr::select(c('DMA',req_cols_fmt)) %>%
    dplyr::ungroup()

  dma_total_target <- tga_df %>%
    dplyr::filter(dma %in% dma_names_vec) %>%
    dplyr::mutate_at(.vars = vars('TGA'),.funs = function(x){format(round(x),big.mark = ',')}) %>%
    dplyr::rename('DMA' = 'dma') %>%
    dplyr::select(c('DMA','TGA')) %>%
    dplyr::ungroup()


  dma_cpmtga_df <- cpmtga_df %>%
    dplyr::filter(DMA %in% dma_names_vec)

  dma_cptga_df <- cptga_df %>%
    dplyr::filter(DMA %in% dma_names_vec)

  return(list('Formatted' = list('Footprint' = list('Spend' = fprint_spend_df,
                                                    'Target' = fprint_target_df,
                                                    'CPMTGA' = fprint_cpmtga_df,
                                                    'CPTGA' = fprint_cptga_df,
                                                    'TGA' = fprint_tga_df),
                                 'DMA' = list('Spend' = dma_quart_budget,
                                              'Target' = dma_quart_target,
                                              'CPMTGA' = dma_cpmtga_df,
                                              'CPTGA'  = dma_cptga_df,
                                              'TGA' = dma_total_target)),
              'Raw' = list('quarterly_spend' = period_spend_df,
                           'quarterly_target' = period_target_df,
                           'target_by_dma' = tga_df)))

}
