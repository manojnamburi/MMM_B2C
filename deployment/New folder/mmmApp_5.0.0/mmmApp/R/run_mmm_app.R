#'Get the Optimization Outputs for the Given Inputs
#'
#'
#'@description
#'Function to get the distribution of media from the Quarterly Media Mix by DMA
#'
#'@details
#'Function that calls the get_quarterly_mix function and is used to expose the API for
#'obtaining the media mix.
#'
#'Generates a summary table by footprint and formatted output by media and DMA.
#'
#'The input is input which is a generic JSON which is treated as a dataframe for generation
#'of the optimal mix.
#'
#'@return For output - Footprint level metrics like target,spend and CPMTGA \cr
#'For download - optimized budget allocation by media and geo
#'
#'@param input JSON object containing all the inputs associated with the particular MMM
#'
#'@section Note:
#'For MMM Acquisition, for this function to work, please specify the following inputs:
#'
#'Input_OPT_budget, \cr
#'Input_SDL_budget, \cr
#'constraint, \cr
#'input_period, \cr
#'flight_flag_OPT, \cr
#'flight_flag_SDL \cr
#'When constraint is 'yes':
#'OPT_DM_budget,
#'SDL_DM_budget
#'for MMM Acquisition
#'
#'
#'@export
run_mmm_app <- function(input){

  ##-------------------Input initialization and tests--------------------------------

  ##Accept the inputs either as csv or as a JSON call
  input_data <- if(is.character(input) && file.exists(input))
    {
    read.csv(input)
  } else {
    as.data.frame(input)
  }

  ##Required inputs
  stopifnot("Input_OPT_budget" %in% names(input_data))
  stopifnot("Input_SDL_budget" %in% names(input_data))
  stopifnot("constraint" %in% names(input_data))
  stopifnot("input_period" %in% names(input_data))
  stopifnot("flight_flag_OPT" %in% names(input_data))
  stopifnot("flight_flag_SDL" %in% names(input_data))
  stopifnot("output_type" %in% names(input_data))
  #stopifnot("DMA" %in% names(input_data))

  if(input$constraint == 'yes')
  {
    stopifnot("OPT_DM_budget" %in% names(input_data))
    stopifnot("SDL_DM_budget" %in% names(input_data))
  }


  ###The datasets are loaded from the Data folder when the package is called

  ##-------------------Initialize parameters needed for the running of the app---------
  ##--------------------------------Media parameters ----------------------------------
  ##Using <<- to make the variables accessible to the Global environment which is the parent
  ##environment for all functional environments and also since these are not modified anywhere
  ##we fix these values at the Global level
  beta_media_mat <<- model_params$media_params$beta_media_mat
  K <<- model_params$media_params$K
  S <<- model_params$media_params$S
  spend_activity_ratio_dma_df <<- model_params$media_params$spend_activity_ratio_dma_df
  min_media_mat <<- model_params$media_params$min_media_mat
  max_media_mat <<- model_params$media_params$max_media_mat
  media_names_vec <<- model_params$media_params$media_names_vec
  cleaned_names_list <<- model_params$media_params$cleaned_names_list
  ##Correcting Tv to TV and dm to Direct Mail
  cleaned_names_list['tv'] = 'TV'
  cleaned_names_list['dm'] = 'Direct Mail'
  cleaned_names_list['paid_search_brand'] = 'Search Branded'
  cleaned_names_list['paid_search_unbranded'] = 'Search Unbranded'
  cleaned_names_list <<- cleaned_names_list

  prospect_mat_media <<- model_params$media_params$prospect_mat_media

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
  dma_names_vec <<- model_params$other_params$dma_names_vec
  optimum_dma <<- model_params$other_params$optimum_dma

  ##--------------------------------Input parameters -------------------------------------
  ##Changed the print to be On/Off by footprint for the entire quarter - 11/04/2020
  ##Updated the number of print weeks for Q12020 refresh to 6 weeks OPT and 3 weeks SDL based on historical spending - 06/02/2021
  var_OPT_budget <- input_data$Input_OPT_budget
  var_SDL_budget <- input_data$Input_SDL_budget
  var_constraint <- input_data$constraint
  var_period     <- input_data$input_period
  var_DMA        <- input_data$DMA
  var_print_flag_OPT <- input_data$flight_flag_OPT
  var_print_flag_SDL <- input_data$flight_flag_SDL
  var_output_type <- input_data$output_type

  ##Default parameters
  var_flight_media    <- c('print')
  var_constr_media    <- NULL
  var_OPT_DM_budget   <- 0
  var_SDL_DM_budget   <- 0

  if(var_constraint == 'yes')
  {
    var_OPT_DM_budget <- input_data$OPT_DM_budget
    var_SDL_DM_budget <- input_data$SDL_DM_budget
    ##Default constraint media
    var_constr_media  <- c('dm')
    constr_txt <- '[Constrained]'
  }else{
    constr_txt <- '[No Constraint]'
  }


  ##If the flag is set to T by footprint, then dont treat print as flight
  if(var_print_flag_OPT){
    flight_media_vec_OPT = var_flight_media
    var_n_flight_period_OPT = 7 ##Based on historical spending
  }else{
    flight_media_vec_OPT = NULL
    var_n_flight_period_OPT = NULL
  }

  if(var_print_flag_SDL){
    flight_media_vec_SDL = var_flight_media
    var_n_flight_period_SDL = 3 ##Based on historical data
  }else{
    flight_media_vec_SDL = NULL
    var_n_flight_period_SDL = NULL
  }

  var_id_opt <<- which(dma_names_vec %in% optimum_dma)

  ##-------------------------------Main code section -------------------------------------
  ##Initialize the lists to be used to generate the mix based on the input

  ##If print flag is set to T, treat print as flight media. Otherwise turn it off for the quarter
  if(var_print_flag_OPT){
    flight_str_OPT <- '1_12'
  }else{
    flight_str_OPT <- 0
  }

  if(var_print_flag_SDL){
    flight_str_SDL <- '1_12'
  }else{
    flight_str_SDL <- 0
  }

  opt_obj_string <- paste('opt',var_constraint,var_flight_media,flight_str_OPT,sep = '_')
  sdl_obj_string <- paste('sdl',var_constraint,var_flight_media,flight_str_SDL,sep = '_')

  opt_res <- get(opt_obj_string)
  sdl_res <- get(sdl_obj_string)

  res <- get_quarterly_mix(input_quarterly_budget_opt = var_OPT_budget,
                           input_quarterly_budget_sdl = var_SDL_budget,
                           constr_media_vec_OPT = var_constr_media,
                           constr_media_vec_SDL = var_constr_media,
                           mean_spend_data = mean_spend_data,
                           constr_media_budget_OPT = var_OPT_DM_budget,
                           constr_media_budget_SDL = var_SDL_DM_budget,
                           sim_results_opt = opt_res,
                           sim_results_sdl = sdl_res,
                           control_beta_mat = beta_control_mat,
                           control_mat = control_mat,
                           control_names_vec = control_names_vec,
                           flight_media_vec_OPT = flight_media_vec_OPT,
                           flight_media_vec_SDL = flight_media_vec_SDL,
                           var_n_flight_period_OPT = var_n_flight_period_OPT,
                           var_n_flight_period_SDL = var_n_flight_period_SDL,
                           var_period = var_period,
                           target_capita_norm_col = target_capita_norm_col
  )
  ##Plot the allocation bar
  if(!is.null(input_data$DMA))
  {
    plot_data <- data.frame(res$Raw$quarterly_budget_mix,target = res$Raw$target_by_dma)

    output_plot <- plot_allocation_bar(DMA ,plot_data)

  }

  ##------------------------Format output for the app----------------------------
  ##Remove all cost per media gross add and total cost per gross add - 11/04/2020
  #cpmtga_df <- res$Formatted$Footprint$CPMTGA
  mtga_df <- res$Formatted$Footprint$Target
  spend_df <- res$Formatted$Footprint$Spend
  dwnld_df <- res$Formatted$DMA$Spend
  #cptga_df <- res$Formatted$Footprint$CPTGA
  tga_df   <- res$Formatted$Footprint$TGA

  ##Adding total GA to the summary table requirement - 2020/08/06

  fprint_tga_df <- tga_df %>%
                   dplyr::select('TGA') %>%
                   dplyr::rename('True Gross Adds' = 'TGA')

  recode_vec <- c('USA','EAST','WEST')
  names(recode_vec) <- c('Altice USA','Optimum','Suddenlink')

  ##Rename the 'footprint' and 'DMA' columns and recode Altice,Optimum and Suddenlink to USA,East and West
  ##Also adding the respective keyword to the footprint column in accordance with the mockup
  dwnld_df <- dwnld_df %>%
              dplyr::filter(!(DMA%in%optimum_dma)) %>%
              dplyr::rename('Footprint' = 'DMA')

  #cpmtga_df <- cpmtga_df %>%
  #             dplyr::ungroup() %>%
  #             dplyr::mutate_at(.vars = vars('Footprint'),.funs = ~recode(.,!!!recode_vec))



  mtga_df <- mtga_df %>%
             dplyr::ungroup() %>%
             dplyr::mutate_at(.vars = vars('Footprint'),.funs = ~recode(.,!!!recode_vec))


  spend_df <- spend_df %>%
              dplyr::ungroup() %>%
              dplyr::mutate_at(.vars = vars('Footprint'),.funs = ~recode(.,!!!recode_vec)) %>%
              dplyr::mutate_at(.vars = vars(-'Footprint'),.funs = ~format_money(as.numeric(gsub('[$,]', '', .)),0))

  #cptga_df <- cptga_df %>%
  #            dplyr::ungroup() %>%
  #            dplyr::mutate_at(.vars = vars('Footprint'),.funs = ~recode(.,!!!recode_vec))
  ##Changing based on requirements from file 'UI features.xlsx' from 07/13/2020
  ##1.Subset all the footprint dfs to get just the total features
  ##2.Rounding the spend in summary table

  total_spend_df <-  spend_df %>%
                     dplyr::select('Footprint','Total') %>%
                     dplyr::rename('Spend $(000)' = 'Total')

  #total_cpmtga_df <- cpmtga_df %>%
  #                   dplyr::select('Total') %>%
  #                   dplyr::mutate_at(.vars = vars('Total'),.funs = ~format_money(as.numeric(gsub('[$,]', '', .)),0)) %>%
  #                   dplyr::rename('Cost per Media True Gross Adds (CPMTGA)' = 'Total')

  total_mtga_df <- mtga_df %>%
                   dplyr::select('Total') %>%
                   dplyr::rename('Media True Gross Adds (MTGA)' = 'Total')

  #total_cptga_df <- cptga_df %>%
  #                  dplyr::select('cptga') %>%
  #                  dplyr::rename('Cost per True Gross Adds (CPTGA)' = 'cptga')


  ##REmoving CPMTGA and replacing it with CPTGA as per discussions on 08/17/2020
  ##Removing CPTGA as per discussions on 11/04/2020

  footprint_df <- cbind(total_spend_df,fprint_tga_df)#,total_cptga_df)



  comb_spend_df <- rbind(spend_df,dwnld_df)

  comb_spend_df <- comb_spend_df %>%
                   dplyr::rename('Spend $(000)' = 'Total')

  #output_list = list()
  if(var_output_type == 'csv'){
    ##Return just the combined spend dataframe
    ##Add text to certain rows and columns as per request on 08/25/2020

    ##Extract the TGA vector to add to the footprint rows
    tga_vec <- tga_df %>%
               dplyr::select('TGA') %>%
               dplyr::pull()
    ##Create a column for TGA
    spend_df <- cbind.data.frame(spend_df, 'TGA' = tga_vec)

    #print(colnames(spend_df))

    spend_df <- spend_df %>%
                dplyr::rename('Spend $(000)' = 'Total') %>%
                dplyr::rename_at(.vars = vars('Spend $(000)'), .funs = ~paste(.,sprintf('[%s]',var_period))) %>%
                dplyr::rename_at(.vars = vars('Direct Mail'), .funs = ~paste(.,constr_txt)) %>%
                #dplyr::rename_at(.vars = vars('Print'), .funs = ~paste(.,sprintf('[%s Weeks]', var_n_flight_period))) %>%
                tidyr::unite('Footprint',c('Footprint','TGA'),sep = ' [TGA :') %>%
                dplyr::mutate_at(.vars = vars('Footprint'), .funs = ~paste0(.,']'))




    output_cols <- colnames(spend_df)

    ##Rename the DMA level df same as the footprint df
    colnames(dwnld_df) <- output_cols

    comb_spend_df_csv <- rbind(spend_df,dwnld_df)

    output_list = comb_spend_df_csv
  }
  else if(var_output_type == 'json'){
    ##Add additional elements like color,id and parent to the dfs
    ##Add id which is the row number for each element
    footprint_df <- footprint_df %>%
                 dplyr::rename('name' = "Footprint") %>%
                 dplyr::mutate(id = row_number(),
                               "__parent" = 'parent',
                               "__color" = "#000000",
                               "expanded" = ifelse(grepl('(USA)',name),'true','false')
                               )


    comb_spend_df <-  comb_spend_df %>%
                  dplyr::rename('name' = "Footprint") %>%
                  dplyr::mutate(id = row_number(),
                                "__parent" = 'parent',
                                "__color" = "#000000",
                                "expanded" = ifelse(grepl('(USA)',name),'true','false')
                  )

    ##Parent column
    usa_id <- footprint_df %>%
                   dplyr::filter(name == 'USA') %>%
                   dplyr::select(id) %>%
                   dplyr::pull()

    usa_spend_id <- comb_spend_df %>%
                    dplyr::filter(name == 'USA') %>%
                    dplyr::select(id) %>%
                    dplyr::pull()


    sdl_spend_id <-   comb_spend_df %>%
                      dplyr::filter(name == 'WEST') %>%
                      dplyr::select(id) %>%
                      dplyr::pull()



    comb_spend_df <- comb_spend_df %>%
                 dplyr::mutate_at(.vars = vars(matches('__parent')),
                                  .funs = ~ifelse(grepl('(WEST|EAST)',name),usa_spend_id,
                                                                       ifelse(name%in%dma_names_vec[-var_id_opt],sdl_spend_id,'parent')

                                                  )
                                  )





    footprint_df <- footprint_df %>%
                    dplyr::mutate_at(.vars = vars(matches('__parent')),
                                     .funs = ~ifelse(grepl('(WEST|EAST)',name),usa_id,'parent'))

    ##Set the colors based on the mockup
    comb_spend_df <- comb_spend_df %>%
                 dplyr::mutate_at(.vars = vars(matches('__color')),
                                  .funs = ~ifelse((grepl('(WEST*)',name))|(name%in%dma_names_vec[-var_id_opt]),'#ff8132',
                                                  ifelse((grepl('(EAST*)',name))|(name%in%dma_names_vec[var_id_opt]),'#0084d6','#000000')
                                                  )
                                  )
    footprint_df <- footprint_df %>%
      dplyr::mutate_at(.vars = vars(matches('__color')),
                       .funs = ~ifelse((grepl('(WEST*)',name))|(name%in%dma_names_vec[-var_id_opt]),'#ff8132',
                                       ifelse((grepl('(EAST*)',name))|(name%in%dma_names_vec[var_id_opt]),'#0084d6','#000000')
                       )
      )


    output_list = list('summary' = footprint_df,
                       'spend' = comb_spend_df)
  }
  else{
    stop(cat('Wrong "output_type" specified'))
  }


  return(output_list)



}
