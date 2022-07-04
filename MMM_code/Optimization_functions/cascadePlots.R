##Dependencies to be installed if not installed already and also load them into the namespace
if((!require(tidyverse))){install.packages("tidyverse",repos="http://cran.us.r-project.org")}
if((!require(plotly))){install.packages("plotly",repos="http://cran.us.r-project.org")}

source('../Common_functions/common_functions_mmm.R')
##Plot the waterfall chart to show how spend and target vary between the last quarter and optimized result
cascadePlots <- function(DMA,actual_spend_data,
                         optimized_spend_df,
                         optimized_target_df = NULL,
                         input_quarter,
                         media_names_spend,
                         media_names_vec,
                         spend_type = 'weekly',
                         order_by_increments = F,
                         exclude_zero_weeks = T,
                         flight_media_vec = NULL,
                         var_n_flight_period = NULL){
#'
#'Function to plot the Waterfall chart of Spend and target
#'
#'@description
#'Function to visualize the difference between actual quarterly mix/target with the optimized solution
#'
#'@details
#'Function to visualize the difference between previous input quarter spending and resulting
#'target variable(currently weekly DMA Media Gross adds) to the optimized spend and resulting
#'target variable. Takes DMA name and quarter as input and produces the plot for the last 
#'quarter available in the data for the input quarter and compares the value of spend and target
#'with the optimized result. Need to input the optimized result by DMA and media as well.
#'
#'@return
#'List of two waterfall plots - spend and target
#'
#'@param DMA Input DMA for which the plot will be generated
#'@param actual_spend_data The actual spend data 
#'@param optimized_spend_df The optimized mix from the optimizer
#'@param optimized_target_df The resultant target values at the optimized spend values
#'@param input_quarter The quarter against which the optimzed solution is being compared against
#'@param media_names_spend The names of the columns which contain the spend values
#'@param media_names_vec The cleaned names of media irrespective of spend/target
#'@param order_by_increments Flag to specify if the plots should be ordered by the values of the increments/decrements. Default value is F
#'@param spend_type The level of spend/optimized data i.e monthly,weekly or quarterly

    
    ##If the data doesnt contain the quarter details, add the necessary columns
    quarter_cols <- c('quarter','qy')
    
    
    actual_spend_data <- get_quarters(actual_spend_data)
    
    ##Get the latest quarter year string
    latest_qy = actual_spend_data %>% filter((quarter == input_quarter)) %>% summarize_at(.vars = 'qy',.funs = max) %>% pull()
    
    ##Get the quarterly spend of the input quarter by adding all the weekly/monthly spends
    input_quarter_spend = actual_spend_data %>%
                          dplyr::filter(qy == latest_qy) %>%
                          dplyr::select(dma,media_names_spend) %>%
                          dplyr::group_by(dma) %>%
                          dplyr::summarize_all(sum) %>%
                          dplyr::ungroup() %>%
                          dplyr::rename_at(media_names_spend,~media_names_vec)
    
    
    ##Calculate the mean weekly spend by excluding zero weeks for calculation of the GA
    input_weekly_spend = actual_spend_data %>% 
                         dplyr::filter(qy == latest_qy) %>%
                         dplyr::select(dma,media_names_spend) %>%
                         dplyr::group_by(dma) %>%
                         dplyr::summarize_all(exclude_zero_spend_weeks) %>%
                         dplyr::ungroup() %>%
                         dplyr::rename_at(media_names_spend,~media_names_vec)
    
    #optimum_dma = c('NEW YORK','New York')
    
    ##Create a names list of cleaned media names to be used in the plotting
#     cleaned_names = c('Radio','TV','Print','Direct Mail','Audio','Display','Video','Branded Search','Unbranded Search','Social') 
#     cleaned_names_list = as.list(cleaned_names)
#     names(cleaned_names_list) = media_names_vec
    
    
    ##Calculate the gross adds for the optimized spend and the weekly spend (Swap this with any other target variable)
    input_weekly_target = data.frame(gross_add_weekly(input_weekly_spend %>% select(media_names_vec),
                                      min_media_mat,max_media_mat,
                                      spend_activity_ratio_dma_df,
                                      target_max_vec,
                                      prospect_mat_media,
                                      beta_media_mat,K,S))
    if(spend_type != 'quarterly'){
    optimized_target_df = data.frame(gross_add_weekly(optimized_spend_df %>% select(media_names_vec),
                                      min_media_mat,max_media_mat,
                                      spend_activity_ratio_dma_df,
                                      target_max_vec,
                                      prospect_mat_media,
                                      beta_media_mat,K,S))
    
    
    optimized_quarter_spend = getQuarterlydata(optimized_spend_df)
    optimized_quarter_target = getQuarterlydata(optimized_target_df)
    
    
    }else{
      if(is.null(optimized_target_df)){stop(cat('Please input the quarter target values for cascade plot.\n'))}
      optimized_quarter_spend = optimized_spend_df
      optimized_quarter_target = optimized_target_df
    }
    ##Change weekly data to quarter data
    
    nweeks_vec <- rep(nbr_period_q,length(media_names_vec))
    
    ##For only the flight media, replace the number of weeks to number of weeks of flight 
    ##otherwise multiply it with number of periods in quarter
    if(!is.null(flight_media_vec)){
        id_flight_media <- which(media_names_vec %in% flight_media_vec)
        nweeks_vec[id_flight_media] <- var_n_flight_period
    }
    
    
    input_quarter_target = data.frame('dma' = dma_names_vec,mapply(`*`,input_weekly_target,nweeks_vec,SIMPLIFY=FALSE))
    

    
    ##Add the target and spend for each footprint and overall for both input and optimized data
    input_quarter_spend = addFootprintData(input_quarter_spend)
    optimized_quarter_spend = addFootprintData(optimized_quarter_spend)
    
    input_quarter_target = addFootprintData(input_quarter_target)
    optimized_quarter_target = addFootprintData(optimized_quarter_target)
    
    ##Variable name for the spend and target to be used in the display
    spend_var = paste('Total_spend',latest_qy,sep='_')
    target_var    = paste('Total_target',latest_qy,sep='_')
    
    
    ###Extract the DMA level information from the data
    quarter_dma_spend = input_quarter_spend %>%
                        dplyr::filter(dma == DMA) %>%
                        dplyr::select(media_names_vec) %>%
                        tidyr::gather(key = media,value = spend)
    
    optimized_dma_spend = optimized_quarter_spend %>%
                          dplyr::filter(dma == DMA) %>%
                          dplyr::select(media_names_vec) %>%
                          tidyr::gather(key = media,value = spend)
    
    quarter_dma_target =      input_quarter_target %>%
                          dplyr::filter(dma == DMA) %>%
                          dplyr::select(media_names_vec) %>%
                          tidyr::gather(key = media,value = target)
    
    optimized_dma_target =    optimized_quarter_target %>%
                          dplyr::filter(dma == DMA) %>%
                          dplyr::select(media_names_vec) %>%
                          tidyr::gather(key = media,value = target)
    
    

    ##Suffix for the joined dataframe
    #suffix_vec = c(paste0(".",latest_qy),"_optimized")
    
    ##Data for debugging any weird patterns
    ##Spend joined df
    
    opt_spend_long = optimized_quarter_spend %>% 
                     dplyr::select(dma,media_names_vec) %>%
                     tidyr::gather(key = media,value = spend_opt,-dma)

    input_spend_long = input_quarter_spend %>% 
                       dplyr::select(dma,media_names_vec) %>%
                       tidyr::gather(key = media,value = spend_input,-dma)

    spend_diff_df = input_spend_long %>% 
                    dplyr::left_join(opt_spend_long,by = c('dma','media')) %>%
                    dplyr::mutate(spend_diff = spend_input - spend_opt) %>%
                    dplyr::arrange(desc(spend_diff))
    
    ##Target joined df
    
    opt_target_long = optimized_quarterly_target %>% 
                      dplyr::select(dma,media_names_vec) %>%
                      tidyr::gather(key = media,value = target_opt,-dma)

    input_target_long = input_quarter_target %>% 
                        dplyr::select(dma,media_names_vec) %>%
                        tidyr::gather(key = media,value = target_input,-dma)

    target_diff_df = input_target_long %>% 
                     dplyr::left_join(opt_target_long,by = c('dma','media')) %>%
                     dplyr::mutate(target_diff = target_input - target_opt) %>%
                     dplyr::arrange(desc(target_diff))
    
    ##Cascade dfs to get the difference by media of spend and target
    
    dma_spend_cascade = quarter_dma_spend %>%
                        dplyr::left_join(optimized_dma_spend,by = 'media',suffix = c('_input','_optimized')) %>%
                        dplyr::mutate(cascade_spend_scaled = round(as.numeric((spend_optimized - spend_input)/1000))
                                     ) ##Scaling the difference in spend by 1000
    
    
    dma_target_cascade = quarter_dma_target %>%
                     dplyr::left_join(optimized_dma_target,by = 'media',suffix = c('_input','_optimized')) %>%
                     dplyr::mutate(cascade_target = round(as.numeric((target_optimized - target_input))))
    

    ##Based on the order specified by the user, arrange the display order of the graph
    if(order_by_increments){
        order_col = 'cascade_spend_scaled'
    }else{
        order_col = 'spend_input'
    }
    
    dma_spend_cascade = dma_spend_cascade %>% 
                        dplyr::arrange(!!rlang::sym(order_col))
    
    ##Get the order of display 
    display_order = dma_spend_cascade %>% 
                    dplyr::select(media) %>% 
                    dplyr::pull()
    ##Change the target order to be the same as spend
    dma_target_cascade = dma_target_cascade %>%
                     dplyr::mutate(media =  factor(media, levels = display_order)) %>%
                     dplyr::arrange(media)

    ##For display in the graph
    spend_cascade_levels = c(spend_var,display_order,'Total_optimized_spend')
    target_cascade_levels = c(target_var,display_order,'Total_optimized_target')
    
    ##Plot data for the cascade waterfall charts
    dma_spend_plot_data = rbind.data.frame(
                          data.frame("media" = spend_var,"cascade_spend_scaled" = round(sum(dma_spend_cascade$spend_input)/1000),'measure' = 'relative'),
                          dma_spend_cascade %>% 
                          dplyr::mutate(measure = 'relative') %>% 
                          dplyr::mutate_at(.vars = vars(media),.funs = factor,levels=spend_cascade_levels) %>%  
                          dplyr::select(media,cascade_spend_scaled,measure),
                          data.frame("media" = 'Total_optimized_spend',"cascade_spend_scaled" = round(sum(dma_spend_cascade$spend_optimized)/1000),'measure' = 'total')
                                         ) %>%
                          dplyr::mutate(text = paste0('$',as.character(cascade_spend_scaled))
                                       )
    
    dma_target_plot_data = rbind.data.frame(
                       data.frame("media" = target_var,"cascade_target" = round(sum(dma_target_cascade$target_input)),'measure' = 'relative'),
                       dma_target_cascade %>% 
                       dplyr::mutate(measure = 'relative') %>% 
                       dplyr::mutate_at(.vars = vars(media),.funs = factor,levels=target_cascade_levels) %>%  
                       dplyr::select(media,cascade_target,measure),
                       data.frame("media" = 'Total_optimized_target',"cascade_target" = round(sum(dma_target_cascade$target_optimized)),'measure' = 'total')
                                       ) %>%
                       dplyr::mutate(text = as.character(cascade_target)
                                    )
    
    
    ################Spend cascade plot#################################
    
    ##Use plotly to plot the waterfall charts
    ##Spend plot
    y_initial <- dma_spend_plot_data %>% filter(media == spend_var) %>% select(cascade_spend_scaled) %>% pull()

    p1 <- plot_ly(
      dma_spend_plot_data, name = "cascade_spend_plot", type = "waterfall", measure = ~measure,
      x = ~media, textposition = "outside", y= ~cascade_spend_scaled, text = ~text,
        textfont = list(size = 14,
                        family = 'Helvetica',
                        position = "top center"),
      connector = list(line = list(color= "rgb(63, 63, 63)")),
      totals = list(marker = list(color = "royalblue", 
                                line = list(color = 'royalblue', width = 3)))
        ,height = 720,width = 1080
    ) %>%
      layout(paper_bgcolor='black',plot_bgcolor='black',
            title = list(text = sprintf("%s Cascade Spend (000) %s ",DMA,str_replace(latest_qy,'[.]',' ')),
                          font = list(family = 'Helvetica',size = 18)),
            xaxis = list(title = "Media", 
                        ticktext = c('Quarter spend',
                                     as.character(unlist(cleaned_names_list[display_order])),
                                     'Total Optimized Spend'),
                        tickfont = list(family = 'Helvetica',size = 14),
                        tickvals = spend_cascade_levels,
                        tickangle = 270,
                        autorange = T),
            yaxis = list(title = "Spend ($)",
                         autorange = T,
                        showticklabels = T,
                        tickformat = "000",
                        tickfont = list(family = 'Helvetica')),
            autosize = F,
            showlegend = FALSE ,
            shapes = list(
                   list(type = "rect",
                        fillcolor = "royalblue", line = list(color = "royalblue"), opacity = 1,
                        x0 = -0.4, x1 = 0.4, xref = "x",
                        y0 = 0.0, y1 = y_initial, yref = "y")),
             font = list(family = 'Helvetica',
                         color = 'white'),
             margin = list(l=50, r=50, b=50, t=50, pad=4)
            )

    ##################################################################
    
    
    ###############target cascade plot####################################
    
    ##Use plotly to plot the waterfall charts
    ##target plot
    y_initial <- dma_target_plot_data %>% filter(media == target_var) %>% select(cascade_target) %>% pull()
    
    p2 <- plot_ly(
      dma_target_plot_data, name = "cascade_target_plot", type = "waterfall", measure = ~measure,
      x = ~media, textposition = "outside", y= ~cascade_target, text = ~text,
        textfont = list(size = 14,
                        family = 'Helvetica',
                        position = "top center"),
      connector = list(line = list(color= "rgb(63, 63, 63)")),
    totals = list(marker = list(color = "royalblue", 
                                line = list(color = 'royalblue', width = 3)))
        ,height = 720,width = 1080
    ) %>%
      layout(
            paper_bgcolor='black',plot_bgcolor='black',
            title = list(text = sprintf("%s Cascade MGA %s ",DMA,str_replace(latest_qy,'[.]',' ')),
                          font = list(family = 'Helvetica',size = 18)),
             
            xaxis = list(title = "Media", 
                        ticktext = c('Quarter MGA',
                                     as.character(unlist(cleaned_names_list[display_order])),
                                     'Total Optimized MGA'),
                        tickfont = list(family = 'Helvetica',size = 14),
                        tickvals = target_cascade_levels,
                        tickangle = 270,
                        autorange = T),
            yaxis = list(title = "Media Gross Adds",
                         autorange = T,
                        showticklabels = T,
                        tickformat = "000",
                        tickfont = list(family = 'Helvetica')),
            autosize = F,
            showlegend = FALSE ,
            shapes = list(
                   list(type = "rect",
                        fillcolor = "royalblue", line = list(color = "royalblue"), opacity = 1,
                        x0 = -0.4, x1 = 0.4, xref = "x",
                        y0 = 0.0, y1 = y_initial, yref = "y")),
             font = list(family = 'Helvetica',
                         color = 'white'),
             margin = list(l=50, r=50, b=50, t=50, pad=4)
             
            )
    
    #######################################################################
    ##Return the plots
    plot_list = list('Spend_Cascade' = p1,
                     'Gross_Add_Cascade' = p2
                    )
    
    data_list = list('Spend_Difference' = spend_diff_df,
                     'Target_Difference' = target_diff_df)
    
    output_list = list('plots' = plot_list,
                       'diff_data' = data_list)
    
    return(output_list)
    
}