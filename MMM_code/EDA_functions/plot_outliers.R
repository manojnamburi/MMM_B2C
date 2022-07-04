##Mark a point as outlier - Tukey's fences
##For boxplot by variable
fun_is_outlier2 <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}


##Pass the input data through the function to get a box plot and time series plot with outliers
plot_outlier <- function(input_df, var_col, group_var = NULL, new_quarters = NULL, events_ts_df = NULL){
 
    plt_list1 <- list()
    plt_list2 <- list()
    outliers_new_period <- data.frame(matrix(nrow = 0,ncol = 2))
    names(outliers_new_period) <- c('dma','outlier')
    
   
    
    if(is.null(events_ts_df)){
        events_ts_df <- data.frame(matrix(nrow = 0,ncol = 3))
        group_var_name <- ifelse(is.null(group_var),'Group',group_var)
        colnames(events_ts_df) <- c('period',group_var_name,'Event')
    }
    
    if(!is.null(group_var)){
        ##Group the input df by the group_var
        grouped_df <- input_df %>%
                      dplyr::group_by(period,!!rlang::sym(group_var),qy) %>%
                      dplyr::summarize_at(.vars = vars(var_col), .funs = sum) %>%
                      dplyr::ungroup()
    
        unique_grps <- input_df %>%
                       dplyr::select(group_var) %>%
                       dplyr::distinct() %>%
                       dplyr::pull()
        
        for(grp in unique_grps){
            
            plot_data <- grouped_df %>%
                         dplyr::filter(!!rlang::sym(group_var) == grp) %>%
                         dplyr::mutate(outlier = ifelse(fun_is_outlier2(!!rlang::sym(var_col)), !!rlang::sym(var_col), as.numeric(NA)),
                                      outlier_label_qy = ifelse(!is.na(outlier),sprintf('%s(%s)',outlier,qy),as.numeric(NA)),
                                       outlier_label_per = ifelse(!is.na(outlier),sprintf('%s(%s)',outlier,period),as.numeric(NA)))
            
            event_grp_data <- events_ts_df %>%
                              dplyr::filter(!!rlang::sym(group_var) == grp & !is.na(Event))
            
            
            if(!is.null(new_quarters)){
            outliers_new_period <- plot_data %>%
                                   dplyr::filter((!is.na(outlier)) & qy %in% new_quarters) %>%
                                   dplyr::select(period,!!rlang::sym(group_var),outlier,outlier_label_per,outlier_label_qy) 
            }
           
    ##Plot the box plot highlighting the new period points for further analysis
           plt_list1[[grp]] <- ggplot() +
                              ggplot2::geom_boxplot(data = plot_data,aes(  x = factor(!!rlang::sym(group_var)), y = !!rlang::sym(var_col))) +
                              ggrepel::geom_text_repel(data = plot_data,aes( x = factor(!!rlang::sym(group_var)), y = !!rlang::sym(var_col),label = outlier_label_qy), na.rm = TRUE, hjust = -0.3,size = 3,box.padding = 0.5, max.overlaps = Inf)+
                              ggplot2::geom_point(data = outliers_new_period,aes(x = factor(!!rlang::sym(group_var)), y = outlier), color = 'red') +
                              ggplot2::ggtitle(sprintf('Outliers box plot for %s and %s var',Caps(grp),Caps(var_col)))  +
                              ggthemes::theme_stata() +
                              ggplot2::theme(axis.title.x = element_blank(),
                                             axis.title.y = element_blank(),
                                             plot.title = element_text(hjust = 0.5))
            ##Plot the time series plot to visualize the time series component of the data
            plt_list2[[grp]] <- ggplot() +
                              ggplot2::geom_line(data = plot_data,aes(  x = period, y = !!rlang::sym(var_col))) +
                              ggrepel::geom_text_repel(data = plot_data,aes( x = period, y = !!rlang::sym(var_col),label = outlier_label_per), na.rm = TRUE, hjust = -0.3,size = 3,box.padding = 0.5, max.overlaps = Inf)+
                              ggplot2::geom_point(data = outliers_new_period,aes(x = period, y = outlier), color = 'red') + 
                              ggplot2::geom_vline(mapping = aes(xintercept = period,
                                                           colour = Event),
                                             linetype = "dashed",
                                             data = event_grp_data) +
                              ggplot2::ggtitle(sprintf('Outliers timeseries plot for %s and %s var',Caps(grp),Caps(var_col)))  +
                              ggplot2::scale_x_date(breaks = scales::breaks_pretty(7)) +
                              ggthemes::theme_stata() +
                              ggplot2::theme(axis.title.x = element_blank(),
                                             axis.title.y = element_blank(),
                                             plot.title = element_text(hjust = 0.5))
        }
                   
        
    } else{
          plot_data <- input_df %>%
                       dplyr::group_by(period,qy) %>%
                       dplyr::summarize_at(.vars = vars(var_col), .funs = sum) %>%
                       dplyr::mutate(outlier = ifelse(fun_is_outlier2(!!rlang::sym(var_col)), !!rlang::sym(var_col), as.numeric(NA)),
                                    outlier_label_qy = ifelse(!is.na(outlier),sprintf('%s(%s)',outlier,qy),as.numeric(NA)),
                                     outlier_label_per = ifelse(!is.na(outlier),sprintf('%s(%s)',outlier,period),as.numeric(NA)),
                                     Group = 'All')
        
        
         if(!is.null(new_quarters)){
            outliers_new_period <- plot_data %>%
                                   dplyr::filter((!is.na(outlier)) & qy %in% new_quarters) %>%
                                   dplyr::select(Group,outlier,outlier_label_per,outlier_label_qy) 
            }
          
          plt_list1[['All']] <- ggplot() +
                              ggplot2::geom_boxplot(data = plot_data,aes(  x = factor(!!rlang::sym(group_var)), y = !!rlang::sym(var_col))) +
                              ggrepel::geom_text_repel(data = plot_data,aes( x = factor(!!rlang::sym(group_var)), y = !!rlang::sym(var_col),label = outlier_label_qy), na.rm = TRUE, hjust = -0.3,size = 3,box.padding = 0.5, max.overlaps = Inf)+
                              ggplot2::geom_point(data = outliers_new_period,aes(x = factor(!!rlang::sym(group_var)), y = outlier), color = 'red') +
                              ggplot2::ggtitle(sprintf('Outliers box plot for %s and %s var',Caps(grp),Caps(var_col))) +
                              ggthemes::theme_stata() +
                              ggplot2::theme(axis.title.x = element_blank(),
                                             axis.title.y = element_blank(),
                                             plot.title = element_text(hjust = 0.5))
        
        plt_list2[['All']] <- ggplot() +
                              ggplot2::geom_line(data = plot_data,aes(  x = period, y = !!rlang::sym(var_col))) +
                              ggrepel::geom_text_repel(data = plot_data,aes( x = period, y = !!rlang::sym(var_col),label = outlier_label_per), na.rm = TRUE, hjust = -0.3,size = 3,box.padding = 0.5, max.overlaps = Inf)+
                              ggplot2::geom_point(data = outliers_new_period,aes(x = period, y = outlier), color = 'red') +
                              ggplot2::geom_vline(mapping = aes(xintercept = period,
                                                           colour = Event),
                                             linetype = "dashed",
                                             data = event_grp_data) +
                              ggplot2::ggtitle(sprintf('Outliers timeseries plot for %s and %s var',Caps(grp),Caps(var_col)))  +
                              ggplot2::scale_x_date(breaks = scales::breaks_pretty(7)) +
                              ggthemes::theme_stata() +
                              ggplot2::theme(axis.title.x = element_blank(),
                                             axis.title.y = element_blank(),
                                             plot.title = element_text(hjust = 0.5))
    } 
    
    final_plt_list <- list()
    final_plt_list[['box_plots']] <- plt_list1
    final_plt_list[['time_series']] <- plt_list2
    
    return(final_plt_list)
}