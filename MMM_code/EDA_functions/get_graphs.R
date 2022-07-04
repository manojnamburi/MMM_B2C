#' ggplot graph
#'
#'
#'@description
#' A custom utility function to plot graph between x_axis and y_axis variables
#'
#'@details The function selects specified X and y variables from dataframe df and
#'plots the specified graph between X and y variables
#'@details - histogram
#'@details - box plot
#'@details - scatter plot
#'@details - bar/stacked bar plot (by default aggregates - sum)
#'@details - line plot (by default aggregates - sum)
#'
#'
#'@return Returns plot
#'
#'
#'@param df - Dataframe for selecting the data
#'@param y_axis - variable to be plotted on the y-axis
#'@param x_axis - variable to be plotted on the x-axis
#'@param group(optional) - variable on which grouping to be done
#'@param plot_type - box/histogram/bar/stacked_bar/scatter/line
#'
#'- default : histogram (of y variable)
#'@param dma_level(optional) - (yes/no) parameter to specify whether graphs are to be plotted for each DMA
#'
#'- default: no
#'
#'@note get_graphs(df_data, y_axis = 'activity_vol')
#'@note get_graphs(df_data, x_axis = 'calendar_month', y_axis = 'activity_vol', plot_type = 'box')
#'@note get_graphs(df_data, x_axis = 'calendar_month', y_axis = 'activity_vol', plot_type = 'bar', group = 'DMA')
#'@note get_graphs(df_data, x_axis = 'DM_spend', y_axis = 'activity_vol', plot_type = 'scatter', dma_level = 'yes')
#'
#'@export


get_graphs = function(df, x_axis = NULL, y_axis = NULL, group = NULL, plot_type = 'histogram', dma_level = 'no'){

  #---------------------------------------------------------------------#
  #  Check INPUTS                                                       #
  #---------------------------------------------------------------------#
  if (is.null(df)) stop("\n'df' should be provided!\n")
  if((class(df)!="data.frame") & (class(df)!="data.table")) {
    stop("Please pass dataframe df")
  }

  if(is.null(x_axis)) {
    stop("Please pass x variable")
  }


  unique_dma <- cbind(unique(df$dma))
  plots_dma <- list()

  #--------------------------------------------------#
  #  Box plot                                        #
  #--------------------------------------------------#
  if(plot_type == 'box'){

    #************* When grouping variable is not provided***********
    if(is.null(group)){
        #************* Plotting graphs for each DMA ***************
        if(dma_level == 'yes'){
            df1 <- df %>% dplyr::select(y_var = y_axis, dma = 'dma')
            for(dma1 in unique_dma){
              plots_dma[[dma1]] <-  ggplot(data = df1 %>% dplyr::filter(dma == dma1),
                                           aes(y = y_var, fill='indianred2')) +
                                    geom_boxplot(alpha=0.75, width=0.3, outlier.colour = "firebrick") +
                                    theme(axis.text.x = element_text(size=11, angle = 60, hjust = 1, vjust = 1),
                                          axis.text.y = element_text(size = 11),
                                          plot.title = element_text(size = rel(1.5)),
                                          legend.position="none") +
                                    labs(title = paste(y_axis, " -- ", dma1), y=y_axis) +
                                    coord_flip()
            }
            return(plots_dma)
        }
        #************* Plotting graph at aggregated level ****************
        else{
            box_plot <-   ggplot(data = df %>% dplyr::select(y_var = y_axis),
                                 aes(y = y_var)) +
                          geom_boxplot(alpha=0.75, width=0.3, outlier.colour = "firebrick") +
                          theme(axis.text.x = element_text(size=11, angle = 60, hjust = 1, vjust = 1),
                                axis.text.y = element_text(size = 11),
                                plot.title = element_text(size = rel(1.5)),
                                legend.position="none") +
                          labs(title = paste(y_axis), y=y_axis) +
                          coord_flip()
            return(box_plot)
        }
    }

    #********** When grouping variable is provided***********
    else{
        #************* Plotting graphs for each DMA ***************
        if(dma_level == 'yes'){

            df1 <- df %>% dplyr::select(y_var = y_axis, dma = 'dma', class = group)
            for(dma1 in unique_dma){
              plots_dma[[dma1]] <-  ggplot(data = df1 %>% dplyr::filter(dma == dma1),
                                           aes(x=class, y=y_var, fill=class)) +
                                    geom_boxplot(alpha=0.75, width=0.3, outlier.colour = "firebrick") +
                                    labs(title = paste(y_axis, " -- ", dma1), y=y_axis) +
                                    theme(axis.text.x = element_text(size=11),
                                          axis.text.y = element_text(size = 11),
                                          plot.title = element_text(size = rel(1.5)),
                                          legend.position="none") +
                                    coord_flip()
            }
            return(plots_dma)
        }
        #************* Plotting graph at aggregated level ****************
        else{
            df1 <- df %>% dplyr::select(y_var = y_axis, class = group)
            box_plot <-   ggplot(data = df1,
                                 aes(x=class, y=y_var, fill=class)) +
                          geom_boxplot(alpha=0.75, width = 0.3, outlier.colour = "firebrick") +
                          labs(title = paste(y_axis), x = group, y=y_axis) +
                          theme(axis.text.x = element_text(size=11),
                                axis.text.y = element_text(size = 11),
                                plot.title = element_text(size = rel(1.5)),
                                legend.position="none") +
                          coord_flip()
            return(box_plot)
        }
    }

  }

  #--------------------------------------------------#
  #  Histogram plot                                  #
  #--------------------------------------------------#

  if(plot_type == 'histogram'){

    #************* When grouping variable is not provided***********
    if(is.null(group)){
        #************* Plotting graphs for each DMA ***************
        if(dma_level == 'yes'){

            df1 <- df %>% dplyr::select(x_var = x_axis, dma = 'dma')
            for(dma1 in unique_dma){
              plots_dma[[dma1]] <-  ggplot(data = df1 %>% dplyr::filter(dma == dma1),
                                           aes(x=x_var)) +
                                    geom_histogram() +
                                    labs(title=paste(x_axis, " -- ", dma1), x=x_axis)
            }
            return(plots_dma)
        }
        #************* Plotting graph at aggregated level ****************
        else{
            hist_plot <-  ggplot(df %>% dplyr::select(x_var = x_axis),
                                 aes(x=x_var)) +
                          geom_histogram() +
                          labs(title=paste(x_axis), x=x_axis)
            return(hist_plot)
        }
    }

    #********** When grouping variable is provided***********
    else{
        #************* Plotting graphs for each DMA ***************
        if(dma_level == 'yes'){

            df1 <- df %>% dplyr::select(x_var = x_axis, dma = 'dma', class = group)
            for(dma1 in unique_dma){
              plots_dma[[dma1]] <-  ggplot(data = df1 %>% dplyr::filter(dma == dma1),
                                           aes(x=x_var)) +
                                    geom_histogram(aes(color=class)) +
                                    labs(title=paste(x_axis, " -- ", dma1), x=x_axis)
            }
            return(plots_dma)
        }
        #************* Plotting graph at aggregated level ****************
        else{
            hist_plot <-  ggplot(df %>% dplyr::select(x_var = x_axis, class = group),
                                 aes(x=x_var)) +
                          geom_histogram(aes(color=class)) +
                          labs(title=paste(x_axis), x=x_axis)
            return(hist_plot)
        }
    }

  }

  #--------------------------------------------------#
  #  Bar plot                                        #
  #--------------------------------------------------#
  if(plot_type == 'bar'){

    #************* When grouping variable is not provided***********
    if(is.null(group)){
        #************* Plotting graphs for each DMA ***************
        if(dma_level == 'yes'){

            df1 <- df %>% select(x_var = x_axis, y_var = y_axis, dma = 'dma')
            for(dma1 in unique_dma){
              plots_dma[[dma1]] <- ggplot(df1 %>% dplyr::filter(dma == dma1) %>%
                                                  dplyr::group_by(x_var) %>%
                                                  dplyr::summarise(y_var = sum(y_var)),
                                          aes(x=factor(x_var), y=y_var)) +
                                   geom_bar(stat = "identity", fill='indianred2') +
                                   geom_text(aes(label = round(y_var,3), vjust = -0.2)) +
                                   labs(title=paste(Caps(y_axis), " vs ", Caps(x_axis), " -- ", dma1), x=Caps(x_axis), y=Caps(y_axis)) +
                                   ggthemes::theme_stata() +
                                   theme(axis.text.x = element_text(angle = 60, hjust = 1),
                                         plot.title = element_text(hjust = 0.5))
            }
            return(plots_dma)
        }
        #************* Plotting graph at aggregated level ****************
        else{
            bar_plot <-  ggplot(df %>% dplyr::select(x_var = x_axis, y_var = y_axis) %>%
                                       dplyr::group_by(x_var) %>%
                                       dplyr::summarise(y_var = sum(y_var)),
                                aes(x=factor(x_var), y=y_var)) +
                         geom_bar(stat = "identity", fill='indianred2') +
                                   geom_text(aes(label = round(y_var,3), vjust = -0.2)) +
                                   labs(title=paste(Caps(y_axis), " vs ", Caps(x_axis), " -- ", dma1), x=Caps(x_axis), y=Caps(y_axis)) +
                                   ggthemes::theme_stata() +
                                   theme(axis.text.x = element_text(angle = 60, hjust = 1),
                                         plot.title = element_text(hjust = 0.5))
            return(bar_plot)
        }
    }

    #********** When grouping variable is provided***********
    else{
        #************* Plotting graphs for each DMA ***************
        if(dma_level == 'yes'){

            df1 <- df %>% select(x_var = x_axis, y_var = y_axis, dma = 'dma', class = group)
            for(dma1 in unique_dma){
              plots_dma[[dma1]] <-  ggplot(df1 %>% dplyr::filter(dma == dma1) %>%
                                                   dplyr::group_by(x_var, class) %>%
                                                   dplyr::summarise(y_var = sum(y_var)),
                                           aes(x=reorder(factor(x_var), -y_var), y=y_var)) +
                                    geom_bar(stat = "identity", position="dodge", aes(fill=class)) +
                                    labs(title=paste(y_axis, " vs ", x_axis, " -- ", dma1), x=x_axis, y=y_axis) +
                                    theme(axis.text.x = element_text(angle = 60, hjust = 1))
            }
            return(plots_dma)
        }
        #************* Plotting graph at aggregated level ****************
        else{
            grouped_bar_plot <-   ggplot(df %>% dplyr::select(x_var = x_axis, y_var = y_axis, class = group) %>%
                                                dplyr::group_by(x_var, class) %>%
                                                dplyr::summarise(y_var = sum(y_var)),
                                         aes(x=reorder(factor(x_var), -y_var), y=y_var)) +
                                  geom_bar(stat = "identity", position="dodge", aes(fill=class)) +
                                  labs(title=paste(y_axis, " vs ", x_axis), x=x_axis, y=y_axis) +
                                  theme(axis.text.x = element_text(angle = 60, hjust = 1))
            return(grouped_bar_plot)
        }
    }

  }

  #--------------------------------------------------#
  #  Stacked bar plot                                #
  #--------------------------------------------------#
  if(plot_type == 'stacked_bar'){

    if(is.null(group)){
        stop("Please pass 'group' parameter for stacking")
    }
    else{
        #************* Plotting graphs for each DMA ***************
        if(dma_level == 'yes'){

            df1 <- df %>% dplyr::select(x_var = x_axis, y_var = y_axis, dma = 'dma', class = group)
            for(dma1 in unique_dma){
              plots_dma[[dma1]] <-  ggplot(df1 %>% dplyr::filter(dma == dma1),
                                           aes(x=reorder(factor(x_var), -y_var), y=y_var)) +
                                    geom_bar(stat = "identity", aes(fill=class)) +
                                    labs(title=paste(y_axis, " vs ", x_axis, " -- ", dma1), x=x_axis, y=y_axis) +
                                    theme(axis.text.x = element_text(angle = 60, hjust = 1))
            }
            return(plots_dma)
        }
        #************* Plotting graph at aggregated level ****************
        else{
            stacked_bar_plot <-   ggplot(df %>% dplyr::select(x_var = x_axis, y_var = y_axis, class = group),
                                         aes(x=reorder(factor(x_var), -y_var), y=y_var)) +
                                  geom_bar(stat = "identity", aes(fill=class)) +
                                  labs(title=paste(y_axis, " vs ", x_axis), x=x_axis, y=y_axis) +
                                  theme(axis.text.x = element_text(angle = 60, hjust = 1))
            return(stacked_bar_plot)
        }
    }
  }

  #--------------------------------------------------#
  #  scatter plot                                    #
  #--------------------------------------------------#
  if(plot_type == 'scatter'){

    #************* When grouping variable is not provided***********
    if(is.null(group)){
        #************* Plotting graphs for each DMA ***************
        if(dma_level == 'yes'){

            df1 <- df %>% select(x_var = x_axis, y_var = y_axis, dma = 'dma')
            for(dma1 in unique_dma){
              plots_dma[[dma1]] <-  ggplot(df1 %>% dplyr::filter(dma == dma1),
                                           aes(x=x_var, y=y_var)) +
                                    geom_point(color='red2') +
                                    labs(title=paste(y_axis,' vs ',x_axis," -- ", dma1), x=x_axis, y=y_axis)
            }
            return(plots_dma)
        }
        #************* Plotting graph at aggregated level ****************
        else{
            scatter_plot <-  ggplot(df %>% dplyr::select(x_var = x_axis, y_var = y_axis),
                                    aes(x=x_var, y=y_var)) +
                             geom_point(color='red2') +
                             labs(title=paste(y_axis,' vs ',x_axis), x=x_axis, y=y_axis)
            return(scatter_plot)
        }
    }

    #********** When grouping variable is provided***********
    else{
        #************* Plotting graphs for each DMA ***************
        if(dma_level == 'yes'){

            df1 <- df %>% select(x_var = x_axis, y_var = y_axis, dma = 'dma', class = group)
            for(dma1 in unique_dma){
              plots_dma[[dma1]] <-  ggplot(df1 %>% filter(dma == dma1),
                                           aes(x=x_var, y=y_var)) +
                                    geom_point(aes(color=class)) +
                                    labs(title=paste(y_axis,' vs ',x_axis," -- ", dma1), x=x_axis, y=y_axis)
            }
            return(plots_dma)
        }
        #************* Plotting graph at aggregated level ****************
        else{
            scatter_plot <-  ggplot(df %>% dplyr::select(x_var = x_axis, y_var = y_axis, class = group),
                                    aes(x=x_var, y=y_var)) +
                             geom_point(aes(color=class)) +
                             labs(title=paste(y_axis,' vs ',x_axis), x=x_axis, y=y_axis)
            return(scatter_plot)
        }
    }

  }

  #--------------------------------------------------#
  #  line graph                                      #
  #--------------------------------------------------#

  if(plot_type == 'line'){

    #********** When grouping variable is not provided***********
    if(is.null(group)){
        #************* Plotting graphs for each DMA ***************
        if(dma_level == 'yes'){

            df1 <- df %>% dplyr::select(x_var = x_axis, y_var = y_axis, dma = 'dma') %>%
                          dplyr::group_by(x_var, dma) %>%
                          dplyr::summarise(y_var = sum(y_var))
            for(dma1 in unique_dma){
              plots_dma[[dma1]] <-  ggplot(data = df1 %>% dplyr::filter(dma == dma1),
                                           aes(x=x_var, y=y_var)) +
                                    geom_line(aes(group=1), color='red2') +
                                    geom_point(color='darkred') +
                                    labs(title=paste(y_axis,' vs ',x_axis," -- ", dma1), x=x_axis, y=y_axis) +
                                    ggthemes::theme_stata() +
                                    theme(axis.text.x = element_text(angle = 60, hjust = 1))
            }
            return(plots_dma)
        }
        #************* Plotting graph at aggregated level ****************
        else{
            line_plot <-  ggplot(data=df %>% dplyr::select(x_var = x_axis, y_var = y_axis) %>%
                                             dplyr::group_by(x_var) %>%
                                             dplyr::summarise(y_var = sum(y_var)),
                                 aes(x=x_var, y=y_var)) +
                          geom_line(aes(group=1), color='red2') +
                          geom_point(color='darkred') +
                          labs(title=paste(y_axis,' vs ',x_axis), x=x_axis, y=y_axis) +
                          ggthemes::theme_stata() +
                          theme(axis.text.x = element_text(angle = 60, hjust = 1))
            return(line_plot)
        }
    }

    #********** When grouping variable is provided***********
    else{
        #************* Plotting graphs for each DMA ***************
        if(dma_level == 'yes'){

            df1 <- df %>% dplyr::select(x_var = x_axis, y_var = y_axis, dma = 'dma', class = group) %>%
                          dplyr::group_by(x_var, class, dma) %>%
                          dplyr::summarise(y_var = sum(y_var))
            for(dma1 in unique_dma){
              plots_dma[[dma1]] <-  ggplot(data=df1 %>% dplyr::filter(dma == dma1),
                                           aes(x=x_var, y=y_var, group=class, colour=class)) +
                                    geom_line() +
                                    geom_point() +
                                    labs(title=paste(y_axis,' vs ',x_axis," -- ", dma1), x=x_axis, y=y_axis) +
                                    ggthemes::theme_stata() + 
                                    theme(axis.text.x = element_text(angle = 60, hjust = 1))
            }
            return(plots_dma)
        }
        #************* Plotting graph at aggregated level ****************
        else{
            line_plot <-  ggplot(data = df %>% dplyr::select(x_var = x_axis, y_var = y_axis, class = group) %>%
                                               dplyr::group_by(class, x_var) %>%
                                               dplyr::summarise(y_var = sum(y_var)),
                                 aes(x=x_var, y=y_var, group=class, colour=class)) +
                          geom_line() +
                          geom_point() +
                          labs(title=paste(y_axis,' vs ',x_axis), x=x_axis, y=y_axis) +
                          ggthemes::theme_stata() + 
                          theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
                          
            return(line_plot)
        }
    }

  }

}
