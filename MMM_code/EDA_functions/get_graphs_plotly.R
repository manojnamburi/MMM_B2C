#' plotly - graph
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
#'@details ****for dma level plots, specify group parameter and filter each dma instead of specifying dma_level = 'yes'
#'@details ****for line graph, 'dma_level' parameter is disabled. Instead specify 'group' parameter.
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
#'- default : histogram (of x variable)
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


get_graphs_plotly = function(df, y_axis = NULL, x_axis = NULL, group = NULL, plot_type = 'histogram', dma_level = 'no'){

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

  x <- list(title = x_axis)
  y <- list(title = y_axis)

  unique_dma <- cbind(unique(df$dma))
  plots_dma <- htmltools::tagList()

  #--------------------------------------------------#
  #  Box plot                                        #
  #--------------------------------------------------#

  if(plot_type == 'box'){

    #************* When grouping variable is not provided***********
    if(is.null(group)){
        #************* Plotting graphs for each DMA ***************
        if(dma_level == 'yes'){

            df1 <- df %>% select(x_var = x_axis, dma = 'dma')
            for(dma1 in unique_dma){
              plots_dma[[dma1]] <-  plot_ly(data = df1 %>% filter(dma == dma1),
                                            x = ~x_var,
                                            type = "box") %>%
                                    layout(title = paste(x, " -- ", dma1), xaxis = x)
            }
            return(plots_dma)
        }
        #************* Plotting graph at aggregated level ****************
        else{
            df1 <- df %>% select(x_var = x_axis)
            box_plot <- plot_ly(data = df1,
                                x = ~x_var,
                                type = "box") %>%
                        layout(title = paste(x), xaxis = x)
            return(box_plot)
        }
    }

    #********** When grouping variable is provided***********
    else{
        #************* Plotting graphs for each DMA ***************
        if(dma_level == 'yes'){
            df1 <- df %>% select(x_var = x_axis, dma = 'dma', class = group)
            for(dma1 in unique_dma){
              plots_dma[[dma1]] <-  plot_ly(data = df1 %>% filter(dma == dma1),
                                            x = ~x_var,
                                            type = "box",
                                            color = ~class) %>%
                                    layout(title = paste(x, " -- ", dma1), xaxis = x)
            }
            return(plots_dma)
        }
        #************* Plotting graph at aggregated level ****************
        else{
            box_plot <- plot_ly(data = df %>% select(x_var = x_axis, class = group),
                                x = ~x_var,
                                type = "box",
                                color = ~class) %>%
                        layout(title = paste(x), xaxis = x)
            return(box_plot)
        }
    }
  }


  #--------------------------------------------------#
  #  Histogram plot                                  #
  #--------------------------------------------------#

  if(plot_type == 'histogram'){
    #************* grouping variable is not considered***********
    if(is.null(group)){
        #************* Plotting graphs for each DMA ***************
        if(dma_level == 'yes'){

            df1 <- df %>% select(x_var =x_axis, dma = 'dma')
            for(dma1 in unique_dma){
              plots_dma[[dma1]] <-  plot_ly(data = df1 %>% filter(dma == dma1),
                                            x = ~x_var,
                                            type = "histogram") %>%
                                    layout(title = paste(x, " -- ", dma1), xaxis = x)
            }
            return(plots_dma)
        }
        #************* Plotting graph at aggregated level ****************
        else{
            df1 <- df %>% select(x_var = x_axis)
            hist_plot <- plot_ly(data = df1,
                                 x = ~x_var,
                                 type = "histogram") %>%
                         layout(title = paste(x), xaxis = x)
            return(hist_plot)
        }
    }

    #********** When grouping variable is provided***********
    else{
        #************* Plotting graphs for each DMA ***************
        if(dma_level == 'yes'){

            df1 <- df %>% dplyr::select(x_var = x_axis, dma = 'dma', class = group)
            for(dma1 in unique_dma){
              plots_dma[[dma1]] <-  plot_ly(data = df1 %>% filter(dma == dma1),
                                            x = ~x_var,
                                            type = "histogram",
                                            color = ~class) %>%
                                    layout(title = paste(x, " -- ", dma1), xaxis = x)
            }
            return(plots_dma)
        }
        #************* Plotting graph at aggregated level ****************
        else{
            hist_plot <-  plot_ly(data = df %>% select(x_var = x_axis, class = group),
                                  x = ~x_var,
                                  type = "histogram",
                                  color = ~class) %>%
                          layout(title = paste(x), xaxis = x)
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
              plots_dma[[dma1]] <-  df1 %>% filter(dma == dma1) %>%
                                            group_by(x_var) %>%
                                            summarise(y_var = sum(y_var)) %>%
                                    plot_ly(x = ~x_var,
                                            y = ~y_var,
                                            type = "bar",
                                            text = y,
                                            textposition = 'auto') %>%
                                    layout(title = paste(y, " vs ", x, " -- ", dma1), xaxis = x, yaxis = y)
            }
            return(plots_dma)
        }
        #************* Plotting graph at aggregated level ****************
        else{
            df1 <- df %>% select(x_var = x_axis, y_var = y_axis)
            bar_plot <- df1 %>% group_by(x_var) %>%
                                summarise(y_var = sum(y_var)) %>%
                        plot_ly(x = ~x_var,
                                y = ~y_var,
                                type = 'bar',
                                text = y,
                                textposition = 'auto') %>%
                        layout(title = paste(y, " vs ", x), xaxis = x, yaxis = y)
            return(bar_plot)
        }

    }

    #********** When grouping variable is provided***********
    else{
        #************* Plotting graphs for each DMA ***************
        if(dma_level == 'yes'){
            df1 <- df %>% select(x_var = x_axis, y_var = y_axis, dma = 'dma', class = group)
            for(dma1 in unique_dma){
              plots_dma[[dma1]] <-  plot_ly(data = df1 %>% filter(dma == dma1),
                                            x = ~x_var,
                                            y = ~y_var,
                                            type = "bar",
                                            color = ~class,
                                            text = y,
                                            textposition = 'auto') %>%
                                    layout(title = paste(y, " vs ", x, " -- ", dma1), xaxis = x, yaxis = y)
            }
            return(plots_dma)
        }
        #************* Plotting graph at aggregated level ****************
        else{
            df1 <- df %>% select(x_var = x_axis, y_var = y_axis, class = group)
            grouped_bar_plot <- df1 %>% group_by(x_var, class) %>%
                                        summarise(y_var = sum(y_var)) %>%
                                plot_ly(x = ~x_var,
                                        y = ~y_var,
                                        type = 'bar',
                                        color = ~class)  %>%
                                layout(title = paste(y, " vs ", x), xaxis = x, yaxis = y)
            return(grouped_bar_plot)
        }
    }
  }


  #--------------------------------------------------#
  #  Stacked bar plot                                #
  #--------------------------------------------------#

  if(plot_type == 'stacked_bar'){
    #************* When grouping variable is not provided***********
    if(is.null(group)){
      stop("Please pass 'group' parameter for stacking")
    }

    #********** When grouping variable is provided***********
    else{
        #************* Plotting graphs for each DMA ***************
        if(dma_level == 'yes'){
            df1 <- df %>% select(x_var = x_axis, y_var = y_axis, dma = 'dma', class = group)
            for(dma1 in unique_dma){
              plots_dma[[dma1]] <-  plot_ly(data = df1 %>% filter(dma == dma1),
                                            x = ~x_var,
                                            y = ~y_var,
                                            type = "bar",
                                            color = ~class) %>%
                                    layout(title = paste(y, " vs ", x, " -- ", dma1), xaxis = x, yaxis = y, barmode = 'stack')
            }
            return(plots_dma)
        }
        #************* Plotting graph at aggregated level ****************
        else{
            df1 <- df %>% select(x_var = x_axis, y_var = y_axis, class = group)
            stacked_bar_plot <- plot_ly(data = df1,
                                        x = ~x_var,
                                        y = ~y_var,
                                        type = 'bar',
                                        color = ~class) %>%
                                layout(title = paste(y, " vs ", x), xaxis = x, yaxis = y, barmode = 'stack')
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
              plots_dma[[dma1]] <-  plot_ly(data = df1 %>% filter(dma == dma1),
                                            x = ~x_var,
                                            y = ~y_var,
                                            type = 'scatter',
                                            mode="markers") %>%
                                    layout(title = paste(y, " vs ", x, " -- ", dma1), xaxis = x, yaxis = y)
            }
            return(plots_dma)
        }
        #************* Plotting graph at aggregated level ****************
        else{
            df1 <- df %>% select(x_var = x_axis, y_var = y_axis)
                   scatter_plot <- plot_ly(data = df1,
                                          x = ~x_var,
                                          y = ~y_var,
                                          type = 'scatter',
                                          mode="markers") %>%
                   layout(title = paste(y, " vs ", x), xaxis = x, yaxis = y)
            return(scatter_plot)
        }

    }
    #********** When grouping variable is provided***********
    else{
        #************* Plotting graphs for each DMA ***************
        if(dma_level == 'yes'){

            df1 <- df %>% select(x_var = x_axis, y_var = y_axis, dma = 'dma', class = group)
            for(dma1 in unique_dma){
              plots_dma[[dma1]] <-  plot_ly(data = df1 %>% filter(dma == dma1),
                                            x = ~x_var,
                                            y = ~y_var,
#                                            group = ~class,
                                            color = ~class,
                                            type = 'scatter',
                                            mode="markers") %>%
                                    layout(title = paste(y, " vs ", x, " -- ", dma1), xaxis = x, yaxis = y)
            }
            return(plots_dma)
        }
        #************* Plotting graph at aggregated level ****************
        else{
            df1 <- df %>% select(x_var = x_axis, y_var = y_axis, class = group)
            scatter_plot <- plot_ly(data = df1,
                                    x = ~x_var,
                                    y = ~y_var,
#                                    group = ~class,
                                    color = ~class,
                                    type = 'scatter',
                                    mode="markers") %>%
                            layout(title = paste(y, " vs ", x), xaxis = x, yaxis = y)
            return(scatter_plot)
        }

    }
  }

  #--------------------------------------------------#
  #  line graph                                      #
  #--------------------------------------------------#

  if(plot_type == 'line'){

      #************* When grouping variable is not provided***********
      if(is.null(group)){
          #************* Plotting graph at aggregated level ****************
          df1 <- df %>% select(x_var = x_axis, y_var = y_axis) %>%
                        group_by(x_var) %>%
                        summarise(y_var = sum(y_var))
                 line_plot <- plot_ly(data = df1,
                                     x=~x_var,
                                     y=~y_var,
                                     type="scatter",
                                     mode="lines+markers") %>%
                 layout(title = paste(y, " vs ", x), xaxis = x, yaxis = y)
          return(line_plot)
      }
      #********** When grouping variable is provided***********
      else{
          #************* Plotting graph at group level ****************
          df1 <- df %>% select(x_var = x_axis, y_var = y_axis, class = group) %>%
                        group_by(class, x_var) %>%
                        summarise(y_var = sum(y_var))
                 line_plot <- plot_ly(data = df1,
                                       x=~x_var,
                                       y=~y_var,
#                                       group=~class,
                                       type="scatter",
                                       color=~class,
                                       mode="lines+markers") %>%
                 layout(title = paste(y, " vs ", x), xaxis = x, yaxis = y)
          return(line_plot)
      }
  }

}
