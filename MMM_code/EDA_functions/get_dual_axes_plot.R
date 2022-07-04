#' get the dual axes plot
#'
#'
#'@description
#' A custom utility function to get the dual axes plot
#'
#'@return Returns dual axes plot
#'
#'
#'@param df - Dataframe for selecting the data
#'@param left_vector - vector of variables to be aggregated and plotted on the left axis
#'@param right_variable - variable to be plotted on the right axis
#'@param left_axis_label - label to be displayed on the left axis
#'@param right_axis_label - label to be displayed on the right axis
#'@param title - title of the plot
#'
#'@note get_dual_axes_plot(df_XyZ, left_vector = media_spend, right_variable = 'gross_add', left_axis_label = 'Total_spend', right_axis_label = 'Target', title = 'Total Spend vs Target')
#'
#'@export



get_dual_axes_plot <- function(df, left_vector, right_variable, left_axis_label = NULL, right_axis_label = NULL, title = NULL){

  #---------------------------------------------------------------------#
  #  Check INPUTS                                                       #
  #---------------------------------------------------------------------#

  if (is.null(df)) stop("\n'df' should be provided!\n")
  if((class(df)!="data.frame") & (class(df)!="data.table")) {
    stop("Please pass dataframe df")
  }

  if(is.null(left_vector)) {
    stop("Please pass left axis vector")
  }

  if(is.null(right_variable)) {
    stop("Please pass right axis variable")
  }

  if(is.null(left_axis_label)) {
    stop("Please pass left_axis_label")
  }

  #--------------------------------------------------#
  #  Dual axes plot                                  #
  #--------------------------------------------------#

  if(is.null(right_axis_label)){right_axis_label = right_variable}

  plot_data = df %>%
    select('period', left_vector, right_variable = right_variable) %>%
    gather(key = left_vector, value = left_axis_label, -period, -right_variable) %>%
    group_by(period) %>%
    summarize(left_axis_label = sum(left_axis_label),
              right_axis_label = sum(right_variable))

  #print(paste('correlation between ',right_axis_label,' & ',left_axis_label,' : ',cor(plot_data$left_axis_label, plot_data$right_axis_label)))
  cor_val = cor(plot_data$left_axis_label, plot_data$right_axis_label)
  max_left  <- max(plot_data %>% dplyr::select(left_axis = left_axis_label))
  max_right <- max(plot_data %>% dplyr::select(right_axis = right_axis_label))
  coeff <- max_right/max_left

  colors <- c('blue', 'red')
  named_vec = c(left_axis_label, right_axis_label)
  names(colors) <- named_vec
  # dual axes plot
  dual_axes_plot <- ggplot(data = plot_data , aes(x = period,group = 1)) +
    geom_line( aes(y=left_axis_label,color = named_vec[1]),size = 1) +
    geom_line( aes(y=(right_axis_label/coeff),color = named_vec[2]),size=1) +

    scale_y_continuous(

      # Features of the first axis
      name = left_axis_label,

      # Add a second axis and specify its features
      sec.axis = sec_axis(~.*coeff, name=right_axis_label)
    )  +
    scale_color_manual(values = colors) +
    labs(title = title,
         subtitle = paste('correlation between ',right_axis_label,' & ',left_axis_label,' : ',cor_val)) +
    theme_stata() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title = element_blank()) +
    scale_x_date(breaks = breaks_pretty(10))

  return(dual_axes_plot)

}
