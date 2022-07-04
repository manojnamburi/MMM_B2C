#' get the dual axes plot
#'
#'
#'@description
#' A custom utility function to get the dual axes plot
#'
#'@return Returns dual axes plot for each DMA
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



get_dual_axes_plot_dma <- function(df, left_vector, right_variable, left_axis_label = NULL, right_axis_label = NULL, title = NULL){

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

  dma_names = df %>%
			  select(dma) %>%
			  distinct() %>%
			  pull()
  output_list <- list()
  for(DMA in dma_names){

	  dma_title = paste(title,DMA,sep = ' - ')

	  dma_data = df %>%
	  			 dplyr::filter(dma == DMA) %>%
				 select('period', left_vector,right_variable)

	  output_list[[DMA]] <- get_dual_axes_plot(dma_data, left_vector = left_vector, right_variable = right_variable,left_axis_label = left_axis_label,
                         right_axis_label = right_axis_label, title = dma_title)

	  


  }

  return(output_list)

}
