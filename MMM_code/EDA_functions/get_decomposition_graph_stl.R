#' Decomposition graph for Trend and Seasonality analysis using the STL decomposition
#'
#'
#'@description
#' A custom utility function to plot Decomposition graph using the STL decomposition
#'
#'
#'@return Prints Decomposition plots
#'
#'
#'@param df
#'
#'- dataframe for selecting the data
#'@param frequency
#'
#'- frequency of the data(default = 12)
#'@param target
#'
#'- vector of target variable(s) for which plots to be returned
#'
#'@note
#' get_seasonality_trend(df_data, frequency = 12, target = 'activity_vol')
#'
#'@export

##Reference: https://otexts.com/fpp2/stl.html

get_decomposition_graph_stl <- function(df, frequency = 12, target = NULL){

  plot_list <- list()

  #---------------------------------------------------------------------#
  #  Check INPUTS                                                       #
  #---------------------------------------------------------------------#

  if (is.null(df)) stop("\n'df' should be provided!\n")
  if ((class(df)!="data.frame") & (class(df)!="data.table")) {stop("Please pass dataframe df")}

  if(is.null(target)) {stop("Please pass 'target' variable")}

  #---------------------------------------------------------------------#
  #  Plot graphs for each target variable                                                        #
  #---------------------------------------------------------------------#

  # For each Target variable, compute decomposition and pass to the plot function
  for(trg in target){

    df$target_var <- df[[trg]]
    df1 <- df %>% group_by(period) %>% summarize(target_var = sum(target_var))

    date_start  = min(df1$period)
    tmp_ts      = stats::ts(df1$target_var, freq=frequency, start=decimal_date(date_start))
    decomp      = stats::stl(x = tmp_ts,s.window = 'periodic') ##Using the default decomposition window.

    plt1 <- plot(decomp, main = sprintf("%s - Decomposition of additive time series using STL",trg))



  }

}
