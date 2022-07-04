#' Decomposition graph for Trend and Seasonality analysis
#'
#'
#'@description
#' A custom utility function to plot Decomposition graph
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
#' get_seasonality(df_data, frequency = 12, target = 'activity_vol')
#'
#'@export

get_decomposition_graph <- function(df, frequency = 12, target = NULL){

  #---------------------------------------------------------------------#
  #  Check INPUTS                                                       #
  #---------------------------------------------------------------------#

  if (is.null(df)) stop("\n'df' should be provided!\n")
  if ((class(df)!="data.frame") & (class(df)!="data.table")) {stop("Please pass dataframe df")}

  if(is.null(target)) {stop("Please pass 'target' variable")}

  #---------------------------------------------------------------------#
  #  Plot graphs for each target variable                                                        #
  #---------------------------------------------------------------------#

  # Function to plot decomposition graph - takes decomposed target variable as input and plots
  my_plot_decomposed_ts = function(x, title="", ...) {
    xx <- x$x
    if (is.null(xx))
      xx <- with(x,
                 if (type == "additive")
                   random + trend + seasonal
                 else
                   random * trend * seasonal)

    plot(cbind(observed = xx, trend = x$trend, seasonal = x$seasonal, random = x$random),
         main=title, ...)
  }

  # For each Target variable, compute decomposition and pass to the plot function
  for(trg in target){

    df$target_var <- df[[trg]]
    df1 <- df %>% group_by(period) %>% summarize(target_var = sum(target_var))

    date_start  = min(df1$period)
    tmp_ts      = stats::ts(df1$target_var, freq=frequency, start=decimal_date(date_start))
    decomp      = stats::decompose(tmp_ts, type="additive")

    my_plot_decomposed_ts(decomp, title = paste(trg, " - Decomposition of additive time series"))

  }

}
