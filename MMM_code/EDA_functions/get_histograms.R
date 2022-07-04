#' Histograms
#'
#'
#'@description
#' A custom utility function which plots histograms of all target and media variables
#'
#'@return Returns list of histograms
#'
#'@param df - Dataframe for selecting the data
#'@param target - vector of target variables
#'@param media - vector of media variables
#'
#'@note
#' get_histograms(df = df1, target = c('activity_vol','revenue'), media = c('cross_channel_spend','DM_spend','outbound_callcost'))
#'
#'@export


get_histograms = function(df, target, media){

  #---------------------------------------------------------------------#
  #  Check INPUTS                                                       #
  #---------------------------------------------------------------------#
  if (is.null(df)) stop("\n'df' should be provided!\n")
  if((class(df)!="data.frame") & (class(df)!="data.table")) {
    stop("Please pass dataframe df")
  }

  # View histograms of all media and target variables
  histogram <- plot_histogram(df %>% select(target, media),ncol = 3L)

  #return(histograms)

}
