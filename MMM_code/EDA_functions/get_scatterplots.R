#' Scatterplots
#'
#'
#'@description
#' A custom utility function which plots scatter plots of all target vs media variables
#'
#'@return Returns list of scatter plots
#'
#'@param df - Dataframe for selecting the data
#'@param target - vector of target variables
#'@param media - vector of media variables
#'
#'@note
#' get_scatterplots(df = df1, target = c('activity_vol','revenue'), media = c('cross_channel_spend','DM_spend','outbound_callcost'))
#'
#'@export

get_scatterplots = function(df, target, media){

  #---------------------------------------------------------------------#
  #  Check INPUTS                                                       #
  #---------------------------------------------------------------------#
  if (is.null(df)) stop("\n'df' should be provided!\n")
  if((class(df)!="data.frame") & (class(df)!="data.table")) {
    stop("Please pass dataframe df")
  }


  # list to store objects
  out <- list()

  # View histograms of all media and target variables
  for(target_var in target){
    out[[target_var]] <- plot_scatterplot(df %>% select(target_var, media), by = target_var, ncol = 3L)
  }

  return(out)

}
