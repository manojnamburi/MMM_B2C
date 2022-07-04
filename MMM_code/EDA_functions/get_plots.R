#' Plots
#'
#'
#'@description
#' A custom utility function which plots
#'
#'- histograms of all media and target variables
#'
#'- scatter plots of each target vs all media variables
#'
#'- correlation heatmap for the media and target variables
#'
#'- correlation spread plots of each target vs all media variables
#'
#'
#'@return Returns a list of below plots
#'
#'-   histograms
#'
#'-   scatter plots
#'
#'-   correlation heatmap
#'
#'-   correlation spread plots
#'
#'
#'
#'@param df - Dataframe for selecting the data
#'@param target - Vector of target variables
#'@param media - Vector of media variables
#'
#'@note
#' Below is the example how we can pass parameters to the function
#'
#' get_plots(df = df1, target = c('activity_vol','revenue'), media = c('cross_channel_spend','DM_spend','outbound_callcost'))
#'
#'
#'Individual functions are also available
#'
#'- get_histograms(df, target, media)
#'
#'- get_scatterplots(df, target, media)
#'
#'- get_correlation_heatmap(df, target, media)
#'
#'- get_correlation_spread(df, target, media)
#'
#'@export

get_plots = function(df, target, media){

  #---------------------------------------------------------------------#
  #  Check INPUTS                                                       #
  #---------------------------------------------------------------------#

  if((class(df)!="data.frame") & (class(df)!="data.table")) {
    stop("Please pass dataframe df")
  }

  if(class(media)!="character" ) {
    stop("Please pass Vector of media variables")
  }

  if(class(target)!="character" ) {
    stop("Please pass Vector of target variables")
  }

  if (any(!(c('dma') %in% names(df)))) {
    if (any(!(c('DMA') %in% names(df)))) {
      stop(paste0("\n'df' should include 'dma' variable to plot correlation spread plots!"))
    }else{
      df <- df %>% rename(dma = DMA)
    }
  }

  #---------------------------------------------------------------------#
  #  Plot graphs                                                        #
  #---------------------------------------------------------------------#
  # list to store all data.frames
  out <- list()

  # View histograms of all media and target variables
  out$histograms <- plot_histogram(df %>% select(target, media),ncol = 3L)

  # View scatterplots of all target vs media variables
  for(target_var in target){
      out$scatterplots[target_var] <- plot_scatterplot(df %>% select(target_var, media), by = target_var, ncol = 3L)
  }

  # View correlation heatmap for the media and target variables
  out$corr_heatmap <- get_correlation_heatmap(df, target, media)

  # View correlation spread plots
  for(target_var in target){
      out$correlation_spread_plots[target_var] <- get_correlation_spread(df %>% select(target_var, media, dma), target_var, media)
  }

  return(out)

}
