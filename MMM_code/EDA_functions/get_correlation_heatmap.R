#' Correlation heatmap
#'
#'
#'@description
#' A custom utility function to get correlation heatmap for the dataframe
#'
#'@return Returns Correlation matrix but prints Heatmap
#'
#'@param df - Dataframe for selecting the data
#'@param target - vector of target variables
#'@param media - vector of media variables
#'
#'@note
#' get_correlation_heatmap(df = df1, target = c('activity_vol','revenue'), media = c('cross_channel_spend','DM_spend','outbound_callcost'))
#'
#'@export

get_correlation_heatmap = function(df, target, media){

  #---------------------------------------------------------------------#
  #  Check INPUTS                                                       #
  #---------------------------------------------------------------------#
  if (is.null(df)) stop("\n'df' should be provided!\n")
  if((class(df)!="data.frame") & (class(df)!="data.table") & (class(df)!="matrix")) {
    stop("Please pass dataframe df")
  }

  if(class(media)!="character" ) {
    stop("Please pass Vector of media variables")
  }

  if(class(target)!="character" ) {
    stop("Please pass Vector of target variables")
  }

  #---------------------------------------------------------------------#
  #  Plot correlation matrix                                            #
  #---------------------------------------------------------------------#

  # get correlation matrix
  df_cor <- cor(df %>% select(target, media))
  corr_matrix <- df_cor[media,target]

  # Return Correlation Matrix if there is only 1 target or 1 media variable
  if((length(target) == 1) | (length(media) == 1)){

      corr_matrix <- as.data.frame(corr_matrix)
      print("Correlation Heat Map can be printed only if there are more than 1 target and 1 media variables.")
      print("Hence returning only Correlation Matrix for target and media variables")

      colnames(corr_matrix) <- target
      return(corr_matrix)
  }


  # Plot heatmap if there is more than 1 target and 1 media variable
  corr_heat_map <- corrplot(corr_matrix, method = "color",
                            rect.lwd = 1,cl.pos = "b", tl.col = "indianred4", tl.cex = 0.75, cl.cex = 0.75, addCoef.col = "white",
                            number.digits = 2, number.cex = 0.5, tl.srt = 60, col = colorRampPalette(c("darkred","white","midnightblue"))(25))

  return(corr_heat_map)

}
