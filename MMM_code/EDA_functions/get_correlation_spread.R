#' Correlation Spread Plots
#'
#'
#'@description
#' A custom utility function to plot correlation spread of each target variable with all the media variables
#'
#'@return Returns a list of Correlation Spread - box plots
#'
#'@param df - Dataframe for selecting the data
#'@param target - vector of target variables
#'@param media - vector of media variables
#'@param opt_dma - optimum dma which is to be highlighted (default = NULL)
#'@param clean_media_names - cleaned media names to be displayed in place of actual media variables
#'
#'@note
#' get_correlation_spread(df = df1, target = c('activity_vol','revenue'), media = c('cross_channel_spend','DM_spend','outbound_callcost'),
#'                        clean_media_names = cleaned_names_vec)
#'
#'@export


get_correlation_spread <- function(df, target, media,opt_dma = NULL, clean_media_names = NULL){

  #---------------------------------------------------------------------#
  #  Check INPUTS                                                       #
  #---------------------------------------------------------------------#

  if (is.null(df)) stop("\n'df' should be provided!\n")
  if((class(df)!="data.frame") & (class(df)!="data.table")) {
    stop("Please pass dataframe df")
  }

  if (any(!(c('dma') %in% names(df)))) {
    if (any(!(c('DMA') %in% names(df)))) {
      stop(paste0("\n'df' should include 'dma' variable to plot correlation spread plots!"))
    }else{
      df <- df %>% rename(dma = DMA)
    }
  }

  if (is.null(clean_media_names))  {
    clean_media_names <- media
  }

  #---------------------------------------------------------------------#
  #  Plot correlation spread                                            #
  #---------------------------------------------------------------------#
  out <- list()

  #   setnames(df, old = media, new = clean_media_names)

  for(i in target){
    # select the X and y variables from df and compute correlation
    df_corr <- df %>%
      rename_at(vars(media), ~ clean_media_names) %>%
      select('dma', i, clean_media_names) %>%
      group_by(dma) %>%
      do(data.frame(Cor=t(cor(.[,-1], .[,-1])))) %>%
      slice(1)

    # remove 'Cor.' from column names
    names(df_corr) <- append(c('dma'),substring(names(df_corr[,-1]), 5))

    df_corr_long <- df_corr %>%
      dplyr::select(-i) %>%
      tidyr::gather(media, correlation, -dma) %>%
      dplyr::mutate(media = factor(clean_media_names,levels = as.character(clean_media_names)))

    #     print(df_corr_long %>% select(clean_media_names) %>% distinct() %>% pull())

    df_corr_means <- df_corr_long %>%
      dplyr::group_by(media) %>%
      dplyr::summarize(mean_corr = mean(correlation,na.rm = T)) %>%
      dplyr::mutate(mean_label = round(mean_corr,2)) 

    if(is.null(opt_dma)){
      # Box plot of correlation spread using ggplot2
      plot <-     ggplot(df_corr_long, aes(x=media, y=correlation, fill=media, na.rm = T)) +
        geom_boxplot(alpha=0.75) +
        geom_hline(yintercept=0.0, color = "red") +
        labs(title = paste("Correlation spread - ", i))  +
        stat_summary(fun=mean, colour="darkred", geom="point",
                     shape=18, size=3,show.legend = FALSE,na.rm = T) +
        geom_text(data = df_corr_means, aes(label = mean_label, y = mean_corr),size = 3,nudge_y = 0.025,color = 'black',fontface = 'bold') +
        theme_stata() +
        theme(legend.position="none", axis.text.x = element_text(size=10, angle = 60, hjust = 1, vjust = 1),axis.title.x = element_blank(),
              axis.text.y = element_text(size = 10), plot.title = element_text(size = rel(1.5),hjust = 0.5), plot.subtitle = element_text(size = rel(1),hjust = 0.5))
    }else{
      ##If OPT DMA is included
      df_corr_opt <- df_corr_long %>%
        dplyr::filter(dma %in% opt_dma) %>%
        dplyr::group_by(media) %>%
        dplyr::summarize(mean_corr = mean(correlation,na.rm = T)) %>%
        dplyr::mutate(mean_label = round(mean_corr,2))

      # Box plot of correlation spread using ggplot2
      plot <-     ggplot(df_corr_long, aes(x=media, y=correlation, fill=media, na.rm = T)) +
        geom_boxplot(alpha=0.75) +
        geom_hline(yintercept=0.0, color = "red") +
        labs(title = paste("Correlation spread - ", Caps(i)),
             subtitle = "x indicates Optimum mean correlation values")  +
        stat_summary(fun=mean, colour="darkred", geom="point",
                     shape=18, size=3,show.legend = FALSE,na.rm = T) +
        geom_text(data = df_corr_means, aes(label = mean_label, y = mean_corr),size = 3,nudge_y = 0.025,color = 'black',fontface = 'bold') +
        geom_point(data = df_corr_opt,aes(x = media,y = mean_corr),shape = 4,size = 2) +
        geom_text(data = df_corr_opt, aes(label = mean_label, y = mean_corr),size = 3,color = 'red',fontface = 'bold',hjust = -0.05) +
        theme_stata() +
        theme(legend.position="none", axis.text.x = element_text(size=10, angle = 60, hjust = 1, vjust = 1),axis.title.x = element_blank(),
              axis.text.y = element_text(size = 10), plot.title = element_text(size = rel(1.5),hjust = 0.5), plot.subtitle = element_text(size = rel(1),hjust = 0.5))
    }

    temp_list = list('plot' = plot,'corr_df_means' = df_corr_means,'corr_df_geo' = df_corr)

    out[[i]] <- temp_list
  }

  return(out)

}
