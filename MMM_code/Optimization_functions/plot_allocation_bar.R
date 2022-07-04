#if((!require(extrafont))){install.packages("extrafont",repos="http://cran.us.r-project.org")}
#if((!require(reshape2))){install.packages("reshape2",repos="http://cran.us.r-project.org")}
#if((!require(ggdark))){install.packages("ggdark",repos="http://cran.us.r-project.org")}
#source('common_functions_mmm.R')
#'Plot the quarterly Media Mix by DMA
#'
#'
#'@description
#'Function to plot the distribution of media from the Quarterly Media Mix by DMA
#'
#'@details
#'Function that leverages the geom_bar of ggplot2 to show the distribution of the budget
#'among various media channels for the quarter.
#'Takes the input of the DMA and outputs the distribution of the media mix for the quarter
#'
#'@return Returns the histogram of the media mix by DMA
#'
#'@param dma Name of the dma for which the distribution needs to be plotted
#'@param quarterly_mix Input spend dataframe for the media mix
#'@param readable_media_names Clean display names for graphs
#'
#'@export
plot_allocation_bar <- function(DMA,
                                quarterly_mix,
                                readable_media_names = c('Radio','TV','Print','DM','Audio','Display','Video','Branded Search','Unbranded Search','Social')){


  final_cols = c('dma',media_names_vec,'target')

  if(!('dma'%in%colnames(quarterly_mix))){quarterly_mix = quarterly_mix %>% dplyr::mutate(dma = dma_names_vec)}

  if(!(c('target')%in%colnames(quarterly_mix))){stop(cat('Please include the target variable as part of the quarterly mix for output.'))}

  if(length(readable_media_names)!=length(media_names_vec)){stop(cat('Please ensure the readable names and the media names are of the same size.'))}

  ##Add the overall and footprint level metrics
  quarterly_mix <- addFootprintData(quarterly_mix,c(media_names_vec,'target'))



  ##Rename the columns to readable names
  dma_data <- quarterly_mix %>%
              dplyr::filter(dma == DMA)%>%
              dplyr::select(final_cols) %>%
              dplyr::rename_at(.vars = media_names_vec,
                               .funs = ~readable_media_names)

  ##Extract the specific DMA plot data and target value for preparing the ggplot

  dma_plot_data <- dma_data %>%
                   dplyr::select(-'target') %>%
                   tidyr::gather(key = 'media',value = 'spend',-dma) %>%
                   dplyr::mutate(label = format_money(spend),
                                 media = as.character(media),
                                 dma = as.character(dma))  %>%
                   dplyr::arrange(dma,media)

  dma_target_data <- dma_data %>%
                     dplyr::select('target') %>%
                     dplyr::mutate('target' = format(round(target),big.mark = ',')) %>%
                     dplyr::pull()

  output_plot <- ggplot2::ggplot(data = dma_plot_data, aes(x=media, y=spend)) +
    ggplot2::geom_bar(stat='identity',fill = 'steelblue3',color = 'gray9') +
    ggplot2::geom_text(aes(label = label), hjust=0.5,size = 3,color = 'gray9')+

    ggplot2::coord_flip() +
    #scale_x_continuous(breaks=seq(min(dma_plot_data$spend),max(dma_plot_data$spend),length.out = 5))+
    ggplot2::labs(title = sprintf("Quarterly Media Mix for %s",DMA),
         subtitle = sprintf("Total quarterly spend: %s \t Quarterly target: %s"
                            ,format_money(sum(dma_plot_data$spend)),dma_target_data),

         x = "Quarterly spend ($)") +
    ggthemes::theme_clean() +
    ggplot2::theme(
      plot.margin = margin(0.5,0.05,0.5,0.05, "cm"),
      plot.title = element_text(family = "serif", color="black", face="bold", hjust=0.5),
      plot.subtitle = element_text(family = "serif", color="black", face="bold", hjust=0.5),
      axis.text.y = element_text(family = "serif", color = "black",hjust = 1,size = 10),
      legend.position = "None",
      axis.text.x = element_blank(),
      #panel.grid.major.x = element_blank(),
      #panel.grid.major.y = element_blank(),
      #panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.ticks = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank()


    )

  return(list('plot' = output_plot,
              'plot_data' = dma_plot_data))
}
