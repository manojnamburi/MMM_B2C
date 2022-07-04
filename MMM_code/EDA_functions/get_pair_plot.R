#' Pairplots
#'
#'
#'@description
#' A custom utility function which plots pair plots of all entered vector of variables
#'
#'@return Returns list of scatter plots
#'
#'@param df - Dataframe for selecting the data
#'@param input_vec - vector of variables for which pair plot needs to be produced
#'@param group_var - Grouping variable by which plots will be generated
#'@param fill_var - Any variable by which the plot should be divided by. For example: Old data vs New data 
#'
#'@note
#' get_pair_plot(df = df1, target = c('activity_vol','revenue'), media = c('cross_channel_spend','DM_spend','outbound_callcost'))
#'
#'@export

get_pair_plot = function(df, input_vec, group_var = NULL,fill_var = NULL){

  #---------------------------------------------------------------------#
  #  Check INPUTS                                                       #
  #---------------------------------------------------------------------#
  if (is.null(df)) stop("\n'df' should be provided!\n")
  if((class(df)!="data.frame") & (class(df)!="data.table")) {
    stop("Please pass dataframe df")
  }


  # list to store objects
  plot_list <- list()

  ##Get the vector of groups 
  if(!is.null(fill_var) & !is.null(group_var)){
   unique_grps <- df %>%
                 dplyr::select(group_var) %>%
                 dplyr::distinct() %>%
                 dplyr::pull()  
  grouped_df <- df %>%
                dplyr::group_by(period,!!rlang::sym(group_var),!!rlang::sym(fill_var)) %>%
                dplyr::summarize_at(.vars = vars(input_vec),.funs = sum)
  }else if(!is.null(group_var)){
     unique_grps <- df %>%
                 dplyr::select(group_var) %>%
                 dplyr::distinct() %>%
                 dplyr::pull()
    grouped_df <- df %>%
                dplyr::group_by(period,!!rlang::sym(group_var)) %>%
                dplyr::summarize_at(.vars = vars(input_vec),.funs = sum)
  }else if(!is.null(fill_var)){
    grouped_df <- df %>%
                dplyr::group_by(period,!!rlang::sym(fill_var)) %>%
                dplyr::summarize_at(.vars = vars(input_vec),.funs = sum)
  }


 if(!is.null(group_var)){
      # Plot the pair plot by group
      for(group in unique_grps){
          group_df <- grouped_df %>%
                    filter(!!rlang::sym(group_var) == group) %>%
                    as.data.frame()

                    #print(group_df)
          #str(group_df)
          if(!is.null(fill_var)){
          plot_list[[group]] <- ggpairs(group_df,columns = c(input_vec,fill_var),
                                        aes(color = !!rlang::sym(fill_var)),
                                        columnLabels = c(text_labels,Caps(fill_var)),
                                        upper = list(continuous = wrap("cor", size = 1.5))) +
                                ggtitle(sprintf('Pair Plot for %s',group)) +
                                theme(plot.title = element_text(hjust = 0.5),
                                      axis.text.x = element_text(size = 4,angle = 45),
                                      axis.text.y = element_text(size = 4),
                                      strip.text.x = element_text(size = 6),
                                      strip.text.y = element_text(size = 6,angle = 0)
                                      )
          }else{
            plot_list[[group]] <- ggpairs(group_df,columns = c(input_vec),
                                        #aes(color = !!rlang::sym(fill_var)),
                                        columnLabels = c(text_labels),
                                        upper = list(continuous = wrap("cor", size = 1.5))) +
                                ggtitle(sprintf('Pair Plot for %s',group)) +
                                theme(plot.title = element_text(hjust = 0.5),
                                      axis.text.x = element_text(size = 4,angle = 45),
                                      axis.text.y = element_text(size = 4),
                                      strip.text.x = element_text(size = 6),
                                      strip.text.y = element_text(size = 6,angle = 0)
                                      )
          }
          
        }
 }else{

   plot_list[['All']] <- ggpairs(grouped_df,columns = c(input_vec,fill_var),
                                        aes(color = !!rlang::sym(fill_var)),
                                        columnLabels = c(text_labels,Caps(fill_var)),
                                        upper = list(continuous = wrap("cor", size = 1.5))) +
                                ggtitle(sprintf('Pair Plot for %s','All Data')) +
                                theme(plot.title = element_text(hjust = 0.5),
                                      axis.text.x = element_text(size = 4,angle = 45),
                                      axis.text.y = element_text(size = 4),
                                      strip.text.x = element_text(size = 6),
                                      strip.text.y = element_text(size = 6,angle = 0)
                                      )
 }
  return(plot_list)

}
