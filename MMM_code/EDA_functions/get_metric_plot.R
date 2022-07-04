if((!require(tidyverse))){install.packages("tidyverse",repos="http://cran.us.r-project.org")}
if((!require(cowplot))){install.packages("cowplot",repos="http://cran.us.r-project.org")}
if((!require(gridGraphics))){install.packages("gridGraphics",repos="http://cran.us.r-project.org")}
###About the function
###Used to compute the cost per activity metric for a given media and group
###In order to make different metrics comparable, (for example cost per impression and cost per click),
###this function leverages scale_data file to come up with a scaling factor(powers of 10) to scale the 
###value. The plot also produces the spend and activity along with the metric in order to see any
###abnormal spend or activity by the time_col.


#source('common_functions_mmm.R')
#source('scale_data.R')

get_metric_plot <- function(media_data,
                            activity_cols_vec,
                            spend_cols_vec,
                            media_cols_vec,
                            time_col = 'period',
                            group_var = 'dma'){
    ##Plot the CPI/CPC/Cost per activity metric along with the spend and activity 
    plot_list = list()
    
    ##Input checks
    if(is.list(spend_cols_vec)){stop(cat('Please input a "vector" or "single element" of spend column names'))}
    if(is.list(activity_cols_vec)){stop(cat('Please input a "vector" or "single element" of activity column names'))}
    if(is.list(media_cols_vec)){stop(cat('Please input a "vector" or "single element" of media names'))}
    if(!is.data.frame(media_data)){stop(cat('Please ensure the media_data is a "dataframe"'))}
    
    cat('Please ensure the order of activity columns and spend columns are same.')
    if((length(activity_cols_vec)!= length(spend_cols_vec)) || (length(spend_cols_vec) !=  length(media_cols_vec))){
        stop(cat('Please ensure the spend/activity columns are the same length as that of the media columns\n'))
    }
    if(!all(c(time_col,group_var)%in%colnames(media_data))){
        stop(cat(sprintf('Please ensure both %s and %s are part of the data',time_col,group_var)))
    }
    
    ## Function to create readable words from media names
    Caps <- function(x) {
        s <- strsplit(x, "_")[[1]]
        paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")
    }

    idx = c(1:length(activity_cols_vec))
    for(i in idx){
        
        spend_var = spend_cols_vec[i]
        activity_var = activity_cols_vec[i]
        media_var = media_cols_vec[i]
        
        # cat(sprintf('Plotting for media: %s\n',media_var))
        
        ##Assuming the name of the activity is 'media_activitytype', using the first letter of last part to name the metric
        start_index = regexpr(pattern = "_[^_]*$",text = activity_var,perl = T) ##Get the index of the last '_'
        end_index = start_index + 1
        
        metric_char = substr(activity_var,start = end_index, stop = end_index)
        
        metric_var = paste(media_var,sprintf('cp%s',metric_char),sep = '_')
        mean_metric_var = paste('mean',metric_var,sep = '_')
        
        group_col_vec =  c(time_col,group_var)
        
        quo_spend_var = rlang::sym(spend_var)
        quo_activity_var = rlang::sym(activity_var)
        quo_metric_var = rlang::sym(metric_var)

        ###Group by the time column and the group to obtain the mean spend to activity ratio 
        grouped_data <- media_data %>% 
                   #dplyr::mutate(!!metric_var := !!quo_spend_var/!!quo_activity_var) %>%
                   dplyr::group_by_at(vars(all_of(group_col_vec))) %>%
                   dplyr::mutate(total_spend = sum(!!quo_spend_var),
                            total_activity = sum(!!quo_activity_var),
                            !!mean_metric_var := mean(total_spend/total_activity)) %>%
                   dplyr::mutate_at(.vars = vars(mean_metric_var),.funs = function(x){ifelse(!is.finite(x),0,x)}) %>%
                            #alt_mean_var = mean(!!quo_metric_var)) %>%
                    dplyr::select(all_of(group_col_vec),total_spend,total_activity,all_of(mean_metric_var)) %>%
                    dplyr::distinct() %>%
                    as.data.frame()
        
        
        
        ###Obtain the scaling factor to reduce the decimal points or size of the calculated metric to range of 1-10 
        scaling_data = get_scaling_factor(grouped_data,mean_metric_var,group_var)
        
        
        
        ##Join the scaled data to the grouped data
        merge_data <- grouped_data %>%
                      dplyr::left_join(scaling_data, by = group_var) %>%
                      dplyr::mutate(scaled_data = round(scaling_fac*!!rlang::sym(mean_metric_var),1))
        
        
        
#         ##Get the plot data by converting all the data into long format
#         plot_data <-  merge_data %>%
#                       dplyr::select(-scaling_fac,-mean_metric_var) %>%
#                       dplyr::mutate(scaled_data = round(scaled_data,2)) %>%
#                       dplyr::rename_at(.vars = vars('scaled_data'),~mean_metric_var) %>%
#                       tidyr::gather(metric,value,-all_of(group_col_vec)) %>%
#                       dplyr::select(x_var = time_col,everything())
        
        
        ###Create a list of combined plots for each dma
        plot_sub_list = list()
        group_list = media_data %>%
                     dplyr::select(group_var) %>%
                     dplyr::distinct() %>%
                     pull()
        
        for(grp_el in group_list){
            
            grp_data <- merge_data %>% 
                        dplyr::filter(!!rlang::sym(group_var) == grp_el) 
            
            # cat(sprintf('Plotting for %s: %s\n',group_var,grp_el))
            flush.console()
            
            max_activity <- max(grp_data$total_activity)
            max_spend <- max(grp_data$total_spend)
            
            colors <- c('Activity' = 'blue','Spend' = 'red')

            coeff <- max_activity/max_spend
            
            plot1 <- ggplot(data = grp_data) +
                     geom_line(aes(x = !!rlang::sym(time_col), y = scaled_data)) +
                     geom_point(aes(x = !!rlang::sym(time_col), y = scaled_data)) +
                     labs(y = Caps(metric_var))

            plot2 <- ggplot(data = grp_data , aes(x = !!rlang::sym(time_col))) +
                     geom_line( aes(y=total_spend,color = "Spend"),size = 1) + 
                     geom_line( aes(y=(total_activity/coeff), color = "Activity"),size=1) + 
  
                     scale_y_continuous(
    
                     # Features of the first axis
                     name = "Total Spend",
    
                     # Add a second axis and specify its features
                     sec.axis = sec_axis(~.*coeff, name="Total Activity")
                                       ) +
                      
                      
                      scale_color_manual(values = colors)+ 
                      theme(legend.position = "top",
                            legend.title = element_blank())
            
             ##Combine the two plots together into one panel
             plot_row <- plot_grid(plot1,plot2,nrow = 2)
            
             title <- ggdraw() +
                          draw_label(
                            sprintf('%s: %s',grp_el, Caps(x = metric_var)),
                            fontface = 'bold',
                            x = 0,
                            hjust = -1
                          ) 
                          theme(
                            # add margin on the left of the drawing canvas,
                            # so title is aligned with left edge of first plot
                            plot.margin = margin(3, 0, 0, 0)
                          )
              final_plot <- plot_grid(
                          title, plot_row,
                          ncol = 1,
                          # rel_heights values control vertical title margins
                          rel_heights = c(0.1, 1)
                        )
            
            ###Clean the name of the group object before assigning to the final plot list
            grp_el = gsub(pattern = "[^[:alnum:]]",replacement = '_',x = grp_el)
            plot_sub_list[[grp_el]] = final_plot
    
            
        }
        
        
        ##Plot the graph
#         plot_list[[media_var]] = ggplot(data = plot_data,aes(x = x_var, y = value)) +
#                                  geom_line(size = 1) +
#                                  facet_grid(rows = vars(metric),cols = vars(!!rlang::sym(group_var)), scales = "free_y") +
#                                  ggtitle(sprintf('%s vs %s',Caps(x = metric_var),time_col)) +
#                                  theme(plot.title = element_text(hjust = 0.5))
        
        media_var = gsub(pattern = "[^[:alnum:]]",replacement = '_',x = media_var)
        plot_list[[media_var]] = plot_sub_list
        
        
    }
    return(plot_list)
}