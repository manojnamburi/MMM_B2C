if((!require(tidyverse))){install.packages("tidyverse",repos="http://cran.us.r-project.org")}
#####Description and purpose of this file#######
##Serves as a dependency for the get_metric_plot
##The main purpose of these functions is the determine the scaling factor 
##for the cost per activity metric to scale it to a value comparable across
##the group.
##Cost per impressions are usually 1000 times smaller than cost per clicks.
##This function helps to come up with the scaling factor of '1000' to be used 
##to calculate the plot and the metric in get_metric_plot.


##Get the scaling factor by the max value of the metric value
get_multip_factor <- function(max_val){
    if(max_val > 1){
        exp_sym = '[+]'
        tenth_pow = 0.1
    }else{
        exp_sym = '[-]'
        tenth_pow = 10
    }
    multip_fac = tenth_pow^as.integer(str_split(formatC(abs(max_val), format = "e", digits = 2),exp_sym,simplify = T)[2])
    return(multip_fac)
}



###Scaling function to get all the metrics to the same scale across the group
get_scaling_factor <- function(plot_data,axis_var,group_var = NULL){
    if(is.null(group_var)){
        scale_plot_data <- plot_data %>%
                         dplyr::summarize(max_val = max(!!rlang::sym(axis_var))) %>%
                         dplyr::select(max_val) %>%
                         dplyr::mutate(scaling_fac = get_multip_factor(max_val)) %>%
                         dplyr::select(scaling_fac)
        
        
    }else{
        scale_plot_data <- plot_data %>%
                         dplyr::group_by_at(vars(all_of(group_var))) %>%
                         dplyr::summarize(max_val = max(!!rlang::sym(axis_var))) %>%
                         dplyr::select(all_of(group_var),max_val) %>%
                         dplyr::rowwise() %>% 
                         dplyr::mutate(scaling_fac = get_multip_factor(max_val)) %>%
                         dplyr::select(all_of(group_var),scaling_fac)
        
    }
    
    return(scale_plot_data)
}