if((!require(tidyverse))){install.packages("tidyverse",repos="http://cran.us.r-project.org")}
if((!require(e1071))){install.packages("e1071",repos="http://cran.us.r-project.org")}
if((!require(scales))){install.packages("scales",repos="http://cran.us.r-project.org")}
#source('common_functions_mmm.R')
#################################################################################################################################################
###################################Function to get the outliers by column########################################################################
#################################################################################################################################################

####For categorical variables
##One with the least frequency is considered to be an outlier

####For numerical variables
##Tukey's fences of IQR
##Reference: https://en.wikipedia.org/wiki/Outlier#Tukey's_fences

get_is_outlier_cat <- function(x,mode,least_freq){
    if(sum(x == mode)==sum(x==least_freq)){
       is_outlier = 0
    }else{
        is_outlier = ifelse(x==least_freq,1,0)
    }
    return(is_outlier)
}

get_is_outlier_num = function(x,q1,q3,iqr,fence=1.5){
    ifelse(((q1 - fence * iqr <= x) & (x <= q3 + fence * iqr)),0,1)
}

##Function to get the summary stats and outliers by group
get_summary_group <- function(input_data,
                              group_var = NULL,
                              summary_cols = NULL,
                              fence = 1.5,
                              percentiles = c(0,0.25, 0.5, 0.75,1)){
    output_list = list()
    
    
    
    ##Data checks
    if ((class(input_data)!="data.frame") & (class(input_data)!="data.table")) {stop("Please pass dataframe df")}
    if(!is.null(group_var)){if(!(group_var%in%colnames(input_data))){stop(cat(sprintf('Please ensure %s is part of input_data',group_var)))}}
    
    if(is.null(summary_cols)){summary_cols = colnames(input_data)}
    
    ##Generate the summary statistics for each column by group
    idx = c(1:length(summary_cols))
    
    for(i in idx){
        col_output_list = list()
        var_col = summary_cols[i]
        quo_var_col = rlang::sym(var_col)
        
        cat(sprintf('Fetching summary data for: %s\n',var_col))
        
        
        ####Get the summary statistics like count of rows and missing values by group
        
        summary_group = input_data %>%
                        dplyr::group_by_at(group_var) %>%
                        dplyr::summarize(total_rows = n(),
                                         non_na_rows = sum(!is.na(!!quo_var_col)),
                                         na_rows = total_rows - non_na_rows,
                                         perc_miss = percent(na_rows/total_rows),
                                         type = class(!!quo_var_col)
                                        ) 
        
        ##If the column is numeric
        if(is.numeric(input_data[[var_col]])){
            ##Create a map of the percentiles to be applied by group

            ##Add 0 and 100 as part of the custom percentiles to get min and max
            if(!(0%in%percentiles)){
                percentiles <- c(0,percentiles)
            }
            if(!(1%in%percentiles)){
                percentiles <- c(percentiles,1)
            }
            ##For the purpose of outlier calculations, add the 25th and 75th percentile
            if(!(.25%in%percentiles)){
                percentiles <- c(percentiles,.25)
            }
            if(!(.75%in%percentiles)){
                percentiles <- c(percentiles,.75)
            }

            p_names <- map_chr(percentiles, ~paste0("pctl_", .x*100))
            

            p_funs <- map(percentiles, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
                      set_names(nm = p_names) 
            ####Get the detailed summary including percentiles,mean,min,max,standard deviation,skewness by group
            quantiles_group = input_data %>%
                              dplyr::group_by_at(group_var) %>%
                              dplyr::summarize_at(vars(var_col), p_funs) %>%
                              rename('min' = 'pctl_0','max' = 'pctl_100') %>%
                              dplyr::mutate(IQR = pctl_75 - pctl_25)
            
            ##Additional summary statistics
            summary_stats = input_data %>%
                            dplyr::group_by_at(group_var) %>%
                            dplyr::summarize(
                                             mean = mean(!!quo_var_col),
                                             sd = sd(!!quo_var_col),
                                             skewness = skewness(!!quo_var_col),
                                             Zeroes = sum(!!quo_var_col==0),
                                             Zeroes_perc = percent(Zeroes/n()),
                                             mean_excluding_zero = exclude_zero_spend_weeks(!!quo_var_col),
                                             Missing = sum(is.na(!!quo_var_col)),
                                             Missing_perc = percent(Missing/n())
                                            )
            
            ##Combine the additional metrics with the quantile stats
            if(!is.null(group_var)){
            detail_summary = summary_stats %>%
                             dplyr::left_join(quantiles_group, by = group_var)
            
            ##Check for outliers based on tukey's fences of IQR
            ##Reference: https://en.wikipedia.org/wiki/Outlier#Tukey's_fences
            outlier_data = input_data %>%
                           dplyr::left_join(quantiles_group,by = group_var) %>%
                           dplyr::mutate(is_outlier = get_is_outlier_num(!!quo_var_col,
                                                                     !!rlang::sym('pctl_25'),
                                                                     !!rlang::sym('pctl_75'),
                                                                     !!rlang::sym('IQR')),
                                                                     fence) %>%
                           dplyr::select(all_of(c(colnames(input_data),'is_outlier')))
            }else{
                detail_summary = cbind(summary_stats,quantiles_group)
                
                outlier_data = input_data %>%
                           cbind(quantiles_group) %>%
                           dplyr::mutate(is_outlier = get_is_outlier_num(!!quo_var_col,
                                                                     !!rlang::sym('pctl_25'),
                                                                     !!rlang::sym('pctl_75'),
                                                                     !!rlang::sym('IQR'))) %>%
                           dplyr::select(all_of(c(colnames(input_data),'is_outlier')))
            }
           
            
        }else{
            ##If the column is not an integer or numeric column, the summary statistics in detail will be different
             detail_summary = input_data %>%
                               dplyr::mutate(!!quo_var_col := as.character(!!quo_var_col)) %>%
                               dplyr::group_by_at(group_var) %>%
                               dplyr::summarize(Nrows = n(),
                                                Unique = n_distinct(!!quo_var_col),
                                                Missing = sum(is.na(!!quo_var_col)),
                                                Missing_perc = percent(Missing/n()),
                                                mode = Mode(!!quo_var_col), 
                                                count_mode = sum(!!quo_var_col == mode),
                                                mode_perc = percent(count_mode/n()),
                                                least_freq = get_least_freq(!!quo_var_col),
                                                count_lf = sum(!!quo_var_col == least_freq),
                                                lf_perc = percent(count_lf/n())
                                                )
              if(!is.null(group_var)){
                  outlier_data = input_data %>%
                                  dplyr::left_join(detail_summary,by = group_var)%>%
                                  dplyr::mutate(is_outlier = get_is_outlier_cat(!!quo_var_col,mode,least_freq)) %>%
                                  select(all_of(c(colnames(input_data),'is_outlier')))
              }else{
                  
                  outlier_data =  input_data %>%
                                  cbind(detail_summary)%>%
                                  dplyr::mutate(is_outlier = get_is_outlier_cat(!!quo_var_col,mode,least_freq)) %>%
                                  select(all_of(c(colnames(input_data),'is_outlier')))
                  
              }
            
              
              
           
                  
              }
             ##Generate the summary of just the outlier values
            outlier_summary = outlier_data %>%
                              dplyr::filter(is_outlier == 1) %>%
                              dplyr::select(all_of(c(group_var,var_col,'is_outlier'))) %>%
                              dplyr::arrange(!!rlang::sym(group_var),!!quo_var_col) %>%
                              dplyr::distinct()
            
             col_output_list$summary <- summary_group
             col_output_list$detail  <- detail_summary
             col_output_list$outliers <- outlier_data
             col_output_list$outlier_summary <- outlier_summary
        
             #cat(length(col_output_list))
             
             ##Append the column list to the final output list
             output_list[[var_col]] <- col_output_list
        
    }
    
    return(output_list)
}