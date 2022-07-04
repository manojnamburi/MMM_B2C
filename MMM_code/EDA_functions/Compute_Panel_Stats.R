compute_panel_stats <- function(input_df,input_var_vec,time_var,group_var,n_digits=2)
{
    ##Reference for code: https://tidyeval.tidyverse.org/dplyr.html
    ##Reference for concept: https://www.youtube.com/watch?v=aUVZWnVnjxs
    ##Function to compute the within and between variance for the input_var and group_var
    output_list <- list()
    panel_stats_list <- list()
    final_group_stats_df <- data.frame(matrix(nrow = 0,ncol = 4))
    colnames(final_group_stats_df) <- c('Variable','Mean','Variance','SD')
    final_panel_stats_df <- data.frame(matrix(nrow = 0,ncol = 5))
    colnames(final_panel_stats_df) <- c('Variable','Overall_Mean','Overall_SD','Between_SD','Within_SD')
    
    for(input_var in input_var_vec){
        anot_group <- rlang::sym(group_var) 
        anot_input <- rlang::sym(input_var)
        anot_time <- rlang::sym(time_var)
        
        ##Get number of groups and periods    
        n_groups <- input_df %>%
                    dplyr::distinct(!!anot_group) %>%
                    dplyr::count() %>%
                    dplyr::pull()
        
        t_periods <- input_df %>%
                    dplyr::distinct(!!anot_time) %>%
                    dplyr::count() %>%
                    dplyr::pull()
        
        nt_obs <- n_groups*t_periods
        
        ##Compute the group means of input_var across time_var
        group_means <- input_df %>%
                    dplyr::group_by(!!anot_time,!!anot_group) %>%
                    dplyr::summarize_at(.vars = vars(input_var), .funs = sum) %>%
                    dplyr::group_by(!!anot_group) %>%
                    dplyr::summarize_at(.vars = vars(input_var), .funs = mean) %>%
                    dplyr::ungroup() %>%
                    as.data.frame()
        cat(sprintf('Panel Data Statistics for %s:\n',input_var))
        
        
        group_stats_df <- input_df %>%
                    dplyr::group_by(!!anot_time,!!anot_group) %>%
                    dplyr::summarize_at(.vars = vars(input_var), .funs = sum) %>%
                    dplyr::group_by(!!anot_group) %>%
                    dplyr::summarize_at(.vars = vars(input_var), .funs = list('Mean' = mean,'Variance' = var,'SD' = sd)) %>%
                    dplyr::mutate('Variable' = input_var) %>%
                    dplyr::ungroup() %>%
                    as.data.frame()
        
        ##Compute the overall mean across group and time
        overall_mean <- input_df %>%
                    dplyr::summarize_at(.vars = vars(input_var), .funs = mean) %>%
                    dplyr::ungroup() %>%
                    dplyr::pull()
        panel_stats_list[['Overall_Mean']] <- overall_mean
        cat(sprintf('Overall Mean:%s\n',str_format(overall_mean,n_digits)))
        
        ##Compute the overall SD
        overall_sd <- input_df %>%
                    dplyr::summarize(overall_sd = sd(!!anot_input)) %>%
                    dplyr::select(overall_sd) %>%
                    dplyr::pull()
        
        panel_stats_list[['Overall_SD']] <- overall_sd
        cat(sprintf('Overall Standard Deviation:%s\n',str_format(overall_sd,n_digits)))
            
        ##Compute the between standard deviation
        group_means['overall_mean'] <- overall_mean
        between_sd <- group_means %>%
                    dplyr::mutate(mean_diffs = !!anot_input - overall_mean) %>%
                    dplyr::summarize(between_var = (1/(n_groups-1))*sum(mean_diffs^2)) %>%
                    dplyr::mutate(between_sd = between_var^0.5) %>%
                    dplyr::select(between_sd) %>%
                    dplyr::pull()
        
        panel_stats_list[['Between_SD']] <- between_sd
        
        cat(sprintf('Between Group Standard Deviation:%s\n',str_format(between_sd,n_digits)))
        
        ##Compute the within standard deviation
        group_means['overall_mean'] <- NULL
        
        within_sd <- input_df %>%
                    dplyr::left_join(group_means, by = group_var,suffix = c('','_group')) %>%
                    dplyr::mutate(mean_diffs = !!anot_input - !!rlang::sym(paste(input_var,'group',sep='_'))) %>%
                    dplyr::summarize(within_var = (1/(nt_obs-1))*sum(mean_diffs^2)) %>%
                    dplyr::mutate(within_sd = within_var^0.5) %>%
                    dplyr::select(within_sd) %>%
                    dplyr::pull()
        
        panel_stats_list[['Within_SD']] <- within_sd
        cat(sprintf('Within Group Standard Deviation:%s\n',str_format(within_sd,n_digits)))
        
        panel_stats_df <- as.data.frame(panel_stats_list,stringsAsFactors = F)
        panel_stats_df['Variable'] <- input_var
        panel_stats_df <- panel_stats_df %>%
                          dplyr::select(Variable,everything())
        
        group_stats_df <- group_stats_df %>%
                          dplyr::select(Variable,everything())
        
        final_panel_stats_df <- rbind.data.frame(final_panel_stats_df,panel_stats_df)
        final_group_stats_df <- rbind.data.frame(final_group_stats_df,group_stats_df)

    }
    output_list[['Group_Stats']] <- final_group_stats_df
    output_list[['Panel_Stats']] <- final_panel_stats_df
    return(output_list)
}