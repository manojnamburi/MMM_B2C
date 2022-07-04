##Mean RSquared
rsquared = function(y, ypred){
    tot = sum((y-mean(y))^2)
    res = sum((y-ypred)^2)
    1-(res/tot)
}

rsquared2 = function(y,ypred){
    rsq = cor(y,ypred)^2
}


##Mean MAPE
MAPE = function(y, ypred){
    mean( abs(y-ypred)/abs(y) )*100
}


##Function to get the point rsquared and MAPE
point_metrics <- function(eval_res,stat = 'Mean'){
    ##Extract the OPT,SDL and Altice series - both actual and predicted
   pred_df <- eval_res$y_fitted_stat$ts %>% 
              dplyr::filter(group %in% c('Altice','Optimum','Suddenlink')) %>% 
              dplyr::select(group,y_true,stat)
    
    ##Get the number of parameters and observations to compute the adjusted rsquared
    n_obs <- eval_res$y_fitted_stat$ts %>%
             dplyr::distinct(time) %>%
             dplyr::summarize(count(.)) %>%
             dplyr::pull()
    
    n_var_control <- eval_res$geo_para_stat$control %>%
                      dplyr::distinct(variable_id) %>%
                      dplyr::summarize(count(.)) %>%
                      dplyr::pull()
    n_var_media <- eval_res$geo_para_stat$media %>%
                   dplyr::distinct(variable_id) %>%
                   dplyr::summarize(count(.)) %>%
                   dplyr::pull()
    n_params <- n_var_control + n_var_media
              
              
     ##Compute the point MAPE and rsquared
     point_output_df <- pred_df %>%
                        dplyr::group_by(group) %>%
                        dplyr::summarize(point_rsq = rsquared(y_true,!!rlang::sym(stat)),
                                         point_MAPE = MAPE(y_true,!!rlang::sym(stat)),
                                         point_rsq2 = rsquared2(y_true,!!rlang::sym(stat)),
                                         r_sq_adj = 1-((1-point_rsq2)*(n_obs-1)/(n_obs-n_params-1))) 

                  
      return(point_output_df)           
               
}