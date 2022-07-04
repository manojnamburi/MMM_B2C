#if((!require(tidyverse))){install.packages("tidyverse",repos="http://cran.us.r-project.org")}
#if((!require(rstan))){install.packages("rstan",repos="http://cran.us.r-project.org")}

##---------------------------------Text functions----------------------------------------
#'Clean DMA Names
#'
#'@description
#'Function to clean the DMA names by removing special characters
#'
#'@details
#'Clean the names of the DMA by removing special characters and lowering the case. \cr
#'Use this function to clean and standardize DMA names before performing joins across different datasets. \cr
#'This function is applied to a single DMA name. So this needs to be used in conjunction with apply/mutate. \cr
#'
#'@return
#'Returns the cleaned name of the input DMA
#'
#'@param dma_name The name of the DMA that needs to be cleaned
#'
clean_dma <- function(dma_name){
    return(tolower(gsub("[[:punct:]]", "", dma_name)))
}

#'Add a Dollar Format to Money Fields
#'@description
#'Function to change numerical values to dollar format
#'
#'@details
#'Use this function to format any money fields for pretty display. \cr
#'The function takes a single value as input, use this in conjunction with apply functions/mutate for multiple values. \cr
#'Any decimal values are rounded up to the nearest dollar value. \cr
#'The output is a character string with ',' seperation for larger values.
#'
#'@return
#'Returns the dollar formatted value of input_money with ',' seperation for large values.
#'
#'@param input_money Numerical value that needs to be dollar formatted
#'
#'@examples
#'format_money(3.5)
#'format_money(50000)
#'
format_money  <- function(input_money,ndigits = 2) {
  paste0("$", formatC(as.numeric(input_money), format = "f", digits = ndigits, big.mark = ","))
}

#'Convert Variable Names to Human Readable String
#'@description
#'Function to Convert Variable Names to Human Readable String
#'
#'@details
#'Use this function to convert any variable names to human readable strings by removing '_'. \cr
#'Removes the '_' in the variable name and converts the result to Camel Case. \cr
#'Best used for naming axis in plots. Replaces the '_' in the name with ' '. \cr
#'The function takes a single string as input. Use in conjunction with apply functions or mutate.
#'
#'@return
#'Returns the Camel Cased human readable variable name after removing '_' from the name
#'
#'@param var_name Variable name with '_'
#'
#'@examples
#'Caps('sample_var_name')
#'Caps('camel_case')
#'
Caps <- function(var_name) {

  s <- strsplit(var_name, "_")[[1]]

  paste(toupper(substring(s, 1,1)), substring(s, 2),

        sep = "", collapse = " ")

}

#'Convert numbers to comma seperated decimals
#'@description
#'Function to change numerical values to comma seperated decimal numbers with n_digits decimal precision
#'
#'@details
#'Use this function to convert any numerical value to human readable strings by formatting \cr
#'it to a comma seperated decimal with user specified decimal point precision. \cr
#'Rounds to nearest n_digits specified by user.
#'
#'@return
#'Returns the comma seperated string formatted rounded number
#'
#'@param val Decimal or integer value to be formatted
#'@param n_digits Number of digits of precision to display 
#'
#'@examples
#'str_format(1245.56,1)
#'str_format(157985412.6785,2)
#'
str_format <- function(val,n_digits){
    return(formatC(round(val,n_digits), format="f", big.mark=",", digits=n_digits))
}


##--------------------------------Normalization functions----------------------------------------------------------

#'Normalize DMA Media Data
#'@description
#'Function to Normalize specific DMA Media Data by Prospect and MinMax
#'
#'@details
#'This function is used to normalize a media of a DMA by prospect (subs/non_subs) and
#'minmax by media. \cr
#'This function follows the normalization method specified under in the Google paper used to build the MMM model. \cr
#'The minmax can vary based on the normalization technique. It is obtained from the preprocessing module. \cr
#'This is a variant of the normalize_x function which normalizes media across DMA to output a matrix. \cr
#'This function returns a single normalized value of the media for a DMA.
#'
#'@return
#'Returns the normalized value of media for the given DMA
#'
#'@param x Value of media for a DMA.
#'@param min_media_mat The minimum matrix of each media by DMA. Dimensions - n_geo*n_media
#'@param max_media_mat The maximum matrix of each media by DMA. Dimensions - n_geo*n_media
#'@param prospect_mat_media The matrix of subs/non_subs by DMA used to normalize media during preprocessing. Dimensions - n_geo*n_media
#'@param media The name of the media which needs to be normalized.
#'@param dma The DMA whose media is being normalized.
#'
#'
normalize_x_dma <- function(x,min_media_mat,max_media_mat,prospect_mat_media,media,dma){
  dma_id <- which(dma == dma_names_vec)
  media_id <- which(media == media_names_vec)
  range_norm <- max_media_mat[dma_id,media_id] - min_media_mat[dma_id,media_id]
  x_prospect <- x/prospect_mat_media[dma_id,media_id]
  x_norm <- rep(0,length(x))
  if(range_norm==0){
    x_norm <- rep(0,length(x))
  }else{
    x_norm <- (x_prospect - min_media_mat[dma_id,media_id])/range_norm
  }
  ##Replace all the places x_norm is NA with 0
  x_norm[is.na(x_norm)] <- 0
  
  ##Replace the negative values with 0 as well
  x_norm[x_norm<0] <- 0

  ##Cap all the max values at 1
  #x_norm[x_norm>1] <- 1
  return(x_norm)
}

#'Normalize DMA Media Data
#'@description
#'Function to Normalize Media Data by Prospect and MinMax
#'
#'@details
#'This function is used to normalize all media data of all DMA by prospect (subs/non_subs) and
#'minmax by media. \cr
#'This function follows the normalization method specified under in the Google paper used to build the MMM model. \cr
#'The minmax can vary based on the normalization technique. It is obtained from the preprocessing module. \cr
#'The function outputs a matrix of normalized media data by DMA
#'
#'@return
#'Returns the normalized matrix of media data by DMA and media
#'
#'@param x Matrix of media data by DMA and media. Dimensions - n_geo*n_media
#'@param min_media_mat The minimum matrix of each media by DMA. Dimensions - n_geo*n_media
#'@param max_media_mat The maximum matrix of each media by DMA. Dimensions - n_geo*n_media
#'@param prospect_mat_media The matrix of subs/non_subs by DMA used to normalize media during preprocessing. Dimensions - n_geo*n_media
#'
normalize_x <- function(x,min_media_mat,max_media_mat,prospect_mat_media){
##X dim <- nbr_dma*nbr_media
##Check if the sizes of the dataframes are in line
    if(all(dim(min_media_mat) != dim(x))){stop('Check if the min df and X have same dimensions')}
    if(all(dim(max_media_mat) != dim(x))){stop('Check if the max df and X have same dimensions')}
    if(all(dim(min_media_mat) != dim(max_media_mat))){stop('Check if the min df and max df have same dimensions')}
    if(all(dim(x) != dim(prospect_mat_media))){stop('Check if the X and prospect matrix have same dimensions')}

    ##Instantiate the return variable
    x_norm <- 0*x

    range_norm <- max_media_mat - min_media_mat

    ##Prospect normalization
    x_prospect <- x/prospect_mat_media

    ##Min-max normalization
    x_norm <- (x_prospect - min_media_mat)/range_norm

    ##Replace all the places x_norm is NA with 0
    x_norm[is.na(x_norm)] <- 0
    
    ##Replace the negative values with 0 as well
    x_norm[x_norm<0] <- 0

    ##Cap all the max values at 1
    x_norm[x_norm>1] <- 1

    return(x_norm)


}

##------------------------------------------MMM Core functions------------------------------------------

#'Compute the Hill value of media
#'@description
#'Function to compute Hill value of media
#'
#'@details
#'This function is used to compute the hill function value of a single DMA across media. \cr
#'The function uses the estimated values of K(half saturation parameter) and S(Slope parameter). \cr
#'Takes a vector of media and vectors of estimated K and S as inputs and outputs the vector of Hill values. \cr
#'Used to evaluate the estimated values of K and S to visualize the S shaped curves for a DMA. \cr
#'The input media needs to be normalized before using this function.
#'
#'@return
#'Returns the vector of hill values for a DMA
#'
#'@param media Vector of normalized media data. Dimensions - n_media
#'@param K Vector of half saturation parameter. Dimensions - n_media
#'@param S Vector of slope parameter. Dimensions - n_media
#'@note
#'This function cannot be used for a matrix of media values.
#'
hill <- function(media, K, S){
    1/(1+(media/K)^(-S))
}

#The beta K,S form of the function in the paper setting beta as 1
hill2 <- function(media,K,S) {
     1 - (K**S)/(media**S + K**S)
}

#'Compute the first derivative of Hill function
#'@description
#'Function to compute first derivative of the Hill function
#'
#'@details
#'This function is used to compute the first derivative (slope) of the hill function value of a single DMA across media. \cr
#'The function uses the estimated values of K(half saturation parameter) and S(Slope parameter). \cr
#'Takes a vector of media and vectors of estimated K and S as inputs and outputs the vector of first derivative. \cr
#'Used to evaluate the estimated values of K and S to visualize the S shaped curves for a DMA. \cr
#'The input media needs to be normalized before using this function.
#'
#'@return
#'Returns the vector of first derivative of hill values for a DMA
#'
#'@param media Vector of normalized media data. Dimensions - n_media
#'@param K Vector of half saturation parameter. Dimensions - n_media
#'@param S Vector of slope parameter. Dimensions - n_media
#'@note
#'This function cannot be used for a matrix of media values.
#'
hill_derivative <- function(media, K, S){
  upper <- (S/K)*(media/K)^(-S-1)
  lower <- (1 + (media/K)^(-S))^2
  upper/lower
}

#'Compute the second derivative of Hill function
#'@description
#'Function to compute second derivative of the Hill function
#'
#'@details
#'This function is used to compute the second derivative (slope) of the hill function value of a single DMA across media. \cr
#'The function uses the estimated values of K(half saturation parameter) and S(Slope parameter). \cr
#'Takes a vector of media and vectors of estimated K and S as inputs and outputs the vector of second derivative. \cr
#'Used to evaluate the estimated values of K and S to visualize the S shaped curves for a DMA. \cr
#'The input media needs to be normalized before using this function.
#'
#'@return
#'Returns the vector of second derivative of hill values for a DMA
#'
#'@param media Vector of normalized media data. Dimensions - n_media
#'@param K Vector of half saturation parameter. Dimensions - n_media
#'@param S Vector of slope parameter. Dimensions - n_media
#'@note
#'This function cannot be used for a matrix of media values.
#'
hill_second_derivative <- function(media, K, S){
  part1 <- (S/K)*(-S-1)*(media/K)^(-S-2)*(1/K)*(1+(media/K)^(-S))
  part2 <- (S/K)*(media/K)^(-S-1)*2*(-S)*(media/K)^(-S-1)/K
  part3 <- (1+(media/K)^(-S))^3
  (part1-part2)/part3
}

#'Compute the adstock transformation on media data
#'@description
#'Function to compute adstock transformation on media data
#'
#'@details
#'This function is used to apply the lagged transformation (adstock) effect of a single media across all DMA and period. \cr
#'Uses the estimated value of the alpha parameter and the lagged transformed input data to produce vector of weighted transformed output by media. \cr
#'The function computes the ratio of dot product of the weight and lagged media data and sum of the weights.
#'Weights being computed from the estimated alpha parameter of the media and number of lags
#'When using both hill and adstock transformation, this is first applied before applying the hill transformation.
#'
#'@return
#'Returns the vector of adstock transformed media data of length - nbr_periods*n_geo where nbr_periods is number of distinct periods of the data
#'
#'@param idx_media ID of the media for which the adstock transformation needs to be computed
#'@param alpha Vector of the adstock parameter. Dimensions - n_media
#'@param input_media Normalized and lagged media data. Dimensions - (n_geo*nbr_periods)xn_mediaxn_lag
#'@param max_lag Number of max lags applied for lag transformation of media data
#'
#'
adStockGeom <- function(idx_media, alpha,input_data,nbr_periods,nbr_dma,max_lag = 14){

    weight <- c(1,max_lag)
    N <- nbr_periods*nbr_dma
    cum_effect <- c(1,N)

    for(lag in 1:max_lag){
        weight[lag] <- alpha^(lag-1)
    }

    for(t in 1:N){
        #using normalized and lagged media matrix
        cum_effect[t] <- pracma::dot(weight,input_data[t,idx_media,])/sum(weight)
    }

    cum_effect

}

##--------------------------------GA Functions-------------------------------------------

##Control GA for the given period
###Control target value
#'
#'Compute the Target Value for the Given Input Period by DMA
#'
#'@description
#'Function to compute the control portion of the target value by DMA and control variable.
#'
#'@details
#'Computes the control portion of the target for the input period.
#'Takes the control matrix and estimated values of the control parameters and computes the \cr
#'control target value. This will be added to the media portion of the target to obtain the total target for a given period (quarter). \cr
#'Use the by_mean parameter to compute the value based on mean or by pulling the last quarter of the input period. \cr
#'For example, if the input quarter is 'Q3' and the latest 'Q3' in the data is 'Q3 2019', it is used to calculate the control target value \cr
#'If by_mean is true, mean across all 'Q3' quarters is taken.
#'
#'@return
#'Returns the control target value by DMA and control variable
#'
#'@param Z The preprocessed control matrix data
#'@param beta_control_df The posterior means of the control variables by DMA
#'@param control_names Names of the control variables
#'@param data_XyZ Actual data containing the period and DMA columns
#'@param input_period The period (quarter) for which the control values need to be computed
#'@param target_capita_norm_col The prospect(sub/non_sub) by DMA to de-normalize the data
#'@param y_max The max value of the target by DMA to de-normalize the target
#'@param by_mean Flag to compute the control value by mean or use the latest quarter (based on input). Default is F
#'@param nbr_period_q Number of periods that need to be considered in a quarter. Weekly - 13, Monthly - 3
#'
gross_add_control <- function(Z,
                              beta_control_df,
                              control_names,
                              data_XyZ,
                              input_period,
                              y_max,
                              target_capita_norm_col,
                              by_mean = F,
                              nbr_period_q=13){
    data_XyZ <- data_XyZ %>% arrange(match(dma,dma_names_vec),period)
    #cat(sprintf('Starting the computation of Control GA for the quarter: %s\n',input_period))
    dma_names <- data_XyZ %>%
                 dplyr::select(dma) %>%
                 dplyr::distinct() %>%
                 dplyr::pull()

    colnames(Z) <- colnames(beta_control_df)

    Z_df <- data.frame(period = data_XyZ$period,dma = data_XyZ$dma,Z,stringsAsFactors = F)

    ##Ensure that the beta_control_df is a dataframe for further computation
    if(!is.data.frame(beta_control_df)){
      beta_control_df <- as.data.frame(beta_control_df)
    }

    #cat('Sample of control dataframe to verify\n')
    #write.table(head(Z_df),row.names = F)
    ##Add quarterly columns to the data
    Z_df <- get_quarters(Z_df)
    qy_table <- Z_df %>%
                dplyr::group_by(qy) %>%
                dplyr::select(period) %>%
                dplyr::distinct() %>%
                dplyr::count(n()) %>%
                dplyr::select(qy,n)
    #write.table(qy_table,row.names = F)

    ##Calculate the control GA by period
    control_ga_weekly <- function(z_df){
    ga_control <- (y_max*target_capita_norm_col)*(beta_control_df*z_df) ##denorm control portion of target. As target is per capita and max-normalized
    ga_control['dma'] <- dma_names
    return(ga_control)
    }
    ###Get the Control GA either by getting the latest quarter or mean of all the selected quarters
    #cat(sprintf('Computing the weekly quarter df by using %s\n',ifelse(by_mean,'mean of quarters','latest quarter')))
    if(by_mean){
        ##Get all the quarters corresponding to the selected period and subset the Z based
        ##on the input_quarters to get the quarterly data and summarize by mean
        quarter_df <- Z_df %>%
                     dplyr::filter(quarter == input_period) %>%
                     dplyr::group_by(dma,quarter) %>%
                     dplyr::summarize_at(.vars = control_names,.funs = ~nbr_period_q*mean(.))  #Roll up the mean value by week to quarter level
    }else{
        ##Get the latest quarter of the entered input period and fetch the data by DMA
        latest_quarter <- Z_df %>%
                         dplyr::filter(quarter == input_period) %>%
                         dplyr::select(qy) %>%
                         dplyr::summarize_all(.funs = max) %>%
                         dplyr::pull()

        quarter_df <- Z_df %>%
                     dplyr::filter(qy == latest_quarter)

    }

    #cat(sprintf('Dimensions of the weekly quarter df by DMA is: nrow - %s ncol - %s\n',dim(quarter_df)[1],dim(quarter_df)[2]))
    #cat('Columns in the weekly quarter df:\n')
    #cat(paste(c(1:dim(quarter_df)[2]),colnames(quarter_df),sep='.',collapse = '\n'))
    #cat('\n')
    control_GA <- quarter_df %>%
                 group_by(dma) %>%
                 summarize_at(.vars = control_names,.funs = sum)  %>%
                 select(control_names) %>%
                 group_map(~control_ga_weekly(.)) %>%
                 bind_rows() %>% select(-dma)
    #cat(sprintf('Final dimensions of the output: nrow - %s ncol - %s\n',dim(control_GA)[1],dim(control_GA)[2]))
    control_GA <- data.matrix(control_GA)
    ###Remove the names of the control_GA to generate a matrix with no names to be consistent with the older version of function
    control_GA <- unname(control_GA)
    #cat(sprintf('Total Control GA predicted for the input_period %s - %s\n',input_period,sum(control_GA)))
    return(control_GA)

}


###Weekly GA solution
#'
#'Compute the Media Target of the Optimized Solution
#'
#'@description
#'Function to compute the media gross adds by DMA of the optimized solution
#'
#'@details
#'Computes the media gross adds of the optimized solution provided by the optimizer.
#'Takes the solution as the input and outputs the gross adds by media and by DMA using
#'the hill transformation. Used to show the weekly activity.
#'
#'@return
#'Returns the media gross add matrix by media and DMA
#'
#'@param solution The output from the optimizer which is the optimized weekly activity
#'@param min_media_mat The minimum matrix of each media by DMA. Dimensions - n_geo*n_media
#'@param max_media_mat The maximum matrix of each media by DMA. Dimensions - n_geo*n_media
#'@param ratio_dma Spend to activity ratio dataframe of each media by DMA. Useful when modelling using activity media. Dimensions - n_dma*n_media
#'@param y_max The max of the target vector by DMA used for normalizing the target variable in preprocessing. Dimensions - n_geo
#'@param prospect_mat_media The matrix of subs/non_subs by DMA and media used to normalize media during preprocessing. Dimensions - n_geo*n_media
#'@param beta_media_df The matrix of beta coefficients of media by DMA estimated from the model. Dimensions - n_geo*n_media.
#'@param K The vector of the Half-saturation parameter by media. Dimensions - n_media
#'@param S The vector of the Slope parameter by media. Dimensions - n_media
#'@param target_capita_norm_col The prospect(sub/non_sub) by DMA to de-normalize the data
#'
gross_add_weekly <- function(solution,
                             min_media_mat,max_media_mat,
                             ratio_dma,
                             y_max,
                             prospect_mat_media,
                             beta_media_df,K,S,
                             target_capita_norm_col){

  results_dma <- data.frame(0*min_media_mat)
  colnames(results_dma) <- media_names_vec

  solution <- as.matrix(solution)

  normalized_solution <- normalize_x(solution*ratio_dma,min_media_mat,max_media_mat,prospect_mat_media)

  x_hill <- t(apply(normalized_solution,function(x){hill(x,K,S)},MARGIN = 1))

  #x_hill_ga <- beta_media_df[idxdma,idxmedia]*x_hill*y_max[idxdma]*target_capita_norm_col[idxdma]

  x_hill_ga <- (y_max*target_capita_norm_col)*(beta_media_df*x_hill) ##denormalize the media portion of the GA per-capita and max-norm

  results_dma <- x_hill_ga

  return(results_dma)
}

##Compute the Total GA for the period by DMA,Footprint
#'
#'Compute the Actual Target for a Given Period
#'
#'@description
#'Function that computes the actual gross adds of a selected period by DMA and media
#'
#'@details
#'Compute the actual Gross adds - Media gross adds + Control gross adds for a given period
#'by DMA and media. Outputs a list of all the components that go into calculation of the
#'total gross adds of the period like control gross adds and media gross adds.
#'
#'@return
#'List of gross add objects like:
#'gross_add_control - Total Control GA for the selected period
#'gross_add_media - Total media GA
#'gross_add_total - Sum of media GA and control GA for the selected period
#'gross_add_total_sdl - Sum of media GA and control GA of Suddenlink DMAs
#'for the selected period
#'gross_add_total_opt - Sum of media GA and control GA of Optimum DMAs
#'for the selected period
#'gross_add_dma - Sum of media and control GA by DMA for the selected period
#'
#'@param media Input media matrix/optimized activity from the optimizer
#'@param period Selected period for which the Control GA will be computed
gross_add <- function(media, period){

    ##Get the latest quarter id
    quarter_vec <- rep(c('Q1','Q2','Q3','Q4'),each=nbr_period_q)
    dma_quarters <- rep_len(quarter_vec,length.out = nrow(Z)/length(dma_names_vec))
    idx_end <- max(which(dma_quarters==period))
    idx_start <- idx_end - nbr_period_q ##Number of weeks in a quarter
    #message(paste("id_start:",idx_start,"idx_end:",idx_end))
    media_GA <- array(rep(0,nbr_media*nbr_dma), dim = c(nbr_dma,nbr_media))
    control_GA <- array(rep(0,length(control_names)*nbr_dma), dim = c(nbr_dma,length(control_names)))

     for(idxdma in 1:nbr_dma){
        for(idxmedia in 1:nbr_media){
            #normalize the media to evaluate the Hill
            #first divide by prosepct

            media_norm0 <- media[idxdma,idxmedia]/prospect_mat_media[idxdma,idxmedia]

            #then minmax
            if((max_actual_df[idxmedia, idxdma] - min_actual_df[idxmedia,idxdma])==0){
                media_norm <- 0
            }else{
                media_norm <- (media_norm0 - min_actual_df[idxmedia,idxdma])/
                                                (max_actual_df[idxmedia,idxdma] - min_actual_df[idxmedia,idxdma])
            }
            if(media_norm <0){
                media_GA[idxdma, idxmedia]<-0
            } else {
                media_GA[idxdma, idxmedia] <- beta_media_df[idxdma,idxmedia]*
                                                hill(media_norm, K = K[idxmedia], S = S[idxmedia])*
                                                nbr_period_q*(y_max[idxdma]*target_capita_norm_col[idxdma])
            }

        }#idxmedia

    }#idxdma

    for(idxdma in 1:nbr_dma){
      for(idxcontrol in 1:length(control_names)){
            control_GA[idxdma, idxcontrol] <- beta_control_df[idxdma,idxcontrol]*
                                  sum(Z[(idx_start:idx_end)*idxdma,idxcontrol])*#sum accross the weeks
                                  (y_max[idxdma]*target_capita_norm_col[idxdma])
        }
    }

    gross_add_total <- sum(control_GA) + sum(media_GA)
	gross_add_total_sdl <- sum(control_GA[-id_opt,]) + sum(media_GA[-id_opt,])
	gross_add_total_opt <- sum(control_GA[id_opt,]) + sum(media_GA[id_opt,])
	gross_add_dma <- apply(control_GA,FUN=sum,MARGIN = 1) + apply(media_GA, FUN = sum, MARGIN = 1)

    list("gross_add_control" = sum(control_GA),
         "gross_add_media" = sum(media_GA),
         "gross_add_total" = gross_add_total,
	       "gross_add_total_sdl" = gross_add_total_sdl,
         "gross_add_total_opt" = gross_add_total_opt,
         "gross_add_dma" = gross_add_dma)

}

##Compute the Total target for the period by DMA,Media
#'
#'Compute the Actual Target for a Given Period
#'
#'@description
#'Function that computes the model driven target value (no adstock) for given media and DMA
#'
#'@details
#'Compute the Media target for given spend
#'by DMA and media. Outputs the 
#'media target for the given spend by DMA and media.
#'
#'@return
#'
#'target - The media target value for the given spend, DMA and media
#'
#'@param input_spend The input spend for which the target value is being computed 
#'@param min_media_mat The minimum matrix of each media by DMA. Dimensions - n_geo*n_media
#'@param max_media_mat The maximum matrix of each media by DMA. Dimensions - n_geo*n_media
#'@param target_max_vec The max of the target vector by DMA used for normalizing the target variable in preprocessing. Dimensions - n_geo
#'@param prospect_mat_media The matrix of subs/non_subs by DMA and media used to normalize media during preprocessing. Dimensions - n_geo*n_media
#'@param beta_media_mat The matrix of beta coefficients of media by DMA estimated from the model. Dimensions - n_geo*n_media.
#'@param K The vector of the Half-saturation parameter by media. Dimensions - n_media
#'@param S The vector of the Slope parameter by media. Dimensions - n_media
#'@param target_capita_norm_col The prospect(sub/non_sub) by DMA to de-normalize the target
#'@param media_name The media for which the media target (without adstock) is being computed
#'@param dma_name The DMA for which the media target (without adstock) is being computed 
#'
#'
get_target_dma_media <- function(input_spend,
                                 min_media_mat,
                                 max_media_mat,
                                 beta_media_mat,
                                 prospect_mat_media,
                                 media_name,
                                 dma_name,K,S,
                                 target_capita_norm_col,
                                 target_max_vec)
{
  ##Normalize the input spend
  input_spend_norm <- normalize_x_dma(input_spend,min_media_mat,max_media_mat,prospect_mat_media,media_name,dma_name)
  
  dma_id = which(dma_names_vec%in%dma_name)
  media_id = which(media_names_vec%in%media_name)
  ##Compute the target value from the normalized spend and denormalize the target (no adstock)
  target <- beta_media_mat[dma_id,media_id]*hill(input_spend_norm, K = K[media_id], S = S[media_id])*target_max_vec[dma_id]*target_capita_norm_col[dma_id][1]
  
  target <- ifelse(is.na(target),0,target)
  return(target)
}


##------------------------------------Support Functions---------------------------------------------

#'Add Quarter Columns to the Input Data
#'@description
#'Function to Add Quarter Columns to the Input Data
#'
#'@details
#'This function adds quarter related columns - 'quarter_year','year' and 'quarter' to any input data \cr
#'with the fields DMA and period. \cr
#'This function uses the global parameter nbr_period_q which is the number of periods in a quarter: \cr
#'If data is weekly, nbr_period_q = 13. If data is monthly, nbr_period_q = 3. \cr
#'This differs from the traditional quarter assigned by the datetime package as this function assumes each quarter has
#'fixed number of weeks (13). \cr
#'The year column is derived from the period. \cr
#'The period column is converted to date format using the as.Date function
#'
#'
#'@return
#'Returns the input data with the columns qy,year and quarter added.
#'
#'@param input_data Input data with the columns dma and period
#'
#'@examples
#'get_quarters(opt_data)
#'get_quarters(sdl_data)
#'
#'@note
#'nbr_period_q should be part of the GlobalEnv or any calling Env inorder to use this function.
#'
get_quarters <- function(input_data){
  input_data <- as.data.frame(input_data)
  nbr_dma <- length(input_data %>% select(dma) %>% distinct() %>% pull())
  ##Clean the DMA names by removing special characters and lowering the case for proper ordering
  input_data <- input_data %>% 
                dplyr::mutate(dma_clean = clean_dma(dma)) 

  ##Extract the cleaned dma names to a vector
  dma_clean_vec <- input_data %>%
                   dplyr::distinct(dma_clean) %>%
                   dplyr::arrange(dma_clean) %>%
                   dplyr::pull()
  
  ##Arrange alphabetically the data based on the clean names and remove the mutated column
  input_data <- input_data %>% dplyr::arrange(match(dma_clean,dma_clean_vec),period) %>% select(-dma_clean)
  input_data$period <- as.Date(input_data$period)
  quarter_cols <- c('quarter','qy')
  if(all(quarter_cols%in%colnames(input_data))){
    return(input_data)
  }
  input_data$year <- sapply(input_data$period,FUN = function(x){format(x,format = '%Y')})


  ###Get the counts of the years in the input_data
  y_count <- input_data %>%
    dplyr::group_by(dma,year) %>%
    dplyr::summarize(count = n()) %>%
    dplyr::ungroup()
  ###Filter out all years that dont have a full quarter at least
  years <- y_count %>%
    dplyr::filter(count>=(nbr_period_q-1)) %>%
    dplyr::select(year) %>%
    dplyr::distinct() %>%
    dplyr::pull()
  years_vec <- rep(years,each = nbr_period_q*4)
  dma_years <- rep_len(years_vec,length.out = nrow(input_data)/nbr_dma)
  quarter_vec <- rep(c('Q1','Q2','Q3','Q4'),each=nbr_period_q)
  dma_quarters <- rep_len(quarter_vec,length.out = nrow(input_data)/nbr_dma)
  #quarters_vec <- rep(dma_quarters,nbr_dma)
  dma_qy <-  paste(dma_quarters,dma_years,sep='_')
  #qy_vec <- rep(dma_qy,nbr_dma)
  input_data['quarter'] <- dma_quarters
  input_data['qy'] <- dma_qy
  #input_data <- input_data %>% select(-year)
  return(input_data)
}

#'
#'Add Footprint level data by period
#'
#'@description
#'Function that adds aggregate data by Footprint by period
#'
#'@details
#'This function is used to add Footprint level aggregate data (Altice, Suddenlink and Optimum) as new DMAs
#'to the input_data, aggregated by period. \cr
#'Used in visualization functions to display metrics at the footprint level.
#'The default aggregation function used is sum. The aggregation function can be changed by passing the name of the aggregator as a string.
#'The function produces 3 new records by period with the data aggregated using group by period and aggregate function.
#'
#'@return
#'Input data subset to the columns being aggregated,period and DMA with new records for the footprint level data
#'
#'@param input_data The data containing period and DMA to which the footprint data needs to be added
#'@param metric_cols The columns for which the footprint data need to be added. Default value is media_names_vec
#'@param agg_func The function that needs to be used to generate the Footprint level aggregate data. Default is 'sum'
#'
#'@note
#'This function uses optimum_dma from the .GlobalEnv or the calling environment to compute the aggregate value for Optimum.
#'Does not remove the already existing Optimum data even if its a single Optimum DMA. If no metric_cols are provided, uses media_names_vec parameter from
#'.GlobalEnv or calling environment.
#'
#'
addFootprintDatabyPeriod <- function(input_data,
                                metric_cols=media_names_vec,
                                agg_func = 'sum'){

    ##Just to avoid multiple creations of agg dma rows, filter out these dmas apriori
    input_data <- input_data %>%
                    dplyr::filter(!(dma %in% c('Altice USA','Suddenlink','Optimum')))

    metric_cols <- rlang::enquo(metric_cols)
    #agg_func <- rlang::enquo(agg_func)

    input_data <-  rbind.data.frame(
                        input_data %>% select(period,dma,!!metric_cols),
                        data.frame(dma = 'Altice USA',
                                input_data %>%
                                dplyr::group_by(period) %>%
                                dplyr::select(!!metric_cols) %>%
                                dplyr::summarize_at(.vars = vars(!!metric_cols),.funs = agg_func) %>%
                                dplyr::select(period,!!metric_cols)
                                ),##Overall Altice spend
                        data.frame(dma = 'Suddenlink',
                                input_data %>%
                                dplyr::group_by(period) %>%
                                dplyr::filter(!(dma %in% optimum_dma)) %>%
                                dplyr::select(!!metric_cols) %>%
                                dplyr::summarize_at(.vars = vars(!!metric_cols),.funs = agg_func) %>%
                                dplyr::select(period,!!metric_cols)
                                ),##Suddenlink spend
                        data.frame(dma = 'Optimum',
                                    input_data %>%
                                    dplyr::group_by(period) %>%
                                    dplyr::filter(dma%in% optimum_dma) %>%
                                    dplyr::select(!!metric_cols) %>%
                                    dplyr::summarize_at(.vars = vars(!!metric_cols),.funs = agg_func) %>%
                                    dplyr::select(period,!!metric_cols)
                                ) ##Optimum spend

                        ) %>% dplyr::distinct()

    return(input_data)

}

#'
#'Add Footprint level data
#'
#'@description
#'Function that adds aggregate data by Footprint
#'
#'@details
#'This function is used to add Footprint level aggregate data (Altice, Suddenlink and Optimum) as new DMAs
#'to the input_data, not aggregated at period level. \cr
#'Used in visualization functions to display metrics at the footprint level.
#'The default aggregation function used is sum. The aggregation function can be changed by passing the name of the aggregator as a string.
#'The function produces 3 new records by period with the data aggregated using aggregate function. (Drops period level)
#'
#'@return
#'Input data subset to the columns being aggregated and DMA column along with new records for the footprint level data
#'
#'@param input_data The data containing period and DMA to which the footprint data needs to be added
#'@param metric_cols The columns for which the footprint data need to be added. Default value is media_names_vec
#'@param agg_func The function that needs to be used to generate the Footprint level aggregate data. Default is 'sum'
#'
#'@note
#'This function uses optimum_dma from the .GlobalEnv or the calling environment to compute the aggregate value for Optimum.
#'Does not remove the already existing Optimum data even if its a single Optimum DMA. If no metric_cols are provided, uses media_names_vec parameter from
#'.GlobalEnv or calling environment.
#'
#'
addFootprintData <- function(input_data,
                             metric_cols=media_names_vec,
                             agg_func = 'sum'){
    metric_cols <- enquo(metric_cols)
    if(!('dma'%in%colnames(input_data))){
      input_data <- input_data %>% dplyr::mutate(dma = dma_names_vec)
    }
    input_data <-  rbind.data.frame(
                        input_data %>% select(dma,!!metric_cols),
                        data.frame(dma = 'Altice USA',
                                input_data %>%
                                dplyr::select(!!metric_cols) %>%
                                dplyr::summarize_at(.vars = vars(!!metric_cols),.funs = agg_func)
                                ),##Overall Altice spend
                        data.frame(dma = 'Suddenlink',
                                input_data %>%
                                dplyr::filter(!(dma %in% optimum_dma)) %>%
                                dplyr::select(!!metric_cols) %>%
                                dplyr::summarize_at(.vars = vars(!!metric_cols),.funs = agg_func)
                                ),##Suddenlink spend
                        data.frame(dma = 'Optimum',
                                    input_data %>%
                                    dplyr::filter(dma%in% optimum_dma) %>%
                                    dplyr::select(!!metric_cols) %>%
                                    dplyr::summarize_at(.vars = vars(!!metric_cols),.funs = agg_func)
                                ) ##Optimum spend

                        )

    return(input_data)

}


#'
#'Aggregate weekly/monthly data to Quarterly level by DMA
#'
#'@description
#'Function to generate Quarterly level KPIs from weekly/monthly data by DMA
#'
#'@details
#'This function is used to generate Quarterly level KPIs by DMAs aggregated at the DMA level.
#'If the input data does not have a column name with 'dma', uses the dma_names_vec from the GlobalEnv to add the column for aggregation.
#'
#'@return
#'nbr_period_q*weekly_data aggregated at DMA level
#'
#'@param weekly_data The weekly/monthly data that needs to be rolled up to quarterly level by DMA.
#'
#'@note
#'This function uses nbr_period_q from the .GlobalEnv or calling environment to generate the quarterly level metrics.
#'
getQuarterlydata <- function(weekly_data){

    if(!('dma'%in%colnames(weekly_data))){
        weekly_data <- weekly_data %>%
                        dplyr::mutate(dma = dma_names_vec)
    }

    quarterly_data <- weekly_data %>%
                    dplyr::group_by(dma) %>%
                    tidyr::gather(key = media, value = weekly_kpi,-dma) %>%
                    dplyr::mutate(weekly_kpi = nbr_period_q*weekly_kpi) %>%
                    tidyr::spread(key = media, value = weekly_kpi) %>%
                    dplyr::select(dma,media_names_vec) %>%
                    dplyr::ungroup()

    return(quarterly_data)
}


##---------------------------------Support functions-------------------------------------------------------

#'
#'Exclude Zero spending periods from mean spending calculation
#'
#'@description
#'Function to Calculate the mean media spending by DMA by excluding periods with zero spending
#'
#'@details
#'This function is used along with dplyr mutate to calculate the mean spending of a media by DMA if
#'zero spending periods are excluded. As some media have more zero spending periods than the others, the optimized results
#'might not be reflective of the actual spending levels. This function is used to calculate the mean spending values of medias by DMAs
#'which is used to compute the limits of simulation as well as optimization
#'@return
#'mean_media_spend dataframe which is mean of spending values removing all periods of zero spending
#'
#'@param media_spend The spend data for which mean needs to be computed excluding zero periods of spending
#'
#'@note
#'This function is to be used as part of summarize_all and group by
#'
exclude_zero_spend_weeks <- function(media_spend){
    mean_media_spend <- mean(media_spend)
    if(mean_media_spend==0){

    }else{
        media_spend <- media_spend[media_spend > 0 ]
        mean_media_spend <- mean(media_spend)
    }
    return(mean_media_spend)
}

#'
#'Compute the Mode of Data
#'
#'@description
#'Function to compute the mode of input data
#'
#'@details
#'This function is used to compute the mode of given categorical vector as R doesnt have an inbuilt mode function
#'
#'@return
#'Mode or element with highest frequency of the vector
#'
#'@param v Vector of categorical values
#'
#'@examples
#'input_vec <- c(3,4,4,5)
#'Mode(input_vec)
#'
#'@note
#'When two elements have the same highest frequency, it returns the first element.
#'
Mode <- function(v) {
   uniqv <- unique(v)
   max_vals <- uniqv[which.max(tabulate(match(v, uniqv)))]
}

#'
#'Compute the Element with Least Frequency
#'
#'@description
#'Function to compute the element with the least frequency
#'
#'@details
#'This function is used to compute the element with least frequency of given categorical vector
#'
#'@return
#'Element with least frequency in the vector
#'
#'@param v Vector of categorical values
#'
#'@examples
#'input_vec <- c(3,4,4,5)
#'get_least_freq(input_vec)
#'
#'@note
#'When two elements have the same least frequency, it returns the first element.
#'
get_least_freq <- function(v){
    uniqv <- unique(v)
    uniqv[which.min(tabulate(match(v, uniqv)))]
}

##----------------------------------Custom Logging function--------------------------------------------

##Function to create a log file for future reference
##Set the run_env option according to the place where the code is being run
##options(run_env = 'jupyter')

start_logging <- function(){
  ts <- format(Sys.time(),"%m%d%Y_%H%M%S")
  log_file <- paste0("log_",ts,'.log')
  log_file <- file(log_file,open = "wt")
  if(getOption('run_env') == 'jupyter')
    {
    ##If jupyter, replace the base cat with new cat as sink doesnt work directly
    cat <- function(...){
    base::cat(..., file=log_file)
    base::cat(...)
    } 
  }else{
   sink(file = log_file, append = T, type = c("output", "message"),split = T)

  }
  
  
  
}

stop_logging <- function(){
  closeAllConnections()
}
