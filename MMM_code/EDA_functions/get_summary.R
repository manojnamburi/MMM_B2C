#' Summary statistics
#'
#'
#'@description
#' A custom utility function to get summary statistics for the input dataframe
#'
#'@details The function gives below details in the Detail output dataframe
#'@details - Datatypes of columns
#'@details - Missing Percentages
#'@details - Zeros Percentages
#'@details - Count of non-null values in each column
#'@details - Number of unique values in each column
#'@details - Mean
#'@details - Standard Deviation
#'@details - Percentiles
#'
#'
#'@return Returns two DataFrames
#'@return 1. Summary - gives high level overview of dataframe
#'@return 2. Detail - gives details with all the above mentioned statistics
#'
#'
#'@param df - Dataframe for selecting the data
#'@param percentiles - (Optional) Default value is NULL which gives 0, 25, 50, 75 and 100 percentile values
#'
#'User can define a vector of percentile values between 0 and 1 which will be displayed in the detail table
#'@param target - (Optional) Vector of target variables that are present in the input dataframe
#'@param media - (Optional) Vector of media variables that are present in the input dataframe
#'
#'@note
#' Below are some of the examples how to pass the parameters
#'
#' get_summary(df = df1)
#'
#' get_summary(df = df1, percentiles = c(0.1,0.2,0.5,0.7,0.9,1))
#'
#' get_summary(df = df1, percentiles = c(0.1,0.2,0.5,0.7,0.9,1), target = c('activity_vol'), media = c('cross_channel_spend','DM_spend','outbound_callcost'))
#'
#'@export

get_summary <- function(df, percentiles = NULL, target = NULL, media = NULL){

  #---------------------------------------------------------------------#
  #  Check INPUTS                                                       #
  #---------------------------------------------------------------------#

  if (is.null(df)) stop("\n'df' should be provided!\n")
  if ((class(df)!="data.frame") & (class(df)!="data.table")) {stop("Please pass dataframe df")}

  #---------------------------------------------------------------------#
  #  Calculate Summary statistics                                       #
  #---------------------------------------------------------------------#

  # list to store all data.frames
  out <- list()

  options(scipen = 999)
  options(digits=2)

  # Datatypes of columns
  df_dtypes <- data.frame(sapply(df, class))
  colnames(df_dtypes) <- 'Datatype'
  df_dtypes$columns <- rownames(df_dtypes)

  if((!is.null(media) & !is.null(target))){
      df_dtypes <- df_dtypes %>% mutate(Variable_type = ifelse(columns %in% media, "Media", ifelse(columns %in% target, "Target", "Control")))
      df_dtypes <- df_dtypes[c(3,2,1)]
  }else{
      df_dtypes <- df_dtypes[c(2,1)]
  }

  # Find missing Perentages in each column
  df_miss_pct = as.data.frame(t(as.matrix(df%>% summarise_all(list(name = ~sum(is.na(.))/length(.))))))
  rownames(df_miss_pct) <- colnames(df)
  colnames(df_miss_pct) <- 'Missing %'
  df_miss_pct$columns <- rownames(df_miss_pct)

  # Find Zeros Percentages in each column
  df_zero_pct <- data.frame(round(colSums(df==0)*100/nrow(df),0))
  colnames(df_zero_pct) <- 'Zeros % '
  df_zero_pct$columns <- rownames(df_zero_pct)

  # Find Count of non-null values in all columns
  df_counts <- data.frame(colSums(!is.na(df)))
  colnames(df_counts) <- 'Count'
  df_counts$columns <- rownames(df_counts)

  # Find number of unique values in each column
  df_unique <- as.data.frame(t(df %>% summarise_all(n_distinct)))
  colnames(df_unique) <- 'Unique'
  df_unique$columns <- rownames(df_unique)

  # Find Mean for each column
  df_mean <- data.frame(df %>% select_if(is.numeric) %>% sapply(mean, na.rm = TRUE))
  colnames(df_mean) <- 'Mean'
  df_mean$columns <- rownames(df_mean)

  # Find Std.Dev for each column
  df_std <- data.frame(df %>% select_if(is.numeric) %>% sapply(sd, na.rm = TRUE))
  colnames(df_std) <- 'Std'
  df_std$columns <- rownames(df_std)

  # Find percentiles for each column
  if(is.null(percentiles)){
      df_stats <- as.data.frame(t(as.matrix(df %>% select_if(is.numeric) %>% sapply(quantile, na.rm = TRUE))))
      df_stats$columns <- rownames(df_stats)
      df_stats <- df_stats %>% rename(min = '0%', '25th_pctl' = '25%', '50th_pctl' = '50%', '75th_pctl' = '75%', max = '100%')
  }else{
      df_stats <- as.data.frame(t(as.matrix(df %>% select_if(is.numeric) %>% sapply(quantile, percentiles, na.rm = TRUE))))
      df_stats$columns <- rownames(df_stats)
  }

  #************* detail dataframe *********************
  # Merge all the above dataframes into one single dataframe
  df_detail <-  left_join(df_dtypes, df_miss_pct, by='columns') %>%
                left_join(., df_zero_pct, by='columns') %>%
                left_join(., df_counts, by='columns') %>%
                left_join(., df_unique, by='columns') %>%
                left_join(., df_mean, by='columns') %>%
                left_join(., df_std, by='columns') %>%
                left_join(., df_stats, by='columns')

  # Round all the values in numerical columns to 2 digits
  if((!is.null(media) & !is.null(target))){
    df_detail[,c(-1,-2,-3)] <- round(df_detail[,c(-1,-2,-3)],2)
  }else{
    df_detail[,c(-1,-2)] <- round(df_detail[,c(-1,-2)],2)
  }

  #************* Summary dataframe ***********************
  df_summary <- introduce(df)
  df_summary <- df_summary[ , 1:7]

  df_summary[,'shape (r*c)'] = paste(df_summary[,'rows'],' * ',df_summary[,'columns'])
  df_summary <- df_summary[, c(8,3:7)]
  df_summary <- df_summary %>% rename('non-numeric columns'         = discrete_columns,
                                      'numeric columns'             = continuous_columns,
                                      'columns with missing values' = all_missing_columns,
                                      'total missing values'        = total_missing_values,
                                      'rows with non-missing values'= complete_rows)
  df_summary_t <- t(df_summary)
  rownames(df_summary_t) <- colnames(df_summary)
  colnames(df_summary_t) <- 'summary'

  #************* Output summary & detail Dataframes ***************
  out$summary <- df_summary_t
  out$detail <- df_detail

  return(out)

}
