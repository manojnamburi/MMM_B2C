#' Transform the data
#'
#'
#'@description
#' A custom utility function to transform the input dataframe
#'
#'@details The function does below things
#'@details - Drop unnecessary columns from Dataframe
#'@details - Impute null values in the columns
#'@details - Convert data types of columns
#'
#'
#'@return Returns Transformed Dataframe
#'
#'
#'@param df - Dataframe for selecting the data
#'@param drop_cols(optional) -  vector of columns to be dropped from the dataframe
#'@param impute_cols(optional) -  vector of columns to be imputed
#'@param impute_type(optional) -  mean/median/mode
#'
#'- anything other than above will be used directly to replace NULL values
#'@param convert_cols(optional) - vector of columns to be converted
#'@param convert_type(optional) - vector of datatypes to which columns need to converted
#'
#'- character/numeric/factor/integer/date/logical are allowed
#'
#'@note
#' do_transformation(df = df1)
#'
#' do_transformation(df = df1, drop_cols = c('period','dma'))
#'
#' do_transformation(df = df1, convert_cols = c('period'), convert_type = c('date'))
#'
#' do_transformation(df = df1, impute_cols = c('activity_vol','revenue'), impute_type = c('mean','median'))
#'
#' @export

do_transformation <- function(df, drop_cols = NULL, impute_cols = NULL, impute_type = NULL, convert_cols = NULL, convert_type = NULL){

  #---------------------------------------------------------------------#
  #  Check INPUTS                                                       #
  #---------------------------------------------------------------------#

  if (is.null(df)) stop("\n'df' should be provided!\n")
  if((class(df)!="data.frame") & (class(df)!="data.table")) {
    stop("Please pass dataframe df")
  }

  #---------------------------------------------------------------------#
  #  Drop columns                                                       #
  #---------------------------------------------------------------------#
  if(!is.null(drop_cols)){
    df <- df %>% select(-drop_cols)
  }

  #---------------------------------------------------------------------#
  #  Impute columns                                                     #
  #---------------------------------------------------------------------#
  if(!is.null(impute_cols) & !is.null(impute_type)) {
    names(impute_type) <- impute_cols

    # Create the mode function.
    getmode <- function(v) {
        v1 <- v[!is.na(v)]
        uniqv <- unique(v1)
        return(uniqv[which.max(tabulate(match(v1, uniqv)))])
    }

    for(column in impute_cols){

        df1 <- df[,column]

        if(impute_type[column] == 'mean'){
          df[,column][is.na(df[,column])] = mean(df1, na.rm=TRUE) ##convert the item with NA to mean value from the column
        }
        else if(impute_type[column] == 'median'){
          df[,column][is.na(df[,column])] = median(df1, na.rm=TRUE) ##convert the item with NA to median value from the column
        }
        else if(impute_type[column] == 'mode'){
          df[,column][is.na(df[,column])] = getmode(df1) ##convert the item with NA to mode value from the column
        }
        else{
          df[,column][is.na(df[,column])] =  impute_type[column]
        }
    }
  }

  #---------------------------------------------------------------------#
  #  Convert Datatypes of columns                                       #
  #---------------------------------------------------------------------#
  if(!is.null(convert_cols) & !is.null(convert_type)) {

    cat('\nNote: Only below datatypes are allowed\n')
    cat(" character, numeric, factor, integer, date, logical\n")

    names(convert_type) <- convert_cols

    for(column in convert_cols){

        # convert datatype
        FUN1 <- switch(convert_type[column],
                       character = as.character,
                       numeric = as.numeric,
                       factor = as.factor,
                       integer = as.integer,
                       date = as.Date,
                       logical = as.logical)

        df[column] <- lapply(df[column], FUN1)
    }
  }

  return(df)

}
