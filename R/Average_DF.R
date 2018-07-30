#' Average multiple data frames in a list
#'
#' Takes any number of data frames and averages them
#' @param DF_list a list of data frames to be averaged
#' @param unique_comb a list of column names that are used as the unique identifiers
#' @return A dataframe which contains average data for each unique combination
#' @export
Average_DF = function(DF_list, unique_comb){
  MEAN = data.table::rbindlist(DF_list[,lapply(.SD,mean), unique_comb])
  return(MEAN)
}
