#' Read a csv file output from VICON Nexus and extracts the electromyographical data
#'
#' Read a csv file output from VICON Nexus and extracts the electromyographical data
#' @param x csv file output from VICON nexus that contains electromyographical data
#' @param from.col numeric value indicating the first column at which electromyographical data appears
#' @param to.col numeric value indicating the last column at which electromyographical data appears
#' @param muscles a vector of strings used to name each channel. Must be the same length as difference between from.col an to.col
#' @param skips_rows numeric value indicating the number of rows to skip until the first row of EMG data. Defaults to 4.
#' @return Dataframe containing the electromyographical data extracted from the csv
#' @export
read.EMG = function(x, from.col = 21, to.col = 28,
                    muscles = c("EMG_1", "EMG_2", "EMG_3", "EMG_4",
                                "EMG_5", "EMG_6", "EMG_7", "EMG_8"), skip_rows=4){
  Initial = read.csv(file = x, skip=skip_rows, na.strings = "")
  Non = which(is.na(Initial$V))
  Clip_row = Non[1]-1
  EMG_data = Initial[1:Clip_row, from:to]
  EMG_data = apply(EMG_data, 2, as.character)
  EMG_data = as.data.frame(apply(EMG_data, 2, as.numeric))
  colnames(EMG_data) = muscles
  return(EMG_data)
}
