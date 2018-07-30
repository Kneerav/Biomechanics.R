#' Correct DC offset in unrectified electromyographical data
#'
#' Takes unrectified electromyographical data, and removes DC offset so data is centred around zero voltage
#' @param x a vector of electromyographical voltage data
#' @return A vector with corrected data, where the mean is now zero
#' @export
Correct_offset = function(x){
  Mean_signal = mean(x)
  Corrected_signal = x-Mean_signal
  return(Corrected_signal)
}
