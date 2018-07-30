#' Process raw electromyographical data to produce a linear envelope
#'
#' Linear envelope of electromyographical data via high-pass filtering, full wave rectify and low-pass filtering the raw data
#' @param x a vector of electromyographical voltage data
#' @param high numeric value indicating the cutoff frequency of the high-pass filter. Defaults to 20Hz.
#' @param low numeric value indicating the cutoff frequency of the low-pass filter. Defaults to 6Hz.
#' @param final_order numeric value indicating the final order of the filter. Defaults to 4.
#' @param sample_frequency numeric value indicating the sampling freuncy fo the data to be filtered. Defaults to 1000
#' @return vector containing linear envelope of the electromyographical data
#' @export
Linear_envelope = function(x, high = 20, low=6, final_order=4, sample_frequency=1000){
  BW_High_20Hz = signal::butter(n=(final_order/2), W=high/(sample_frequency/2), plane="z", type="high")
  High_passed = signal::filtfilt(BW_High_20Hz, x)
  Rectified = abs(High_passed)
  BW_Low_6Hz = signal::butter(n=(final_order/2), W=low/(sample_frequency/2), plane="z", type="low")
  Low_passed = signal::filtfilt(BW_Low_6Hz, Rectified)
  return(Low_passed)
}
