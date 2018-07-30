#' Process raw data to determine residuals between the raw and filtered signal using a range of low pass cutoff frequencies for butterworth filter
#'
#' Process raw data to determine residuals between the raw and filtered signal using a range of low pass cutoff frequencies for butterworth filter.
#' @param raw_signal vector of raw values of which you wish to determine the optimal cutoff frequency.
#' @param cutoff_range numeric vector of format c(lower,upper) containing the lower and upper cutoff frequency limit. Defaults to c(0,20).
#' @param sample_freq numeric value indicating the frequency of the signal. Defaults to 1000Hz. 
#' @param final_order numeric value indicating the final order of the filter. Defaults to 4.
#' @param interval numeric value indicating the increment by which the cutoff frequency will increase within the limits specified by cutoff_range. Defaults to 0.01.
#' @return dataframe containing the cutoff frequencies and the respective residual at the given cutoff frequency.
#' @export
residual_analysis = function(raw_signal, cutoff_range = c(0,20), sample_freq = 1000, final_order=4, interval=0.01){
  
  residual = vector()
  cut_off_seq = seq(cutoff_range[1], cutoff_range[2], interval)
  for(i in 1:length(cut_off_seq)){
    BW = signal::butter(n=final_order/2, W=cut_off_seq[i]/(sample_freq/2), type="low", plane="z")
    Filtered = signal::filtfilt(BW, raw_signal)
    residual[i] = sum(abs(raw_signal - Filtered))
  }
  residual = data.frame(cut_off = cut_off_seq, residual = residual)
  plot(residual[,1], residual[,2], xlab = "Cut-off frequency (Hz)", ylab="Residual")
  return(residual)
}