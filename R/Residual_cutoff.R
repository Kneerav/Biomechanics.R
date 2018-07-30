#' Process cutoff frequency and residual data to determine optimal low pass cutoff frequency for butterworth filter
#'
#' Process residual_analysis frequency and residual data to determine optimal low pass cutoff frequency for butterworth filter.
#' @param residual_analysis dataframe where first column contains cutoff frequencies, and the second column contains the respective residual between the raw and filtered signal. Can be generated using the Residual_analysis function. 
#' @param Hz_range numeric vector of format c(lower,upper) containing the lower and upper cutoff frequency limit which identifies the linear region of the curve.
#' @param interval numeric value indicating the increment by which the cutoff frequency will increase within the limits specified by cutoff_range. Defaults to 0.01.
#' @return numeric value of the recommended cutoff frequency.
#' @export
residual_cut = function(residual_analysis, Hz_range, interval = 0.01){
  y = residual_analysis[(Hz_range[1]*(1/interval)+1):(Hz_range[2]*(1/interval)+1),2]
  x = seq(Hz_range[1], Hz_range[2], interval)
  linear_model = lm(y~x)
  
  Below = which(residual_analysis[,2]<linear_model$coefficients[1])
  Cut_off = residual_analysis[Below[1],1]
  
  g = ggplot2::ggplot(data=residual_analysis, ggplot2::aes(cut_off, residual))
  g = g+ggplot2::geom_point()+
    ggplot2::geom_smooth(data=subset(residual_analysis, cut_off > Hz_range[1] & cut_off < Hz_range[1]), lwd=2)+
    ggplot2::geom_point(data=NULL, ggplot2::aes(0, linear_model$coefficients[1]), size=5, col="blue")+
    ggplot2::geom_hline(data=NULL, ggplot2::aes(yintercept=linear_model$coefficients[1]), lty=2, col="red", lwd=2)+
    ggplot2::geom_vline(data=NULL, ggplot2::aes(xintercept = Cut_off), lty=2, lwd=2, col="red")+
    ggplot2::geom_point(data=NULL, ggplot2::aes(x=Cut_off, y=linear_model$coefficients[1]), fill="red", size=5, pch=22)+
    ggplot2::annotate("text", x=(max(residual_analysis$cut_off)/2), y=((((max(residual_analysis$residual)-min(residual_analysis$residual))/2))+(min(residual_analysis$residual))), label=paste(Cut_off, "Hz"))
  plot(g)
  return(Cut_off)
  

}