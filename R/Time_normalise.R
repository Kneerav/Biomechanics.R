#' Time normalises continuous data
#'
#' Takes a dataframe containing continuous data and resamples it to the specified number of points
#' @param DF a dataframe containing the continuous data that needs to be time normalised to specified number of points. The first column in the data frame must be the time or sample number column in ascending order
#' @param nodes numeric value which indicates how many points you would like your data to be resampled to
#' @return A dataframe of nrow(nodes) which contains the resampled data
#' @export

Time_normalise = function(DF, nodes = 101){

  Normal_cycle = function(x,y){
    n = length(x)
    t0 = y[,1]
    t1 = seq(t0[1], t0[n], length.out = nodes)
    y1 = pracma::interp1(t0, x, t1, method="spline")
    return(y1)
  }

  Normalised = as.data.frame(apply(DF,2,Normal_cycle, y=DF))
  return(Normalised)
}
