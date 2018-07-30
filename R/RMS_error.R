#' Compute the root-mean-square difference between two vectors of the same length
#'
#' Compute the root-mean-square difference between two vectors of the same length
#' @param x numeric vector
#' @param y numeric vector of the same length as x
#' @return numeric value of the root-mean-square difference between vectors x and y
#' @export
RMS_error = function(x,y){
  Difference = (x-y)
  Diff_sqaured = Difference^2
  Total = mean(Diff_sqaured)
  root = sqrt(Total)
  return(root)
}
