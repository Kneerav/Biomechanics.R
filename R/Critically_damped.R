#' Critically damped filter
#'
#' This function allows you to low-pass filter data via a critically damped filter
#' @param raw_data vector of raw numeric data to be filtered
#' @param sampling_frequency numeric value indicating the sampling frequency fo the data to be filtered. Defaults to 1000.
#' @param cutoff_frequency numeric value indicating the cutoff frequency of the low-pass filter. Defaults to 20Hz.
#' @param filter_passes numeric value indicating the number of passes to be made by the filter. Defaults to 10.
#' @export
Critically_damped = function(raw_data, sampling_frequency = 1000, cutoff_frequency = 20, filter_passes = 10){

  c_critical = 1 / (((2^(1/(2*filter_passes)))-1)^(1/2));
  f_adjusted_critical = cutoff_frequency * c_critical;

  w_adjusted_critical = tan(pi*f_adjusted_critical/sampling_frequency);

  k1_critical = 2 * w_adjusted_critical;
  k2_critical = w_adjusted_critical^2;

  a0_critical = k2_critical / (1 + k1_critical + k2_critical);
  a2_critical = a0_critical;
  a1_critical = 2 * a0_critical;

  b1_critical = 2*a0_critical*(1/k2_critical-1);
  b2_critical = 1-(a0_critical + a1_critical + a2_critical + b1_critical);


  B_manual_critical = c(a0_critical, a1_critical, a2_critical);
  A_manual_critical = c(1, -b1_critical, -b2_critical );

  # Filter the data.
  i=0
  repeat{
    i = i+1
    filtered_data = rep(NaN, length(raw_data))

    # First pass in the forward direction.
    for(x in 1:length(raw_data)){
      if (x >= 3){
        filtered_data[x] = a0_critical*raw_data[x] + a1_critical*raw_data[x-1] + a2_critical*raw_data[x-2] + b1_critical*filtered_data[x-1] + b2_critical*filtered_data[x-2];
      }else{if(x == 2){
        filtered_data[x] = a0_critical*raw_data[x] + a1_critical*raw_data[x-1] + b1_critical*filtered_data[x-1];
      }else{
        filtered_data[x] = a0_critical*raw_data[x]
      }

      }
    }

    # reverse data to pass again
    raw_data = rev(filtered_data)
    if(i == filter_passes){
      break
    }


  }

  # Flip the data back.
  filtered_data = rev(filtered_data)
  return(filtered_data)
}
