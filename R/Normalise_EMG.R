#' Normalise EMG to reference value.
#'
#' Normalise EMG to reference value.
#' @param x a dataframe where each column provide a different channel of electromyographical voltage data.
#' @param maxes a dataframe or list of dataframes containing electromyographical data of which you want to find the maximum reference value. Dataframes must have same columns as x. 
#' @return dataframe with normalised electromyographical voltage data.
#' @export
Normalise_EMG = function(x,maxes){
  
  Get_max = function(x){
    maxes = apply(x,2,max)
    return(maxes)
  }
  
  if(class(maxes)=="list"){
    
    max_data = list()
    for(i in 1:length(maxes)){
      max_data[[i]] = Get_max(maxes[[i]])
    }
    
    max_data = as.data.frame(do.call("rbind",max_data))
    dmaxes = Get_max(max_data)}else{
      dmaxes = Get_max(maxes)
    }
  
  
  Normalised = sweep(x,2,dmaxes, '/')
  return(Normalised)
}