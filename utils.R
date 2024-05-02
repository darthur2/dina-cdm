check_input <- function(input, input_name){
  if (!is.data.frame(input) | !is.matrix(input)){
    stop(paste(input_name, "must be a data.frame or matrix."))
  }
  
  if (!all(input == 0 | input == 1)){
    stop(paste(input_name, "must contain only dichotomous responses."))
  }
  
  if (any(is.null(input)) | any(is.na(input))){
    stop(paste(input_name, "cannot have missing or null values."))
  }
}