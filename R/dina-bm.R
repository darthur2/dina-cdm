dinaBM <- function(Y, Q, epsilon, max_iter, delta,
                   slip_a, slip_b, guess_a, guess_b,
                   mono_constraint, reduced){
  
  check_input(Y, "Y")
  check_input(Q, "Q")
  
  N <- nrow(Y)
  J <- ncol(Y)
  K <- ncol(Q)
  
  if (epsilon <= 0){
    stop("epsilon in optim_options must be decimal/float greater than 0.")
  }
  
  if (max_iter <= 0 | !is.integer(max_iter)){
    stop("max_iter in optim_options must be an integer greater than 0.")
  }
  
  if (!(length(delta) == 2^K & all(delta > 0))){
    stop("Please ensure that all values in delta are positive and that delta
         is a vector of length 2^K")
  }
  
  if (reduced){
    if ()
  }
  
  
  if (mono_constraint){
    if (reduced){
      
    } else {
      
    }
  } else {
    if (reduced){
    
    } else {
      
    }
  }
}