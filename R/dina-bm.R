dinaBM <- function(Y, Q, optim_options, prior, mono_constraint, reduced){
  
  check_input(Y, "Y")
  check_input(Q, "Q")
  
  J <- nrow(Q)
  K <- ncol(Q)
  
  check_optim(optim_options)
  
  check_prior(prior, J, K)
  
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