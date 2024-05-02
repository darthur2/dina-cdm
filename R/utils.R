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

check_prior <- function(prior, J, K){
  if (!is.list(prior)){
    stop("prior must be a list containing values for delta, slip_a, slip_b,
             guess_a, and guess_b.")
  }
  
  prior_lengths <- sapply(prior, length)
  prior_names <- c("delta", "slip_a", "slip_b", "guess_a", "guess_b")
  
  if (!all(prior_names %in% names(prior))){
    stop("prior must be a list with following names: 
             delta, slip_a, slip_b, guess_a, guess_b")
  }
  
  if (reduced){
    
    if (!all(prior_lengths == c(2^K, 1, 1, 1, 1))){
      stop("For reduced model, prior must be a list with five elements 
               for hyperparameters delta, slip_a, slip_b, guess_a, guess_b
               with respective lengths of 2^K, 1, 1, 1, 1.")
    }
  } else {
    if (!all(prior_lengths) == c(2^K, J, J, J, J)){
      stop("For reduced model, prior must be a list with five elements 
               for hyperparameters delta, slip_a, slip_b, guess_a, guess_b
               with respective lengths of 2^K, J, J, J, J.")
    }
  }
  
  if (any(delta <= 0) | any(slip_a) <= 0 | any(slip_b) <= 0){
    stop("Values for prior parameters must be greater than 0.")
  }
}

check_optim <- function(optim_options){
  if (!is.list(optim_options)){
    stop("optim_options must be a list containing values for epsilon and max_iter.")
  }
  
  optim_option_names <- c("epsilon", "max_iter")
  
  if (!all(optim_option_names %in% names(optim_options))){
    stop("optim_options must contain values for epsilon and max_iter.")
  }
  
  if (optim_options$epsilon <= 0){
    stop("epsilon in optim_options must be decimal/float greater than 0.")
  }
  
  if (optim_options$max_iter <= 0 | !is.integer(optim_options$max_iter)){
    stop("max_iter in optim_options must be an integer greater than 0.")
  }
}

check_mcmc <- function(mcmc_options){
  if (!is.list(mcmc_options)){
    stop("mcmc_options must be a list containing values for niter and nburn.")
  }
  
  mcmc_option_names <- c("niter", "nburn")
  
  if (!all(mcmc_option_names %in% names(mcmc_options))){
    stop("mcmc_options must contain values for niter and nburn.")
  }
  
  if (mcmc_options$niter <= 0 | !is.integer(mcmc_options$niter)){
    stop("niter in mcmc_options must be an integer greater than 0.")
  }
  
  if (mcmc_options$nburn <= 0 | !is.integer(mcmc_options$nburn)){
    stop("nburn in mcmc_options must be an integer greater than 0.")
  }
}
}