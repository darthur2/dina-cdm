dinaCDM <- function(Y, Q, estimation_method = "BM", 
                    prior = NULL, mcmc_options = NULL,
                    optim_options = NULL,
                    mono_constraint = TRUE,
                    reduced = FALSE){
  
  check_input(Y, "Y")
  check_input(Q, "Q")
  
  N <- nrow(Y)
  J <- ncol(Y)
  K <- ncol(Q)
  
  item_names <- colnames(Y)
  
  if (is.null(item_names)){
    item_names <- paste("Item ", 1:J)
  }
  
  Y <- unname(as.matrix(Y))
  Q <- unname(as.matrix(Q))
  
  if (!(estimation_method %in% c("EM", "BM", "MCMC"))){
    stop("estimation_method must be either EM, BM, or MCMC")
  }
  
  if (estimation_method %in% c("EM", "BM")){
    
    if (is.null(optim_options)){
      
      optim_options <- list(epsilon = 10e-05, max_iter = 1000)
      
    } else {
      
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
  } 
  
  if (estimation_method %in% c("BM", "MCMC")){
    
    if (is.null(prior)){
      
      prior <- list(delta = rep(1, 2^K))
      
      if (reduced){
        
        prior$slip_a <- 1
        prior$slip_b <- 1
        prior$guess_a <- 1
        prior$guess_b <- 1
        
      } else {
        
        prior$slip_a <- rep(1, J)
        prior$slip_b <- rep(1, J)
        prior$guess_a <- rep(1, J)
        prior$guess_b <- rep(1, J)
        
      }
    } else {
      
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
  }
  
  if (estimation_method == "MCMC"){
    
    if (is.null(mcmc_options)){
      
      mcmc_options <- list(niter = 10000, nburn = 1000)
      
    } else {
      
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
  
  if (estimation_method == "EM"){
    
    epsilon <- optim_options$epsilon
    max_iter <- optim_options$max_iter
    
    dina_fit <- dinaEM(Y, Q, epsilon, max_iter, mono_constraint, reduced)
    
  } else if (estimation_method == "BM"){
    
    epsilon <- optim_options$epsilon
    max_iter <- optim_options$max_iter
    
    delta <- prior$delta
    slip_a <- prior$slip_a
    slip_b <- prior$slip_b
    guess_a <- prior$guess_a
    guess_b <- prior$guess_b
    
    niter <- mcmc_options$niter
    nburn <- mcmc_options$nburn
    
    dina_fit <- dinaBM(Y, Q, epsilon, max_iter, delta, slip_a, slip_b,
                       guess_a, guess_b, niter, nburn, mono_constraint, reduced)
  } else {
    
    delta <- prior$delta
    slip_a <- prior$slip_a
    slip_b <- prior$slip_b
    guess_a <- prior$guess_a
    guess_b <- prior$guess_b
    
    niter <- mcmc_options$niter
    nburn <- mcmc_options$nburn
    
    dina_fit <- dinaBayes(Y, Q, delta, slip_a, slip_b, guess_a, guess_b,
                          niter, nburn, mono_constraint, reduced)
  }
  
  
}