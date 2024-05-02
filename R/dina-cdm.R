dinaCDM <- function(Y, Q, estimation_method = "BM", 
                    prior = NULL, optim_options = NULL,
                    mcmc_options = NULL,
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
  
  if (is.null(prior)){
    
    prior <- list(delta = rep(1, 2^K))
    
    if (reduced){
      
      prior$slip_a <- prior$slip_b <- 1
      prior$guess_a <- prior$guess_b <- 1
      
    } else {
      
      prior$slip_a <- prior$slip_b <- rep(1, J)
      prior$guess_a <- prior$guess_b <- rep(1, J)
      
    }
  } else {
    
    check_prior(prior)
    
    if (estimation_method == "EM"){
      
      estimation_method == "BM"
      
      warning("Changing estimation method to BM since prior is not NULL.")
    }
  }
  
  if (estimation_method %in% c("EM", "BM")){
    
    if (is.null(optim_options)){
      
      optim_options <- list(epsilon = 10e-05, max_iter = 1000)
      
    } else {
      
      check_optim(optim_options)
    }
  } 
  
  if (estimation_method == "MCMC"){
    
    if (is.null(mcmc_options)){
      
      mcmc_options <- list(niter = 10000, nburn = 1000)
      
    } else {
      
      check_mcmc(mcmc_options)
      
    }
  }
  
  if (estimation_method %in% c("EM", "BM")){
    
    dina_fit <- dinaBM(Y, Q, prior, optim_options, mono_constraint, reduced)
    
  } else {
    
    dina_fit <- dinaMCMC(Y, Q, prior, mcmc_options, mono_constraint, reduced)
  }
  
  
}