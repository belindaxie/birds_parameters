# Computes a set of generalisation probabilities
#
# Args:
#   test: set of points to compute generalisation probabilities for
#   train: training data observed by the learner
#   theta: sampling model (default = 1)
#   phi: prior (default = 1)
#
# Returns:
#   Set of generalisation probabilities


BayesGen <- function(train, test, theta = 1, phi = 1) {
  
  ################### helper function to compute the integrals ################### 
  int_ul <- function(zl, zu, n) {                      # I don't know what's happening here!
    
    # avoid nasty edge cases
    zl <- min(zl, 0.999)                  # zl = lower bound of training data
    zl <- max(zl, 0.001)
    zu <- min(zu, 0.999)                  # zu = upper bound of training data
    zu <- max(zu, 0.001)
    zl <- min(zl, zu - 0.0005)
    
    # actual calculations (gibberish corresponding to the integrals in the
    # appendix of Navarro et al 2012)
    u <- 1
    if(n == 0) {
      u <- (1 - zu)*zl
    }
    else if(n == 1) {
      if(zu == zl) {
        u <- -(1 - zl)*log(1 - zl) - zu*log(zu)
      }
      else {
        u <- (zu - zl)*log(zu - zl) - (1 - zl)*log(1 - zl) - zu*log(zu)
      }
    }
    else if(n == 2) {
      if(zu == zl) {
        u <- log(1 - zl) + log(zu)
      }
      else {
        u <- log(1 - zl) + log(zu) - log(zu - zl)
      }
    }
    else {
      u <- (1 + (zu - zl)^(2-n) - (1 - zl)^(2 - n) - zu^(2 - n))/((1 - n)*(2 - n))
    }
    return(u);
  }
  
  ################### actual computations ################### 
  
  # enforce parameter constraints
  theta <- max(theta, 0)  # theta must be between 0 and 1?
  theta <- min(theta, 1)

  # bounds on the training data
  zu <- max(train)
  zl <- min(train)
  
  # number of train/test points
  n <- length(train)
  m <- length(test)

  # binomial weight function to describe              ??
  w <- dbinom(0:n, n, theta)

  # normalising function
  Z <- 0
  for(k in 0:n) {                                       # for each training item;
    Z <- Z + w[k + 1]*int_ul(zl, zu, k - (phi - 1))     # ??? 
  }

  # initialise generalisation probabilities
  gen <- rep(0, length(test))
  
  for(p in 1:m) {                                       # for each test item;
    # test data are in the middle
    if(test[p] >= zl && test[p] <= zu) {                # if test item is within the range of training items
      gen[p] <- 1                                       # probability of generalising = 1
    }

    # test data are on the left side
    else if (test[p] < zl) {                            # if test items are 'smaller' than training items
      for(k in 0:n) {
        gen[p] <- gen[p] + w[k + 1]*int_ul(test[p], zu, k - (phi - 1))  # ??
      }
      gen[p] <- gen[p]/Z                                # I think this normalises the probability (of generalising)?
    }
    
    # test data are on the right side
    else {
      for(k in 0:n) {
        gen[p] <- gen[p] + w[k + 1]*int_ul(zl, test[p], k - (phi - 1))
      }
      gen[p] <- gen[p]/Z
    }
  }
  
  return( gen )
}

# I don't think this function runs? What format should part responses be in?
fitBayesGenOLS <- function( train, test, responses, simplify=TRUE ) {  
  # Estimates parameters theta and phi via ordinary least squares (OLS).
  # 
  # train - a list, the i-th element of which is a vector specifying the training 
  #         data shown on the i-th block
  # test - same: a list, but with vectors specifying the test points
  # response - again, a list: i-th element is the participant responses on that block
  #
  # returns a vector containing estimated valus of theta and phi

  modelSSE <- function( par ) {
    # (the values of train, test and responses specified by scope)
    
    # read of parameters
    theta <- par[1]
    phi <- par[2]
    
    # count the number of generalisation gradients
    nGen <- length(responses)
    
    # get model predictions for each gradient, and 
    # compute the SSE
    modelGen <- list()
    sse <- 0
    for( i in 1:nGen ) {
      modelGen[[i]] <- BayesGen( train[[i]], test[[i]], theta, phi)
      sse <- sse + sum( (modelGen[[i]] - responses[[i]])^2 )
    }
    
    return(sse) 
  }
  
  # now optimise...
  opt <- optim( par=c(.5,1), fn=modelSSE,
                lower=c(0,0), upper=c(1,10),
                method="L-BFGS-B")
  
  # simplified output if requested
  if( simplify ) {
    opt <- c( theta=opt$par[1], phi=opt$par[2], sse=opt$value )
  }
  return(opt) 
  
}


# plotting function to show a SINGLE generalisation gradient
plotOne <- function( train, test, probs, main="" ) {
  plot.new()
  plot.window( xlim=c(0,1), ylim=c(0,1))
  axis(1); axis(2)
  lines( test, probs, type="b", pch=19, col="black")
  lines( train, rep.int(0,length(train)), type="p")
  title( xlab="Stimulus Value", ylab="Generalisation Probability", main=main, font.main=1)
}


