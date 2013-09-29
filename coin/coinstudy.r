library ("rjags")


reportResults <- function(var, name, monitor) {
  
  par( mfrow = c( 2, 2 ) )
  
  plot( 1:monitor, var, type = 'l', xlab = 'Iteration Number',
        ylab = name )
  
  # the upper left graph is a time-series plot of the monitored
  # iterations (to visually check for stationarity)
  
  plot( density( var ), xlab = 'p', main = '' )
  
  # the upper right graph is a density trace of the marginal posterior
  # distribution for the quantity being monitored
  
  acf( as.ts( var[ 1:monitor ], start = 1, 
              end = monitor, frequency = 1 ), lag.max = 10, main = '' )
  
  pacf( as.ts( var[ 1:monitor ], start = 1, 
               end = monitor, frequency = 1 ), lag.max = 10, main = '' )
  
  return(reportVar(var))
}

reportVar <- function(var) {
  return (c( mean(var), sd( var ), quantile(var, 
                                            probs = c( 0.05, 0.95 ) ) ))   #probs = c( 0.025, 0.975 ) ) ))
  
}
runModel <- function (headcnt, tailcnt) {
  # using defenti theorm the sampling dist must be draw from bernoli.  FOr proior we are just using a diffused uniform prior over all possibilities.
  modelstring="
  model {
    p ~ dunif( 0.4, 0.6 )
    for( i in 1 : n)  {
      h[i] ~ dbern( p )
    }
    hh <-  p * p
    tt <- (1-p) * (1-p)
  }
  "
  
  # NO DATA just report the prior
  # Single head or tail changes the plausability of the coin's p(head)
  # Because data is drawn from a iid distribution the outcomes reflect the underlying process and not any bias in our sampling.   
  # SO, every sample either removes pl of zero at the outcome (e.g. head => it is not two tail coin)
  # or a data enforces plausability of an existing outcome.  First case, by removing a doubt, you have incresed the plausability of others.
  # 2nd case you have added more plausability to some outcomes => you have increased doubts, or decreased plausability on others.
  heads <- headcnt
  tails <- tailcnt
  modeldata <- list(h=c(rep(1, heads), rep(0, tails)), n=heads+tails)
  #modeldata  <- list(h=c(1,1,1,1,1,1,1,1,1,1,0,0,0,0), n=14)    
  #modeldata  <- list(h=c(1,1,1,1,1,0,0), n=7)    # try 1/2 the 10/14 see if the 5/7 is same as 10/14
  #modeldata  <- list(h=c(), n=0)   #no data
  #modeldata  <- list(h=c(0), n=1)   # single tail
  #modeldata  <- list(h=c(1,0,1), n=3)
  
  # TODO:  SHOW A CASE WHERE WE HAVE PRIOR IN THE MIDDLE (FAIR COIN) BUT HAVE A LOT OF DATA THAT MOVES THE POSTERIOR TO UNFAIR COIN
  # AT WHAT PONT THE MOVE HAPPENS.  HOW IS THAT RELATED WITH STATISTICALLY SGNIFICANT>
  
  flips.inits.1 <- list(p=0.5)
  
  flips.run.1 <- jags.model( textConnection(modelstring), data = modeldata, 
                              inits = flips.inits.1 )
  
  set.seed( 65748392 )
  
  n.burnin <- 1000
  
  update( flips.run.1, n.iter = n.burnin )
  
  n.monitor <- 10000
  
  flips.run.1.results <- jags.samples( flips.run.1, 
                                        variable.names = c( "p" , "hh", "tt"), 
                                        n.iter = n.monitor )
  return (flips.run.1.results)
}


res <- runModel(10,4)

p.star <- res$p
hh.star <- res$hh
tt.star <- res$tt


### probability of head
reportResults(p.star, 'p', length(p.star))

#####  Probability of two heads
reportResults(hh.star, 'hh', length(hh.star))


#####  Probability of two tails
reportResults(tt.star, 'tt', length(tt.star))



