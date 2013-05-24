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
  
  
  return (c( mean(var), sd( var ), quantile(var, 
                                            probs = c( 0.025, 0.975 ) ) ))
}

# using defenti theorm the sampling dist must be draw from bernoli.  FOr proior we are just using a diffused uniform prior over all possibilities.
modelstring="
model {
  p ~ dunif( 0, 1 )
  for( i in 1 : n)  {
    h[i] ~ dbern( p )
  }
  hh <-  p * p
  tt <- (1-p) * (1-p)
}
"

modeldata  <- list(h=c(1,1,1,1,1,1,1,1,1,1,0,0,0,0), n=14)

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

p.star <- flips.run.1.results$p
hh.star <- flips.run.1.results$hh
tt.star <- flips.run.1.results$tt


### probability of head
reportResults(p.star, 'p', n.monitor)

#####  Probability of two heads
reportResults(hh.star, 'hh', n.monitor)


#####  Probability of two tails
reportResults(tt.star, 'tt', n.monitor)



