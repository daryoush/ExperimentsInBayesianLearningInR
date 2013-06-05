
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

priceisright.inits.1 <- list(snowblower=30000, Toronto=10000)
modelstring="
model {

  snowblower ~ dnorm(3000, 500)
  Toronto ~ dnorm(12000, 3000)
  priceestimate <- snowblower + Toronto
  trueprice ~ dnorm(35000, 7500)
  error <- trueprice - priceestimate

}
"


modeldata  <- list()

priceisright.run.1 <- jags.model( textConnection(modelstring), data=modeldata,
                           inits = priceisright.inits.1 )

set.seed( 65748392 )

n.burnin <- 1000

update( priceisright.run.1, n.iter = n.burnin )

n.monitor <- 10000

priceisright.run.1.results <- jags.samples( priceisright.run.1, 
                                     variable.names = c( "error", "priceestimate", "trueprice"), 
                                     n.iter = n.monitor )

error.star <- priceisright.run.1.results$error
priceestimate.star <- priceisright.run.1.results$priceestimate
trueprice.star <- priceisright.run.1.results$trueprice



reportResults(error.star, 'error', n.monitor)
reportResults(priceestimate.star, 'price estimate', n.monitor)
reportResults(trueprice.star, ' true price', n.monitor)

