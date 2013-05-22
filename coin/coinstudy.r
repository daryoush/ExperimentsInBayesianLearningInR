library ("rjags")

setwd("~/Documents/workspace/ExperimentsInBayesianLearningInR/coin")

modeldata  <- list(h=c(1,1,1,1,1,1,1,1,1,1,0,0,0,0), n=14)

flips.inits.1 <- list(p=0.5)

flips.run.1 <- jags.model( file = "coinmodel.txt", data = modeldata, 
                            inits = flips.inits.1 )

set.seed( 65748392 )

n.burnin <- 1000

update( flips.run.1, n.iter = n.burnin )

n.monitor <- 100000

flips.run.1.results <- jags.samples( flips.run.1, 
                                      variable.names = c( "p" , "hh"), 
                                      n.iter = n.monitor )

p.star <- flips.run.1.results$p
hh.star <- flips.run.1.results$hh

par( mfrow = c( 2, 2 ) )

plot( 1:n.monitor, p.star, type = 'l', xlab = 'Iteration Number',
      ylab = 'p' )

# the upper left graph is a time-series plot of the monitored
# iterations (to visually check for stationarity)

plot( density( p.star ), xlab = 'p', main = '' )

# the upper right graph is a density trace of the marginal posterior
# distribution for the quantity being monitored

acf( as.ts( p.star[ 1:n.monitor ], start = 1, 
            end = n.monitor, frequency = 1 ), lag.max = 10, main = '' )

pacf( as.ts( p.star[ 1:n.monitor ], start = 1, 
             end = n.monitor, frequency = 1 ), lag.max = 10, main = '' )


c( mean( p.star ), sd( p.star ), quantile(p.star, 
                                             probs = c( 0.025, 0.975 ) ) )




#####  Probability of two heads

par( mfrow = c( 2, 2 ) )

plot( 1:n.monitor, hh.star, type = 'l', xlab = 'Iteration Number',
      ylab = 'hh' )

# the upper left graph is a time-series plot of the monitored
# iterations (to visually check for stationarity)

plot( density( hh.star ), xlab = 'p', main = '' )

# the upper right graph is a density trace of the marginal posterior
# distribution for the quantity being monitored

acf( as.ts( hh.star[ 1:n.monitor ], start = 1, 
            end = n.monitor, frequency = 1 ), lag.max = 10, main = '' )

pacf( as.ts( hh.star[ 1:n.monitor ], start = 1, 
             end = n.monitor, frequency = 1 ), lag.max = 10, main = '' )


c( mean( hh.star ), sd( hh.star ), quantile(hh.star, 
                                          probs = c( 0.025, 0.975 ) ) )

