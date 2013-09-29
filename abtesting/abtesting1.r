library ("rjags")

rm(list=ls())
reportResults <- function(var, name, monitor) {
  
  par( mfrow = c( 2, 2 ) )
  
  plot( 1:monitor, var, type = 'l', xlab = 'Iteration Number',
        ylab = name )
  
  # the upper left graph is a time-series plot of the monitored
  # iterations (to visually check for stationarity)
  
  plot( density( var ), xlab = name, main = '' )
  
  # the upper right graph is a density trace of the marginal posterior
  # distribution for the quantity being monitored
  
  acf( as.ts( var[ 1:monitor ], start = 1, 
              end = monitor, frequency = 1 ), lag.max = 10, main = '' )
  
  pacf( as.ts( var[ 1:monitor ], start = 1, 
               end = monitor, frequency = 1 ), lag.max = 10, main = '' )
  
  
  return (c( mean(var), sd( var ), quantile(var, 
                                            probs = c( 0.05, 0.95 ) ) ))   #probs = c( 0.025, 0.975 ) ) ))
}


# assume messages have same effect on all recipient 
# msgCnt is number of messages sent  (known)
# opened is number of messages opened   (wish to estimate)
# purchased are the number of purches (can only happen after opening the message)  
# message has constant open probabality (p)
# opened is binamial(p, total numnber of messages send)   -- number of sucesses
# purchase probabality is (q)   so number of purchases (known) is the binomial(q, opened)


# to add later:
# message acceptance is not a property of message but each person (with no covariate) 
#  
# messages open cnt have known covariates
# messages open cnt have unknown covariates (find the covariate)
# be able to incoporate the messages purchase count from other sources (to calculate the covariates) so we can see
# if our message has imporved the purchases

# model 1 assume a the probablity of opening is function of message and purchase as function of product and message,
# all users behave the same way.
modelstring="
model {
p ~ dunif( 0, 1 )
opened ~ dbinom(p, msgCnt)
q ~ dunif( 0, 1 )
purchased ~ dbinom(q, opened)
}
"
pcnt = 60
mcnt = 100
modeldata <- list(purchased=pcnt, msgCnt=mcnt)
flips.inits.1 <- list(p=pcnt/mcnt, q=pcnt/mcnt)


# model 2  Assume each person has his/her own open and purchase proababilty.   Each person 's p and q are draws from 
# beta distribution that is learned from other purchases of the same user




# Model 3 Assume we have other data to estimate the person's likelihood of the purchase.  So instead of single distribution
# for your p and q there are clusters that are determined by purchases.



flips.run.1 <- jags.model( textConnection(modelstring), data = modeldata, 
                           inits = flips.inits.1 )

set.seed( 65748392 )

n.burnin <- 1000

update( flips.run.1, n.iter = n.burnin )

n.monitor <- 100000

# Synthetic Future
flips.run.1.results <- jags.samples( flips.run.1, 
                                     variable.names = c( "opened", "p", "q"), 
                                     n.iter = n.monitor )

opened.star <- flips.run.1.results$opened
p.star <- flips.run.1.results$p
q.star <- flips.run.1.results$q


### probabality of opening a messages
reportResults(opened.star, 'messaged opened', n.monitor)
reportResults(p.star, 'prob of opening messages', n.monitor)
reportResults(q.star, 'prop of purchase', n.monitor)


