# so likelihood of the data is l(theta | data)  in frequentist world, this is interpreted as fixed theta for variable data
#theta hat is MLE estimate of the underlying theta of the process 
# theta hat is from drivitive = 0, but it turns out it is basically the sample mean.
# note any positive constant multiple of the joint sampling distribution would have the same max likelihood theta
# Fisher give a genral way to point estimate in any problem that you  can write down  as joint sampling distribution (de-fenati ?) 
# interpret it as function of parameter, a you write the likelihood,     take its log, differentiate with respect to the parameter, set it to zero
# solve it for point estimate. Same idea works when theta is vector.
# he also came up an approximation for variance, neyman tries to figure out how the sample mean would compare to the real mean in probablistic terms
# mean, sd, histogram of the long run mean, long run stand deviation, long run distribution (sample data, calculate sample mean/variance, then do it again and do
# distribution of the sample mean)

# NOTE: if the original data has mean mu and varaince sigma square
# then if we sample form distribution and get the mean, then repeat and take the mean of the means we have mean of the original sample
# varaince of each n sample is sigma sqare / n   (we don't know the sigma square but this is what it would be)
# if we do a mean of variance calculation from each samle then it would be



# IDEA here is that you have a distribution that we are sampling from (here we know what it is but in general we are getting sample 
# without having access to the distribution)

# From our sample we can calucluate some function (here we do the mean) and we do the point estimate of it .  This is mu hat.  (hat to say estiamte)
# then we do iid sample from the samples and for each one we do the same function (mean again)  these are not mu-hat-star (star to say it is one of many sample)

# for the demonstration here we are doing the theat.hat  which is coeficient of variation

# The coefient of variation distance from the mean you get from bootstrap can be used to correct the estimate
# you get from original sample

rm(list-ls())
n <- 10

# mean is lot larger than distribution, this is to get strickly positive values
mu.DG <- 100

sigma.DG <- 30

# population CV is sigma.DG / mu.DG = 0.3

seed <- 675849301

set.seed( seed )

print( y <- round( sort( rnorm( n, mu.DG, sigma.DG ) ), 1 ) )

# 46.5  48.1  61.5  75.1  93.2 101.2 106.8 108.4 109.2 173.2

print( mu.hat.MLE <- mean( y ) )

# 92.32

# this is divide by n and not the MLE that R calculates that does n-1
print( sigma.hat.MLE <- sqrt( mean( ( y - mu.hat.MLE )^2 ) ) )

# 35.60289

# coeficinet of variation = noise in data generating process relative to the signal
# for expontial family it is sigma /mu
print( theta.hat.MLE <- sigma.hat.MLE / mu.hat.MLE )

# 0.3856465

M <- 1000000

theta.hat.MLE.star <- rep( NA, M )

for ( j in 1:M ) {

  y.star <- sample( y, size = n, replace = T )

  mu.hat.MLE.star <- mean( y.star )

  sigma.hat.MLE.star <- 
    sqrt( mean( ( y.star - mu.hat.MLE.star )^2 ) )

  theta.hat.MLE.star[ j ] <- 
    sigma.hat.MLE.star / mu.hat.MLE.star

  if ( ( j %% 10000 ) == 0 ) print( j )

}

hist( theta.hat.MLE.star, breaks = 50, probability = T,
  xlab = 'theta.hat.MLE.star', ylab = 'Density', 
  main = 'Bootstrap Distribution of theta.hat.MLE' )

print( bootstrap.percentile.CI <- 
  quantile( theta.hat.MLE.star, probs = c( 0.025, 0.975 ) ) )

#      2.5%     97.5% 
# 0.1951311 0.5116136

abline( v = theta.hat.MLE, col = 'red' )

abline( v = bootstrap.percentile.CI[ 1 ], col = 'red',
  lty = 2 )

abline( v = bootstrap.percentile.CI[ 2 ], col = 'red',
  lty = 2 )

# 
bias_term <- mean(theta.hat.MLE.star) - theta.hat.MLE

# so idea is that figure out how far the theta.hat.mle is from the mean then  move it to the right the same
# amount.   that is non parameteric estimate of our uncertainty about theta hat mle

# the variance in the histogram helps in the to estimate of stand error of theta hat.
bias.corrected.MLE.for.theta <- theta.hat.MLE - bias_term
