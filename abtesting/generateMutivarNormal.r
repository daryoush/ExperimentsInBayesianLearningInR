
rm(list=ls())


# from http://statistical-research.com/simulating-random-multivariate-correlated-data-continuous-variables/


getMultiVarNormal <- function(covar) {
  U = t(chol(covar))
  nvars = dim(U)[1]
  numobs = 100000
  set.seed(1)
  random.normal = matrix(rnorm(nvars*numobs,0,1), nrow=nvars, ncol=numobs);
  X = U %*% random.normal
  newX = t(X)
  raw = as.data.frame(newX)
  orig.raw = as.data.frame(t(random.normal))
  names(raw) = c("response","predictor1","predictor2")
  cor(raw)
  return (list(res=raw, raw=orig.raw))
}

R = matrix(cbind(1,.80,.2,  .80,1,.7,  .2,.7,1),nrow=3)
x <- getMultiVarNormal(covar=R)
plot(head(x$res, 100))
plot(head(x$raw,100))



# from email

##########################################################
# Generating a random positive-definite matrix with user-specified positive eigenvalues
# If eigenvalues are not specified, they are generated from a uniform distribution
Posdef <- function (n, ev = runif(n, 0, 10)) 
{
  Z <- matrix(ncol=n, rnorm(n^2))
  decomp <- qr(Z)
  Q <- qr.Q(decomp) 
  R <- qr.R(decomp)
  d <- diag(R)
  ph <- d / abs(d)
  O <- Q %*% diag(ph)
  Z <- t(O) %*% diag(ev) %*% O
  return(Z)
}

R = matrix(cbind(1,.80,.2,  .80,1,.7,  .2,.7,1),nrow=3)
x1 <- getMultiVarNormal(covar=R)
plot(head(x1$res, 100))
plot(head(x1$raw,100))


pdmat <- Posdef(n=5, ev=1:5)
eigen(pdmat)$val
pdmat
x2 <- getMultiVarNormal(covar=pdmat)
plot(head(x2$res, 100))
plot(head(x2$raw,100))


# from http://blogs.sas.com/content/iml/2010/12/10/converting-between-correlation-and-covariance-matrices/
crossCor <- matrix(cbind(1,.25,.9,.25,1,.5, .9,.5,1), nrow=3)
#crossCor <- matrix(cbind(1,.25,-.9,.25,1,-.5, -.9,-.5,1), nrow=3)
var <- c(1,4,9)
diagVar <- diag(var)
coVar <- diagVar %*% crossCor %*% diagVar
x3 <- getMultiVarNormal(covar=coVar)
plot(head(x3$res, 100))
plot(head(x3$raw,100))

