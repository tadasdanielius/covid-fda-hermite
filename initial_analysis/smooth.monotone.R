polintmat <- function(xa, ya, x) {
  #  Polynomial extrapolion for a converging sequence
  #  YA is an 3-D array with 1st D same as XA
  n     <- length(xa)
  dimya <- dim(as.array(ya))
  if (length(dimya) == 1) ya <- array(ya,c(dimya[1],1,1))
  if (length(dimya) == 2) ya <- array(ya,c(dimya[1],dimya[2],1))
  if (dimya[1] != n)      stop('First dimension of YA must match XA')
  difx <- xa - x
  absxmxa <- abs(difx)
  ns <- min((1:n)[absxmxa == min(absxmxa)])
  cs <- ds <- ya
  y  <- ya[ns,,]
  ns <- ns - 1
  for (m in 1:(n-1)) {
    for (i in 1:(n-m)) {
      ho      <- difx[i]
      hp      <- difx[i+m]
      w       <- (cs[i+1,,] - ds[i,,])/(ho - hp)
      ds[i,,] <- hp*w
      cs[i,,] <- ho*w
    }
    if (2*ns < n-m) {
      dy <- cs[ns+1,,]
    } else {
      dy <- ds[ns,,]
      ns <- ns - 1
    }
    y <- y + dy
  }
  return( list(y, dy) )
}

#  --------------------------------------------------------------------------

mongrad <- function(x, Wfdobj, basislist=vector("list",JMAX), 
                    returnMatrix=FALSE) {
  #  Evaluates the gradient with respect to the coefficients in Wfdobj
  #     of a monotone function of the form
  #            h(x) = [D^{-1} exp Wfdobj](x)
  #  where  D^{-1} means taking the indefinite integral.
  #  The interval over which the integration takes places is defined in
  #  the basisfd object in Wfdobj.
  #  Arguments:
  #  X      ... argument values at which function and derivatives are evaluated
  #  WFDOBJ ... a functional data object
  #  BASISLIST ... a list containing values of basis functions
  #  Returns:
  #  GVAL   ... value of gradient at input values in X.
  #  RETURNMATRIX ... If False, a matrix in sparse storage model can be returned
  #               from a call to function BsplineS.  See this function for
  #               enabling this option.
  
  #  Last modified 9 May 2012 by Jim Ramsay
  
  JMAX <- 15
  JMIN <- 11
  EPS  <- 1E-5
  
  coef  <- Wfdobj$coefs
  coefd <- dim(coef)
  ndim  <- length(coefd)
  if (ndim > 1 && coefd[2] != 1) stop("Wfdobj is not a single function")
  
  basisfd  <- Wfdobj$basis
  rangeval <- basisfd$rangeval
  nbasis   <- basisfd$nbasis
  onebas   <- rep(1,nbasis)
  width    <- rangeval[2] - rangeval[1]
  
  #  set up first iteration
  
  JMAXP <- JMAX + 1
  h <- rep(1,JMAXP)
  h[2] <- 0.25
  #  matrix SMAT contains the history of discrete approximations to the
  #    integral
  smat <- matrix(0,JMAXP,nbasis)
  #  array TVAL contains the argument values used in the approximation
  #  array FVAL contains the integral values at these argument values,
  #     rows corresponding to argument values
  #  the first iteration uses just the endpoints
  j   <- 1
  tval <- rangeval
  if (is.null(basislist[[j]])) {
    bmat <- getbasismatrix(tval, basisfd, 0, returnMatrix)
    basislist[[j]] <- bmat
  } else {
    bmat <- basislist[[j]]
  }
  fx   <- as.matrix(exp(bmat %*% coef))
  fval <- as.matrix(outer(c(fx),onebas)*bmat)
  smat[1,]  <- width*apply(fval,2,sum)/2
  tnm <- 0.5
  
  #  now iterate to convergence
  for (iter in 2:JMAX) {
    tnm  <- tnm*2
    del  <- width/tnm  
    flag <- ifelse(rangeval[1]+del/2 >= rangeval[2]-del/2, -1, 1)
    tj   <- seq(rangeval[1]+del/2, rangeval[2]-del/2, by=flag*abs(del))
    tval <- c(tval, tj)
    if (is.null(basislist[[iter]])) {
      bmat <- getbasismatrix(tj, basisfd, 0, returnMatrix)
      basislist[[iter]] <- bmat
    } else {
      bmat <- basislist[[iter]]
    }
    fx   <- as.matrix(exp(bmat %*% coef))
    gval <- as.matrix(outer(c(fx),onebas)*bmat)
    fval <- rbind(fval,gval)
    smat[iter,] <- (smat[iter-1,] + width*apply(fval,2,sum)/tnm)/2
    if (iter >= max(c(5,JMIN))) {
      ind <- (iter-4):iter
      result <- polintmat(h[ind],smat[ind,],0)
      ss  <- result[[1]]
      dss <- result[[2]]
      if (all(abs(dss) < EPS*max(abs(ss))) || iter >= JMAX) {
        # successful convergence
        # sort argument values and corresponding function values
        ordind <- order(tval)
        tval   <- tval[ordind]
        fval   <- as.matrix(fval[ordind,])
        # set up partial integral values
        lval   <- outer(rep(1,length(tval)),fval[1,])
        del    <- tval[2] - tval[1]
        fval   <- del*(apply(fval,2,cumsum) - 0.5*(lval + fval))
        gval   <- matrix(0,length(x),nbasis)
        for (i in 1:nbasis) gval[,i] <- approx(tval, fval[,i], x)$y
        return(gval)
      }
    }
    smat[iter+1,] <- smat[iter,]
    h[iter+1]     <- 0.25*h[iter]
  }
}

fngrad.smooth.monotone <- function(yi, argvals, zmat, wtvec, cveci, lambda,
                                   basisobj, Kmat, inact, basislist)
{
  if (!is.null(zmat)) {
    ncov   <- ncol(zmat)
    ncovp1 <- ncov + 1
  } else {
    ncov   <- 1
    ncovp1 <- 2
  }
  n      <- length(argvals)
  nbasis <- basisobj$nbasis
  Wfdobj <- fd(cveci, basisobj)
  h      <- monfn(argvals, Wfdobj, basislist)
  Dyhat  <- mongrad(argvals, Wfdobj, basislist)
  if (!is.null(zmat)) {
    xmat <- cbind(zmat,h)
  } else {
    xmat <- cbind(matrix(1,n,1),h)
  }
  Dxmat  <- array(0,c(n,ncovp1,nbasis))
  Dxmat[,ncovp1,] <- Dyhat
  wtroot <- sqrt(wtvec)
  wtrtmt <- wtroot %*% matrix(1,1,ncovp1)
  yroot  <- yi*as.numeric(wtroot)
  xroot  <- xmat*wtrtmt
  #  compute regression coefs.
  #betai  <- lsfit(xmat, yi, wt=as.vector(wtvec), intercept=FALSE)$coef
  betai <- c(0,1)
  #  update fitted values
  yhat   <- xmat %*% betai
  #  update residuals and function values
  res    <- yi - yhat
  f      <- mean(res^2*wtvec)
  grad   <- matrix(0,nbasis,1)
  #print(betai)
  for (j in 1:nbasis) {
    Dxroot <- Dxmat[,,j]*wtrtmt
    yDx <- crossprod(yroot,Dxroot) %*% betai
    xDx <- crossprod(xroot,Dxroot)
    #print(crossprod(betai,(xDx+t(xDx))))
    #print(2*yDx)
    grad[j] <- crossprod(betai,(xDx+t(xDx))) %*% betai - 2*yDx
  }
  grad <- grad/n
  if (lambda > 0) {
    grad <- grad +          2 * Kmat %*% cveci
    f    <- f    + t(cveci) %*% Kmat %*% cveci
  }
  if (any(inact)) grad[inact] <- 0
  norm <- sqrt(sum(grad^2)) #  gradient norm
  Flisti <- list("f"=f,"grad"=grad,"norm"=norm)
  return(list(Flisti, betai, Dyhat))
}