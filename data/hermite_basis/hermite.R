hermite <- function(x, nbasis=3, nderiv = 0) {
  x <- as.vector(x)
  n <- length(x)
  xNames <- names(x)
  fNames <- paste('h',0:(nbasis-1), sep='')
  basismat <- matrix(0,n,nbasis)
  polynoms = hermite.he.polynomials(nbasis, normalized=TRUE)
  if (nderiv > 0) {
    for (i in 1:nderiv) {
      polynoms = polynomial.derivatives(polynoms)
    }
  }
  fn = polynomial.functions(polynoms)
  for (i in 1:nbasis) {
    basismat[,i] = fn[[i]](x)
    #basismat[basismat[,i] < 0,i] = 0
  }
  dimnames(basismat) <- list(xNames, fNames)
  basismat
}
