create.hermite.basis <- function (rangeval=c(0,1), nbasis=3,
                                  dropind=NULL, quadvals=NULL,
                                  values=NULL, basisvalues=NULL, names=NULL, axes=NULL)
{
  
  type <- 'hermite'
  dropind <- vector("numeric",0)
  
  params <- NULL
  basisobj <- basisfd(type=type,     rangeval=rangeval, nbasis=nbasis,
                      params=params, dropind=dropind, quadvals=quadvals,
                      values=values, basisvalues=basisvalues)
  
  basisobj$names <- as.vector(outer('H', 1:nbasis, paste, sep=''))
  basisobj
}
