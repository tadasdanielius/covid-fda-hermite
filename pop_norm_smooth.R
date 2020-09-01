

source('jhu_data.R')

smooth.row <- function(row, basisobj) {
  country_dat = t(row)
  country_dat = country_dat[country_dat > 0]
  y = c(rev(country_dat), country_dat)
  x = seq(-2, 2, length.out = length(y))
  fdobj <- smooth.basis(x, y, basisobj)
  return(fdobj)
}


smooth.fdata.hermite <- function(dat, reps, nbasis=5, rangeval=c(-2, 2)) {
  #min_x = rangeval[1]
  #max_x = rangeval[2]
  #rev_dat = cbind(rev(dat), dat)
  #x = seq(min_x, max_x, length.out = dim(rev_dat)[2])
  
  fdobj <- smooth.fdata.row(dat[1,], type.basis='hermite')
  for (i in 2:dim(dat)[1]) {
    f <- smooth.fdata.row(dat[i,], type.basis='hermite')
    fdobj$coefs = cbind(fdobj$coefs, f$coefs)
  }
  fdobj$fdnames$reps = reps
  return(fdobj)
}

smooth.fdata.row <- function(row, nbasis=5, type.basis='fourier') {
  country_dat = as.vector(t(row))
  country_dat = country_dat[country_dat > 0]
  
  y = c(rev(country_dat), country_dat)
  x = seq(-2, 2, length.out = length(y))
  fdata.y = fdata(y, argvals=x)
  fdobj <- fdata2fd(fdata.y, type.basis=type.basis, nbasis=nbasis)
  return(fdobj)
}


smooth.hermite <- function(dat, nbasis=5, rangeval=c(-2, 2)) {
  basisobj <- create.hermite.basis(rangeval=rangeval, nbasis=nbasis)
  
  min_x = rangeval[1]
  max_x = rangeval[2]
  rev_dat = cbind(rev(dat), dat)
  x = seq(min_x, max_x, length.out = dim(rev_dat)[2])
  
  fdobj <- smooth.row(dat[1,], basisobj)
  for (i in 2:dim(dat)[1]) {
    f <- smooth.row(dat[i,], basisobj)
    fdobj$fd$coefs = cbind(fdobj$fd$coefs, f$fd$coefs)
  }
  return(fdobj)
}


fddat = smooth.fdata.hermite(normalized, rownames(normalized))
par(mfrow=c(1,1))
plot(fddat)
