library(fda)
source('basisfd.R')
source('create.hermite.basis.R')
source('hermite.R')
source('getbasismat.R')


unlockBinding("getbasismatrix", getNamespace('fda'))
assign("getbasismatrix", getbasismatrix, getNamespace('fda'))

unlockBinding("basisfd", getNamespace('fda'))
assign("basisfd", basisfd, getNamespace('fda'))


plot(f)
plot(b)

y = sin(seq(-3,3, 0.01))
plot(y, type='l')
x = seq(-1,1, length.out = length(y))

b <- create.hermite.basis(c(min(x), max(x)), nbasis=8)
f <- create.fourier.basis(c(min(x), max(x)), nbasis = 5)
plot(b)

fdobj = smooth.basis(x,y,b)
plot(x, y, type='l', col='red')
lines(fdobj$fd)


plot(fdobj$fd)

plot(eval.basis(seq(-1, 1, 0.1), b)[,4], type='l')

dat = read.csv(
  '../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
)

get_country_data <- function(country, dat) {
  y = dat[dat['Country.Region'] == country]
  y[5:length(y)]
}

lt = get_country_data('Lithuania', dat)
plot(lt, type='l')



ydat = t(dat[,6:length(dat)[1]])
plot(ydat[,1], type='l')
x = seq(-1,1, length.out = dim(ydat)[1])
b <- create.hermite.basis(c(min(x), max(x)), nbasis=8)
fdobj = smooth.basis(x, ydat, b)
plot(fdobj)

d = cbind(sin(seq(-3,3, 0.01)), cos(seq(-3,3, 0.01)))
fdobj = smooth.basis(x, d, b)
plot(fdobj)

get_fd = function(y_dat, nbasis=8, type.basis='bspline') {
  y_dat = y_dat[5:length(y_dat)]
  y_dat = as.numeric(y_dat)
  #y_dat = y_dat[y_dat > 0]
  if (length(y_dat) < nbasis*2) {
    return(NULL)
  }
  argvals = seq(0, 1, length.out = length(y_dat))
  fdataobj = fdata(y_dat, argvals=argvals)
  fdobj = fdata2fd(
    fdataobj, 
    type.basis = type.basis, 
    nbasis = nbasis
  )
  fdobj$fdnames$time = seq(0,1, 0.01)
  return(fdobj)
  
}

fdata_list = get_fd(dat[1,])
fdata_list$fdnames$reps = paste(as.character(dat[1,2]),as.character(dat[1,1]))
for (i in 2:dim(dat)[1]) {
  fdobj = get_fd(dat[i, ])
  if (!is.null(fdobj)) {
    fdata_list$coefs = cbind(fdata_list$coefs, fdobj$coefs)
    fdata_list$fdnames$reps = c(
      fdata_list$fdnames$reps, 
      paste(as.character(dat[i,2]),as.character(dat[i,1]))
    )
  }
}
fdata_list$fdnames$values = 'Confirmed'
