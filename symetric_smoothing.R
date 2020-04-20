library(fda)
library(orthopolynom)
source('basisfd.R')
source('create.hermite.basis.R')
source('hermite.R')
source('getbasismat.R')
source('hermitepen.R')
source('eval.penalty.R')

get_country_data <- function(country, dat) {
  y = dat[dat['Country.Region'] == country]
  as.numeric(y[5:length(y)])
}


unlockBinding("getbasismatrix", getNamespace('fda'))
assign("getbasismatrix", getbasismatrix, getNamespace('fda'))

unlockBinding("basisfd", getNamespace('fda'))
assign("basisfd", basisfd, getNamespace('fda'))

unlockBinding("eval.penalty", getNamespace('fda'))
assign("eval.penalty", eval.penalty, getNamespace('fda'))

#nuskaitymas
dat = read.csv(
  '../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
)

dates = seq(as.Date("2020/01/22"), by='day', length.out = dim(dat)[2]-4)
yy = get_country_data('Lithuania', dat)
y = c(rev(yy), yy)
#y = yy
dates_x = c(rev(dates), dates)
plot(y, type='l')

x = seq(-2, 2, length.out = length(y))
min_x = min(x)
max_x = max(x)
basis_count = 8

basisobj <- create.hermite.basis(c(min_x, max_x), nbasis=basis_count)
par(mfrow=c(1,1))
plot(basisobj, main='Bazines funkcijos')

fdobj = smooth.basis(x, y, basisobj)
plot(fdobj)
lines(x, y, lty=2, col='red')

future_days = 30
future_dates = seq(as.Date("2020/01/22"), by='day', length.out = dim(dat)[2]-4+future_days)
xx = seq(0, max_x+ max_x/length(dates)*future_days, length.out=length(future_dates))

basisval = eval.basis(xx, basisobj)
coefs = fdobj$fd$coefs
yfn = basisval %*% coefs

plot(xx, yfn, type='l')
lines(seq(0,max_x, length.out=length(dates)), yy, lty=2, col='red', type='p')
