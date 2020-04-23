library(fda)
library(orthopolynom)
library(pracma)
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
y = get_country_data('Lithuania', dat)
y = y[y>0]
plot(y, type='l')
y = c(rev(y), y)
plot(y, type='l')

min_x = -2
max_x = 2
x = seq(min_x, max_x, length.out = length(y))
min_x = min(x)
max_x = max(x)
basisobj <- create.hermite.basis(rangeval=c(min_x, max_x), nbasis=5)
plot(basisobj)
fdobj <- smooth.basis(x, y, basisobj)

plot(fdobj)
lines(x, y, col='red')

fx = -4.5
tx = 4.5
x_hat = seq(fx, tx, length.out = length(y))
fdobj$fd$basis$rangeval = c(fx, tx)
y_hat = eval.fd(x_hat, fdobj$fd)
plot(x_hat, y_hat, type='l')
lines(x, y, col='red', type='p')



