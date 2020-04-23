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

colnames(dat)
header <- colnames(dat)
dates_str <- header[5:length(header)]
dates_str = gsub('X', '', dates_str)
dates = as.Date(dates_str, format="%m.%d.%y")

y_full = get_country_data('Lithuania', dat)
dates = dates[y_full>0]
y = y_full[y_full>0]
y_pos = y

plot(dates, y, type='l')
y = c(rev(y), y)
symetric_dates = c(rev(dates), dates)
plot(symetric_dates, y, type='l')

min_x = -2
max_x = 2
date_step = length(dates)/max_x

x = seq(min_x, max_x, length.out = length(y))
min_x = min(x)
max_x = max(x)
basisobj <- create.hermite.basis(rangeval=c(min_x, max_x), nbasis=5)
plot(basisobj)
fdobj <- smooth.basis(x, y, basisobj)

plot(fdobj)
lines(x, y, col='red')

tx = 3.3
fx = -1*tx
x_hat = seq(fx, tx, length.out = length(y))
fdobj$fd$basis$rangeval = c(fx, tx)
y_hat = eval.fd(x_hat, fdobj$fd)
plot(x_hat, y_hat, type='l')
lines(x, y, col='red', type='p')


to_date = dates[1] + date_step*tx

x_hat = seq(0, tx, length.out = length(y))
y_hat = eval.fd(x_hat, fdobj$fd)

ext_dates = seq(dates[1],to=to_date, along.with = x_hat)
par(mar=c(3,3.8,2.3,1))
plot(ext_dates, y_hat, type='l', 
     main='Lithuania', xlab='Time', ylab='Confirmed', 
     cex.main=0.8, cex.lab=0.6, cex.axis=0.7, las=1, col='blue')
lines(dates, y_pos, col='red', type='l', pch=20)
abline(v=Sys.Date(), col='gray')

