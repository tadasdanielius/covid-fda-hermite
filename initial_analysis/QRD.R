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

# -- Data load --------

dat_R = read.csv(
  '../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv'
)

dat_D= read.csv(
  '../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'
)

dat_Q= read.csv(
  '../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
)


header <- colnames(dat_Q)
dates_str <- header[5:length(header)]
dates_str = gsub('X', '', dates_str)
dates = as.Date(dates_str, format="%m.%d.%y")

yQ = get_country_data('Lithuania', dat_Q)
yR = get_country_data('Lithuania', dat_R)
yD = get_country_data('Lithuania', dat_D)

plot(dates, yQ, type='l', col='blue', main='Lithuania')
lines(dates, yR, type='l', col='green')
lines(dates, yD, type='l', col='red')

legend('topleft', 
       legend=c('Confirmed', 'Recovered', 'Death'), 
       col=c('blue', 'green', 'red'), 
       lty=1,
       cex=0.6)

# -- Smoothing -----------

min_x = -2
max_x = 2
x = seq(min_x, max_x, length.out = length(dates))
basisobj <- create.hermite.basis(rangeval=c(min_x, max_x), nbasis=14)

fdobjR <- smooth.basis(x, yR, basisobj)
plot(fdobjR)

fdobjR.deriv = deriv.fd(fdobjR$fd,1)
plot(fdobjR.deriv)

fdobjD <- smooth.basis(x, yD, basisobj)
plot(fdobjD, main='Death')

fdobjD.deriv = deriv.fd(fdobjD$fd,1)
plot(fdobjD.deriv, main='Death Deriv')

# -- Confirmed smoothing and deriv
ydat_Q = t(dat_Q[,5:length(dat_Q)[1]])

fdall.Q = smooth.basis(x, ydat_Q, basisobj)
fdall.Q$fd$fdnames$time = dates
fdall.Q$fdnames$values = 'Confirmed'
plot(fdall.Q, main='Visos salys (Confirmed)')

fdall.Q.deriv = deriv.fd(fdall.Q$fd, 1)
plot(fdall.Q.deriv, main='Confirmed deriv')


# -- Death smoothing and deriv ------
ydat.D = t(dat_D[,5:length(dat_D)[1]])

fdall.D = smooth.basis(x, ydat.D, basisobj)
fdall.D$fd$fdnames$time = dates
fdall.D$fdnames$values = 'Death'
plot(fdall.D, main='Visos salys (Death)')

fdall.D.deriv = deriv.fd(fdall.D$fd, 1)
plot(fdall.D.deriv, main='Death deriv')

# -- Recovered Smoothing and dderiv ------

ydat_R = t(dat_R[,5:length(dat_R)[1]])

fdall.R = smooth.basis(x, ydat_R, basisobj)
fdall.R$fd$fdnames$time = dates
fdall.R$fdnames$values = 'Recovered'
plot(fdall.R, main='Visos salys (Recovered)')

fdall.R.deriv = deriv.fd(fdall.R$fd, 1)
plot(fdall.R.deriv, main='Recovered deriv')

# --- PCA (Confirmed) ------------------------

f = fdall.Q.deriv
par(mfrow=c(2,2))
pca = pca.fd(f, nharm = 4)
plot(pca)


# --- PCA (Recovered) ------------------------

f = fdall.R.deriv
par(mfrow=c(2,2))
pca = pca.fd(f, nharm = 4)
plot(pca)

# --- PCA (Death) ------------------------

f = fdall.D.deriv
par(mfrow=c(2,2))
pca = pca.fd(f, nharm = 4)
plot(pca)


