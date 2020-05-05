source('common.R')

loaded_csv <- load_data()
dat = loaded_csv$dat
dates = loaded_csv$dates

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
     cex.main=0.8, cex.lab=0.6, cex.axis=0.7, las=1, col='blue', ylim=c(0, max(y_hat)))
lines(dates, y_pos, col='red', type='l', pch=20)
abline(v=Sys.Date(), col='gray')

