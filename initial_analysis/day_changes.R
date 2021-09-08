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


ref_data = read.csv('../COVID-19/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv')
head(ref_data)

dat = read.csv(
  '../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
)

colnames(ref_data)[7] <- 'Province.State' 
colnames(ref_data)[8] <- 'Country.Region'

min_confirmed = 5
total =rowSums(dat[,5:dim(dat)[2]])

# Atmetam salis kurios turi maziau nei is viso 5 atvejus
dat = dat[total>min_confirmed,]

day_count = dim(dat)[2]


diff_dat = cbind(dat[,1:4], dat[6:day_count] - dat[5:(day_count-1)])
df = merge(diff_dat, ref_data, by=c('Province.State', 'Country.Region'))

ncols = dim(df)[2]

population = 10000/df[,ncols]
normed_df = df[, 5:(ncols-10)] * population
normed_df = cbind(df[,1:4], normed_df)



#dat = cbind(dat[1:4], diff_dat)
dat = normed_df
dat[is.na(dat)] <- 0

rep_names = paste(dat[total>min_confirmed,2], dat[total>min_confirmed,1])
ydat = t(dat[,5:length(dat)[1]])

dates = seq(as.Date("2020/01/22"), by = "day", length.out = dim(ydat)[1])
dimnames(ydat) = list(dates, rep_names)

x = seq(0,1, length.out = dim(ydat)[1])


#basisobj <- create.hermite.basis(c(min(x), max(x)), nbasis=25)
basisobj <- create.bspline.basis(nbasis = 25)
par(mfrow=c(1,1))
plot(basisobj, main='Bazines funkcijos')

par(mfrow=c(2,2))
countries = c('Lithuania', 'Estonia', 'Latvia', 'North Macedonia')
for (country in countries) {
  y = get_country_data(country, dat)
  fdobj1 = smooth.basis(x, y, basisobj)
  plot(fdobj1, main=country, ylim=c(0, max(y)))
  lines(x, y, lty=2, col='red')
  rss = sum(   (y - eval.fd(fdobj1$fd, x))^2   )
  rss = sqrt(rss)
  message(country, ' rss=',rss)
}

# -- visos salys

par(mfrow=c(1,1))
fdall = smooth.basis(x, ydat, basisobj)
fdall$fd$fdnames$time = dates
fdall$fdnames$values = 'Confirmed'
plot(fdall, main='Visos salys')


# -- isvestines

y = get_country_data('Lithuania', dat)

fdobj1 = smooth.basis(x, y, basisobj)
plot(deriv.fd(fdobj1$fd,1))


# -- PCA

f = fdall$fd
par(mfrow=c(2,2))
pca = pca.fd(f, nharm = 4)
plot(pca)


f = fdall$fd
par(mfrow=c(2,1))
pca = pca.fd(f, nharm = 2)
plot(pca)
par(mfrow=c(1,1))
plot(pca$scores[,1], pca$scores[,2])

# -- VARIMAX
par(mfrow=c(2,2))
vmx = varmx.pca.fd(pca.fd(f, nharm = 4))
plot(vmx)

# -- Score clustering
par(mfrow=c(1,2))

cl = kmeans(pca$scores, 10)
par(mfrow=c(1,1))
plot(pca$scores, col=cl$cluster, xlim=c(-0.1, 1), ylim=c(-0.5, 0.5), main="PCA score kmeans")
points(cl$centers, col = 1, pch = 8, cex = 2)

as.vector(dat[,'Country.Region'])[cl$cluster==4]
as.vector(dat[,'Country.Region'])[cl$cluster==5]

# -- Varimax score clustering
vmx = varmx.pca.fd(pca.fd(f, nharm = 2))
cl = kmeans(vmx$scores, 10)

plot(vmx$scores, col=cl$cluster*2, xlim=c(-0.3, 1), ylim=c(-0.1, 0.5), main="VARIMAX score kmeans")
points(cl$centers, col = 1, pch = 8, cex = 2)

# -- Registration
par(mfrow=c(1,1))
regobj = register.fd(f)
r_same_len = regobj$regfd
plot(r_same_len) #, ylim=c(0,1000))

par(mfrow=c(1,2))
plot(f, main='Neregistruotos')
plot(r_same_len, main='Registruotos')


# -- Nevienoda pradzia

get_fd = function(y_dat, basisobj, rangeval=c(0, 1)) {
  #y_dat = dat[1,]
  message(as.vector(y_dat[,2]), ' ', as.vector(y_dat[,1]))
  y_dat = y_dat[5:length(y_dat)]
  
  ypos = which(as.numeric(y_dat) > 0)[1]
  if (is.na(ypos)) {
    return(NULL)
  }
  y_dat = as.numeric(y_dat[ypos:length(y_dat)])
  
  # domina tik salys, kurios turi bent tiek tasku kiek baziniu funkciju
  if (length(y_dat) < basisobj$nbasis) {
    return(NULL)
  }
  x = seq(rangeval[1],rangeval[2], length.out = length(y_dat))
  fdobj = smooth.basis(x, y_dat, basisobj)
  return(fdobj$fd)
}

basisobj1 <- create.hermite.basis(c(-5, 5), nbasis=20)
basisobj1 <- basisobj
fdata_list = get_fd(dat[1,], basisobj1)
fdata_list$fdnames$reps = paste(as.character(dat[1,2]),as.character(dat[1,1]))
for (i in 2:dim(dat)[1]) {
  fdobj = get_fd(dat[i, ], basisobj1)
  if (!is.null(fdobj)) {
    fdata_list$coefs = cbind(fdata_list$coefs, fdobj$coefs)
    fdata_list$fdnames$reps = c(
      fdata_list$fdnames$reps, 
      paste(as.character(dat[i,2]),as.character(dat[i,1]))
    )
  }
}
fdata_list$fdnames$values = 'Confirmed'
par(mfrow=c(1,2))
plot(fdall$fd, main='Vienodo ilgio')
plot(fdata_list, main='f-jos pradzia nuo pirmo atvejo')

# ---- PCA


# -- PCA

f = fdata_list
par(mfrow=c(2,2))
pca = pca.fd(f, nharm = 4)
plot(pca)

f = fdata_list
par(mfrow=c(2,1))
pca = pca.fd(f, nharm = 2)
plot(pca)
par(mfrow=c(1,1))
plot(pca$scores[,1], pca$scores[,2])

# -- VARIMAX
par(mfrow=c(2,2))
vmx = varmx.pca.fd(pca.fd(f, nharm = 4))
plot(vmx)

# -- Score clustering
par(mfrow=c(1,2))

cl = kmeans(pca$scores, 10)
plot(pca$scores, col=cl$cluster, main="PCA score kmeans", xlim=c(-0.5, 0.5))
points(cl$centers, col = 1, pch = 8, cex = 2)

# -- Varimax score clustering
vmx = varmx.pca.fd(pca.fd(f, nharm = 2))
cl = kmeans(vmx$scores, 10)

plot(vmx$scores, col=cl$cluster, main="VARIMAX score kmeans")
points(cl$centers, col = 1, pch = 8, cex = 2)







# -- Registracija nevienodo ilgio funkciju
regobj1 = register.fd(fdata_list)
r = regobj1$regfd
par(mfrow=c(1,1))
plot(r) #, ylim=c(0,1000))

par(mfrow=c(1,2))
plot(fdata_list, main='Neregistruotos')
plot(r, main='Registruotos')

par(mfrow=c(1,2))
plot(r_same_len, main="Reg. Vienodo ilgio")
plot(r, main="Reg. nevienodo ilgio")
