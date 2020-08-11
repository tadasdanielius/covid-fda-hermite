source('common.R')

smooth.row <- function(row, basisobj) {
  country_dat = t(row)
  country_dat = country_dat[country_dat > 0]
  y = c(rev(country_dat), country_dat)
  x = seq(-2, 2, length.out = length(y))
  fdobj <- smooth.basis(x, y, basisobj)
  return(fdobj)
}


smooth.fdata.hermite <- function(dat, reps, nbasis=5, rangeval=c(-2, 2)) {
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
library(fda.usc)
cases_per_pop = 100000
min_pop = 500000

loaded_csv <- load_data()
dat = loaded_csv$dat
dates = loaded_csv$dates

pop <- read.csv('../COVID-19/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv')
pop <- pop[c("Province_State", "Country_Region", "Combined_Key", "Population")]

colnames(pop) <- c("Province.State", "Country.Region", "Combined_Key", "Population")
pop_dat <- merge(pop,dat,by=c("Country.Region",  "Province.State"))

# Filter out Countries with small pop
pop_dat <- pop_dat[pop_dat$Population > min_pop, ]

pop_dat = pop_dat[complete.cases(pop_dat), ]
normalized = pop_dat[, 7:dim(pop_dat)[2] ] * (cases_per_pop/pop_dat$Population)

rvals = c(-10, 10)

lt_data = as.vector(t(normalized[which(pop_dat$Combined_Key == 'Lithuania'),]))
#basisobj <- create.hermite.basis(rangeval=c(1,length(lt_data)), nbasis=5)
basisobj <- create.hermite.basis(rangeval=rvals, nbasis=5)
plot(basisobj)
lt.fd = Data2fd(lt_data, basisobj = basisobj)
plot(lt.fd, main='Lithuania')
x = seq(rvals[1], rvals[2], length.out = length(lt_data))
lines(x, lt_data, col='blue')
