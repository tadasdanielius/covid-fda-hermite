source('jhu_data.R')

# Function will select rows only where we have "less than" days with zero.
# The point is to get rid of Countries where first case starts long after others
# Number of allowed zeros are specified in threshold argument 
# Threshold = (max_values - zero_values) / max_values
#
# Since each country started logging and having non zero values at a different interval
# we need to align with other countries. There are two options:
#  1. When each country start from non-zero values, 
#     but we loose tail if country has more non-zero values than other (tail_cut=T)
#  2. Taking values from the position where last zero is found. 
#     In this case we loose head of some of the countries who has earlier start (tail_cut=F)
select_non_zero_subset = function(dat, threshold, tail_cut=T) {
  
  fn <- function(x, values) {
    zeros = sum(x == 0)
    start_pos = zeros+1
    end_pos = start_pos + values-1
    return(x[start_pos:end_pos])
  }
  
  dat_col_len = dim(dat)[2]
  x = apply(dat, 1, function(x) sum(x == 0))
  ratios = (dat_col_len - x)/dat_col_len
  selected = dat[ratios >= threshold,]
  
  # Take only values after filtering with threshold
  x = x[ratios >= threshold]
  min_zeros = min(x)
  max_zeros = max(x)
  
  if (tail_cut == T) {
    s_rnames <- rownames(selected)
    rownames(selected) <- 1:nrow(selected)
    values = ncol(selected) - max_zeros
    mat_cut = c()
    for(i in 1:nrow(selected)) {
      z = fn(selected[i,], values)
      colnames(z) <- 1:length(z)
      mat_cut = rbind(mat_cut,z)
    }
    selected = mat_cut
    rownames(selected) <- s_rnames
  } else {
    selected = selected[, max_zeros:ncol(dat)]
  }
  return(selected)  
}



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
  
  fdobj <- smooth.fdata.row(dat[1,], type.basis='hermite', rangeval=rangeval, nbasis=nbasis)
  for (i in 2:dim(dat)[1]) {
    f <- smooth.fdata.row(dat[i,], type.basis='hermite', rangeval=rangeval, nbasis=nbasis)
    fdobj$coefs = cbind(fdobj$coefs, f$coefs)
  }
  fdobj$fdnames$reps = reps
  return(fdobj)
}

smooth.fdata.row <- function(row, nbasis=5, type.basis='fourier', reverse=F, rangeval=c(-2,2)) {
  country_dat = as.vector(t(row))
  country_dat = country_dat[country_dat > 0]
  if (reverse == T) {
    y = c(rev(country_dat), country_dat)
  } else {
    y = country_dat
  }
  x = seq(rangeval[1], rangeval[2], length.out = length(y))
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
dat = normalized[1:10,]
dat = normalized
selected = select_non_zero_subset(dat, 0.8, F)

fddat = smooth.fdata.hermite(selected, rownames(selected), rangeval=c(-2,2), nbasis=10)
par(mfrow=c(1,1))
plot(fddat)

sd_col_p2sigm = apply(selected, 2, function(x) mean(x)+sd(x))
sd_col_n2sigm = apply(selected, 2, function(x) mean(x)-sd(x))
mean_line = apply(selected, 2, mean)
plot(sd_col_p2sigm, col='red', type='l', ylim=c(min(sd_col_n2sigm), max(selected)))
for (i in 1:nrow(selected)) {
  lines(t(selected[i,]), col='gray')
}
lines(sd_col_p2sigm, col='red', type='l')
lines(sd_col_n2sigm, col='red', type='l')
lines(mean_line, col='blue')

plot(sd_col_p2sigm, col='red', type='l', ylim=c(0, max(sd_col_p2sigm)+10))
curves = c()
for (i in 1:nrow(selected)) {
  y = t(selected[i,])
  if (mean(y < sd_col_p2sigm) > 0.8) {
    lines(t(selected[i,]), col='gray')
    curves = rbind(curves, selected[i,])
  }
}

fddat = smooth.fdata.hermite(selected, rownames(selected), rangeval=c(-2,2), nbasis=8)
par(mfrow=c(1,1))
plot(fddat)



