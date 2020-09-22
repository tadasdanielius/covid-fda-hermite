source('common.R')

# ----- Smoothing ----
# remove countries which has "bad" data that is "China Ningxia" "China Qinghai" "China Tibet" 
incompatible_regions = c(47, 48, 55)
cov_data = selected[-incompatible_regions,]
y_len = dim(cov_data)[2]
country_count = dim(cov_data)[1]
arg_range = c(-2,2)
x = seq(arg_range[1], arg_range[2], length.out = y_len)

nbasis=12
wbasis = create.hermite.basis(arg_range, nbasis=nbasis)
cvec0 = matrix(0, nbasis, country_count)
Wfd0 = fd(cvec0, wbasis)

Lfdobj    <- 3          #  penalize curvature of acceleration
lambda    <- 10^(-0.5)  #  smoothing parameter
covfdPar <- fdPar(Wfd0, Lfdobj, lambda)
fdCov_data <- smooth.monotone(x, t(cov_data), covfdPar)

# ---- Analysis ----

fd_eval = eval.monfd(x, fdCov_data$Wfdobj)
plot(fd_eval[,1])
fd_values = fdCov_data$beta[1,] + fdCov_data$beta[2,] * t(fd_eval)
colnames(fd_values) <- colnames(cov_data)
plot(fd_values[10,], type='l')  # fd_values[country, day]
conv_values = matrix(0, country_count, y_len*2-1)
sd_values = c()
for (i in seq(1, country_count)) {
  f = fd_values[84,]
  g = fd_values[i,]
  f = f/max(f)
  g = g/max(g)
  result = convolve(f, g, type='open')
  conv_values[i,] = result
  lines(result)
  if (i == 1) {
    plot(result, type='l', col='red', ylim=c(0, 100))
    res = result
  } else {
    lines(result, col='gray')
  }
}
lines(res, col='red')

std_row = apply(conv_values, 2, sd)
mean_row = apply(conv_values, 2, mean)
std_row_p = mean_row + 10*std_row/sqrt(length(conv_values[1,]))
std_row_m = mean_row - 10*std_row/sqrt(length(conv_values[1,]))
lines(std_row_p, col='black')
lines(std_row_m, col='black')


fdval_mean = apply(fd_values, 2, mean)
fdval_std = apply(fd_values, 2, std)
fdval_upper = fdval_mean + 6*fdval_std/sqrt(length(fdval_std))
fdval_lower = fdval_mean - 6*fdval_std/sqrt(length(fdval_std))
