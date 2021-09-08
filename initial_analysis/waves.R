source('common.R')
source('jhu_data.R')
library(pvar)


incompatible_curves = c(42, 43, 103, 184, 86)

cov_data = normalized[-incompatible_curves,]
y_len = dim(cov_data)[2]
country_count = dim(cov_data)[1]
arg_range = c(-2,2)
x = seq(arg_range[1], arg_range[2], length.out = y_len)

nbasis=5
wbasis = create.hermite.basis(arg_range, nbasis=nbasis)
cvec0 = matrix(0, nbasis, country_count)
Wfd0 = fd(cvec0, wbasis)


Lfdobj    <- 10        #  penalize curvature of acceleration
lambda    <- 10^(-0.5)  #  smoothing parameter
covfdPar <- fdPar(Wfd0, Lfdobj, lambda)
fdCov_data <-smooth.monotone(x, t(cov_data), covfdPar)


Dvalues = eval.monfd(x, fdCov_data$Wfdobj, 1)
colnames(Dvalues) <- rownames(cov_data)
fd_eval = eval.monfd(x, fdCov_data$Wfdobj)

fd_values = fdCov_data$beta[1,] + fdCov_data$beta[2,] * t(fd_eval)
rownames(fd_values) <- rownames(cov_data)

plot(fd_values['Lithuania',], type='l')
plot(Dvalues[,'Lithuania'], col='blue', type='l')

waves <- function(x, dx, min_val=0.01, p=1.01) {
  country_count <- dim(dx)[2]
  country_data = list()
  for (i in 1:country_count) {
    country_name <- colnames(dx)[i]
    dat <- dx[,i]
    skip_values <- length(dat[dat<min_val])
    total_points <- length(dat)
    dat <- dat[skip_values:total_points]
    pvar_values <- pvar(dat, p)
    partition <- pvar_values$partition
    partition_values <- dat[partition]
    wave_points = list()
    if (length(partition) >= 3) {
      for (j in 1:(length(partition)-1)) {
        if (j <= (length(partition)-2)) {
          p1 <- partition_values[j]
          p2 <- partition_values[j+1]
          p3 <- partition_values[j+2]
          wave_pos = partition[j:(j+2)] + skip_values
        } else {
          p1 <- partition_values[j]
          p2 <- partition_values[j+1]
          if (p1 < p2) {
            p3 <- partition_values[j]
          } else {
            p3 <- p2
          }
          wave_pos = partition[j:(j+1)] + skip_values
          wave_pos = c(wave_pos, (partition[j+1] + skip_values))
        }
        is_wave = (p1 < p2 & p2 > p3)
        if (is_wave == T) {
          
          wave_points[[(length(wave_points)+1)]] = wave_pos
        }
      }
    } 
    country_data[[country_name]] = wave_points
  }
  return(country_data)
}

plot_waves <- function(y, wave) {
  plot(y, type='l')
  for(i in 1:length(wave)) {
    pts <- wave[[i]]
    y1 <- y[pts[1]-1]
    y2 <- y[pts[2]-1]
    y3 <- y[pts[3]-1]
    segments(pts[1], y1, pts[3], y3, col='blue')
    #segments(pts[2], y2, pts[3], y3, col='blue')
    
    lines(pts[1]:pts[2], y[pts[1]:pts[2]], col='red')
    lines(pts[2]:pts[3], y[pts[2]:pts[3]], col='red')
    
    abline(v=pts[1], col='gray')
    abline(v=pts[3], col='gray')
  }
}


wave_dat <- waves(x, Dvalues)
country = 'Lithuania'
wave <- wave_dat[[country]]
plot_waves(fd_values[country,], wave)
lines(t(normalized[country,]), col='gray')

plot(Dvalues[,country], type='l')
