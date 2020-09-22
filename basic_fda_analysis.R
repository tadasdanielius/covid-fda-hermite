source('common.R')
library(pvar)

# ---- smoothing ----
x = seq(-2, 2, length.out = dim(normalized)[2])
cov_fdata = fdata(normalized, argvals=x)
cov_fd = fdata2fd(cov_fdata, type.basis='hermite', nbasis=13)
plot(cov_fd)

# ---- pca -----

cov_pca = pca.fd(cov_fd, 2)
cumsum(cov_pca$varprop)
par(mfrow=c(2,1))
plot(cov_pca)


# ---- depth ----

par(mfrow=c(1,1))
cov_fm = depth.FM(cov_fd)
plot(cov_fm)

par(mfrow=c(1,1))
cov_rp = depth.RP(cov_fd)
plot(cov_rp)

# ---- derivatives ----
plot(deriv.fd(cov_fd), ylim=c(0,2000))
plot(deriv.fd(cov_fd, 3), ylim=c(-1000,4000))


# ---- outliers ----
out_trim = outliers.depth.trim(cov_fd,dfunc=depth.mode,nb=50, quan=0.9, trim = 0.2)
out_trim$outliers

plot(cov_fd,col=1,lty=1)
lines(cov_fd[out_trim[[1]]],col='red')

plot(cov_fd[out_trim[[1]]],col=2)



# ---- Centrality ----
plot(func.mean(cov_fdata),main = "Centrality measures")
legend(x = -2,y = 400, cex = 1, box.col = "white", lty = 1:5, col = 1:5,legend = c("mean", "trim.mode", "trim.RP", "median.mode", "median.RP"))
lines(func.trim.mode(cov_fdata, trim = 0.15), col = 2, lty =2)
lines(func.trim.RP(cov_fdata, trim = 0.15),col = 3,lty = 3)
lines(func.med.mode(cov_fdata, trim = 0.15),col = 4,lty = 4)
lines(func.med.RP(cov_fdata, trim = 0.15),col = 5, lty = 5)


# ---- Standard Deviation ----

cov_sd = std.fd(cov_fd)
plot(cov_sd)
plot(cov_fd, ylim=c(0,1000))
lines(cov_sd, lwd=3)

# ---- variance ----
cov_var = var.fd(cov_fd)
plot(var.fd(cov_fd))
cov_var_mat = eval.bifd(x,x,cov_var)

filled.contour(x, x, cov_var_mat)
filled.contour(x, x, sqrt(cov_var_mat))


persp(x, x, cov_var_mat, xlab="Days", ylab="Days", zlab="Covariance",
      shade=0.1, col="red", border="blue", box=T)

cov_cca = cca.fd(cov_fd)
plot(cov_cca)




# ---- rainbow ----
#install.packages('rainbow')
library(rainbow)
cov_fts = fts(x, t(normalized), start = 1, frequency = 1, 'time', 'confirmed')
plot(cov_fts)
summaryfunction(cov_fts, plot.type = "summarystats", plot.legend = T)

SVDplot(cov_fts, order = 3, plot = TRUE, plot.type = "fts",
        mfrow = c(2, 3))

par(mfrow=c(1,1))
fboxplot(cov_fts, type="hdr")



# ---- find growth waves ----


find_waves = function(x, f) {
  is_wave = function(values, ppoints, x) {
    p = values[ppoints]
    p1 = p[1]
    p2 = p[2]
    p3 = p[3]
    is_w = (p1 < p2 & p2 > p3)
    # we may need certain rules to define if it is really fast grow
    form = (p3-p1)/ (ppoints[3]-ppoints[1])
    return(list(
      wave=is_w, form=form, x=x[ppoints[1]:ppoints[3]]
    ))
  }
  waves = list()
  d = deriv.fd(f)
  d_vals = eval.fd(x, d)
  pvar_val = pvar(d_vals, p=1.01)
  partition_points = pvar_val$partition
  if (length(partition_points) < 3) {
    return(waves)
  }
  if (length(partition_points) == 3) {
    pts = partition_points
    curve_details = is_wave(d_vals, pts, x)
    if (curve_details$wave) {
      waves[[1]] = curve_details
    }
    return(waves)
  }
  for (i in 3:length(partition_points)) {
    pts = partition_points[(i-2):i]
    curve_details = is_wave(d_vals, pts, x)
    if (curve_details$wave) {
      waves[[length(waves)+1]] = curve_details
    }
  }
  return(waves)
}

f = cov_fd['Lithuania']
waves = find_waves(x, f)

f_values = eval.fd(x, f)
plot(f)
for (i in 1:length(waves)) {
  xx = waves[[i]]$x
  #if (waves[[i]]$form > 0 & waves[[i]]$form < 2) {
  y = eval.fd(xx, f)
  lines(xx, y, col='red')
  segments(min(xx), y[1], max(xx), y[length(xx)], col='blue')
  abline(v=min(xx), col='gray')
  abline(v=max(xx), col='gray')
  #}
}


plot(log(eval.fd(waves[[2]]$x, f)), type='l')
