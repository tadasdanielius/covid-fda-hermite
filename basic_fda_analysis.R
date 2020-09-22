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



# ---- find exp growth ----
c_id = 'Germany'

sub_x = x

f1 = deriv.fd(cov_fd[c_id])
y_deriv = eval.fd(sub_x, f1)
d_pvar = pvar(y_deriv, 1.01)
plot(d_pvar)

part_values = head(y_deriv[d_pvar$partition], -1)
part_max_idx = which.max(part_values)

max_idx = d_pvar$partition[part_max_idx+1]
prev_max_idx = d_pvar$partition[part_max_idx-1]

max_val = y_deriv[max_idx]
prev_max_val = y_deriv[prev_max_idx]
point_count = max_idx - prev_max_idx
xx = seq(prev_max_idx, max_idx)
yy = y_deriv[xx]

plot(y_deriv, type='l')
lines(xx, yy, col='red', lwd=3)

cov_y = eval.fd(x, cov_fd[c_id])
cov_min_val = cov_y[prev_max_idx]
cov_max_val = cov_y[max_idx]
f_grow_y = seq(cov_min_val, cov_max_val, length.out = length(xx))
plot(cov_y, type='l')
lines(xx, cov_y[xx], col='red', lwd=3)
lines(xx, f_grow_y, col='blue')

lines(y_deriv, col='gray')


cov_yn = cov_y/max(cov_y)
dcov_yn = y_deriv/max(y_deriv)
line_yn = f_grow_y/max(cov_y)
plot(cov_yn, type='l', ylim=c(-1, 1))
lines(dcov_yn, col='gray')
lines(xx, line_yn, col='blue')
lines(xx, cov_yn[xx], col='red', lwd=3)

cov_d2 = deriv.fd(cov_fd[c_id], 2)
cov_d2 = eval.fd(sub_x, cov_d2)
cov_d2 = cov_d2/max(cov_d2)
lines(cov_d2, col='green')



# -- Search for second wave

# Starting position is the last point of previous wave
sub_x = x[max(xx):length(x)]
sf1 = deriv.fd(cov_fd[c_id])
yd1 = eval.fd(sub_x, sf1)
plot(f1)
lines(sub_x, yd1, col='green')

d_pvar1 = pvar(yd1, 1.01)
head(y_deriv[d_pvar1$partition], -1)

all_x = x
f = cov_fd['Lithuania']


find_waves = function(x, f) {
  is_wave = function(p) {
    p1 = p[1]
    p2 = p[2]
    p3 = p[3]
    return(p1 < p2 & p2 > p3)
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
    pts = d_vals[partition_points]
    if (is_wave(pts)) {
      waves[[1]] = x[partition_points[(i-2)]:partition_points[i]]
    }
    return(waves)
  }
  for (i in 3:length(partition_points)) {
    pts = d_vals[partition_points[(i-2):i]]
    if (is_wave(pts)) {
      waves[[length(waves)+1]] = x[partition_points[(i-2)]:partition_points[i]]
    }
  }
  return(waves)
}
