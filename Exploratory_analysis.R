

library(ggrepel)
library(lubridate)
library(tidyverse)
source('common.R')
library(fda)
library(fda.usc)
library(ggplot2)
source('subdivision_of_countries.R')
library(ggthemes)
load("smoothed_20201117.RData")
library(kableExtra) # complex tables


path_to_write <- ""

#####################################################################
add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}

################################################################
#Quarantined cases
#fd object
fd_values_ <- ifelse(fd_values<0,0,fd_values)
#matplot(t(fd_values_),type="l")
fd_cases <- Data2fd(x, t(fd_values_), wbasis)
#plot(fd_cases)
mean_cases_fd <- mean.fd(fd_cases)
#lines(mean.fd(fd_cases), col=1, lwd=2)
#plot(fdCov_data$yhatfd)
#mean_cases <- colMeans(fd_values_)

#fdata object
fdata_cases <- fdata(fd_cases, argvals = x)
#plot(fdata_cases)
plot(sqrt(func.var(fdata_cases)))

cases_data <- t(fd_values_)
#matplot(cases_data, type="l")
#matlines(mean_cases, col=1, lwd=2)
i=1

sd_cases <- numeric(dim(cases_data)[1])
for(i in 1:dim(cases_data)[1]){
  mean_cases <- rowMeans(cases_data)
  sd_cases[i] <- sqrt((sum((cases_data[i,]-mean_cases[i])^2))/(dim(cases_data)[1]-1))
  
}
plot(sd_cases, type="l")


#Centered cases

cases_centr <- matrix(NA, nrow=dim(cases_data)[1],ncol=dim(cases_data)[2])
for(i in 1:length(colnames(cases_data))){
  cases_centr[,i] <- cases_data[,i]-mean_cases
}
colnames(cases_centr) <- colnames(cases_data)

#matplot(cases_centr,type="l") 

sd_cases_fd = std.fd(fd_cases)
plot(sd_cases_fd)

var_cases_fd = var.fd(fd_cases)
#plot(var_cases_fd)
logprec.varmat = eval.bifd(x, x,
                           var_cases_fd)
contour(x, x, logprec.varmat,
        xlab="Days",
        ylab="Days", lwd=2,
        labcex=1)

persp(x, x, logprec.varmat,
      theta=-45, phi=25, r=3, expand = 0.5,
      ticktype='detailed',
      xlab="Days",
      ylab="Days",
      zlab="variance")

#Recovered cases
#fd object
fd_values_rec_ <- ifelse(fd_values_rec<0,0,fd_values_rec)
fd_recov <- Data2fd(x, t(fd_values_rec_), wbasis)
#plot(fd_recov)

mean_recov_fd <- mean.fd(fd_recov)
#lines(mean.fd(mean_recov_fd), col=1, lwd=2)
#mean_recov <- eval.fd(x,mean_recov_fd)
mean_cases <- colMeans(fd_values_rec_)


#plot(fdRec_data$yhatfd)

#fdata object
fdata_recov <- fdata(fd_recov, argvals = x)
#plot(fdata_recov)

recov_data <- t(fd_values_rec)
#matplot(recov_data, type="l")




recov_centr <- matrix(NA, nrow=dim(recov_data)[1],ncol=dim(recov_data)[2])
for(i in 1:length(colnames(recov_data))){
  recov_centr[,i] <- recov_data[,i]-mean_recov
}
colnames(recov_centr) <- colnames(recov_data)

#matplot(recov_centr,type="l") 

#Closed cases
#fd object
fd_values_dth_ <- ifelse(fd_values_dth<0,0,fd_values_dth)
fd_closed <- Data2fd(x, t(fd_values_dth_), wbasis)
#plot(fd_closed)

mean_closed_fd <- mean.fd(fd_closed)
#lines(mean_closed_fd, col=1, lwd=2)
#mean_closed <- eval.fd(x,mean_closed_fd)
mean_cases <- colMeans(fd_values_dth_)

#plot(fdDth_data$yhatfd)

#fdata object
fdata_closed <- fdata(fd_closed, argvals = x)
#plot(fdata_closed)

closed_data <- t(fd_values_dth)
#matplot(closed_data, type="l")



closed_centr <- matrix(NA, nrow=dim(closed_data)[1],ncol=dim(closed_data)[2])
for(i in 1:length(colnames(closed_data))){
  closed_centr[,i] <- closed_data[,i]-mean_closed
}
colnames(closed_centr) <- colnames(closed_data)

#matplot(closed_centr,type="l") 



#################################################################
#information about regions
Countries_info <- subdivision_of_countries(fd_cases$fdnames$reps)

regions <- unique(Countries_info$region)
subregions <- unique(Countries_info$`sub-region`)


#################################################################
#Exploratory analysis

which_c <-5
which_type <- cases_data
countries_in_c <- Countries_info[which(Countries_info$region %in% regions[which_c]),5]
cases_data_c <- which_type[,which(colnames(which_type) %in% countries_in_c)]
cases_data_cc <- cases_data_c[,-which(colnames(cases_data_c) %in% "France French Polynesia")]


data_df <- as.data.frame(cases_data_cc)
date_seq <- seq(as.Date("2020-01-22",format="%Y-%m-%d"),
                by=1, length.out=dim(data_df)[1])
data_df$Date <- date_seq

to_plot_table <- data_df %>%
  pivot_longer(!Date, names_to="country", values_to="var"
  )


ggplot(to_plot_table, aes(x = Date, y = var, color = country, group = country)) + 
  geom_line() +
  # Add labels at the end of the line
  geom_text(data = filter(to_plot_table, Date == max(Date)),
            aes(label = country),
            hjust = -0.2, nudge_x =0.1, size=2.5) +
  # 
  # geom_text_repel(data = filter(to_plot_table, Date == max(Date)),
  #                 aes(label = country),
  #                 hjust = 0, nudge_x = 0.1, size=2.5)+
  # Allow labels to bleed past the canvas boundaries
  coord_cartesian(clip = 'off') +
  # Remove legend & adjust margins to give more space for labels
  # Remember, the margins are t-r-b-l
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  labs(title="",
        x ="Date", y = "Cases per 100,000 ")+
  theme_minimal()  + 
  theme(legend.position = 'none',
        plot.margin = margin(0.1, 2.6, 0.1, 0.1, "cm"),
        axis.text.x=element_text(angle=0, hjust=0.5))


#############################################################################
original_plot <- function(to_plot){
#to_plot <-  cases_data
to_plot <- ifelse(to_plot<0,0,to_plot)  
matplot(to_plot, type="l", cex.axis=0.9, xaxt='n', xlab="Date", ylab="Cases per 100,000")
date_labels <- unique(format(seq(as.Date("2020-01-22",format="%Y-%m-%d"),
                by=1, length.out=dim(to_plot)[1]), '%b'))
date_at <- which(seq(as.Date("2020-01-22",format="%Y-%m-%d"),
                     by=1, length.out=dim(to_plot)[1])   %in% 
                   unique(floor_date(seq(as.Date("2020-01-22",
                                                 format="%Y-%m-%d"),
                      by=1, length.out=dim(to_plot)[1]), 'month')))
axis(side = 1, at = date_at, labels = date_labels[-1], tck = -0.03, cex.axis=1)
abline(v=date_at, col="lightgrey", lty=3)
y_at_grid <- round(seq(0, max(to_plot)+1000, by=1000),0)
abline(h=y_at_grid, col="lightgrey", lty=3)
matlines(to_plot, type="l")
# mean_var <- rowMeans(to_plot)
# matlines(mean_var, type="l", lwd=3)
# legend("topleft", c("Mean"), lwd = 3, col = 1, cex=0.8)
}
######################################################################

#Exploratory plots
file_path <- paste(path_to_write,"original_plots/closed_all.pdf", sep="")
pdf(file_path, width=8, height=5)
original_plot(closed_data)
dev.off()



original_plot(recov_data)
original_plot(closed_data)


#Regions
#which_c <- 5
for (which_c in 1:5){
which_type <- closed_data
countries_in_c <- Countries_info[which(Countries_info$region %in% regions[which_c]),5]
cases_data_c <- which_type[,which(colnames(which_type) %in% countries_in_c)]
file_path <- paste(path_to_write,"original_plots/closed_",regions[which_c],".pdf", sep="")
pdf(file_path, width=8, height=5)
original_plot(cases_data_c)
dev.off()
}
#############################################################################
#Derivatives

#to_plot <-  cases_data
to_plot <- ifelse(to_plot<0,0,to_plot) 
deriv_to_plot <- diff(to_plot)
matplot(deriv_to_plot, type="l", cex.axis=0.9, xaxt='n', xlab="Date", ylab="Cases per 100,000")

derivatives_plot <- function(to_plot){
  #to_plot <-  cases_data
  to_plot <- ifelse(to_plot<0,0,to_plot)
  deriv_to_plot <- diff(to_plot)
  matplot(deriv_to_plot, type="l", cex.axis=0.9, xaxt='n', xlab="Date", ylab="Cases per 100,000")
  date_labels <- unique(format(seq(as.Date("2020-01-22",format="%Y-%m-%d"),
                                   by=1, length.out=dim(deriv_to_plot)[1]), '%b'))
  date_at <- which(seq(as.Date("2020-01-22",format="%Y-%m-%d"),
                       by=1, length.out=dim(to_plot)[1])   %in% 
                     unique(floor_date(seq(as.Date("2020-01-22",
                                                   format="%Y-%m-%d"),
                                           by=1, length.out=dim(to_plot)[1]), 'month')))
  axis(side = 1, at = date_at, labels = date_labels[-1], tck = -0.03, cex.axis=1)
  abline(v=date_at, col="lightgrey", lty=3)
  y_at_grid <- round(seq(0, max(deriv_to_plot)+50, by=50),0)
  abline(h=y_at_grid, col="lightgrey", lty=3)
  matlines(deriv_to_plot, type="l")
  # mean_var_deriv <- rowMeans(deriv_to_plot)
  # matlines(mean_var_deriv, type="l", lwd=3)
  # legend("topleft", c("Mean"), lwd = 3, col = 1, cex=0.8)
}

###############################################################################
file_path <- paste(path_to_write,"derivative_plots/recov_all.pdf", sep="")
pdf(file_path, width=8, height=5)
derivatives_plot(recov_data)
dev.off()


derivatives_plot(recov_data)
derivatives_plot(closed_data)



#Regions
#which_c <- 1
for (which_c in 1:5){
which_type <- cases_data
countries_in_c <- Countries_info[which(Countries_info$region %in% regions[which_c]),5]
cases_data_c <- which_type[,which(colnames(which_type) %in% countries_in_c)]
file_path <- paste(path_to_write,"derivative_plots/cases_",regions[which_c],".pdf", sep="")
pdf(file_path, width=8, height=5)
derivatives_plot(cases_data_c)
dev.off()
}


##################################################################################
exploratory_plot <- function(to_plot){
#to_plot <- cases_data
x <- seq(-2,2,length=dim(to_plot)[1])
fd_var <- Data2fd(x, to_plot, wbasis)
fdata_var <- fdata(fd_var, argvals = x)
#depths
fmd = depth.FM(fdata_var,draw=F)
md =  depth.mode(fdata_var)
rpd = depth.RP(fdata_var, nproj = 50)
cur <- c(fmd$lmed, md$lmed, rpd$lmed)
matplot(to_plot,col="grey", type="l", cex.axis=0.9, xaxt='n',yaxt='n', xlab="Date", ylab="Cases per 100,000")
matlines(to_plot[,cur], lwd = 2, lty = c(2,4,5) , col = c("chocolate1","navy", "green3" ))
date_labels <- unique(format(seq(as.Date("2020-01-22",format="%Y-%m-%d"),
                                 by=1, length.out=dim(to_plot)[1]), '%b'))
date_at <- which(seq(as.Date("2020-01-22",format="%Y-%m-%d"),
                     by=1, length.out=dim(to_plot)[1])   %in% 
                   unique(floor_date(seq(as.Date("2020-01-22",
                                                 format="%Y-%m-%d"),
                                         by=1, length.out=dim(to_plot)[1]), 'month')))
axis(side = 1, at = date_at, labels = date_labels[-1], tck = -0.03, cex.axis=0.9)
y_at_grid <- round(seq(0, max(to_plot)+1000, by=1000),0)
axis(side = 2, at = y_at_grid, labels = y_at_grid, tck = -0.03, cex.axis=0.8, las=2)
#mean function
mean_var <- rowMeans(to_plot)
matlines(mean_var, type="l", lwd=2)
#outliers
out2<-outliers.depth.pond(fdata_var,nb=100,dfunc=depth.FM)$outliers
matlines(to_plot[,out2], lwd = 2, lty = 2 , col = "red")
#to_plot_out <- to_plot[,-which(colnames(to_plot) %in% out2)]
#matplot(to_plot_out, lwd = 2, lty = 2 , col = "lightgrey", type="l")

matlines(t(sqrt(func.var(fdata_var))$data), col = "black", lwd = 2, lty = 2)

add_legend("topleft", c(paste("Integrated Depth: the deepest function = ",names(cur)[1],".",sep=""),
                        paste("Modal depth: the deepest function = ",names(cur)[2],".",sep=""),
                        paste("Random Projection depth: the deepest function = ",names(cur)[3],".",sep=""),
                       "Outliers"), 
           lwd = 2, lty = c(2,4,5,2) ,col = c("chocolate1","navy", "green3", "red"), 
           cex=0.7, bty="n")
add_legend("topright", c("Mean function",
                         "Standard deviation"), 
           lwd = 2, lty = c(1,2) ,col = c("black", "black"), 
           cex=0.7, bty="n")

print(paste("Outliers:"))
print(out2)
to_write <- c("","cases",regions[which_c],out2)
filename <- paste(path_to_write,"exploratory_plots/outliers.csv", sep="")
if(is.na(file.info(filename)$size)){
  write.csv(to_write, 
            filename)
}else{
  write.table(to_write, 
              filename, 
              sep = ",", 
              col.names = !file.exists(filename), append = T)
}
}
#############################################################################


#Mean, Depths, Outliers functions
file_path <- paste(path_to_write,"exploratory_plots/closed_all.pdf", sep="")
pdf(file_path, width=8, height=5)
exploratory_plot(closed_data)
dev.off()

exploratory_plot(recov_data)
exploratory_plot(closed_data)


#Regions
#which_c <- 1
for (which_c in 1:5){
  which_type <- cases_data
  countries_in_c <- Countries_info[which(Countries_info$region %in% regions[which_c]),5]
  cases_data_c <- which_type[,which(colnames(which_type) %in% countries_in_c)]
  file_path <- paste(path_to_write,"exploratory_plots/cases_",regions[which_c],".pdf", sep="")
  pdf(file_path, width=8, height=5)
  exploratory_plot(cases_data_c)
  dev.off()
}

#############################################################################



outliers_functions <- function(to_plot){
  #to_plot <- cases_data_c
  x <- seq(-2,2,length=dim(to_plot)[1])
  fd_var <- Data2fd(x, to_plot, wbasis)
  fdata_var <- fdata(fd_var, argvals = x)
  #outliers
  out2<-outliers.depth.pond(fdata_var,nb=100,dfunc=depth.FM)$outliers
  matplot(to_plot,col="grey", type="l", cex.axis=0.9, xaxt='n',yaxt='n', xlab="Date", ylab="Cases per 100,000")
  date_labels <- unique(format(seq(as.Date("2020-01-22",format="%Y-%m-%d"),
                                   by=1, length.out=dim(to_plot)[1]), '%b %Y'))
  date_at <- which(seq(as.Date("2020-01-22",format="%Y-%m-%d"),
                       by=1, length.out=dim(to_plot)[1])   %in% 
                     unique(floor_date(seq(as.Date("2020-01-22",
                                                   format="%Y-%m-%d"),
                                           by=1, length.out=dim(to_plot)[1]), 'month')))
  axis(side = 1, at = date_at, labels = date_seq[-1], tck = -0.03, cex.axis=0.9)
  y_at_grid <- round(seq(0, max(to_plot)+1000, by=1000),0)
  axis(side = 2, at = y_at_grid, labels = y_at_grid, tck = -0.03, cex.axis=0.8, las=2)
  matlines(to_plot[,out2], lwd = 2, lty = 2 , col = "red")
  #to_plot_out <- to_plot[,-which(colnames(to_plot) %in% out2)]
  #matplot(to_plot_out, lwd = 2, lty = 2 , col = "lightgrey", type="l")
  add_legend("topleft", c("Outliers"), 
             lwd = 2, lty = 2 ,col = c("red" ), cex=0.8, bty="n")
  
  print(out2)
}

########################################################################################

outliers_functions(cases_data)

#######################################################################################

#fPCA

#Europe
which_c <- 1
which_type <- closed_data
countries_in_c <- Countries_info[which(Countries_info$region %in% regions[which_c]),5]
cases_data_c <- which_type[,which(colnames(which_type) %in% countries_in_c)]

fd_cases_EU <- Data2fd(x, cases_data_c, wbasis)

pca_var <- pca.fd(fd_cases_EU, nharm=3, centerfns=T)
print(pca_var$varprop)


file_path <- paste(path_to_write,"fPCA/closed_",regions[which_c],"_harm3.pdf", sep="")
pdf(file_path, width=8, height=5)
par(mfrow=c(1,1))
plot.pca.fd(pca_var,  xaxt='n', xlab="Date", harm=3)

date_labels <- unique(format(seq(as.Date("2020-01-22",format="%Y-%m-%d"),
                                 by=1, length.out=length(pca_var$meanfd$fdnames$time)), '%b'))
date_at <- which(seq(as.Date("2020-01-22",format="%Y-%m-%d"),
                     by=1, length.out=length(pca_var$meanfd$fdnames$time))   %in% 
                   unique(floor_date(seq(as.Date("2020-01-22",
                                                 format="%Y-%m-%d"),
                                         by=1, length.out=length(pca_var$meanfd$fdnames$time)), 'month')))
axis(side = 1, at = x[date_at], labels = date_labels[-1], tck = -0.03, cex.axis=0.9)

dev.off()



rotpcascores = pca_var$scores


scores_table <- as.data.frame(cbind(rotpcascores[,1],rotpcascores[,2],rotpcascores[,3],fd_cases_EU$fdnames$reps))
colnames(scores_table) <- c("Harmonic_I","Harmonic_II","Harmonic_III","Country")
scores_table$Harmonic_I<- as.numeric(scores_table$Harmonic_I)
scores_table$Harmonic_II<- as.numeric(scores_table$Harmonic_II)
scores_table$Harmonic_III<- as.numeric(scores_table$Harmonic_III)

file_path <- paste(path_to_write,"fPCA/closed_",regions[which_c],"_scores12.pdf", sep="")
pdf(file_path, width=8, height=5)
ggplot(scores_table) +
  geom_point(aes(Harmonic_I, Harmonic_II), color = 'red') +
  geom_text_repel(aes(Harmonic_I, Harmonic_II, label = Country)) +
  theme_classic(base_size = 16) +
  labs(x="Harmonic I",y="Harmonic II")
dev.off()

file_path <- paste(path_to_write,"fPCA/closed_",regions[which_c],"_scores13.pdf", sep="")
pdf(file_path, width=8, height=5)
ggplot(scores_table) +
  geom_point(aes(Harmonic_I, Harmonic_III), color = 'red') +
  geom_text_repel(aes(Harmonic_I, Harmonic_III, label = Country)) +
  theme_classic(base_size = 16) +
  labs(x="Harmonic I",y="Harmonic III")
dev.off()

file_path <- paste(path_to_write,"fPCA/closed_",regions[which_c],"_scores23.pdf", sep="")
pdf(file_path, width=8, height=5)
ggplot(scores_table) +
  geom_point(aes(Harmonic_II, Harmonic_III), color = 'red') +
  geom_text_repel(aes(Harmonic_II, Harmonic_III, label = Country)) +
  theme_classic(base_size = 16) +
  labs(x="Harmonic II",y="Harmonic III")
dev.off()
