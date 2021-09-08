list.of.packages <- c("fda", 'orthopolynom', 'pracma', 'fda.usc')

world.map.pkgs <- c("cowplot", "googleway", "ggplot2", "ggrepel", 
                    "ggspatial", "sf", "rnaturalearth", "rnaturalearthdata", "maps")
list.of.packages <- c(world.map.pkgs, list.of.packages)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(fda)
library(orthopolynom)
library(pracma)
library(fda.usc)
source('basisfd.R')
source('create.hermite.basis.R')
source('hermite.R')
source('getbasismat.R')
source('hermitepen.R')
source('eval.penalty.R')
source('smooth.monotone.R')

get_country_data <- function(country, dat) {
  y = dat[dat['Country.Region'] == country]
  as.numeric(y[5:length(y)])
}


load_data <- function(fname = '../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv') {
  dat = read.csv(fname)
  
  colnames(dat)
  header <- colnames(dat)
  dates_str <- header[5:length(header)]
  dates_str = gsub('X', '', dates_str)
  dates = as.Date(dates_str, format="%m.%d.%y")
  return(list(
    dat = dat,
    dates = dates
  ))
}


unlockBinding("getbasismatrix", getNamespace('fda'))
assign("getbasismatrix", getbasismatrix, getNamespace('fda'))

unlockBinding("basisfd", getNamespace('fda'))
assign("basisfd", basisfd, getNamespace('fda'))

unlockBinding("eval.penalty", getNamespace('fda'))
assign("eval.penalty", eval.penalty, getNamespace('fda'))

unlockBinding('fngrad.smooth.monotone', getNamespace('fda'))
assign('fngrad.smooth.monotone', fngrad.smooth.monotone, getNamespace('fda'))
