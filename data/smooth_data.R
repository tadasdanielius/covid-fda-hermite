


source('data_import.R')
source('hermite_basis/common.R')

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

smooth_data <- function(data){

  y_len = dim(data)[2]
  country_count = dim(data)[1]
  arg_range = c(-2,2)
  x = seq(arg_range[1], arg_range[2], length.out = y_len)
  
  nbasis=8
  wbasis = create.hermite.basis(arg_range, nbasis=nbasis)
  cvec0 = matrix(0, nbasis, country_count)
  Wfd0 = fd(cvec0, wbasis)
  
  
  Lfdobj    <- 10        #  penalize curvature of acceleration
  lambda    <- 10^(-0.5)  #  smoothing parameter
  fdPar <- fdPar(Wfd0, Lfdobj, lambda)
  fda_data <-smooth.monotone(x, t(data), fdPar)
  
  
  Dvalues = eval.monfd(x, fda_data$Wfdobj, 1)
  colnames(Dvalues) <- rownames(data)
  fd_eval = eval.monfd(x, fda_data$Wfdobj)
  
  fd_values = fda_data$beta[1,] + fda_data$beta[2,] * t(fd_eval)
  rownames(fd_values) <- rownames(data)
  return(list(fda_object=fda_data, eval_value=fd_values, first_deriv=Dvalues))
  
}


#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#>>>>>>>> Cases <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#accumulated new cases per 100,000 inhabitants for countries
cases_norm_countries_fd = smooth_data(cases_norm_countries)

#accumulated new cases per 100,000 inhabitants for continents
cases_norm_continents_fd = smooth_data(cases_norm_continents)

#accumulated new cases per 100,000 inhabitants for world
cases_norm_world_fd = smooth_data(cases_norm_world)

#accumulated new cases per 100,000 inhabitants for US states
cases_US_states_fd = smooth_data(cases_norm_US_states)




#>>>>>>>> Deaths <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#accumulated deaths per 100,000 inhabitants for countries
deaths_norm_countries_fd = smooth_data(deaths_norm_countries)

#accumulated deaths per 100,000 inhabitants for continents
deaths_norm_continents_fd = smooth_data(deaths_norm_continents)

#accumulated deaths per 100,000 inhabitants for world
deaths_norm_world_fd = smooth_data(deaths_norm_world)

#accumulated deaths per 100,000 inhabitants for US states
deaths_US_states_fd = smooth_data(deaths_norm_US_states)




#>>>>>>>> Vaccinated <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#accumulated vaccinated per 100,000 inhabitants for countries
vacc_norm_countries_fd = smooth_data(vacc_norm_countries)

#accumulated vaccinated per 100,000 inhabitants for continents
vacc_norm_continents_fd = smooth_data(vacc_norm_continents)

#accumulated vaccinated per 100,000 inhabitants for world
vacc_norm_world_fd = smooth_data(vacc_norm_world)

#accumulated vaccinated per 100,000 inhabitants for US states
vacc_US_states_fd = smooth_data(vacc_norm_US_states)


 # matplot(t(vacc_norm_countries_fd$eval_value), type="l")
 # matplot(vacc_norm_countries_fd$first_deriv, type="l")


 matplot(t(vacc_norm_countries[which(rownames(vacc_norm_countries_fd$eval_value) %in% "Lithuania"),]), pch=1)
 matlines(vacc_norm_countries_fd$eval_value[which(rownames(vacc_norm_countries_fd$eval_value) %in% "Lithuania"),], type="l", col=2, lwd=2)

 # cov_data = cases_norm_countries
# ii = c()
# for (i in 1:length(rownames(cov_data))) {
#   res <- tryCatch({
#     cur = smooth.monotone(x, t(cov_data)[,i], covfdPar)
#   }, warning = function(w) {
# 
#   }, error = function(e) {
#     print(e)
#     ii = c(ii, i)
#     print('Failed')
#     return(NA)
#   }, finally = {
# 
#   })
# 
#   if (length(res)<2) {
#     ii = c(ii, i)
#     print(ii)
#   }
# }


# <<<<<<<<<<<<<<<< Env variables >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

rm(list=setdiff(ls(), c("cases_norm_countries_fd", # [cases countries]
                        "cases_norm_continents_fd", # [cases world]
                        "cases_norm_world_fd",  # [cases continents]
                        "cases_US_states_fd", # [cases countries per 100,000 inhabitants]
                        "deaths_norm_countries_fd", # [cases world per 100,000 inhabitants]
                        "deaths_norm_continents_fd",  # [cases continents per 100,000 inhabitants]
                        "deaths_norm_world_fd", # [deaths countries]
                        "deaths_US_states_fd", # [deaths world]
                        "vacc_norm_countries_fd",  # [deaths continents]
                        "vacc_norm_continents_fd", # [deaths countries per 100,000 inhabitants]
                        "vacc_norm_world_fd", # [deaths world per 100,000 inhabitants]
                        "vacc_US_states_fd"  # [deaths continents per 100,000 inhabitants]
                       
)))
