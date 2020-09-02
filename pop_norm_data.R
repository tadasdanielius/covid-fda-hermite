source('common.R')


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

rownames(normalized) <- trimws(paste(pop_dat$Country.Region, pop_dat$Province.State),"r")
all_date <- as.Date(matrix(unlist(strsplit(colnames(normalized), "X")), ncol=2, byrow=T)[,2], format="%m.%d.%y")
colnames(normalized) <- all_date

