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
  
  #dat = normalized
  dat_col_len = dim(dat)[2]
  x = apply(dat, 1, function(x) sum(x == 0))
  ratios = (dat_col_len - x)/dat_col_len
  selected = dat[ratios >= threshold,]
  
  # Take only values after filtering with threshold
  x = x[ratios >= threshold]
  min_zeros = min(x)
  max_zeros = max(x)
  
  if (tail_cut == T) {
    values = ncol(selected) - max_zeros
    mat_cut = c()
    for(i in 1:nrow(selected)) {
      print(selected[i, ])
      z = fn(selected[i,], values)
      colnames(z) <- NULL
      print(z)
      mat_cut = rbind(mat_cut,z)
    }
    selected = mat_cut
  } else {
    selected = dat[, max_zeros:data_col_len]
  }
  return(selected)  
}

select_non_zero_subset(normalized, 0.95)

# dat = normalized
# 
# x = apply(dat, 1, function(x) sum(x == 0))
# ratios = (dat_col_len - x)/dat_col_len
# threshold=1
# selected = dat[ratios >= threshold,]
# length(x)
# max(x)
