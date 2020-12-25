rm(list = ls())
library(data.table)
library(magrittr)
library(stringr)
dtt <- fread('a9.csv')

find_bad <- function(dt, lns, preamble_len = 25 ) {
  input_num <- as.numeric(dt[(lns - preamble_len):(lns-1), input])
  out <- dt[lns, input]
  all_combs <- data.table(expand.grid(input_num, input_num)) %>%
    .[Var1 != Var2]
  all_sums <- all_combs[['Var1']] + all_combs[['Var2']]
  
  if (any(all_sums == out) & lns != nrow(dt)) {
    find_bad(dt, lns = (lns + 1))
  } else {
    return(out)
  }
}

answer <- find_bad(dt = dtt, lns = 26, preamble_len = 25)

################################################################################
## -----------------------------------------------------------------------------
## --- SECOND PART ---
## -----------------------------------------------------------------------------
################################################################################

answer <- as.numeric(answer)
dtt[, line := 1:nrow(dtt)]
lst <- dtt[input == answer, line]

sum_by_window <- function(dt, target, lst, window_size = 5) {
  st <- 1
  if(window_size == 1) return(1)
  for (i in 1:(lst - window_size)) {
    out <- sum(dt[i:(i+window_size -1), input])
    if (out == answer) {
      out <- dt[i:(i+window_size - 1), input]
      break
    }
  }
  
  if ( sum(out) == target) {
    return(out)
  } else {
    sum_by_window (dt, lst= lst, target = target, 
                   window_size = (window_size -1))
  }
}
out <- sum_by_window(dt = dtt, target = answer, lst = lst, window_size = 20)
print(out)
print(max(out) + min(out))

##------------------------------------------------------------------------------

find_sum <- function(int_vect, target, start = 1) {
  sum_answ <- cumsum(int_vect[start:length(int_vect)])
  if (any (sum_answ == target)) {
    return(int_vect[start:(which(sum_answ == target) + start -1)])
  } else find_sum(int_vect, target, start = start + 1)
}

out <- find_sum (int_vect = dtt[['input']], target = answer, start =1)
max(out) + min(out)
