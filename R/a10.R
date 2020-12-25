rm(list = ls())
library(data.table)
library(magrittr)
library(stringr)
dtt <- fread('a10.csv')

inp <- c(0, sort(dtt[['input']]))
inp <- c(inp, last(inp) + 3)

# all.dif <- inp[2:length(inp)] - inp[1:length(inp)-1]
all.dif <- diff(inp)
answ_1 <- sum(all.dif == 1)
answ_3 <- sum(all.dif == 3)
answ_1 * answ_3


# --------------------------------------------------------------
# SECOND PART
# --------------------------------------------------------------

one_repeats <- paste(all.dif, collapse = '')
one_repeats <- unlist(strsplit(one_repeats, '3'))
one_repeats <- one_repeats[one_repeats!='']
one_repeats <- nchar(one_repeats)
one_repeats <- one_repeats[one_repeats!=1]

n <- 1

for (i in one_repeats) {
  if (i == 2) {
    n <- n * 2
  } else if (i == 3) {
    n <- n * 4
  } else if (i == 4) {
    n <- n * 7
  }
}
options(scipen = 999)

as.integer(n)
