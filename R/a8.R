rm(list = ls())
library(data.table)
library(magrittr)
library(stringr)
dtt <- fread('a8.csv') 

dtt <- dtt %>%
  # .[, operation := gsub('(^[a-z]{3}) .*$', '\\1', input)] %>%
  # .[, argument  := as.integer(gsub('^[a-z]{3} (.*$?)', '\\1', input))] %>%
  .[, line := 1:nrow(dtt)]

loop <- T
lns <- 1
accumulator  <- 0
lines_list <- c()
while (loop) {
  if (lns %in% lines_list) break
  lines_list <- c(lines_list, lns)
  operation <-  dtt[lns, operation]
  if (operation == 'acc') {
    accumulator <- accumulator + dtt[lns, argument]
    lns <- lns + 1
  } else if (operation == 'nop') {
    lns <- lns + 1
  } else {
    lns = lns + dtt[lns, argument]
  }
}
accumulator
################################################################################
## -----------------------------------------------------------------------------
## --- SECOND PART ---
## -----------------------------------------------------------------------------
################################################################################
func_check <- function(from, to, dtt, which_ch = 1) {
  lns <- 1
  accumulator  <- 0
  lines_list <- c()
  dtt_tmp <- copy(dtt)
  dtt_tmp[which(dtt_tmp[['operation']] == from)[which_ch], operation := to]
  
  loop2 <- T
  while (loop2) {
    if (lns %in% lines_list) break
    if (lns == nrow(dtt)) loop2 <- F
    lines_list <- c(lines_list, lns)
    operation <-  dtt_tmp[lns, operation]
    
    if (operation == 'acc') {
      accumulator <- accumulator + dtt_tmp[lns, argument]
      lns <- lns + 1
    } else if (operation == 'nop') {
      lns <- lns + 1
    } else {
      lns = lns + dtt_tmp[lns, argument]
    }
  }
  if (!loop2) {
    print('HURA')
    return(accumulator)
  } else {
    which_ch <- which_ch + 1
    if (which_ch > length(which(dtt_tmp[['operation']] == from))) return(NULL)
    func_check(from = from, to = to, dtt = dtt, which_ch = which_ch)
  }

}
func_check (from = 'nop', to = 'jmp', dtt = dtt, which_ch = 1)
func_check (from = 'jmp', to = 'nop', dtt = dtt, which_ch = 1)

