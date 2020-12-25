rm(list = ls())
library(data.table)
library(magrittr)
library(stringr)
dtt <- read.csv('a7.csv', stringsAsFactors = FALSE) %>%
  data.table()

dtt<- dtt %>%
  .[, main_bag := gsub('(^.*? *.?) bags .*?$', '\\1', name)] %>%
  .[, inside_bags := gsub('^.*? *.? bags contain (.*?$)', '\\1', name)] %>%
  .[, check_gold := grepl('shiny gold', inside_bags)]
loop <- T
answer <- sum(dtt[['check_gold']])
checks <- matrix(dtt[['check_gold']])
while (loop) {
  patt <- paste0('(', paste(dtt[(check_gold), main_bag], collapse = '|'), ')')
  dtt <- dtt %>%
    .[, check_gold := grepl(patt, inside_bags)]
  if (sum(dtt[['check_gold']]) == 0) {
    break
  } else {
    checks <- cbind(checks, matrix(dtt[['check_gold']]))
  }
}

sum(rowSums(checks) != 0)

################################################################################
## -----------------------------------------------------------------------------
## --- PART TWO ---
## -----------------------------------------------------------------------------
################################################################################
dtt <- dtt %>%
  .[, list(inside_bags = unlist(strsplit(inside_bags, ', '))), 
    by = .(main_bag)]  %>%
  .[, inside_bags := gsub(' bags?\\.?', '', inside_bags)] %>%
  .[, n := as.integer(gsub('(^\\d{1,3}).*?$', '\\1', inside_bags))] %>%
  .[is.na(n), n:= 0] %>%
  .[, inside_bags := gsub('(^\\d{1,3} )(.*$)', '\\2', inside_bags)] 

dtt[main_bag == 'shiny gold']

bags_inside <- setdiff(dtt[main_bag == 'shiny gold', inside_bags], 'no other')

dtt.tmp <- copy(dtt[main_bag == 'shiny gold', .(inside_bags, n)]) 
setnames(dtt.tmp, c('inside_bags', 'n'), c('main_bag', 'n_multi'))

answer <- sum(dtt[main_bag == 'shiny gold', n])
loop <- T
while (loop) {
  dtt.tmp <- dtt[main_bag %in% bags_inside & !inside_bags %in% c('no other'), ] %>%
    copy() %>%
    merge(dtt.tmp, by = 'main_bag') %>%
    .[, n_multi := n * n_multi] %>%
    .[, .(inside_bags, n_multi)] %>%
    setnames('inside_bags', 'main_bag')
  
  answer <- answer + sum(dtt.tmp[, n_multi])
  bags_inside <- setdiff(unique(dtt[main_bag %in% bags_inside, inside_bags]), 'no other')
  
  if (length(bags_inside) == 0) loop <- F
  
}
answer


