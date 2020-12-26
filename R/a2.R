rm(list = ls())
library(data.table)
library(magrittr)
library(stringr)
import::from('tidyr', 'separate')

dtt <- read.csv('../Input/a2.txt', header = FALSE, stringsAsFactors = FALSE) %>%
  data.table() %>%
  setnames('V1', 'input')
  
dtt <- data.table(str_split_fixed(dtt$input, ' ', n=3))

dtt <- dtt %>%
  .[, l := trimws(gsub(':', '', V2))] %>%
  .[, c('n.min', 'n.max') :=
      list(as.integer(gsub('(^\\d{1,3}).*?$', '\\1', V1)),
           as.integer(gsub('^.*?-(\\d{1,3}$)', '\\1', V1)))] %>%
  setnames('V3', 'password') %>%
  .[, .(n.min, n.max, l, password)]

dtt <- dtt %>%
  .[, count := str_count(password, l)]

dtt <- dtt %>%
  .[, check := (count >= n.min & count <= n.max)]
print(sum(dtt$check))

##------------------------------------------------------------------------------

dtt <- dtt %>%
  .[, check1 := substring(password, n.min, n.min) == l |
       substring(password, n.max, n.max) == l] %>%
  .[, check2 := substring(password, n.min, n.min) == l &
      substring(password, n.max, n.max) == l]
print(sum(dtt[['check1']] & !dtt[['check2']]))

