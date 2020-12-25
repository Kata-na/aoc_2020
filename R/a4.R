rm(list = ls())
library(data.table)
library(magrittr)
library(stringr)

dtt <- fread('../Input/a4.csv') %>%
  .[name == '', name :='~~~~~~~']
# --------------------------------------------------------------
# Reducing number of lines
# --------------------------------------------------------------
str <- paste(dtt[['name']], collapse = ' ')
str <- unlist(strsplit(str, ' ~~~~~~~ '))
input_list <- strsplit(str, ' ' )

dt_final <- data.table()
for (i in 1:length(input_list)) {
  d <- unlist(strsplit(input_list[[i]], ':'))
  even <- seq(2, length(d), 2)
  uneven<- seq(1, length(d), 2)
  dt_final <- data.table(cbind(d[uneven], d[even])) %>%
    .[, r := i] %>%
    rbind(dt_final)
}

dt_final <- data.table::dcast(dt_final, r~V1,
                    value.var = 'V2') %>%
  .[, -'r']

dt_final[, 'count_non_empty'] <- 
  rowSums(!is.na(dt_final[, c('byr', 'ecl', 'pid', 'eyr', 'iyr', 'hcl', 'hgt')]))

answer <- nrow(dt_final[count_non_empty == 7, ])
answer
##------------------------------------------------------------------------------

# --------------------------------------------------------------
# PREP data
# --------------------------------------------------------------

dt_final <- dt_final %>%
  .[, c('byr', 'iyr', 'eyr') := lapply(.SD, as.integer),
    .SDcols = c('byr', 'iyr', 'eyr')]

dt_final <- dt_final %>%
  .[, hgt_n := as.integer(gsub('(^\\d{2,3}).*?$', '\\1', hgt))] %>%
  .[, hgt_ci := gsub('.*?([a-z]{2}$)', '\\1', hgt)] %>%
  .[grepl('\\d', hgt_ci), hgt_ci := NA_character_] 

# --------------------------------------------------------------
# DATA VERIFICATION
# --------------------------------------------------------------
dt_final <- dt_final %>%
  .[, byr_ok := grepl('^\\d{4}$', byr) & byr >= 1920 & byr <= 2002] %>%
  .[, iyr_ok := grepl('^\\d{4}$', iyr) & iyr >= 2010 & iyr <= 2020] %>%
  .[, eyr_ok := grepl('^\\d{4}$', eyr) & eyr >= 2020 & eyr <= 2030] %>%
  .[, hgt_ok := !(is.na(hgt_ci) | is.na(hgt_n))] %>%
  .[hgt_ci == 'cm', hgt_ok := hgt_n >= 150 & hgt_n <= 193] %>%
  .[hgt_ci == 'in', hgt_ok := hgt_n >= 59 & hgt_n <= 76] %>%
  .[, hcl_ok := grepl('^#[A-Za-z0-9]{6}$', hcl)] %>%
  .[, ecl_ok := ecl %in% c('amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth')] %>%
  .[, pid_ok := grepl('^\\d{9}$', pid)]


dt_final[, 'count_ok'] <- 
  rowSums(dt_final[, c('byr_ok', 'iyr_ok', 'eyr_ok', 'hgt_ok', 'hcl_ok', 'ecl_ok', 'pid_ok')])

answer2 <- nrow(dt_final[count_ok == 7, ])
print(answer2)
