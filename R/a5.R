rm(list = ls())
library(data.table)
library(magrittr)
library(stringr)
dtt <- read.csv('a5.csv', stringsAsFactors = FALSE) %>%
  data.table()

min_max_func <- function(id, skip_symbols, min_v, max_v, upper_l) {
  min_v <- ifelse(grepl(paste0('^.{', skip_symbols, '}',upper_l), id),
                  min_v, min_v + ceiling((max_v - min_v)/2))
  max_v <- ifelse(grepl(paste0('^.{', skip_symbols, '}', upper_l), id),
                  min_v + floor((max_v - min_v)/2), max_v)
  return(list(min_v, max_v))
}

dtt <- dtt %>%
  .[, c('min_v', 'max_v') := list(ifelse(grepl('^F', id), 0, 64),
                              ifelse(grepl('^F', id), 63, 127))] 
for (i in 1:5) {
  dtt <- dtt %>%
    .[, c('min_v', 'max_v') :=  min_max_func(id, skip_symbols = i, min_v = min_v, 
                                             max_v = max_v, upper_l = 'F')]
}
dtt <- dtt %>%
  .[, final_row := ifelse(grepl('^.{6}F', id), min_v, max_v)] %>%
  .[, c('lower', 'upper') := list(ifelse(grepl('^.{7}L', id), 0, 4),
                                    ifelse(grepl('^.{7}L', id), 3, 7))] %>%
  .[, c('lower', 'upper') :=  min_max_func(id, skip_symbols = 8, min_v = lower, 
                                           max_v = upper, upper_l = 'L')] %>%
  .[, final_columnn := ifelse(grepl('^.{9}L', id), lower, upper)] 

dt_final <- dtt[, .(id, final_row, final_columnn)]

dt_final[, seat_id := final_row * 8 + final_columnn]
max(dt_final[, seat_id])

##------------------------------------------------------------------------------
##### SECOND PART ######
##------------------------------------------------------------------------------
all_ids <- dt_final[, seat_id]

dt_final[, check_rule := ((seat_id - 2) %in% all_ids & !(seat_id - 1) %in% all_ids) ]
my_seat_id <- dt_final[(check_rule), seat_id] - 1
my_seat_id
# Other way
sort(dt_final[, seat_id])[diff(sort(dt_final[, seat_id]))!=1] +1

# --------------------------------------------------------------
# --------------------------------------------------------------
# A LOT EASIER WAY USING BINARY CONVERSION
# --------------------------------------------------------------
# --------------------------------------------------------------
dt_final[, 'binary'] <- gsub('R|B', 1, gsub('F|L', 0, dtt[, id]))

# --------------------------------------------------------------
# TO CONVERT BINARY TO integer
# --------------------------------------------------------------
dt_final <- dt_final %>%
  .[, int_seat_id := strtoi(binary , base = 2)]

sort(dt_final[, int_seat_id])[diff(sort(dt_final[, int_seat_id]))!=1] +1
