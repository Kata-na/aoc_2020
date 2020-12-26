rm(list = ls())
library(data.table)
library(magrittr)
library(stringr)

dtt <- read.csv('../Input/a3.txt', header = FALSE, stringsAsFactors = FALSE) %>%
  data.table() %>%
  setnames('V1', 'p')

count_tree_2 <- 0
nchar(dtt[1, p])
info <- data.table()
n_col <- length(unlist(strsplit(dtt[1, p], '')))
start_position_row <- 1
start_position_col <- 1
for(i in 1:nrow(dtt)) {
  new_position_col <- start_position_col + 3
  new_position_row <- start_position_row + 1
  if (new_position_col > 31){
    # browser()
    new_position_col <- new_position_col - 31
    # new_position_row <- new_position_row + 1
  }
  bump <- unlist(strsplit(dtt[new_position_row, p], ''))[new_position_col]
  if (bump == '#') count_tree_2 <- count_tree_2 + 1
  
  info <- info %>%
    rbind(data.table(r = new_position_row,
                     c = new_position_col,
                     symbol = bump,
                     line = paste(unlist(strsplit(dtt[new_position_row, p], '')),
                                  collapse = ' ')))
    
  
  start_position_col <- new_position_col
  start_position_row <- new_position_row
  
  if (start_position_row == nrow(dtt)) break
}
print(count_tree_2)
# count_tree 
# count_tree_1
# count_tree_2
# count_tree_3
# count_tree_4
# count_tree_5
# 
# count_tree * count_tree_1 * count_tree_2 * count_tree_4 * count_tree_5
