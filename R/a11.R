rm(list = ls())
library(data.table)
library(magrittr)
library(stringr)
source('a11_func.R')
dtt <- fread('../Input/a11.csv', sep = '')
dtt <- dtt %>%
  .[, r := .I] %>%
  .[, list(input = unlist(strsplit(input, ''))),
    by = r] %>%
  .[, n := seq_len(.N), by = r] %>%
  data.table::dcast(n~r, value.var = 'input')


m.init <- as.matrix(transpose(dtt[, -'n']))

seat_check <- function(check_empty = TRUE, m, adjec_occupied_seat_thresh = 4) {
  ##############################################################################
  ## ---------------------------------------------------------------------------
  ## --- CKECK MATRIX FOR EMPTY/OCUPIED SEATS ---
  ##  check_empty - boolean, if TRUE Empty seat rules will be checked; 
  ##  FALSE Occupied
  ##  m - matrix
  ## ---------------------------------------------------------------------------
  ##############################################################################
  
  if (check_empty) {
    check_for <- c('L', '.')
    seat_status <- matrix(m == 'L', nrow = nrow(m), ncol = ncol(m))
    default_frame <- TRUE
    
  } else {
    check_for <- c( '#')
    seat_status <- matrix(m == '#', nrow = nrow(m), ncol = ncol(m))
    default_frame <- FALSE
    
  }

  l <- check_adjacent_seats(check_for = check_for, m = m, default_frame = default_frame)
  
  if(check_empty) {
    check_seat <- seat_status & l$down_seat & l$up_seat &
      l$left_seat & l$right_seat &
      l$diag_down_left & l$diag_down_right &
      l$diag_up_left & l$diag_up_right
  } else {
    check_seat <- seat_status &
      (l$down_seat + l$up_seat +
         l$left_seat + l$right_seat +
         l$diag_down_left + l$diag_down_right +
         l$diag_up_left + l$diag_up_right) >= adjec_occupied_seat_thresh
  }

  return(check_seat)

}

m <- m.init
it <- 0
while (TRUE) {
  it <- it + 1
  if(it %% 2 == 1) {
    check_empty <- TRUE
  } else check_empty <- FALSE

  check_seats <- seat_check(check_empty = check_empty, m = m)

  if (length(m[check_seats]) == 0) {
    break
  }
  if(it %% 2 == 1) {
    m[check_seats] <- '#'
  } else   m[check_seats] <- 'L'
}

print(sum(m == '#'))


################################################################################
## -----------------------------------------------------------------------------
## --- SECOND PART ---
## -----------------------------------------------------------------------------
################################################################################
# m <- m.init
# 
# seat_check2 <- 
#   function(check_empty = TRUE, m, fl, adjec_occupied_seat_thresh = 5) {
#   if (check_empty) {
#     check_for <- c('L')
#     seat_status <- matrix(m == 'L', nrow = nrow(m), ncol = ncol(m))
#     default_frame <- TRUE
#     
#   } else {
#     check_for <- c( '#')
#     seat_status <- matrix(m == '#', nrow = nrow(m), ncol = ncol(m))
#     default_frame <- FALSE
#     
#   }
#   # l <- check_adjacent_seats(check_for = '.', m = m, default_frame=FALSE)
#   ll <- check_adjacent_seats(check_for = check_for, m = m, default_frame=default_frame)
# 
#   ll$up_seat[1, ] <- TRUE
#   up_seat <- 
#     check_seat_to_up_down(m_bool = ll$up_seat, m_bool_empty= fl$up_seat, direction ='up',
#                           padding_val = default_frame)
#   down_seat <- 
#     check_seat_to_up_down(m_bool = ll$down_seat, m_bool_empty= fl$down_seat, direction ='down',
#                           padding_val = default_frame)
#   
#   
#   right_seat <- 
#     check_seat_to_right_left(m_bool = ll$right_seat, m_bool_empty= fl$right_seat, direction ='right',
#                              padding_val = default_frame)
#   left_seat <- 
#     check_seat_to_right_left(m_bool = ll$left_seat, m_bool_empty= fl$left_seat, direction ='left',
#                              padding_val = default_frame)
#   
#   
#   diag_down_left <-
#     check_seat_diag(m_bool = ll$diag_down_left,
#                        m_bool_empty=fl$diag_down_left, direction ='down_left',
#                        padding_val = default_frame)
#   diag_down_right <-
#     check_seat_diag(m_bool = ll$diag_down_right,
#                        m_bool_empty=fl$diag_down_right, direction ='down_right',
#                        padding_val = default_frame)
# 
#   diag_up_left <-
#     check_seat_diag(m_bool = ll$diag_up_left,
#                        m_bool_empty=fl$diag_up_left, direction ='up_left',
#                        padding_val = default_frame)
#   diag_up_right <-
#     check_seat_diag(m_bool = ll$diag_up_right,
#                        m_bool_empty=fl$diag_up_right, direction ='up_right',
#                        padding_val = default_frame)
#   
#   if(check_empty) {
#     check_seat <- seat_status & down_seat & up_seat &
#       left_seat & right_seat &
#       diag_down_left & diag_down_right &
#       diag_up_left & diag_up_right
#   } else {
#     check_seat <- seat_status &
#       (down_seat + up_seat +
#          left_seat + right_seat +
#          diag_down_left + diag_down_right +
#          diag_up_left + diag_up_right
#        ) >= adjec_occupied_seat_thresh
#   }
# }
# 
# 
# m <- m.init
# it <- 0
# fl <- check_adjacent_seats(check_for = '.', m = m, default_frame=FALSE)
# while (TRUE) {
#   it <- it + 1
#   if(it %% 2 == 1) {
#     check_empty <- TRUE
#   } else check_empty <- FALSE
#   print(it)
#   check_seats <- seat_check2(check_empty = check_empty, m = m, fl = fl)
#   
#   if (length(m[check_seats]) == 0) {
#     break
#   }
#   if(it %% 2 == 1) {
#     m[check_seats] <- '#'
#   } else   m[check_seats] <- 'L'
# }
# 
# sum(m == '#')
# 
# nrow(m)
# ncol(m)
# row(m) - col(m)
# row(m) + col(m)
# 
# m <- m.init
# fl <- m == '.'
# floor_indeces <- which(fl, arr.ind=TRUE) %>%
#   data.table() %>%
#   .[, bind := 1]
# 
# non_floor_indeces <- which(!fl, arr.ind=TRUE)%>%
#   data.table() %>%
#   .[order(row, col)] %>%
#   .[, bind := 1] %>%
#   setnames(c('row', 'col'), c('row_nf', 'col_nf')) %>%
#   merge(floor_indeces, by = 'bind',
#         allow.cartesian = TRUE) %>%
#   .[, up := col == col_nf & row_nf < row] %>%
#   .[, down := col == col_nf & row_nf > row] %>%
#   .[, left := row == row_nf & col_nf < col] %>%
#   .[, right := row == row_nf & col_nf > col] %>%
#   .[, diag_up_right := ((row - row_nf) == (col - col_nf) &
#                           row >= row_nf & col_nf >= col)] %>%
#   .[, diag_up_left := ((row - row_nf) == (col - col_nf) &
#                          row >= row_nf & col >= col_nf)] %>%
#   .[, diag_down_right := ((row_nf - row) == (col_nf - col) &
#                             row_nf >= row & col_nf >= col)] %>%
#   .[, diag_down_left := ((row_nf - row) == (col_nf - col) &
#                           row_nf >= row & col >= col_nf)] %>%
#   .[(up | down | left | right | diag_up_right | diag_up_left |
#        diag_down_right | diag_down_left)] %>%
#   .[, -'bind']
# 
# up <- copy(non_floor_indeces) %>%
#   .[(up)] %>%
#   .[order(abs(row - row_nf))] %>%
#   .[, .SD[1], by = .(col, row)]  %>%
#   .[, .(col, row, col_nf, row_nf)] %>%
#   .[, direction := 'up']
# 
# down <- copy(non_floor_indeces) %>%
#   .[(down)] %>%
#   .[order(abs(row - row_nf))] %>%
#   .[, .SD[1], by = .(col, row)]  %>%
#   .[, .(col, row, col_nf, row_nf)] %>%
#   .[, direction := 'down']
# 
# left <- copy(non_floor_indeces) %>%
#   .[(left)] %>%
#   .[order(abs(col - col_nf))] %>%
#   .[, .SD[1], by = .(col, row)]  %>%
#   .[, .(col, row, col_nf, row_nf)] %>%
#   .[, direction := 'left']
# 
# 
# right <- copy(non_floor_indeces) %>%
#   .[(right)] %>%
#   .[order(abs(col - col_nf))] %>%
#   .[, .SD[1], by = .(col, row)]  %>%
#   .[, .(col, row, col_nf, row_nf)] %>%
#   .[, direction := 'right']
# 
# diag_up_right <- copy(non_floor_indeces) %>%
#   .[(diag_up_right)] %>%
#   .[order(abs(row_nf - row), abs(col_nf -col))] %>%
#   .[, .SD[1], by = .(col, row)]  %>%
#   .[, .(col, row, col_nf, row_nf)] %>%
#   .[, direction := 'diag_up_right']
# 
# diag_up_left <- copy(non_floor_indeces) %>%
#   .[(diag_up_left)] %>%
#   .[order(abs(row_nf - row), abs(col_nf -col))] %>%
#   .[, .SD[1], by = .(col, row)]  %>%
#   .[, .(col, row, col_nf, row_nf)] %>%
#   .[, direction := 'diag_up_left']
# 
# diag_down_right <- copy(non_floor_indeces) %>%
#   .[(diag_down_right)] %>%
#   .[order(abs(row_nf - row), abs(col_nf -col))] %>%
#   .[, .SD[1], by = .(col, row)]  %>%
#   .[, .(col, row, col_nf, row_nf)] %>%
#   .[, direction := 'diag_down_right']
# 
# diag_down_left <- copy(non_floor_indeces) %>%
#   .[(diag_down_left)] %>%
#   .[order(abs(row_nf - row), abs(col_nf -col))] %>%
#   .[, .SD[1], by = .(col, row)]  %>%
#   .[, .(col, row, col_nf, row_nf)] %>%
#   .[, direction := 'diag_down_left']


