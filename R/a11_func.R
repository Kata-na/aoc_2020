
check_adjacent_seats <- function(check_for, m,
                                 default_frame = TRUE){
  
  down_seat <- rbind(matrix(m[2:nrow(m), ] %in% check_for, 
                            nrow = nrow(m) - 1, ncol = ncol(m)),
                     rep(default_frame, ncol(m)))
  up_seat <- rbind(rep(default_frame, ncol(m)),
                   matrix(m[1:nrow(m) -1, ] %in% check_for, 
                          nrow = nrow(m)-1, ncol = ncol(m)))
  
  right_seat <- cbind(matrix(m[, 2:ncol(m)] %in% check_for, 
                             nrow = nrow(m), ncol = ncol(m)-1), 
                      rep(default_frame, nrow(m)))
  left_seat <- cbind(rep(default_frame, nrow(m)),
                     matrix(m[, 1:ncol(m)-1] %in% check_for, 
                            nrow = nrow(m), ncol = ncol(m)-1))
  # --------------------------------------------------------------
  # DIAGONALS
  # --------------------------------------------------------------
  diag_down_right <- 
    matrix(m[2:nrow(m), 2:ncol(m)] %in% check_for,
           nrow = nrow(m)-1, ncol = ncol(m)-1)
  
  diag_down_right <- rbind(
    cbind(diag_down_right, rep(default_frame, nrow(m)-1)),
    rep(default_frame, ncol(m)))
  ##------------------------------------------------------------------------------
  diag_down_left <- 
    matrix(m[2:nrow(m), 1:ncol(m)-1] %in% check_for,
           nrow = nrow(m)-1, ncol = ncol(m)-1)
  
  diag_down_left <- rbind(
    cbind(rep(default_frame, nrow(m)-1), diag_down_left),
    rep(default_frame, ncol(m)))
  
  ##------------------------------------------------------------------------------
  diag_up_right <- 
    matrix(m[1:nrow(m)-1, 2:ncol(m)] %in% check_for,
           nrow = nrow(m)-1, ncol = ncol(m)-1)
  
  diag_up_right <- rbind(rep(default_frame, ncol(m)),
                         cbind(diag_up_right, rep(default_frame, nrow(m)-1)))
  ##------------------------------------------------------------------------------
  diag_up_left <- 
    matrix(m[1:nrow(m)-1, 1:ncol(m)-1] %in% check_for,
           nrow = nrow(m)-1, ncol = ncol(m)-1)
  
  diag_up_left <- rbind(rep(default_frame, ncol(m)),
                        cbind(rep(default_frame, nrow(m)-1), diag_up_left))
  l <- list(down_seat = down_seat, up_seat= up_seat,
            left_seat = left_seat, right_seat=right_seat,
            diag_down_left = diag_down_left, diag_down_right= diag_down_right,
            diag_up_left = diag_up_left, diag_up_right = diag_up_right)
  return(l)
}

##------------------------------------------------------------------------------

check_seat_to_right_left <- function(m_bool, m_bool_empty, 
                                     direction =c('left', 'right'),
                                     padding_val = TRUE) {
  floor_indeces <- which(m_bool_empty, arr.ind=TRUE) %>%
    data.table() %>%
    setnames('col', 'col_floor')
  
  non_floor_indeces <- which(!m_bool_empty, arr.ind=TRUE)%>%
    data.table() %>%
    .[, value := m_bool[!m_bool_empty]] %>%
    .[order(row, col)] %>%
    merge(floor_indeces, by = 'row',
          allow.cartesian = T)
  if (direction == 'right') {
    non_floor_indeces <- non_floor_indeces %>%
      .[col > col_floor]
  } else {
    non_floor_indeces <- non_floor_indeces %>%
      .[col < col_floor]
  }
  non_floor_indeces <- non_floor_indeces %>%
    .[order(abs(col_floor - col))] %>%
    .[, .SD[1], by = c('row', 'col_floor')] %>%
    .[, ID := paste0(row, '_', col_floor)]
  
  miss_id <- 
    setdiff(paste0(floor_indeces[['row']], '_', floor_indeces[['col_floor']]),
            non_floor_indeces$ID)
  if (length(miss_id) != 0) {
    non_floor_indeces   <- data.table(ID = miss_id,
                                      value = padding_val) %>%
      rbind(non_floor_indeces, fill = TRUE)
  }
  
  non_floor_indeces <- 
    non_floor_indeces[order(match(ID, paste0(floor_indeces[['row']], 
                                             '_', floor_indeces[['col_floor']])))]
  
  m_bool[m_bool_empty] <- non_floor_indeces[['value']]
  return(m_bool)
  
}
##------------------------------------------------------------------------------
check_seat_to_up_down <- function(m_bool, m_bool_empty, 
                                     direction =c('up', 'down'),
                                  padding_val = TRUE) {
  floor_indeces <- which(m_bool_empty, arr.ind=TRUE) %>%
    data.table() %>%
    setnames('row', 'row_floor')
  
  non_floor_indeces <- which(!m_bool_empty, arr.ind=TRUE)%>%
    data.table() %>%
    .[, value := m_bool[!m_bool_empty]] %>%
    .[order(row, col)] %>%
    merge(floor_indeces, by = 'col',
          allow.cartesian = T)
  if (direction == 'down') {
    non_floor_indeces <- non_floor_indeces %>%
      .[row > row_floor]
  } else {
    non_floor_indeces <- non_floor_indeces %>%
      .[row < row_floor]
  }
  non_floor_indeces <- non_floor_indeces %>%
    .[order(abs(row_floor - row))] %>%
    .[, .SD[1], by = c('col', 'row_floor')] %>%
    .[, ID := paste0(col, '_', row_floor)]
  
  miss_id <- 
    setdiff(paste0(floor_indeces[['col']], '_', floor_indeces[['row_floor']]),
          non_floor_indeces$ID)
  if (length(miss_id) != 0) {
    non_floor_indeces   <- data.table(ID = miss_id,
                                      value = padding_val) %>%
      rbind(non_floor_indeces, fill = TRUE)
  }
  
  non_floor_indeces <- 
    non_floor_indeces[order(match(ID, paste0(floor_indeces[['col']], 
                                             '_', floor_indeces[['row_floor']])))]
  
  m_bool[m_bool_empty] <- non_floor_indeces[['value']]
  
  return(m_bool)
  
}

check_seat_diag <- function(m_bool, m_bool_empty, 
                                  direction = c('down_right', 'up_left',
                                                'down_left', 'up_right'),
                               padding_val = TRUE) {
  floor_indeces <- which(m_bool_empty, arr.ind=TRUE) %>%
    data.table()  %>%
    .[, bind := 1]
    
  non_floor_indeces <- which(!m_bool_empty, arr.ind=TRUE)%>%
    data.table() %>%
    .[, value := m_bool[!m_bool_empty]] %>%
    .[order(row, col)] %>%
    .[, bind := 1] %>%
    setnames(c('row', 'col'), c('row_f', 'col_f')) %>%
    merge(floor_indeces, by = 'bind',
          allow.cartesian = TRUE) 
  
  if (direction == 'down_left') {
    non_floor_indeces <- non_floor_indeces %>%
      .[(row_f - row) == (col_f - col) &
          row_f >= row & col >= col_f]
  } else if (direction == 'down_right') {
    non_floor_indeces <- non_floor_indeces %>%
      .[(row_f - row) == (col_f - col) &
          row_f >= row & col_f >= col]
  } else if (direction == 'up_left') {
    non_floor_indeces <- non_floor_indeces %>%
      .[(row - row_f) == (col - col_f) &
          row >= row_f & col >= col_f]
  } else if (direction == 'up_right') {
    non_floor_indeces <- non_floor_indeces %>%
      .[(row_f - row) == (col_f - col) &
          row >= row_f & col_f >= col]
  }
  
  non_floor_indeces <- non_floor_indeces %>%
    .[order(abs(row_f - row), abs(col_f -col))] %>%
    .[, .SD[1], by = .(row, col)]%>%
    .[, ID := paste0(col, '_', row)]
  
  miss_id <- setdiff(paste0(floor_indeces[['col']],
                            '_', floor_indeces[['row']]), non_floor_indeces$ID)
  if (length(miss_id) != 0) {
    non_floor_indeces   <- data.table(ID = miss_id,
                                      value = padding_val) %>%
      rbind(non_floor_indeces, fill = TRUE)
  }
  non_floor_indeces <- 
    non_floor_indeces[order(match(ID, paste0(floor_indeces[['col']], 
                                             '_', floor_indeces[['row']])))]
  m_bool[m_bool_empty] <- non_floor_indeces[['value']]
  
  return(m_bool)
}
