rm(list = ls())
library(data.table)
library(magrittr)
library(stringr)
source('a12_func.R')

dtt <- fread('../Input/a12.csv', sep = '') %>%
  .[, c('direction', 'n') := list(gsub('(^[[:alpha:]]).*?$', '\\1', INPUT),
                                 as.numeric(gsub('^.(\\d{1,4}$)', '\\1', INPUT)))]

direction <- dtt[['direction']]
n <- dtt[['n']]
# --------------------------------------------------------------
# INFO FOR TURNS
# --------------------------------------------------------------
turns <- data.table(current_direction= 'E',
                         degree = c(90, 180, 270),
                         new_direction = c('S', 'W', 'N'),
                         sign = c(-1, -1, 1)) %>%
  rbind(data.table(current_direction= 'S',
                   degree = c(90, 180, 270),
                   new_direction = c('W', 'N', 'E'),
                   sign = c(1, -1, -1))) %>%
  rbind(data.table(current_direction= 'W',
                   degree = c(90, 180, 270),
                   new_direction = c('N', 'E', 'S'),
                   sign = c(-1, -1, 1))) %>%
  rbind(data.table(current_direction= 'N',
                   degree = c(90, 180, 270),
                   new_direction = c('E', 'S', 'W'),
                   sign = c(1, -1, -1)))  %>%
  .[, side := 'R']

turns <- copy(turns) %>%
  .[, degree := ifelse(degree == 90, 270, 
                      ifelse(degree == 270, 90, degree))] %>%
  .[order(current_direction, degree)] %>%
  .[, side := 'L'] %>%
  rbind(turns)


# --------------------------------------------------------------
# PART 1
# --------------------------------------------------------------
cdr <- 'E'
coordinates <- c(0, 0)

for (i in 1:length(n)) {
  action <-  direction[i]
 if (action  %in% c('R', 'L')) {
   cdr <- turns[current_direction == cdr & 
                degree == n[i] & side == action, new_direction]
 } else if (action  == 'F') {
   coordinates <-
     coordinates_change(action = cdr, coordinates = coordinates, size = n[i])
 } else if (action %in% c('N', 'S', 'W', 'E')) {
   coordinates <-
     coordinates_change(action = action, coordinates = coordinates, size = n[i])
 }
}
print(sum(abs(coordinates)))

# --------------------------------------------------------------
# PART 2
# --------------------------------------------------------------
cwp <- c(-10, -1) # initial waypoint coordinates
c_eswn <- c('E', 'N') # way point direction
ship_coor <- c(0, 0) # initial sheep coordinates

for (i in 1:length(n)) {
  action <-  direction[i]
  if (action %in% c('N', 'S', 'W', 'E')) {
    cwp <-
      coordinates_change(action = action, coordinates = cwp, size = n[i])
    
  } else if (action == 'F') {
    ship_coor <- cwp * n[i] + ship_coor
  } else if (action  %in% c('R', 'L')) {
    cwp <- cwp *
      c(turns[current_direction == c_eswn[1] & side == action & degree == n[i], sign],
        turns[current_direction == c_eswn[2] & side == action & degree == n[i], sign])
    
    c_eswn[1] <- turns[current_direction == c_eswn[1] & side == action &
                 degree == n[i], new_direction]
    c_eswn[2] <- turns[current_direction == c_eswn[2] & side == action &
                   degree == n[i], new_direction]
    if (n[i] != 180)  {
      # Flipping coordinates 
      cwp <- c(cwp[2], cwp[1])
      c_eswn <- c(c_eswn[2], c_eswn[1])
    }
  }
}
print(sum(abs(ship_coor)))
