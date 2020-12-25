
assemble_initial_two_tiles <- function(selected_tile, 
                                       click_tiles_info,
                                       checked_tiles,
                                       all_tiles) {
  ti <- all_tiles[[selected_tile]]
  l <- nrow(ti)
  click_t <- click_tiles_info[tile == selected_tile, click_tile]
  click_t <- click_t[!click_t %in% checked_tiles][1]
  click_t_matrix <- all_tiles[[click_t[1]]]
  while (TRUE) {
    
    if (sum(ti[, l] == click_t_matrix[, 1]) == l) break
    if (sum(ti[, l] == click_t_matrix[l:1, 1]) == l) {
      click_t_matrix <- click_t_matrix[l:1, ]
      break
    }
    click_t_matrix <- rot90(click_t_matrix)
    if (sum(ti[, l] == click_t_matrix[, 1]) == l) break
    if (sum(ti[, l] == click_t_matrix[l:1, 1]) == l) {
      click_t_matrix <- click_t_matrix[l:1, ]
      break
    }
    click_t_matrix <- rot90(click_t_matrix)
    if (sum(ti[, l] == click_t_matrix[, 1]) == l) break
    if (sum(ti[, l] == click_t_matrix[l:1, 1]) == l) {
      click_t_matrix <- click_t_matrix[l:1, ]
      break
    }
    click_t_matrix <- rot90(click_t_matrix)
    if (sum(ti[, l] == click_t_matrix[, 1]) == l) break
    if (sum(ti[, l] == click_t_matrix[l:1, 1]) == l) {
      click_t_matrix <- click_t_matrix[l:1, ]
      break
    }
    ti <-  rot90(ti)
  }
  
  ti <- cbind(ti, click_t_matrix)
  checked_tiles <- c(checked_tiles, click_t)
  
  out <- list(ti = ti,
              checked_tiles = checked_tiles)
  return(out)
  
}
##------------------------------------------------------------------------------


assemble_tiles_in_row <- function (x, all_tiles, selected_tile,
                                   checked_tiles, click_tiles_info,
                                   l, corner_tiles,
                                   frame = TRUE){
  
  click_t <- click_tiles_info[tile == selected_tile, click_tile]
  if (!frame) {
    click_t <- unique(c(click_tiles_info[tile %in% click_t &
                                n == 4, tile], 
                        click_t[click_t %in% corner_tiles]))

  }
  click_t <- click_t[!click_t %in% checked_tiles]

  for (i in click_t){
    click_t_matrix <- all_tiles[[i]]
    status <- 'Not found'
    it <- 0
    while (TRUE) {
      if (sum(x[, ncol(x)] == click_t_matrix[, 1]) == l) {
        status <- 'found'
        break
      }
      if (sum(x[, ncol(x)] == click_t_matrix[l:1, 1]) == l) {
        click_t_matrix <- click_t_matrix[l:1, ]
        status <- 'found'
        break
      }
      click_t_matrix <- rot90(click_t_matrix)
      if (sum(x[, ncol(x)] == click_t_matrix[, 1]) == l) {
        status <- 'found'
        break
      }
      if (sum(x[, ncol(x)] == click_t_matrix[l:1, 1]) == l) {
        click_t_matrix <- click_t_matrix[l:1, ]
        status <- 'found'
        break
      }
      click_t_matrix <- rot90(click_t_matrix)
      if (sum(x[, ncol(x)] == click_t_matrix[, 1]) == l){
        status <- 'found'
        break
      } 
      if (sum(x[, ncol(x)] == click_t_matrix[l:1, 1]) == l) {
        click_t_matrix <- click_t_matrix[l:1, ]
        status <- 'found'
        break
      }
      click_t_matrix <- rot90(click_t_matrix)
      if (sum(x[, ncol(x)] == click_t_matrix[, 1]) == l) {
        status <- 'found'
        break
      }
      if (sum(x[, ncol(x)] == click_t_matrix[l:1, 1]) == l) {
        click_t_matrix <- click_t_matrix[l:1, ]
        status <- 'found'
        break
      }
      if (it == 1) break
      it <- it+1
      x <-  x[l:1, ]
    }
    if (status == 'found') break
  }
  ti <- cbind(x, click_t_matrix)
  
  if (!i %in% corner_tiles) {
    assemble_tiles_in_row(x = ti, all_tiles, selected_tile = i,
                          checked_tiles = c(checked_tiles, selected_tile),
                          click_tiles_info = click_tiles_info,
                          l = l, corner_tiles = corner_tiles, frame = frame)
  } else {
    out <- list(x = ti, 
              checked_tiles = c(checked_tiles, selected_tile, i))
    return(out)
  }
  
}
