rm(list = ls())
t1 <- Sys.time()

library(data.table)
library(magrittr)
library(stringr)
source('a20_func.R')
import::from('pracma', 'rot90')
import::from('dplyr', 'last')


tiles <- read.csv('a20.txt', header = FALSE, stringsAsFactors = FALSE)
tiles <- tiles$V1

id <- which(grepl('Tile', tiles))
l  <- length((id[1] + 1):(id[1 + 1]-1))

# --------------------------------------------------------------
# Preparing inputs
# --------------------------------------------------------------
all_tiles <- list()
for (x in 1:length(id)) {
  inp <- tiles[(id[x] + 1):(id[x] + l)] %>% 
    gsub('\\.', 0, .) %>%
    gsub('#', 1, .) %>% 
    strsplit(., '') %>%
    unlist() %>% as.integer()%>%
    matrix(nrow = l, byrow = TRUE)
  
  tile_no <- gsub('Tile |:', '', tiles[id[x]])
  all_tiles[[tile_no]] <- inp

  ##### Saving only edges of each tile ######
  edges <- inp[, c(1,l)] %>%
    cbind( inp[c(1, l),] %>% t())
  colnames(edges) <- paste0(tile_no, '_', 1:4)
  
  if (x == 1) {
    all_edges         <- edges
  } else all_edges    <- cbind(all_edges, edges)
  
}
rm(inp)

# --------------------------------------------------------------
# PART 1
# --------------------------------------------------------------
### Gathering info about each tile clicking (what tiles click together) ####
click_tile_info <- data.table()
all_names <- colnames(all_edges)
for (tn in names(all_tiles)) {
  tside <- all_edges[, paste0(tn, '_', 1:4)]
  other_tiles <- all_names[!all_names %in% paste0(tn, '_', 1:4)]
  
  for (s in 1:4) { #Checking each of four sides
  ##### CHECKING SIDES ######
    if (any(colSums(tside[, s] == all_edges[, other_tiles]) == l)) {
      click <- which(colSums(tside[, s] == all_edges[, other_tiles]) == l)
      click_tile_info <- click_tile_info %>%
        rbind(data.table(tile = tn, click_tile = gsub('_.$', '', names(click)),
                         side = paste(tside[, s], collapse = '')))
    } 
    if (any(colSums(rev(tside[, s]) == all_edges[, other_tiles]) == l)) {
      click <- which(colSums(rev(tside[, s]) == all_edges[, other_tiles]) == l)
      click_tile_info <- click_tile_info %>%
        rbind(data.table(tile = tn, click_tile = gsub('_.$', '', names(click)),
                         side = paste(rev(tside[, s]), collapse = '')))
      
    } 
  } #End tile's 4 sides check
}# End of check for all tiles
rm(other_tiles, tside)
click_tile_info[, n := .N, by = .(tile)] 
##### Corner tiles should click only with two other tiles ######
corner_tiles <- unique(click_tile_info[n == 2, tile])

ans <- prod(as.integer(corner_tiles))
print(format(ans, scientific = FALSE))

# --------------------------------------------------------------
# PART 2
# IMAGE ASSEMBLING in THRRE PARTS, I think it should be possible 
# to asseble in one step/round (maybe two)
# --------------------------------------------------------------
##### Assembling frame/Edges ######

stop_loop <- FALSE
select_tile <- corner_tiles[1] #initializing from random corner
checked <- c()
assembled_edges <- list()
for (j in 1:4) {#LOOPING throuth all corners and creating each edge (from corner to corner)
  out <- assemble_initial_two_tiles(selected_tile = select_tile,
                                    click_tiles_info = click_tile_info,
                                    checked_tiles = checked,
                                    all_tiles = all_tiles)
  
  checked <- out$checked_tiles
  ##----------------------------------------------------------------------------
  x <- assemble_tiles_in_row(x = out$ti, all_tiles = all_tiles, 
                             selected_tile = last(checked),
                             checked_tiles = checked, 
                             click_tiles_info = click_tile_info,
                             l = l, corner_tiles = corner_tiles)
 
  checked <- unique(c(checked, x$checked_tiles))
  select_tile <- checked[length(checked)]
  assembled_edges[[j]] <- x$x

}

##### Assembling inners/Middle of the image BY ROW ######
frame_tiles <- c(checked[length(checked)], checked)

tt <- which(frame_tiles %in% corner_tiles)
top <- frame_tiles[(tt[1]+1):(tt[2]-1)]
bottom <- frame_tiles[(tt[3]+1):(tt[4]-1)]
inner_tiles <- list()
print('***************')
for (st in top) {
  out <- assemble_initial_two_tiles(selected_tile = st, 
                                    click_tiles_info = click_tile_info,
                                    checked_tiles = checked,
                                    all_tiles = all_tiles)
  checked <- out$checked_tiles
  ##----------------------------------------------------------------------------
  x <- assemble_tiles_in_row(x = out$ti,all_tiles = all_tiles, 
                             selected_tile = last(checked),
                             checked_tiles = setdiff(checked, bottom), 
                             click_tiles_info = click_tile_info,
                             l = l, corner_tiles = bottom,
                             frame = FALSE)
  inner_tiles[[st]] <- x$x
  checked <- unique(c(checked, x$checked_tiles))
}

##### Pasting together EDGES/Frame and MIDDLE ROWS of the image ######
image_full <- assembled_edges[[4]] %>% t()
nr <- dim(image_full)
edge_last <- assembled_edges[[2]] %>% t()

for (st in c(top, 'last_edge')){
  if (st != 'last_edge') {
    inner <- inner_tiles[[st]] %>% t()
  } else  inner  <- edge_last
  stop_loop <- FALSE
  l2 <- ncol(image_full)
  while (!stop_loop){
    xi <- NULL
    if (all(image_full[, l2] == inner[, 1])) {
      xi <- cbind(image_full, inner)
    } else if (all(image_full[, l2] == rev(inner[, 1]))) {
      xi <- cbind(image_full, inner[nr[[1]]:1, ])
    } else if (all(image_full[, l2] == inner[, l])) {
      xi <- cbind(image_full, inner[, (nr[[2]]):1])
    } else if (all(image_full[, l2] == rev(inner[, l]))) {
      xi <- cbind(image_full, inner[nr[[1]]:1, (nr[[2]]):1])
    }
    
    if (is.null(xi)) {
      image_full <- image_full[, nr[[2]]:1]
    } else  {
      image_full <- xi
      stop_loop <- TRUE
    }
  }
}

##------------------------------------------------------------------------------
#Dropping edges of each tile
drop <- c(seq(0, nr[1], by = 10), seq(1, nr[1], by = 10))
image_full <- image_full[setdiff(1:nrow(image_full), drop),
                         setdiff(1:nrow(image_full), drop)]

dim(image_full)
rm(inner, edges, drop, checked, edge_last,
   out, xi, tile_no, l, l2, all_edges)
# --------------------------------------------------------------
# FINDING monsterS/MONSTERS
# --------------------------------------------------------------
monster <- read.csv('a20_monster.txt', header = FALSE, stringsAsFactors = FALSE)

nrow_monster        <- nrow(monster)
monster             <- unlist(strsplit(monster$V1, ''))
monster             <- as.integer(gsub(' ', 2, gsub('#', 1, monster)))
monster             <- matrix(monster, nrow = nrow_monster,
                              byrow = TRUE)
dim_monster         <- dim(monster)

img                 <- image_full
dim_img             <- dim(img)
expected_sum_by_row <- rowSums(monster == 1)
monster_count       <- 0
for (z in 1:2) { #ROTATION
  for (x in 1:4) {#FLIPPING
    img_tmp <- img
    ##### Sliding through image ######
    for (i in 1:(dim_img[[2]] - dim_monster[[2]] + 1)) { 
      for (j in 1:(dim_img[[1]] - dim_monster[[1]] + 1) ) {
        if (all(rowSums(img[j:(j+dim_monster[[1]]-1),
                            i:(i+dim_monster[[2]]-1)] == monster) == expected_sum_by_row)) {
          # browser()
          img_tmp[j:(j+dim_monster[[1]]-1),
                  i:(i+dim_monster[[2]]-1)][img[j:(j+dim_monster[[1]]-1),
                      i:(i+dim_monster[[2]]-1)] == monster] <- 0.5
          monster_count <- monster_count + 1}
      }#END OF ROW SLIDE
    }#END OF COLUMN SLIDE
    
    if (monster_count != 0) break
    if (x %in% c(1, 3)) {
      img <- img[nrow(img):1, ]
    } else if (x == 2) img <- img[, ncol(img):1]
  

  }#END OF FLIP LOOP
  
  if (monster_count != 0) break
  img <- rot90(image_full)
}#END OF ROTATION LOOP

ans <- sum(colSums(image_full == 1)) - monster_count * sum(expected_sum_by_row)
print(ans)

print(Sys.time() - t1)
img_tmp[img_tmp %in% c(1)] <- 0
image(img_tmp, col = grey(seq(0, 1, length = 256)))
