rm(list = ls())

t1 <- Sys.time()
library(data.table)
library(magrittr)
library(stringr)
cycles  <- 6

inp <- fread('a17.txt', header = FALSE, col.names = 'input') %>%
  .[, input := gsub('#', 1, gsub('\\.', 0, input))]

inp_matrix <- 
  matrix(as.integer(unlist(strsplit(inp$input, ''))), ncol = nrow(inp)) %>% t()
inp_matrix<- stringr::str_split_fixed(inp$input, '', Inf)
storage.mode(inp_matrix) <- 'integer'

################################################################################
## -----------------------------------------------------------------------------
## --- PART 1 ---
## -----------------------------------------------------------------------------
################################################################################
# --------------------------------------------------------------
# INITIALIZING ARRAY, WHICH Will represennt 3D CUBE
# --------------------------------------------------------------
arra_dims <- c(nrow(inp_matrix) + 2 * cycles,
               ncol(inp_matrix) + 2 * cycles,
               1 + 2 * cycles)
inp_cube <- array(0, dim = arra_dims)
inp_cube[(cycles + 1):(cycles + nrow(inp_matrix)),
         (cycles + 1):(cycles + ncol(inp_matrix)),
          cycles + 1] <- inp_matrix

for (it in 1:cycles) {
  upd_cube <- inp_cube
  d <- dim(inp_cube)
  
  for (x in 1:d[1]) {
    for (y in 1:d[2]) {
      for (z in 1:d[3]) {
        target <- upd_cube[x, y, z]
        neighbours <- sum(inp_cube[x + (-1:1)[x + (-1:1) <= d[1]], 
                                   y + (-1:1)[y + (-1:1) <= d[2]],
                                   z + (-1:1)[z + (-1:1) <= d[3]]]) - target
        
        if(target == 1 && !(neighbours %in% c(2,3))){
          upd_cube[x, y, z] <- 0L
        } else if(target == 0 && neighbours == 3){
          upd_cube[x, y, z] <- 1
        }
        
      }
    }
  }
  inp_cube <- upd_cube
}
ans <- sum(inp_cube)

print(ans)
################################################################################
## -----------------------------------------------------------------------------
## --- PART 4 ---
## -----------------------------------------------------------------------------
################################################################################
# --------------------------------------------------------------
# INITIALIZING ARRAY, WHICH Will represennt 4D hypercuboid
# --------------------------------------------------------------

arra_dims <- c(nrow(inp_matrix) + 2 * cycles,
               ncol(inp_matrix) + 2 * cycles,
               1 + 2 * cycles,
               1 + 2 * cycles)
inp_hypercube <- array(0, dim = arra_dims)
inp_hypercube[(cycles + 1):(cycles + nrow(inp_matrix)),
              (cycles + 1):(cycles + ncol(inp_matrix)),
               cycles + 1,
               cycles + 1] <- inp_matrix

ind_x <- (cycles + 1):(cycles + nrow(inp_matrix))
ind_y <- (cycles + 1):(cycles + nrow(inp_matrix))
ind_z <- cycles + 1
ind_w <- cycles + 1

for (it in 1:cycles) {
  upd_cube <- inp_hypercube
  d <- dim(inp_hypercube)
  
  # Optimizing, looping only through potential region were might be chages
  ind_x <- unique(c(ind_x -1, ind_x +1))
  ind_y <- unique(c(ind_y -1, ind_y +1))
  ind_z <- unique(c(ind_z -1, ind_z, ind_z +1))
  ind_w <- unique(c(ind_w -1, ind_w, ind_w +1))
  for (x in ind_x) {
    for (y in ind_y) {
      for (z in ind_z) {
        for (w in ind_w) {
          target <- upd_cube[x, y, z, w]
          neighbours <- sum(inp_hypercube[x + (-1:1)[x + (-1:1) <= d[1]], 
                                          y + (-1:1)[y + (-1:1) <= d[2]],
                                          z + (-1:1)[z + (-1:1) <= d[3]],
                                          w + (-1:1)[w + (-1:1) <= d[4]]]) - target
          
          if(target == 1 && !(neighbours %in% c(2,3))){
            upd_cube[x, y, z, w] <- 0L
          } else if(target == 0 && neighbours == 3){
            upd_cube[x, y, z, w] <- 1
          }
        }
      }
    }
  }
  inp_hypercube <- upd_cube
}
ans <- sum(inp_hypercube)
print(ans)

print(Sys.time() - t1)