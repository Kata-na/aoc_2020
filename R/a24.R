rm(list = ls())
t1 <- Sys.time()
library(data.table)
import::from('magrittr', '%>%')
import::from('purrr', 'reduce')

inp <- read.table('../Input/a24.txt', header = FALSE, stringsAsFactors = FALSE) %>%
  data.table() %>%
  .[, input := V1]

allowed_dir <- c('e', 'se', 'sw', 'w', 'nw', 'ne')
input <- inp$input
##------------------------------------------------------------------
## PREP DATA
##------------------------------------------------------------------
### Separating directions ###
### Not the most efficient way !!!
patt <- paste(allowed_dir, collapse = '|')
stop_loop <- FALSE
it <- 1
while (!stop_loop){
  inp[, paste0('dir', it) :=  gsub(glue::glue('^({patt}).*?$'), '\\1', V1)] 
  inp[, V1 := gsub(paste0('^',.SD,'(.*?$)'), '\\1', V1),
      .SDcols = glue::glue('dir{it}'), by = input]
  if (all(inp$V1 == '')) stop_loop <- TRUE
  it <- it + 1
}
dir_matrix <- as.matrix(inp[, paste0('dir', 1:(it-1)), with = FALSE])
dir_matrix[dir_matrix==''] <- 'no_change'

# Change Matrix - describes how coordinates change depending from step direction 
coord_change <- matrix(c(1, -1, 0, -1, 1, 0, 1, 0, -1, 0, -1, 1, -1, 0, 1,
                         0, 1, -1, 0, 0, 0), ncol = 3, byrow = TRUE)
rownames(coord_change) <- c('e', 'w', 'ne', 'se', 'sw', 'nw', 'no_change')
colnames(coord_change) <- c('X', 'Y', 'Z')

##------------------------------------------------------------------
## PART 1
##------------------------------------------------------------------
final_coord <- matrix(0, nrow = nrow(dir_matrix), ncol = 3)
#Stepping through all changes. Each column of dir_matrix describes one step
for (i in 1:ncol(dir_matrix)) { #LOOPING THROUGH Columns of the direction matrix
  # cc  <- dir_matrix[, i]
  cc <- coord_change[dir_matrix[, i], ]
  final_coord <- final_coord  + cc
}
rownames(final_coord) <- NULL
black_tiles <- data.table(final_coord) %>%
  .[, n := .N, by = .(X, Y, Z)] %>%
  .[n %% 2 != 0] # Eliminating tiles which were flipped to black and then back to white 
print(nrow(black_tiles))
print(Sys.time() - t1)

##------------------------------------------------------------------
## PART 2
##------------------------------------------------------------------
t1 <- Sys.time()
black_tiles[, n := NULL]

for (it in 1:100) {
  adjacent_to_black <-rep(1,6) %x% as.matrix(black_tiles, ncol = 3, byrow = TRUE) +
    (data.table(rep(1, nrow(black_tiles)) %x% coord_change[1:6, ]) %>%
       .[order(V1, V2, V3)])
  
  # Looking for white tiles and then checking which of them appear 2 times in adjacent tiles #
  white_tiles_to_black <- adjacent_to_black[!reduce(adjacent_to_black, `paste`) %in%
                                     reduce(black_tiles, `paste`)] %>% #Finding white tiles
    .[, n := .N, by = .(V1,  V2, V3)] %>%
    .[n == 2, -'n'] #Finding white tiles which should be flipped
##------------------------------------------------------------------------------
  # Looking for black tiles which have less or equal of 2 adjacent tiles, but not 0 #
  # Those tiles will not be flipped to white side
  check_tiles <- reduce(adjacent_to_black, `paste`) %in% reduce(black_tiles, `paste`)
  black_tiles <- adjacent_to_black[check_tiles] %>%
    .[, n := .N, by = .(V1,  V2, V3)] %>%
    .[n  <= 2, -'n'] %>%
    ### Adding white tiles which where flipped to black ###
    rbind(white_tiles_to_black) %>%
    unique()
}

print(nrow(black_tiles))
print(Sys.time() - t1)
