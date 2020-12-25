#all takes about ~1 sec
rm(list = ls())
t1 <- Sys.time()
options(scipen = 999)

library(data.table)
library(stringr)
import::from('dplyr', 'lead')
import::from('magrittr', '%>%')
source('a14_func.R')

bit_base <- 36
dtt <- fread('../Input/a14.csv', header = FALSE, col.names = c('input', 'V2', 'value')) %>%
  .[, .(input, value)]

############################### PREPARING INPUTS ###############################
dtt <- dtt[, r := .I] #Adding row number

# Adding appropriate mask to each memory step
mask <- copy(dtt[input == 'mask']) %>%
  .[, l := lead(r) - 1] %>%
  .[is.na(l), l := nrow(dtt)] %>%
  .[, list(r = r:l), by = .(value)] %>%
  setnames('value', 'mask')

dtt <- dtt[!grepl('mask', input)] %>%
  merge(mask, all.x = TRUE, by = 'r') %>%
  .[, memory_address := as.integer(gsub('^mem\\[(.*?)\\]', '\\1', input))] %>%
  .[, value := as.integer(value)]

#################################### PART 1 ####################################
# Only last operation on memory is important
dtt_f <- copy(dtt)[order(-r)] %>%
  .[, .SD[1], by = memory_address]

m <- mask_to_matrix(dtt_f[['mask']]) #MASK
b <- decimal_vec_to_bit_matrix(dtt_f[['value']], bit_base = bit_base) #BINARY

b[!is.na(m)] <- m[!is.na(m)] 
ans <- bit_matrix_to_decimals(b) %>% sum()
print(glue::glue('Answer for part 1: {ans}'))
rm(dtt_f, mask)
print(Sys.time() - t1)

#################################### PART 2 ####################################
# Creating grid of 0-1 combinations, based of how many X values are in each mask
n_x       <- unique(str_count(dtt[['mask']], 'X'))
comb_list <- lapply(unique(n_x), function(n){
  return(as.matrix(expand.grid(rep(list(0:1), n))))
})
names(comb_list) <- paste0(n_x)
##------------------------------------------------------------------------------
bit_matrix  <- decimal_vec_to_bit_matrix(dtt[['memory_address']], bit_base = bit_base)
mask_matrix <- mask_to_matrix(dtt[['mask']])

bit_matrix[mask_matrix == 1 | is.na(mask_matrix)] <- 
  mask_matrix[mask_matrix == 1 | is.na(mask_matrix)]

##### LOOP THROUG ALL MEMORY ASSIGMENTS ######
memory <- list()
for (i in 1:nrow(dtt)) {
  bm <- bit_matrix[i, ]
  
  ##### Creating matrix of all possible binary memory addresses ######
  comb <- comb_list[[glue::glue('{sum(is.na(bm))}')]] 
  bm   <- do.call("rbind", rep(list(bm), nrow(comb)))
  bm[is.na(bm)] <- comb
  ##### Retrieving decimal values of all possible addresses  ######
  all_addresses <- as.vector(bit_matrix_to_decimals(bm)) #not really necessary
  
  ##### SAVING OUTPUT ######
  memory[[i]] <- data.table(memory_address =  all_addresses, #apply(bm,1,paste,collapse = '') #takes longer than converting to decimals
                            value = dtt[i, value], iteration = i)
}

##### Leaving unique values per address (only last saved should be left) ######
memory <- rbindlist(memory) %>%
  .[order(-iteration)] %>%
  .[, .SD[1], by = memory_address]

print(glue::glue('Answer for part 2: {format(sum(memory[["value"]]), scientific = FALSE)}'))
print(Sys.time() - t1)
