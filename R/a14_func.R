# Function converts vector of decimals to binary matrix
# Rows corresponds to decimals order in vector and cols represts each bit
# Each row in created matrix will represents decimal as binary number
decimal_vec_to_bit_matrix <- function(dec_vector, bit_base) {
   bit_matrix <- floor(matrix(1/2^(0:(bit_base - 1)), ncol = 1) %*% 
                         matrix(dec_vector, nrow = 1)) %% 2 
   bit_matrix <- bit_matrix[nrow(bit_matrix):1,] %>% t()
   return(bit_matrix)
}

# M must be binary values in matrix form. Each colums should represent one bit
# Each row should represent a number/decimal in binary system.
#' Returns Matrix [nrow(m)x1] 
bit_matrix_to_decimals <- function(m) {
  return(m %*% 2^((ncol(m)-1):0))
}

##### Cleans vector of MASKS and converts them to a matrix ######
mask_to_matrix <- function(mask_v) {
  m <- matrix(unlist(strsplit(mask_v, '')), 
              nrow = nchar(mask_v[1]), ncol = length(mask_v)) %>% t()
  m[m =='X'] <- NA
  storage.mode(m) <- "integer" #converting string to integer
  return(m)
}

# decimal_to_bit <- function(dec) {
#   paste0('0000', paste(as.integer(rev(intToBits(value))), collapse = ''))
# }
# 
# ##### Function to convert unique decimal to binary ######
# dec_to_bit <- function(dec, bit_base = 36) {
#   return(rev(dec %/% 2^(0:(bit_base-1))%%2))
# }