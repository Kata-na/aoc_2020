rm(list = ls())

card_pub_key <- 11239946
door_pub_key <- 10464955

find_encryption_key <- function(start_num, subject_number,
                                divisor, target = Inf, iterations = Inf) {
  stop_loop <- FALSE
  it <- 0
  while (!stop_loop) {
    start_num <- (start_num * subject_number) %% divisor
    it <- it + 1
    if (start_num == target | it == iterations) stop_loop <- TRUE
  }
  return(list(it = it, out_num = start_num))
}

card_loop      <- find_encryption_key(start_num = 1, subject_number = 7, 
                                      target = card_pub_key, divisor = 20201227)

encryption_key <- find_encryption_key(start_num = 1, subject_number = door_pub_key,
                                      target = Inf, divisor = 20201227,
                                      iterations = card_loop$it)
print(encryption_key$out_num)
