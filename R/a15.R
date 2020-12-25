rm(list = ls())
library(stringr)

game_of_nums <- function(inp, last_n) {
  inp <- as.integer(strsplit(inp, ',')[[1]])
  initial_num <- inp[length(inp)]
  
  last_mentioned_log <- rep(0, last_n)
  last_mentioned_log[setdiff(inp, initial_num)+1] <- 1:(length(inp)-1)
  
  for (it in length(inp):(last_n - 1)){
    if (last_mentioned_log[initial_num + 1] == 0) {
      num <- 0
    } else num <- it  - last_mentioned_log[initial_num+1]
    last_mentioned_log[initial_num+1] <- it
    initial_num <- num
    it = it + 1
  }
  return(num)
}

ans <- game_of_nums('16,1,0,18,12,14,19', 2020)
print(ans)
t1 <- Sys.time()
ans <- game_of_nums('16,1,0,18,12,14,19', 30000000)
print(ans)
print(Sys.time() - t1)
