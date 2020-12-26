library(data.table)
library(magrittr)

dtt <- fread('../Input/a1.txt')

n <- length(dtt[['V1']])
for (i in 1:n-1) {
  x <- dtt[i, V1]
  
  check <- x + dtt[(i+1):n, V1] == 2020
  if (any(check)) {
    y <- dtt[(i+1):n, V1][check]
    
    final_list <- c(x, y)
    break
  }
}

out <- x * y
##------------------------------------------------------------------------------

for (i in 1:(n-2)) {
  for (j in (i+1):(n-1)) {
    x <- dtt[i, V1]
    y <- dtt[j, V1]
    check <- x + y + dtt[(j+1):n, V1] == 2020
    if (any(check)) {
      z <- dtt[(j+1):n, V1][check]
      
      final_list2 <- c(x, y, z)
      break
    }
  }
  
  if (any(check)) break
}
x + y + z

final_list2

x * y * z
