rm(list = ls())
library(data.table)
library(stringr)

# inp <- 389125467
inp  <- 389125467
cups <- as.integer(strsplit(as.character(inp), '')[[1]])

current_cup  <- cups[1]
circle_cups  <- cups
for (x in 1:100) {
  i <- which(circle_cups == current_cup)
  three_cups   <- c(circle_cups, circle_cups)[(i+1):(i+3)]
  circle_cups  <- circle_cups[!circle_cups %in% three_cups]
  destination_cup <- current_cup - 1
  while (destination_cup %in% three_cups | destination_cup < min(circle_cups)) {
    destination_cup <- destination_cup - 1
    
    if (destination_cup < min(circle_cups)){
      destination_cup   <- max(circle_cups)
      break()
    }
  }
  
  ii <- which(circle_cups == destination_cup)
  if (ii == 1) {
    circle_cups <- c(three_cups[length(three_cups)], 
                    circle_cups[(ii + 1):length(circle_cups)],
                     circle_cups[1:ii], three_cups[-length(three_cups)])
    
  } else if (ii != length(circle_cups)) {
    circle_cups <- c(circle_cups[1:ii], three_cups, 
                     circle_cups[(ii + 1):length(circle_cups)])
    
  } else circle_cups <- c(circle_cups[1:ii], three_cups)

  # print(circle_cups)
  i <- which(circle_cups == current_cup)
  current_cup <- c(circle_cups, circle_cups)[i+1]
}

xi <- which(c(circle_cups, circle_cups) == 1)
ans <- paste(c(circle_cups, circle_cups)[(xi[1] +1):(xi[2] - 1)], collapse = '')
print(ans)

##------------------------------------------------------------------
## PART 2
##------------------------------------------------------------------
cups <- c(cups, 10:10^6)
current_cup  <- cups[1]
circle_cups  <- cups
for (x in 1:10000000) {
  if (x %% 10000  ==0) {
    print(x)
    print(max(circle_cups))
  }
  # print(x)
  i <- which(circle_cups == current_cup)
  three_cups   <- c(circle_cups, circle_cups)[(i+1):(i+3)]
  circle_cups  <- circle_cups[!circle_cups %in% three_cups]
  destination_cup <- current_cup - 1
  while (destination_cup %in% three_cups | destination_cup < min(circle_cups)) {
    destination_cup <- destination_cup - 1
    
    if (destination_cup < min(circle_cups)){
      # if (x <10) {
        destination_cup   <- max(circle_cups)
      # } else destination_cup <- max(circle_cups) + 1
      break()
    }
  }
  
  ii <- which(circle_cups == destination_cup)
  if (length(ii) == 0) {
    circle_cups <- c(circle_cups, destination_cup, three_cups)
  } else  if (ii == 1) {
    circle_cups <- c(three_cups[length(three_cups)], 
                     circle_cups[(ii + 1):length(circle_cups)],
                     circle_cups[1:ii], three_cups[-length(three_cups)])
    
  } else if (ii != length(circle_cups)) {
    circle_cups <- c(circle_cups[1:ii], three_cups, 
                     circle_cups[(ii + 1):length(circle_cups)])
    
  } else circle_cups <- c(circle_cups[1:ii], three_cups)
  
  # print(circle_cups)
  i <- which(circle_cups == current_cup)
  current_cup <- c(circle_cups, circle_cups)[i+1]
}

xi <- which(c(circle_cups, circle_cups) == 1)
ans <- paste(c(circle_cups, circle_cups)[(xi[1] +1):(xi[2] - 1)], collapse = '')
print(ans)