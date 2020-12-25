rm(list = ls())
t1 <- Sys.time()
library(data.table)
library(magrittr)

decks  <- read.csv('a22.txt', sep = ';', header = FALSE, stringsAsFactors = FALSE)
id <- grep('Player', decks$V1)
player_1 <- as.integer(decks[(id[1]+1):(id[2]-1), 'V1'])
player_2 <- as.integer(decks[(id[2]+1):nrow(decks), 'V1'])

# --------------------------------------------------------------
# PART 1
# --------------------------------------------------------------

stop_loop <- FALSE
while (!stop_loop) {
  deck_len_1 <- length(player_1)
  deck_len_2 <- length(player_2)
  
  if (deck_len_1 > deck_len_2) {
    reminder_of_deck <- player_1[(deck_len_2+1):deck_len_1]
    player_1 <- player_1[1:deck_len_2]
    
  } else if (deck_len_1 != deck_len_2) {
    reminder_of_deck <- player_2[(deck_len_1+1):deck_len_2]
    player_2 <- player_2[1:deck_len_1]
  }
  
  check <- player_1 > player_2
  player_1_up <- c(rbind(player_1[check], player_2[check]))
  player_2_up <- c(rbind(player_2[!check], player_1[!check]))
  
  if (deck_len_1 > deck_len_2) {
    player_1 <- c(reminder_of_deck, player_1_up)
    player_2 <- player_2_up
    
  } else if (deck_len_1 != deck_len_2) {
    player_1 <- player_1_up
    player_2 <- c(reminder_of_deck, player_2_up)
    
  } else {
    player_1 <- player_1_up
    player_2 <- player_2_up
  }
  
  if (length(player_1) == 0 | length(player_2) == 0) stop_loop <- TRUE
}

if (length(player_1) != 0) {
  winner_deck <- player_1
} else   winner_deck <- player_2

ans <- sum(length(winner_deck):1 * winner_deck)
print(ans)
# print(Sys.time() - t1)


# --------------------------------------------------------------
# PART 2/Recursive Combat
# --------------------------------------------------------------
t1 <- Sys.time()

player_1 <- as.integer(decks[(id[1]+1):(id[2]-1), 'V1'])
player_2 <- as.integer(decks[(id[2]+1):nrow(decks), 'V1'])

recursive_combat_game <- function(player_1, player_2){
  
  seen_configurations1 <- paste0('Player1: ', paste(player_1, collapse = ','))
  seen_configurations2 <- paste0('Player2: ', paste(player_2, collapse = ','))
  stop_loop <- FALSE
  while (!stop_loop) {
    if (player_1[1] <= (length(player_1) - 1) & player_2[1] <= (length(player_2) - 1)) {
      # print('Recursive Combat')
      game_out <- recursive_combat_game(player_1 = player_1[2:(player_1[1]+1)],
                                        player_2 = player_2[2:(player_2[1]+1)])
      if (game_out$winner == 'Player1') {
        player_1 <- c(player_1[-1], player_1[1], player_2[1])
        player_2 <- player_2[-1]
      } else {
        player_2 <- c(player_2[-1], player_2[1], player_1[1])
        player_1 <- player_1[-1]
      }
    } else {
      if (player_1[1] > player_2[1]) {
        player_1 <- c(player_1[-1], player_1[1], player_2[1])
        player_2 <- player_2[-1]
      } else {
        player_2 <- c(player_2[-1], player_2[1], player_1[1])
        player_1 <- player_1[-1]
      }
    }
 
    round_configuration1 <- paste0('Player1: ', paste(player_1, collapse = ',')) 
    round_configuration2 <- paste0('Player2: ', paste(player_2, collapse = ','))
    if (length(player_1) == 0 ) {
      stop_loop <- TRUE
      # print('WINNER: Player2')
      return(list(winner = 'Player2',  deck = player_2))
      
    } else if (round_configuration1 %in% seen_configurations1 |
               round_configuration2 %in% seen_configurations2 |
               length(player_2) == 0) {
      stop_loop <- TRUE
      # print('WINNER: Player1')
      return(list(winner = 'Player1',  deck = player_1))
    }
    seen_configurations1 <- c(seen_configurations1, round_configuration1)
    seen_configurations2 <- c(seen_configurations2, round_configuration2)
    
  }
}
out <- recursive_combat_game(player_1, player_2)
ans <- sum(length(out$deck):1 * out$deck)
print(ans)
print(Sys.time() - t1)
#too high 32125