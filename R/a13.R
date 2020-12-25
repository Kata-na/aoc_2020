rm(list = ls())
library(data.table)
library(magrittr)
library(stringr)
library(pracma)
import::from('numbers', 'modinv', 'chinese')
dtt <- read.table('../Input/a13.txt', header = FALSE, stringsAsFactors = FALSE) %>%
  data.table()

# --------------------------------------------------------------
# PART 1
# --------------------------------------------------------------
time_stamp <- as.integer(dtt[1, V1])
bus_no <- unlist(strsplit(dtt[2, V1], ','))
bus_no <- as.integer(bus_no[bus_no != 'x'])

depart_after_ts <- bus_no * floor(time_stamp/bus_no) + bus_no
wait_time <- depart_after_ts - time_stamp
answer <- bus_no[which(wait_time == min(wait_time))] * 
  wait_time[which(wait_time == min(wait_time))]
print(answer)
# --------------------------------------------------------------
# PART 2
# --------------------------------------------------------------
bus_no <- unlist(strsplit(dtt[2, V1], ','))

dtsr <- 0:(length(bus_no)-1) #departure time stamp rule relative to first bus
dtsr <- dtsr[bus_no !='x']
bus_no <- as.integer(bus_no[bus_no != 'x'])
remainder <- (bus_no - dtsr%%bus_no)%%bus_no

# --------------------------------------------------------------
# Answer for FIRST and second buses combinations
# --------------------------------------------------------------
i <- 1:1000
while (TRUE) {
  ans <- i[i%%bus_no[1] == remainder[1] &
      i%%bus_no[2] == remainder[2]]
  if (length(ans) != 0) break
  i <- i + 1000
}
lcm <- Lcm(bus_no[1], bus_no[2])

# --------------------------------------------------------------
# Looping through remaining buses
# --------------------------------------------------------------

for (j in 3:length(bus_no)) {
  steps <- 1:1000
  # --------------------------------------------------------------
  # Checking only those cases where previous rules applies
  # --------------------------------------------------------------
  while (TRUE) { #loops goes until at least one option which would comply with rule is found
    opt <- ans + lcm * steps #option for checking
    opt <- min(opt[opt%%bus_no[j] == remainder[j]]) #selecting uniwue best option
    if (length(opt) != 0) break
    steps <- steps + 1000
  }
  ans <-  opt
  lcm <- Lcm(lcm, bus_no[j])
}
options(scipen = 999)
ans  
all(ans %% bus_no == remainder) #Check if answer aligned with rules, i.e. remainders are correct
print(ans)


#####################################################################
# --------------------------------------------------------------
# Solving Part 2 using Chinese Remainder Theorem CRT
# --------------------------------------------------------------
#####################################################################
remainder <- (bus_no - dtsr%%bus_no)%%bus_no
Ni <- prod(bus_no)/bus_no

# --------------------------------------------------------------
# Xi <- Mod inverses Inverses
# --------------------------------------------------------------
Xi <- rep(0, length(Ni))
multi <- 1

while (any(Xi == 0)) { #Maybe there is better way of finding Xi
  Xi[which(((Ni%%bus_no) * multi)%%bus_no == 1 & Xi == 0)] <- multi
  multi <- multi +1
}

# --------------------------------------------------------------
# Calculating Xi using modinv function. mod_invrese should equal 
# to Xi  which are calvculated mannualy. It is ony check if  my 
# way of calculating is coorect. Maybe using modinv is
# better from time wise prespective
# --------------------------------------------------------------
mod_inverse <- lapply(1:length(bus_no), function(i) {
  modinv(Ni[i], bus_no[i])
})
mod_inverse <- unlist(mod_inverse)
print(glue::glue('modinv: {paste(mod_inverse, collapse =", ")}\n
                 Manually calc mod inverses: {paste(Xi, collapse =", ")}'))
##------------------------------------------------------------------------------
# log(Ni)/log(bus_no)
# options(scipen = 999)
answer <- sum(remainder * Ni * Xi)%%prod(bus_no) 

##### for first 8 options/bus_no, correct answer is given. However for 9th 
# it needs to be corrected by remainder, same happens even then calculating 
# using package. Might be some rule for CRT I am not aware of :(
median((answer%%bus_no) - remainder)
answer_corrected <- answer  - median((answer%%bus_no) - remainder)
all(answer_corrected%%bus_no == remainder)

# --------------------------------------------------------------
# Using chinese function from  package numbers to check if 
# calculations where ok
# --------------------------------------------------------------
packet_answer <- chinese(a = remainder, m = bus_no)
print(paste0('My answer: ', answer))
print(paste0('Packet answer: ', packet_answer))
packet_answer - median((answer%%bus_no) - remainder)

