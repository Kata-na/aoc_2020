#all takes about ~1 sec
rm(list = ls())
t1 <- Sys.time()
options(scipen = 999)

library(data.table)
library(stringr)
import::from('magrittr', '%>%')


t1 <- Sys.time()
out <-  readLines("../Input/a16.txt")
nl <- which(out == '')
ticket_rules <- data.table(V1 = out[1:(nl[1]-1)])
my_ticket <- out[nl[1]+2]

nearby_tickets <- out[(nl[2]+2):length(out)]
n <- length(nearby_tickets)
nearby_tickets <- as.integer(unlist(strsplit(nearby_tickets, ',')))
nearby_tickets <- matrix(nearby_tickets, ncol = n) %>% t()
my_ticket <- as.integer(strsplit(my_ticket, ',')[[1]])

# --------------------------------------------------------------
# PREP TICKET RULES
# --------------------------------------------------------------

ticket_rules <- str_split_fixed(ticket_rules[['V1']], ': | or |-', n=5) %>%
  data.table() %>%
  setnames(c('V1', 'V2', 'V3', 'V4', 'V5'),
           c('rule_name', 's1','e1', 's2','e2'))

ticket_rules_unlisted <- ticket_rules %>%
  copy() %>%
  .[, list(allowed_val = c(s1:e1, s2:e2)),
    by = 'rule_name']

# --------------------------------------------------------------
# PART 1
# --------------------------------------------------------------
ans <- sum(nearby_tickets[!nearby_tickets %in% 
                            ticket_rules_unlisted[['allowed_val']]])
print(ans)

# --------------------------------------------------------------
# PART 2
# --------------------------------------------------------------
ch_matrix <- matrix(nearby_tickets %in% ticket_rules_unlisted[['allowed_val']],
          nrow = nrow(nearby_tickets),
          ncol = ncol(nearby_tickets))

good_tickets <- which(rowSums(ch_matrix)%/%ncol(ch_matrix) == 1)
good_tickets <- nearby_tickets[good_tickets, ]

ix <- c(nrow(good_tickets), ncol(good_tickets))

all_candidates <- list()
for (rl in ticket_rules[['rule_name']]) {
  
  rl_val <- ticket_rules_unlisted[rule_name == rl, allowed_val]
  
  ch_rule <- matrix(good_tickets %in% rl_val, 
         nrow = ix[1], ncol = ix[2])
  
  prelim_fields <- which(colSums(ch_rule) %/% ix[1] == 1)
  all_candidates[[rl]] <- data.table(rule = rl,
                                   field_candidates = prelim_fields)
}
all_candidates <- rbindlist(all_candidates)

all_candidates <- all_candidates %>%
  .[order(field_candidates)] %>%
  .[, field_candidates := paste0('X', field_candidates)] %>%
  .[, val := 1] %>%
  data.table::dcast(rule ~ field_candidates,
                    value.var = 'val')
candidates <- as.matrix(all_candidates[,2:(ix[2]+1)])
candidates[is.na(candidates)] <- 0
rownames(candidates) <- all_candidates[['rule']]

dt_intermediate <- data.table()
for (it in 1:(ix[2]-1)) {

  ix1<- which(rowSums(candidates) == min(rowSums(candidates)))[1]
  ix2<- which(colSums(candidates) ==  min(colSums(candidates)))
  
  fields <- colnames(candidates)[candidates[ix1, ] != 0]
  rules <- rownames(candidates)[candidates[, ix2] != 0]
  
  ch <- fields == names(ix2)
  
  dt_intermediate <- dt_intermediate %>%
    rbind(data.table(rule = names(ix1),
                     field = fields[1]))
  candidates <- candidates[-ix1, colnames(candidates) != fields[1]]

}

dt_intermediate <- dt_intermediate %>%
  rbind(data.table(rule = setdiff(all_candidates[['rule']], dt_intermediate$rule),
                   field = setdiff(colnames(all_candidates)[2:(ix[2]+1)], dt_intermediate$field)))

dt_intermediate <- dt_intermediate %>%
  .[, field := as.integer(gsub('^X', '', field))]

nm <- as.integer(dt_intermediate[grepl('departure', rule), field])
print(prod(my_ticket[nm]))
print(Sys.time()- t1)