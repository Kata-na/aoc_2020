rm(list = ls())

library(data.table)
library(magrittr)
library(stringr)

t1 <- Sys.time()
dtt <- read.csv('a19.txt', header = FALSE, stringsAsFactors = FALSE)

msg   <- dtt[['V1']][which(grepl('^[a-z]*$', dtt[['V1']]))]
rules <- data.table(inp = dtt[['V1']][which(!grepl('^[a-z]*$', dtt[['V1']]))]) %>%
  .[, ruleNo := gsub('(^\\d*?): .*?$', '\\1', inp)] %>%
  .[, rule := gsub('^\\d*?: (.*?$)', '\\1', inp)] %>%
  .[!grepl('^[a-z |()]*$', rule), rule := paste0('(', rule, ')')]

rules[, ruleOriginal := rule]
multiple_string_replacment <- function(str, patterns, replacements) {
  
  for (i in 1:length(patterns)) {
    str <- str_replace_all(str, patterns[i], replacements[i])
  }
  return(str)
}

# --------------------------------------------------------------
# Preparing Rules
# --------------------------------------------------------------

stop_loop <- FALSE
drop_id <- Inf
while (!stop_loop) {
  id <- which(grepl('^[a-z |()]*$', rules[['rule']]))
  id <- id[!id %in% drop_id]
  rn <- paste0("\\b", rules[id, ruleNo], '\\b')
  repl <- rules[id, rule]
  rules[, 'rule'] <- multiple_string_replacment(rules[['rule']], rn, repl)
  if (all(!grepl('[0-9]', rules[['rule']]))) stop_loop <- TRUE
  drop_id <- c(drop_id, id)
}

rules <- rules %>%
  .[, rule := gsub(' ', '', rule)] %>%
  .[, rule := paste0('^(', rule, ')$')]

# --------------------------------------------------------------
# PART 1
# --------------------------------------------------------------

rule_zero <- rules[ruleNo == 0, rule]
ans <- sum(grepl(rule_zero, msg))
print(ans)

# --------------------------------------------------------------
# PART 2 
# VERY dumm way of solving with regex and few loops
# --------------------------------------------------------------

rules[ruleNo == 8, 'ruleOriginal'] <- paste0('(', '42 | 42 8', ')')
rules[ruleNo == 11, 'ruleOriginal'] <- paste0('(', '42 31 | 42 11 31', ')')
rules[, rule := ruleOriginal]

stop_loop <- FALSE
drop_id <- Inf
while (!stop_loop) {
  id <- which(grepl('^[a-z |()]*$', rules[['rule']]))
  id <- id[!id %in% drop_id]
  rn <- paste0("\\b", rules[id, ruleNo], '\\b')
  repl <- rules[id, rule]
  rules[, 'rule'] <- multiple_string_replacment(rules[['rule']], rn, repl)
  if (all(!grepl('[0-9]', rules[['rule']]) | grepl('(\\(| )(11|8)(\\)| )', rules[['rule']]))) {
    stop_loop <- TRUE
  }
  drop_id <- c(drop_id, id)
}

create_infinity_rules <- function(rule_number, rules, it = 10) {
  rl <- strsplit(rules[ruleNo == rule_number, rule], '\\(|\\)|\\|| ')[[1]]
  rl <- unique(rl[rl!='' & rl != rule_number])
  repl <- rules[ruleNo %in% rl, rule]
  rules[ruleNo == rule_number, rule]
  rl <- paste0("\\b", rl, '\\b')
  rule_regex <- multiple_string_replacment(rules[ruleNo == rule_number, rule], rl, repl)

  for (i in 1:it){
    rule_regex <- str_replace_all(rule_regex, glue::glue('\\b{rule_number}\\b'),
                                  rule_regex)
  }
  rule_regex <- gsub(rule_number, '', rule_regex)
  return(rule_regex)
}


rule_8  <- create_infinity_rules(8, rules, it = 3)
rule_11 <- create_infinity_rules(11, rules, it = 3)

rule_0 <- multiple_string_replacment(rules[ruleNo == 0, rule],c('\\b8\\b', '\\b11\\b'), 
                    c(rule_8, rule_11))
rule_0 <- paste0('^(',gsub(' ', '', rule_0), ')$')


ans <- sum(grepl(rule_0, msg))
print(ans)

