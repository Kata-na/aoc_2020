rm(list = ls())
t1 <- Sys.time()
options(scipen = 999)

library(data.table)
library(stringr)
import::from('magrittr', '%>%')
t1 <- Sys.time()

inp_raw <- read.csv('../Input/a18.txt', header = FALSE, stringsAsFactors = FALSE) %>%
  data.table() %>%
  .[, r := .I]

# --------------------------------------------------------------
# Defining my functions: infix
# --------------------------------------------------------------
`%sum%` <- function(x, y) {
  return(x+y)
}
`%multip%` <- function(x, y) {
  return(x*y)
}

inp <- copy(inp_raw) %>%
  .[, input := gsub('\\+', '%sum%', V1)] %>%
  .[, input := gsub('\\*', '%multip%', input)] %>%
  .[, out := eval(parse(text = input)), by = r]

ans <- sum(inp[['out']])
print(ans)

# --------------------------------------------------------------
# PART 2, addition precedence multiplication
# --------------------------------------------------------------

inp <- copy(inp_raw)

inp <- inp %>%
  .[, input := gsub('\\+', '%sum%', V1)]%>%
  .[, out := eval(parse(text = input)), by = r]

ans <- sum(inp[['out']])
print(ans)
print(Sys.time() - t1)


################################################################################
## -----------------------------------------------------------------------------
## --- EXPERIMENTS ---
## LOOPING AND USING REGEX/REPLACEMENT; WORKS BUT INFIX BETTER
## -----------------------------------------------------------------------------
################################################################################
cat('\nREGEX SOLUTION EXPERIMENT\n')
t2 <- Sys.time()
inp <- copy(inp_raw)

eval_parentheses <- function(input) {
  p <- str_extract_all(input, '(\\([^()]*?\\))')[[1]]
  for (x in p) {
    x_tmp <- eval_addit_then_multip(x = x)
    o <- eval(parse(text = x_tmp))
    input <- sub(x, o, input, fixed = TRUE)
  }
  if (grepl('\\(|\\)', input)) {
    input <- eval_parentheses(input)
  } else  return(input)
}

eval_addit_then_multip <- function(x){
  x_tmp <- gsub('\\(|\\)', '', x)
  x2 <- trimws(str_extract_all(x, '([0-9 +]*)')[[1]])
  x2 <- x2[grepl('\\+', x2)]
  for (y in x2)
    x_tmp <- sub(y, eval(parse(text = y)), x_tmp, fixed = TRUE)
  return(x_tmp)
}

out_list <-c()
for (input in inp[['V1']]) {

  out <- eval_parentheses(input)
  out <- eval_addit_then_multip(x = out)
  
  out_list <- c(out_list, eval(parse(text = out)))
}
ans <- sum(out_list)
print(ans)
print(Sys.time() - t2)

