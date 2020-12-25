rm(list = ls())
library(data.table)
library(magrittr)
library(stringr)


# --------------------------------------------------------------
# '-' separator between different 
# '~' separator between respondent in one group
# --------------------------------------------------------------

dtt <- fread('a6.csv') %>%
  .[name == '', name := '-']

input_string <- paste(dtt[['name']], collapse = '')
input_string <- unlist(strsplit(input_string, '-'))

count_answers <- lapply(input_string, function(x) {
  x <- length(unique(unlist(strsplit(x, ''))))
  return(x)
})
answer <- sum(unlist(count_answers))

# --------------------------------------------------------------
# SECOND PART
# --------------------------------------------------------------
input_string <- paste(dtt[['name']], collapse = '~')
input_string <- unlist(strsplit(input_string, '-'))

count_answers <- lapply(input_string, function(x) {
  x <- unlist(strsplit(x, '~'))
  x <- x[x!='']
  number_of_respondents <- length(x)

  x <- unlist(strsplit(x, '')) %>%
    table() %>%
    data.table()
  n <- sum(x[['N']] == number_of_respondents)
  
  return(n)
})
   
answer <- sum(unlist(count_answers))
