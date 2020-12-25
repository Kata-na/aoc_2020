rm(list = ls())
t1 <- Sys.time()
library(data.table)
library(magrittr)
library(stringr)

ingredients  <- read.csv('../Input/a21.txt', sep = ';', header = FALSE, stringsAsFactors = FALSE)
ingredients <- paste0(ingredients$V1, ',', ingredients$V2, ',', ingredients$V3)

allergens <- gsub('.*?\\((.*?)\\).*?$', '\\1', ingredients)
allergens <- gsub('^contains ', '', allergens)
ingredients <- trimws(gsub('(^.*?)\\(.*?\\).*?$', '\\1', ingredients))

ingredients_info <- data.table(ingredients = ingredients,
                               allergens = allergens) %>%
  .[, list(allergens = trimws(unlist(strsplit(allergens, ', ')))), 
    by = .(ingredients)] %>%
  .[,  count_allergens := .N, by = c('allergens')] %>%
  .[, list(ingredients_split = unlist(strsplit(ingredients, ' '))), 
    by = .(allergens, ingredients, count_allergens)] %>%
  .[, count_allergens_ingredients_combo := .N, by = c('ingredients_split', 'allergens')] 

count_ingredtients <- copy(ingredients_info[, .(ingredients, ingredients_split)] ) %>%
  unique() %>%
  .[, count_ingredients := .N, by = .(ingredients_split)]
 
ingredients_info <- ingredients_info  %>%
  merge(count_ingredtients, all.x = TRUE, by = c('ingredients', 'ingredients_split'),
        allow.cartesian = TRUE)
rm(count_ingredtients, allergens, ingredients)

# --------------------------------------------------------------
# PART 1
# --------------------------------------------------------------

possibly_assigned_allergens <- copy(ingredients_info) %>%
  .[count_allergens == count_allergens_ingredients_combo, -'ingredients'] %>%
  unique()

not_assigned_allergens <- copy(ingredients_info) %>%
  .[count_allergens != count_allergens_ingredients_combo, -'ingredients'] %>%
  unique() %>%
  .[!ingredients_split %in% unique(possibly_assigned_allergens[['ingredients_split']])] %>%
  .[, .(ingredients_split, count_ingredients)] %>%
  unique()
ans <- sum(not_assigned_allergens[['count_ingredients']])
print(ans)
# --------------------------------------------------------------
# PART 2
# --------------------------------------------------------------
assigned_allergens <- data.table()
stop_loop <- FALSE
while (!stop_loop) {
  unique_combo <- possibly_assigned_allergens[, n := .N, by = ingredients_split] %>%
    .[n == 1]
  
  if (nrow(unique_combo) == 0) stop('Unexpected output :(')
  assigned_allergens <- assigned_allergens %>%
    rbind(unique_combo[, .(ingredients_split, allergens)])
  possibly_assigned_allergens <- possibly_assigned_allergens %>%
    .[!allergens %in% unique_combo[['allergens']]]
  if (nrow(possibly_assigned_allergens) == 0) stop_loop <- TRUE
}

assigned_allergens <- assigned_allergens[order(allergens)]

canonical_dangerous_ingredients_list <- 
  paste(assigned_allergens[['ingredients_split']], collapse = ',')
print(canonical_dangerous_ingredients_list)
print(Sys.time() - t1)