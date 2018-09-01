library(dplyr)
library(httr)
library(purrr)
library(roomba)
 
# utility functions
na_to_zero <- function(x) {
  replace(x, is.na(x), 0)
}
clean_data <- function(x) {
  x <- x %>%
    mutate(charisma = ifelse(name == "Aboleth", 18, charisma))
  to_clean <- c("strength_save", "dexterity_save", "constitution_save",
                "intelligence_save", "wisdom_save", "charisma_save",
                "perception", "history", "insight", "stealth", "survival",
                "athletics", "deception", "persuasion", "arcana", 
                "investigation", "religion", "intimidation", "performance",
                "medicine", "acrobatics", "nature")
  x[,to_clean] <- map(x[,to_clean], na_to_zero)
  x
}
add_listcol <- function(x, colname) {
  listcol <- mdata %>%
    map(colname) %>%
    map(bind_rows) %>%
    unname()
  mutate(x, !! colname := listcol)
}
has_feature <- function(tbl, nm) {
  nm %in% tbl$name
}

mlist <- "http://dnd5eapi.co/api/monsters" %>%
  GET() %>%
  content(as = "parsed") %>%
  roomba(c("name", "url")) 

mdata <- mlist %>%
  select(url) %>%
  unlist() %>%
  map(GET) %>%
  map(content, as = "parsed") 

df <- mdata %>%
  map(~keep(.x, ~!is.list(.x))) %>%
  bind_rows() %>%
  clean_data() %>%
  add_listcol("special_abilities") %>%
  mutate(amphibious = map_lgl(special_abilities, has_feature, "Amphibious"))

qplot(challenge_rating, hit_points, data=df, geom=c("point", "smooth"),
      method="loess", formula=y~x,
      main="Regression of HP on CR",
      xlab="CR", ylab="HP")

"%d%" <- function(x, y) sum(sample.int(y, x, replace = TRUE))