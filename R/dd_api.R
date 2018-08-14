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
    map(bind_rows)
  mutate(x, !! colname := listcol)
}

mlist <- "http://dnd5eapi.co/api/monsters" %>%
  GET() %>%
  content(as = "parsed") %>%
  roomba("name", "url") 

mdata <- mlist %>%
  select(url) %>%
  unlist() %>%
  map(GET) %>%
  map(content, as = "parsed") 

df <- mdata %>%
  roomba(c("index", "size", "type", "subtype", "alignment", "armor_class", 
           "hit_points", "hit_dice", "speed", "strength", "dexterity", 
           "constitution", "intelligence", "wisdom", "charisma", 
           "strength_save", "dexterity_save", "constitution_save", 
           "intelligence_save", "wisdom_save", "charisma_save", "perception", 
           "history", "insight", "stealth", "survival", "athletics", 
           "deception", "persuasion", "arcana", "investigation", "religion",
           "intimidation", "performance", "medicine", "acrobatics", "nature",
           "damage_vulnerabilities", "damage_resistances", "damage_immunities", 
           "condition_immunities", "senses", "languages", "challenge_rating", 
           "url"),
         keep = any) %>%
  inner_join(mlist, by = "url") %>%
  select(name, 1:(ncol(.)-1)) %>%
  clean_data() %>%
  add_listcol("actions") %>%
  add_listcol("special_abilities") %>%
  add_listcol("legendary_actions")

qplot(challenge_rating, hit_points, data=df, geom=c("point", "smooth"),
      method="loess", formula=y~x,
      main="Regression of HP on CR",
      xlab="CR", ylab="HP")

"%d%" <- function(x, y) sum(sample.int(y, x, replace = TRUE))