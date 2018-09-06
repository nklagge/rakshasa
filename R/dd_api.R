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
  mutate(aggressive = map_lgl(special_abilities, has_feature, "Aggressive"),
         ambusher = map_lgl(special_abilities, has_feature, "Ambusher"),
         angelic = map_lgl(special_abilities, has_feature, "Angelic Weapons"),
         avoidance = map_lgl(special_abilities, has_feature, "Avoidance"), # none - demilich
         frenzy = map_lgl(special_abilities, has_feature, "Blood Frenzy"),
         brute = map_lgl(special_abilities, has_feature, "Brute"),
         charge = map_lgl(special_abilities, has_feature, "Charge"),
         constrict = map_lgl(special_abilities, has_feature, "Constrict"), # none - constrictor snake - this is an attack
         dmgxfer = map_lgl(special_abilities, has_feature, "Damage Transfer"),
         deathburst = map_lgl(special_abilities, has_feature, "Death Burst"),
         dive = map_lgl(special_abilities, has_feature, "Dive Attack"), # none - aarakocra
         elemental = map_lgl(special_abilities, has_feature, "Elemental Body"), # none - heated body per azer
         enlarge = map_lgl(special_abilities, has_feature, "Enlarge"), # none - duergar - this is an action
         fiendish = map_lgl(special_abilities, has_feature, "Fiendish Blessing"), # none - cambion
         frightful = map_lgl(special_abilities, has_feature, "Frightful Presence"), # none - this is an action
         horrify = map_lgl(special_abilities, has_feature, "Horrifying Visage"), # none - this is an action
         cast_innate = map_lgl(special_abilities, has_feature, "Innate Spellcasting"),
         legendary = map_lgl(special_abilities, has_feature, "Legendary Resistance"), # none - includes day count in text
         mr = map_lgl(special_abilities, has_feature, "Magic Resistance"),
         martial = map_lgl(special_abilities, has_feature, "Martial Advantage"),
         nimble = map_lgl(special_abilities, has_feature, "Nimble Escape"),
         pack = map_lgl(special_abilities, has_feature, "Pack Tactics"),
         parry = map_lgl(special_abilities, has_feature, "Parry"), # none - this is a reaction
         possession = map_lgl(special_abilities, has_feature, "Possession"), # none - this is an action
         pounce = map_lgl(special_abilities, has_feature, "Pounce"),
         psychic = map_lgl(special_abilities, has_feature, "Psychic Defense"), # none - githzerai
         rampage = map_lgl(special_abilities, has_feature, "Rampage"),
         regen = map_lgl(special_abilities, has_feature, "Regeneration"),
         shadow = map_lgl(special_abilities, has_feature, "Shadow Stealth"),
         relentless = map_lgl(special_abilities, has_feature, "Relentless"), # none - has recharge text
         cast_spell = map_lgl(special_abilities, has_feature, "Spellcasting"),
         stench = map_lgl(special_abilities, has_feature, "Stench"),
         superinv = map_lgl(special_abilities, has_feature, "Superior Invisibility"), # none - faerie dragon
         surprise = map_lgl(special_abilities, has_feature, "Surprise Attack"),
         swallow = map_lgl(special_abilities, has_feature, "Swallow"), # none - this is an action
         unfort = map_lgl(special_abilities, has_feature, "Undead Fortitude"),
         web = map_lgl(special_abilities, has_feature, "Web"), # none - this is an action
         wounded = map_lgl(special_abilities, has_feature, "Wounded Fury")) # none - quaggoth


qplot(challenge_rating, hit_points, data=df, geom=c("point", "smooth"),
      method="loess", formula=y~x,
      main="Regression of HP on CR",
      xlab="CR", ylab="HP")