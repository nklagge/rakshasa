library(dplyr)
library(purrr)

# functions
# roll xdy dice
"%d%" <- function(x, y) sum(sample.int(y, x, replace = TRUE))
# resolve a single weapon swing by a against b
"%swing%" <- function(a, b) {
  res <- 0
  roll <- 1%d%20 
  if (roll + a$to_hit >= b$ac) res <- 1
  if (roll >= a$crit_min) res <- 2
  res
}
# resolve a single set of attacks by group a against group b
"%attack%" <- function(list_a, list_b) {
  # loop over attackers
  for (a in 1:length(list_a)) {
    # only active attackers
    if (list_a[[a]]$hp > 0) {
      # loop over attacks
      for (i in 1:list_a[[a]]$apr) {
        # choose attack target: enemy with lowest positive hp
        tgt <- list_b %>%
          map_dbl(~ifelse(.$hp > 0, .$hp, 999)) %>%
          which.min()
        # swing at target
        swing_res <- list_a[[a]] %swing% list_b[[tgt]]
        dmg <- 0
        if (swing_res > 0) {
          dmg <- swing_res * list_a[[a]]$atk_ndice %d% list_a[[a]]$atk_die + 
            list_a[[a]]$dmg_mod
        }
        list_b[[tgt]]$hp <- list_b[[tgt]]$hp - dmg
      # end loop over attacks of group a individual
      }
    }
  # end loop over group a individuals
  }
  list_b
}
# roll initiative
roll_init <- function(list_a, list_b) {
  roll_a <- 1%d%20 + mean(map_dbl(list_a, "dexmod"))
  roll_b <- 1%d%20 + mean(map_dbl(list_b, "dexmod"))
  res <- roll_a > roll_b
  if (roll_a == roll_b) res <- sample(c(TRUE, FALSE), 1)
  unname(res)
}
# resolve a single duel between multiple characters and multiple monsters
multi_duel <- function(list_a, list_b) {
  # initial conditions
  victor <- NULL
  rounds <- 0
  a_max_hp <- map_dbl(list_a, "hp")
  b_max_hp <- map_dbl(list_b, "hp")
  # fight!
  while (max(map_dbl(list_a, "hp")) > 0 & max(map_dbl(list_b, "hp")) > 0) {
    # pcs attack monsters
    list_b <- list_a %attack% list_b
    # monsters attack pcs
    list_a <- list_b %attack% list_a
    rounds <- rounds + 1
  }
  # resolve winner
  if (max(map_dbl(list_a, "hp")) <= 0) {
    if (max(map_dbl(list_b, "hp")) <= 0 & roll_init(list_a, list_b)) {
      victor <- list_a[[1]]$name
    } else {
      victor <- list_b[[1]]$name
    }
    # if a is not dead, b is dead and a won
  } else {
    victor <- list_a[[1]]$name
  }
  # output data
  dmg_to_a <- sum(a_max_hp - pmax(map_dbl(list_a, "hp"), 0))
  dmg_to_b <- sum(b_max_hp - pmax(map_dbl(list_b, "hp"), 0))
  ko_ct_a <- sum(map_dbl(list_a, "hp") <= 0)
  ko_ct_b <- sum(map_dbl(list_b, "hp") <= 0)
  dead_ct_a <- sum(map_dbl(list_a, "hp") <= -a_max_hp)
  dead_ct_b <- sum(map_dbl(list_b, "hp") <= -b_max_hp)
  list(a = list_a[[1]]$name, b = list_b[[1]]$name, victor = victor, 
       rounds = rounds, ko_a = ko_ct_a, ko_b = ko_ct_b,
       dead_a = dead_ct_a, dead_b = dead_ct_b,
       dmg_to_a = dmg_to_a, dmg_to_b = dmg_to_b)
}
# simulate `reps` duels between the same character / monster groups
duel_sim <- function(list_a, list_b, reps) {
  res <- vector("list", reps)
  for (r in 1:reps) res[[r]] <- multi_duel(list_a, list_b)
  dplyr::bind_rows(res)
}

# example characters
# CR 1/8 paragon
bandit <- list(name = "bandit",
              ac = 12,
              hp = 1,
              dexmod = 1,
              apr = 1,
              to_hit = 3,
              atk_ndice = 1,
              atk_die = 6,
              dmg_mod = 1,
              crit_min = 20)

# CR 1/4 paragon
goblin <- list(name = "goblin",
              ac = 15,
              hp = 7,
              dexmod = 2,
              apr = 1,
              to_hit = 4,
              atk_ndice = 1,
              atk_die = 6,
              dmg_mod = 2,
              crit_min = 20)

# CR 1/2 paragon
orc <- list(name = "orc",
            ac = 13,
            hp = 15,
            dexmod = 1,
            apr = 1,
            to_hit = 5,
            atk_ndice = 1,
            atk_die = 12,
            dmg_mod = 2,
            crit_min = 20)

# CR 1 paragon
hogre <- list(name = "half ogre",
              ac = 12,
              hp = 30,
              dexmod = 0,
              apr = 1,
              to_hit = 5,
              atk_ndice = 2,
              atk_die = 10,
              dmg_mod = 3,
              crit_min = 20)

# CR 2 paragon
ogre <- list(name = "ogre",
              ac = 11,
              hp = 59,
              dexmod = -1,
              apr = 1,
              to_hit = 6,
              atk_ndice = 2,
              atk_die = 8,
              dmg_mod = 4,
              crit_min = 20)

# CR 3 paragon
# can't handle third attack with short sword, use long sword two handed twice 
veteran <- list(name = "veteran",
              ac = 17,
              hp = 58,
              dexmod = 1,
              apr = 2,
              to_hit = 5,
              atk_ndice = 1,
              atk_die = 10,
              dmg_mod = 3,
              crit_min = 20)

# CR 4 paragon
ettin <- list(name = "ettin",
              ac = 12,
              hp = 85,
              dexmod = -1,
              apr = 2,
              to_hit = 7,
              atk_ndice = 2,
              atk_die = 8,
              dmg_mod = 5,
              crit_min = 20)

# CR 5 paragon
hill_giant <- list(name = "hill giant",
              ac = 13,
              hp = 105,
              dexmod = -1,
              apr = 2,
              to_hit = 8,
              atk_ndice = 3,
              atk_die = 8,
              dmg_mod = 5,
              crit_min = 20)

# CR 6 paragon
cyclops <- list(name = "cyclops",
                ac = 14,
                hp = 138,
                dexmod = 0,
                apr = 2,
                to_hit = 9,
                atk_ndice = 3,
                atk_die = 8,
                dmg_mod = 6,
                crit_min = 20)

# CR 7 paragon
stone_giant <- list(name = "stone giant",
              ac = 17,
              hp = 126,
              dexmod = 2,
              apr = 2,
              to_hit = 9,
              atk_ndice = 3,
              atk_die = 8,
              dmg_mod = 6,
              crit_min = 20)

# CR 8 paragon
frost_giant <- list(name = "frost giant",
              ac = 15,
              hp = 138,
              dexmod = -1,
              apr = 2,
              to_hit = 9,
              atk_ndice = 3,
              atk_die = 12,
              dmg_mod = 6,
              crit_min = 20)

# CR 9 paragon
fire_giant <- list(name = "fire giant",
              ac = 18,
              hp = 162,
              dexmod = -1,
              apr = 2,
              to_hit = 11,
              atk_ndice = 6,
              atk_die = 6,
              dmg_mod = 7,
              crit_min = 20)

# CR 0: hp 1-6, ac <= 13, dpr 0-1, ab <= 3
cr0 <- list(name = "cr0",
                ac = 13,
                hp = 4,
                dexmod = 0,
                apr = 1,
                to_hit = 3,
                atk_ndice = 1,
                atk_die = 1,
                dmg_mod = 0,
                crit_min = 20)

# CR 0.125: hp 7-35, dpr 2-3
cr1_8 <- list(name = "cr1/8",
            ac = 13,
            hp = 21,
            dexmod = 0,
            apr = 1,
            to_hit = 3,
            atk_ndice = 1,
            atk_die = 4,
            dmg_mod = 0,
            crit_min = 20)

# CR 0.25: hp 36-49, dpr 4-5
cr1_4 <- list(name = "cr1/4",
              ac = 13,
              hp = 43,
              dexmod = 0,
              apr = 1,
              to_hit = 3,
              atk_ndice = 1,
              atk_die = 8,
              dmg_mod = 0,
              crit_min = 20)

# CR 0.5: hp 50-70, dpr 6-8
cr1_2 <- list(name = "cr1/2",
              ac = 13,
              hp = 60,
              dexmod = 0,
              apr = 1,
              to_hit = 3,
              atk_ndice = 1,
              atk_die = 8,
              dmg_mod = 2,
              crit_min = 20)

# CR 1: hp 71-85, dpr 9-14
cr1 <- list(name = "cr1",
              ac = 13,
              hp = 78,
              dexmod = 0,
              apr = 1,
              to_hit = 3,
              atk_ndice = 2,
              atk_die = 8,
              dmg_mod = 3,
              crit_min = 20)

# CR 2: hp 86-100, dpr 15-20
cr2 <- list(name = "cr2",
            ac = 13,
            hp = 93,
            dexmod = 0,
            apr = 2,
            to_hit = 3,
            atk_ndice = 2,
            atk_die = 8,
            dmg_mod = 0,
            crit_min = 20)

# CR 3: hp 101-115, dpr 21-26
cr3 <- list(name = "cr3",
            ac = 13,
            hp = 108,
            dexmod = 0,
            apr = 2,
            to_hit = 4,
            atk_ndice = 2,
            atk_die = 10,
            dmg_mod = 1,
            crit_min = 20)

# CR 4: hp 116-130, dpr 27-32
cr4 <- list(name = "cr4",
            ac = 14,
            hp = 123,
            dexmod = 0,
            apr = 2,
            to_hit = 5,
            atk_ndice = 2,
            atk_die = 10,
            dmg_mod = 4,
            crit_min = 20)

# CR 5: hp 131-145, dpr 33-38
cr5 <- list(name = "cr5",
            ac = 15,
            hp = 138,
            dexmod = 0,
            apr = 2,
            to_hit = 6,
            atk_ndice = 3,
            atk_die = 8,
            dmg_mod = 4,
            crit_min = 20)

# CR 6: hp 146-160, dpr 39-44
cr6 <- list(name = "cr6",
            ac = 15,
            hp = 153,
            dexmod = 0,
            apr = 2,
            to_hit = 6,
            atk_ndice = 3,
            atk_die = 10,
            dmg_mod = 4,
            crit_min = 20)

# CR 7: hp 161-175, dpr 45-50
cr7 <- list(name = "cr7",
            ac = 15,
            hp = 168,
            dexmod = 0,
            apr = 2,
            to_hit = 6,
            atk_ndice = 4,
            atk_die = 8,
            dmg_mod = 6,
            crit_min = 20)

# CR 8: hp 176-190, dpr 51-56
cr8 <- list(name = "cr8",
            ac = 16,
            hp = 183,
            dexmod = 0,
            apr = 3,
            to_hit = 6,
            atk_ndice = 3,
            atk_die = 8,
            dmg_mod = 6,
            crit_min = 20)

# CR 9: hp 191-205, dpr 57-62
cr9 <- list(name = "cr9",
            ac = 16,
            hp = 198,
            dexmod = 0,
            apr = 3,
            to_hit = 6,
            atk_ndice = 4,
            atk_die = 8,
            dmg_mod = 2,
            crit_min = 20)

# pc1: Level 1 human fighter
# str 16 dex 14 con 15 int 13 wis 11 cha 9
# duelling style
# long sword, chain mail, shield
pc1 <- list(name = "pc1",
            ac = 18,
            hp = 12,
            dexmod = 2,
            apr = 1,
            to_hit = 5,
            atk_ndice = 1,
            atk_die = 8,
            dmg_mod = 5,
            crit_min = 20)

# pc2: Level 2 human fighter
# str 16 dex 14 con 15 int 13 wis 11 cha 9
# duelling style
# long sword, chain mail, shield
pc2 <- list(name = "pc2",
            ac = 18,
            hp = 20,
            dexmod = 2,
            apr = 1,
            to_hit = 5,
            atk_ndice = 1,
            atk_die = 8,
            dmg_mod = 5,
            crit_min = 20)

# pc3: Level 3 human fighter
# str 16 dex 14 con 15 int 13 wis 11 cha 9
# duelling style
# long sword, splint mail, shield
pc3 <- list(name = "pc3",
            ac = 19,
            hp = 28,
            dexmod = 2,
            apr = 1,
            to_hit = 5,
            atk_ndice = 1,
            atk_die = 8,
            dmg_mod = 5,
            crit_min = 19)

# pc4: Level 4 human fighter
# str 18 dex 14 con 15 int 13 wis 11 cha 9
# duelling style
# long sword, splint mail, shield
pc4 <- list(name = "pc4",
            ac = 19,
            hp = 36,
            dexmod = 2,
            apr = 1,
            to_hit = 6,
            atk_ndice = 1,
            atk_die = 8,
            dmg_mod = 6,
            crit_min = 19)

# pc5: Level 5 human fighter
# str 18 dex 14 con 15 int 13 wis 11 cha 9
# duelling style
# long sword, plate mail, shield
pc5 <- list(name = "pc5",
            ac = 20,
            hp = 44,
            dexmod = 2,
            apr = 2,
            to_hit = 7,
            atk_ndice = 1,
            atk_die = 8,
            dmg_mod = 6,
            crit_min = 19)

# pc6: Level 6 human fighter
# str 20 dex 14 con 15 int 13 wis 11 cha 9
# duelling style
# long sword, plate mail, shield
pc6 <- list(name = "pc6",
            ac = 20,
            hp = 52,
            dexmod = 2,
            apr = 2,
            to_hit = 8,
            atk_ndice = 1,
            atk_die = 8,
            dmg_mod = 7,
            crit_min = 19)