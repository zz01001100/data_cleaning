# ------------------------------------------------------------------------------
# AUTHOR:  Leroy Zahno
# DATE:    2026-10-14
# ------------------------------------------------------------------------------


# SETUP -------------------------------------------------------------------

library(DBI)
library(RSQLite)
library(dplyr)
library(lubridate)
library(tidyverse)

# CONNECT ------------------------------------------------------------------

con <- dbConnect(RSQLite::SQLite(), dbname = "Data/database5.db")

# IMPORT ------------------------------------------------------------------

co_block <- dbReadTable(con, "co_block")
co_blockdata_map <- dbReadTable(con, "co_blockdata_map")
co_chat <- dbReadTable(con, "co_chat")
co_command <- dbReadTable(con, "co_command")
co_container <- dbReadTable(con, "co_container")
co_entity <- dbReadTable(con, "co_entity")
co_entity_map <- dbReadTable(con, "co_entity_map")
co_item <- dbReadTable(con, "co_item")
co_material_map <- dbReadTable(con, "co_material_map")
co_session <- dbReadTable(con, "co_session")


# CLEAN -------------------------------------------------------------------

co_block <- co_block[-c(12,9,3)]

# CREATE VARIABLES ------------------------------------------------

co_session <- co_session %>%
  mutate(time = as_datetime(time, origin = lubridate::origin, tz = "Europe/Berlin"))

co_block <- co_block %>%
  mutate(time = as_datetime(time, origin = lubridate::origin, tz = "Europe/Berlin"))

numer_of_logins_total <- sum(co_session$action)

total_blocks_placed <- sum(co_block$action == 1)
total_blocks_broken <- sum(co_block$action == 0)
total_blocks_clicked <- sum(co_block$action == 2)
total_blocks_environment <- sum(co_block$action == 3)


# PLAYERS ---------------------------------------------------------

players <- c(1, 12, 13, 14, 16, 21, 29, 31, 45, 47)

names <- c("Player 1", "Player 2",
           "Player 3", "Player 4",
           "Player 5", "Player 6", "Player 7",
           "Player 8", "Player 9", "Player 10")


# SUMMARIZE -------------------------------------------------------

user_stats <- data.frame(matrix(nrow = 8, ncol = 0))

for (i in seq_along(players)) {
  
  player <- co_session |> filter(user == players[i])
  blocks <- co_block |> filter(user == players[i])
  
  logins <- sum(player$action)
  
  session_length <- 0
  for (j in seq(1, nrow(player) - 1, by = 2)) {
    session_length <- session_length +
      as.numeric(difftime(player$time[j+1], player$time[j], units = "mins"))
  }
  
  blocks_placed <- sum(blocks$action == 1)
  blocks_broken <- sum(blocks$action == 0)
  blocks_clicked <- sum(blocks$action == 2)
  blocks_environment <- sum(blocks$action == 3)
  
  blocks_broken_per_login <- blocks_broken / logins
  
  blocks_placed_per_login <- blocks_placed / logins
  
  user_stats[[names[i]]] <- c(
    logins,
    session_length,
    blocks_placed,
    blocks_broken,
    blocks_clicked,
    blocks_environment,
    blocks_broken_per_login,
    blocks_placed_per_login
  )
}

rownames(user_stats) <- c(
  "Logins", "Session length", "Blocks placed",
  "Blocks Broken", "Blocks Clicked",
  "Environment Blocks", "Broken per login", "Placed per login"
)

user_stats <- round(user_stats, digits = 0)


# Diagrams ----------------------------------------------------------------

