Day <- 12

# Load libraries ----------------------------------------------------------

library(here)
library(glue)
library(tidyverse)

# Read data ---------------------------------------------------------------

d <- readLines(here("data", glue("day_{Day}_input.txt")))
test <- c("F10","N3","F7","R90","F11")

# Functions ---------------------------------------------------------------

update_direction <- function(instruction, value, direction) {
  compass <- tibble(heading = c("N", "E", "S", "W"),
         degrees = c(0, 90, 180, 270))
  
  current_deg <- compass %>%
    filter(heading == direction) %>%
    pull(degrees)
  
  new_deg <- case_when(
    instruction == "R" ~ (current_deg + value)%%360,
    instruction == "L" ~ (current_deg - value)%%360
  )
  
  new_direction <- compass %>%
    filter(degrees == new_deg) %>%
    pull(heading)
  
  return(new_direction)
}

move_ship <- function(d) {
  movements <- list("N" = 0, "E" = 0, "S" = 0, "W" = 0)
  direction <- "E"
  
  d <- d %>% as_tibble() %>%
    separate(value, into = c("instruction", "value"), sep = 1) %>%
    mutate(value = as.numeric(value))
  
  for (row in 1:nrow(d)) {
    if (d[row,]$instruction == "F") {
      movements[[direction]] <- movements[[direction]] + d[row,]$value
    } else if (d[row,]$instruction %in% c("R", "L")) {
      direction <- update_direction(d[row,]$instruction,
                                    d[row,]$value,
                                    direction)
    } else {
      movements[[d[row,]$instruction]] <- movements[[d[row,]$instruction]] + d[row,]$value
    }
  }
  
  return(movements)
}

rotate_waypoint <- function(instruction, value, waypoint) {
  
  
  return(new_waypoint)
}

waypoint_move <- function(d) {
  movements <- list("N" = 0, "E" = 0, "S" = 0, "W" = 0)
  direction <- "E"
  waypoint <- c(10, 1)
  
  d <- d %>% as_tibble() %>%
    separate(value, into = c("instruction", "value"), sep = 1) %>%
    mutate(value = as.numeric(value))
  
  for (row in 1:nrow(d)) {
    if (d[row,]$instruction == "F") {
      if (direction %in% c("N", "S")) {
        units <- d[row,]$value * waypoint[2]
      } else if (direction %in% c("E", "W")) {
        units <- d[row,]$value * waypoint[1]
      }
      movements[[direction]] <- movements[[direction]] + units
    } else if (d[row,]$instruction %in% c("R", "L")) {
      direction <- rotate_waypoint(d[row,]$instruction,
                                    d[row,]$value,
                                    waypoint)
    } else if (d[row,]$instruction %in% c("N", "S", "E", "W")) {
      # Update waypoint based on value
      if (d[row,]$instruction %in% c("N", "S")) {
       waypoint[1] <- waypoint[1] + d[row,]$value
      } else if (d[row,]$instruction %in% c("E", "W")) {
        waypoint[2] <- waypoint[2] + d[row,]$value
      }
    }
  }
  
  return(movements)
}

calc_manhattan <- function(movements) {
  return(abs(movements$N - movements$S) +
           abs(movements$E - movements$W))
}

# Question 1 --------------------------------------------------------------
answer1 <- d %>% move_ship() %>% calc_manhattan()
answer1

# Question 2 --------------------------------------------------------------
answer2 <- d %>% waypoint_move() %>% calc_manhattan()
answer2