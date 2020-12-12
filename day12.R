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

move_ship <- function(d, ...) {
  movements <- list("N" = 0, "E" = 0, "S" = 0, "W" = 0)
  direction <- "E"
  
  args <- list(...)
  
  if ("waypoint" %in% names(args)) {
    waypoint <- args$waypoint
  }
  
  
  d <- d %>% as_tibble() %>%
    separate(value, into = c("instruction", "value"), sep = 1) %>%
    mutate(value = as.numeric(value))
  
  wp <- tibble(primary = c("N", "E", "S", "W"),
               secondary = c("W", "N", "E", "S"))
  
  for (row in 1:nrow(d)) {
    if (d[row,]$instruction == "F") {
      if(hasArg(waypoint)) {
        # Move with waypoint multipliers
        sec <- wp %>% filter(primary == direction) %>% pull(secondary)
        movements[[direction]] <- movements[[direction]] + (d[row,]$value * waypoint[1])
        movements[[sec]] <- movements[[sec]] + (d[row,]$value * waypoint[2])
      } else{
      movements[[direction]] <- movements[[direction]] + d[row,]$value
      }
    } else if (d[row,]$instruction %in% c("R", "L")) {
      direction <- update_direction(d[row,]$instruction,
                                    d[row,]$value,
                                    direction)
    } else {
      if(hasArg(waypoint)) {
        # Now this means to move the waypoint,
        # Taking the direction into account
        if (direction %in% c("E", "W")) {
          if (d[row,]$instruction %in% c("N", "S")) {
            # Adjust secondary
            if ((direction == "E" &  d[row,]$instruction == "S") | (direction == "W" &  d[row,]$instruction == "N")) {
              waypoint[2] <- waypoint[2] - d[row,]$value
            } else {
            waypoint[2] <- waypoint[2] + d[row,]$value
            }
          } else {
            # Adjust primary
            if (direction != d[row,]$instruction) {
            waypoint[1] <- waypoint[1] - d[row,]$value
            } else {
              waypoint[1] <- waypoint[1] + d[row,]$value
            }
          }
        } else if (direction %in% c("N", "S")) {
          if (d[row,]$instruction %in% c("E", "W")) {
            # Adjust secondary
            if ((direction == "N" &  d[row,]$instruction == "E") | (direction == "S" &  d[row,]$instruction == "W")) {
            waypoint[2] <- waypoint[2] - d[row,]$value
          } else {
            waypoint[2] <- waypoint[2] + d[row,]$value
          }
          } else {
            # Adjust primary
            if (direction != d[row,]$instruction) {
              waypoint[1] <- waypoint[1] - d[row,]$value
            } else {
              waypoint[1] <- waypoint[1] + d[row,]$value
            }
          }
        }
      } else {
        # Just update the movement in the direction given
      movements[[d[row,]$instruction]] <- movements[[d[row,]$instruction]] + d[row,]$value
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
answer2 <- d %>% move_ship(waypoint = c(10, 1)) %>% calc_manhattan()
answer2