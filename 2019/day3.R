Day <- 3

# Load libraries ----------------------------------------------------------

library(here)
library(glue)
library(tidyverse)

# Read data ---------------------------------------------------------------

d <- readLines(here("2019", "data", glue("day_{Day}_input.txt")))

# Functions ---------------------------------------------------------------

trace_wire <- function(wire_path) {
  # Initialize coordinates
  center <- c(0,0)
  # First value is up/down
  # Second value is right/left
  # Positive for up/right
  # Negative for down/left
  position <- center
  
  movements <- str_split(wire_path, ",")[[1]]
  walked <- list()
  
  gen_path <- function(movement, start_pos) {
    direction <- substr(movement, 1, 1)
    distance <- substr(movement, 2, nchar(movement)) %>%
      as.numeric()
    
    steps <- seq(from = 1, to = distance)
    
    if(direction %in% c("D", "L")) {
      steps <- steps * -1
    }
    
    if(direction %in% c("U", "D")) {
      visited <- map(steps, ~c(start_pos[1] + ., start_pos[2]))
    } else if (direction %in% c("R", "L")) {
      visited <- map(steps, ~c(start_pos[1], start_pos[2] + .))
    }
    
    position <<- visited[[length(visited)]]
    walked <- c(walked, visited)
    return(walked)
  }
  
  for(movement in movements) {
    walked <- gen_path(movement, position)
  }
  
  return(walked)
}

find_intersection <- function(wire_walk_1, wire_walk_2) {
  junction <- intersect(wire_walk_1, wire_walk_2)[which.min(intersect(wire_walk_1, wire_walk_2) %>% map(., abs) %>% map(., sum))]
  return(sum(abs(junction[[1]])))
}

vdf <- function(veclist) {
  # Lists of vectors are stupid.
  do.call(rbind, veclist) %>%
    as_tibble(.name_repair = "minimal")
}

shortest_steps <- function(wire_walk_1, wire_walk_2) {
  crossings <- vdf(intersect(wire_walk_1, wire_walk_2))
  w1 <- vdf(wire_1)
  w2 <- vdf(wire_2)

}
# Question 1 --------------------------------------------------------------
wire_1 <- trace_wire(d[1])
wire_2 <- trace_wire(d[2])
answer1 <- find_intersection(wire_1, wire_2)
answer1

# Question 2 --------------------------------------------------------------

answer2