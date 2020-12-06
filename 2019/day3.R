Day <- 3

# Load libraries ----------------------------------------------------------

library(here)
library(glue)
library(tidyverse)

# Read data ---------------------------------------------------------------

d <- readLines(here("2019", "data", glue("day_{Day}_input.txt")))

# Functions ---------------------------------------------------------------

prep_data <- function(d) {
  wires <- map(d, ~str_split(., pattern = ",")[[1]] %>% 
        as_tibble() %>%
        separate(value, into = c("dir", "steps"), sep = 1) %>%
          mutate(steps = as.numeric(steps)))
  
  return(wires)
}

trace_wire <- function(wire) {
  visited <- tibble(x = 0,
                    y = 0,
                    steps = 0)
  
  for(i in seq(nrow(wire))) {
    state <- visited[nrow(visited),]
    movement <- wire[i,]
    
    new_steps <- seq(from = pull(state, steps) + 1,
                     to = pull(state, steps) + pull(movement, steps))
    
    if (pull(movement, dir) %in% c("L", "D")) {
      this_seq <- seq(from = -1, to = pull(movement, steps) * -1)
    } else {
      this_seq <- seq(from = 1, to = pull(movement, steps))
    }
    
    if (pull(movement, dir) %in% c("L", "R")) {
      new_x = pull(state, x) + this_seq
      new_y = rep(pull(state, y), length(new_steps))
    } else {
      new_y = pull(state, y) + this_seq
      new_x = rep(pull(state, x), length(new_steps))
    }
  
    path <- tibble(x = new_x,
                   y = new_y,
                   steps = new_steps)
    
    visited <- bind_rows(visited, path)
  }
  
  return(visited)
}

# Question 1 --------------------------------------------------------------
wires <- prep_data(d)
paths <- map(wires, trace_wire)
intersects <- inner_join(paths[[1]], paths[[2]], 
                         by = c("x", "y")) %>%
  filter(x != 0)

answer1 <- intersects %>%
  mutate(distance = abs(x) + abs(y)) %>%
  top_n(-1, wt = distance) %>%
  pull(distance)

answer1

# Question 2 --------------------------------------------------------------

answer2 <- intersects %>%
  mutate(steps = steps.x + steps.y) %>%
  top_n(-1, wt = steps) %>%
  pull(steps)

answer2
