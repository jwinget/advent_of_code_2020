# Day 3

library(here)
library(tidyverse)

# Starting at the top-left corner of your map and following a slope of right 3 and down 1, 
# how many trees would you encounter?

map <- read_lines(here("data", "day_3_input.txt"))

transit_map <- function(map, steps) {
  ending_row <- length(map) + 1
  map_width <- nchar(map[1])
  
  starting_position <- c(1,1)
  current_position <- starting_position
  trees <- 0
  
  while(current_position[1] < ending_row) {
    # Check if we're sitting on a tree
    this_char <- substr(map[current_position[1]], 
                        current_position[2], 
                        current_position[2])
    if(this_char == "#") {
      trees <- trees + 1
    }
    
    # Now move
    current_position[1] <- current_position[1] + steps[2]
    
    if(current_position[2] + steps[1] > map_width) {
      total <- current_position[2] + steps[1]
      current_position[2] <- total %% map_width
    } else {
      current_position[2] <- current_position[2] + steps[1]
    }
  }
  
  return(trees)
}

answer1 <- transit_map(map, c(3,1))
answer1

# Part 2
slopes <- list(c(1, 1), c(3, 1), c(5,1), c(7, 1), c(1, 2))

trees <- map(slopes, ~transit_map(map, .))
trees

answer2 <- prod(unlist(trees))
answer2
