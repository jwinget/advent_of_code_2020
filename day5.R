Day <- 5

# Load libraries ----------------------------------------------------------

library(here)
library(glue)
library(tidyverse)

# Read data ---------------------------------------------------------------

d <- readLines(here("data", glue("day_{Day}_input.txt")))

# Functions ---------------------------------------------------------------

binary_partition <- function(chars, range) {
  search_range <- range
  for(char in chars) {
    if (char %in% c("F", "L")) {
      search_range[2] <- search_range[1] + floor((search_range[2] - search_range[1]) / 2)
  } else if (char %in% c("B", "R")) {
    search_range[1] <- search_range[2] - floor((search_range[2] - search_range[1]) / 2)
  }
  }
 
  if(chars[length(chars)] %in% c("F", "L")) {
    return(search_range[1])
  } else {
    return(search_range[2])
  }
}

parse_pass <- function(bpstr) {
  chars <- strsplit(bpstr, "")[[1]]
  row <- binary_partition(chars[1:7], c(0,127))
  column <- binary_partition(chars[8:10], c(0,7))
  return(c(row, column))
}

calc_seat_id <- function(position) {
  return((position[1] * 8) + position[2])
}

# Question 1 --------------------------------------------------------------
positions <- map(d, parse_pass)
seat_ids <- map(positions, calc_seat_id)

answer1 <- max(unlist(seat_ids))
answer1

# Question 2 --------------------------------------------------------------
listed_ids <- sort(unlist(seat_ids))
complete_list <- seq(from = listed_ids[1], to = listed_ids[length(listed_ids)])
answer2 <- setdiff(complete_list, listed_ids)
answer2
