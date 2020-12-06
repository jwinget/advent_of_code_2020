Day <- 6

# Load libraries ----------------------------------------------------------

library(here)
library(glue)
library(tidyverse)

# Read data ---------------------------------------------------------------

d <- readLines(here("data", glue("day_{Day}_input.txt")))

# Functions ---------------------------------------------------------------

group_count <- function(d) {
  group_str <- list()
  group_sum <- ""
  i <- 1
  
  # Split the data by group
  for(line in d) {
    if(nchar(line) == "0") {
      # New group. Assign count to list and reset
      group_str[[i]] <- group_sum
      group_sum <- ""
      i <- i + 1
    } else {
      group_sum <- glue("{group_sum}{line}")
    }
  }
  # Write the last record
  group_str[[i]] <- group_sum
  
  # Calculate the sums
  res <- map(group_str, ~sum(!!str_count(., letters)))
  
  return(sum(unlist(res)))
}

group_count_2 <- function(d) {
  # I'm going to do this a very naive and inefficient way
  group_str <- list()
  i <- 1
  
  # Split the data by group
  for(line in d) {
    if(nchar(line) == "0") {
      # New group
      i <- i + 1
    } else {
      # Still in the same group. Check
      if(length(group_str) < i) {
        # First entry in this group. Initialize
        group_str[[i]] <- strsplit(line, "")[[1]]
      } else {
        matches <- intersect(strsplit(line, "")[[1]],
                         group_str[[i]])
        group_str[[i]] <- matches
      }
    }
  }
  # Write the last record
  #group_str[[i]] <- group_sum
  
  # Calculate the sums
  res <- map(group_str, length)
  
  return(sum(unlist(res)))
}
# Question 1 --------------------------------------------------------------
answer1 <- group_count(d)
answer1

# Question 2 --------------------------------------------------------------
answer2 <- group_count_2(d)
answer2
