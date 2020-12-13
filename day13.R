Day <- 13

# Load libraries ----------------------------------------------------------

library(here)
library(glue)
library(tidyverse)

# Read data ---------------------------------------------------------------

d <- readLines(here("data", glue("day_{Day}_input.txt")))

# Functions ---------------------------------------------------------------
generate_departures <- function(d) {
  leave_time <- as.numeric(d[1])
  busses <- as.numeric(strsplit(d[2],",")[[1]][which(strsplit(d[2],",")[[1]] != "x")])
  
  max_leave <- leave_time + max(busses*2)
  
  # Find the closest leave time
  res <- busses %>%
    as_tibble() %>%
    rowwise() %>%
    mutate("leave_times" = list(seq(from = 0, to = max_leave, by = value))) %>%
    unnest(cols = c(leave_times)) %>%
    mutate(leave_diff = leave_times - leave_time) %>%
    filter(leave_diff > 0) %>%
    top_n(-1, wt = leave_diff)
  
  return(res$value * res$leave_diff)
}

# Question 1 --------------------------------------------------------------
answer1 <- generate_departures(d)
answer1

# Question 2 --------------------------------------------------------------

answer2